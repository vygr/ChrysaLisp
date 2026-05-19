# ChrysaLisp Network & IPC Architecture

ChrysaLisp implements a location-transparent, message-passing microkernel
architecture. Communication between tasks uses the same semantics whether the
tasks are on the same core, different cores, or different physical machines.
This document details the low-level Link Drivers, the fragmentation of large
messages, the routing algorithms, and the system's self-cleaning mechanisms.

## 1. The Link Drivers (`sys/link`)

The Link Driver is the bridge between the logical message queue and the physical
transport layer (Shared Memory). It provides a lock-free, continuous
byte-ring-buffered communication channel between two nodes.

### Shared Memory Structure

Defined in `sys/link/class.inc`, the shared memory region (`lk_shmem`) is mapped
into the address space of both communicating nodes. It is divided into two
unidirectional continuous ring buffers, padded to page alignment:

* `chan_1` (Write for Node A, Read for Node B)

* `chan_2` (Write for Node B, Read for Node A)

**The Ownership Protocol:**

To determine which node uses which channel, the driver uses a robust negotiation
protocol during initialization (in `sys/link/class.vp` `:link` task):

1. The node checks if `host_a` (embedded in the alignment padding) is clear
   (`mem->lk_shmem_host_a` == 0).

2. If it is 0, it attempts to claim leadership by writing its `node_id` to
   `host_a`. It then waits a short period (100 microseconds) for any bus
   propagation or simultaneous write collisions to settle.

3. It then re-reads `host_a`. If it matches its own ID, it has won leadership.
   It sets itself as the `owner`, using `chan_1` for transmission and `chan_2`
   for reception.

4. If it doesn't match, it is the `guest`. It registers its `node_id` in
   `host_b` to signal its presence to the owner. It uses `chan_2` for
   transmission and `chan_1` for reception.

5. Both nodes wait for both `host_a` and `host_b` to be non-zero before
   starting the TX and RX tasks, ensuring a fully established link.

### Channel Protocol (`lk_chan`)

Unlike earlier fixed-slot designs, the modern protocol treats the channel as a
continuous byte ring buffer. Data is written in variable-sized chunks. Each
chunk is prefixed with a header (`lk_buf`) containing the `length` and a
`status`.

**Buffer States (`lk_chan_status`):**

* **`ready` (0):** The buffer segment is empty and available for the Writer.

* **`ping` (1):** Contains load-balancing/telemetry data (`task_count`).

* **`frag` (2):** Contains a data fragment.

* **`skip` (3):** Padding to instruct the Reader to wrap around to the start of
  the ring.

### Transmission (`:out` task)

The `:out` task (`sys/link/out.vp`) manages transmission:

1. **Space Check:** It calculates the `free` space between its write head and
   the reader's tail. If insufficient, it yields.

2. **Ping / Load Balancing:** If the local kernel's `task_count` has changed
   since the last update, it writes a `ping` status block sharing the new load.

3. **Queue Polling:** It queries `:sys_mail :ready` for messages destined for
   the peer.

4. **Fragmentation & Write:**

   * It maps the message into an `lk_frag` header (dest, src, length, offset,
     total).

   * It copies the payload into the ring using the wrap-aware `:sys_mem
     :copy_to_ring`.

   * It commits the write by setting the buffer status to `frag`.

5. **Wrap-Around:** Because headers must be contiguous, if the write head nears
   the end of the linear memory block and there isn't room for a header, it
   writes a `skip` status to advance the reader's pointer back to 0.

### Reception (`:in` task)

The `:in` task (`sys/link/in.vp`) handles incoming ring data:

1. **Poll:** It spins/yields, waiting for the head's `status` to change from
   `ready`.

2. **Process by Status:**

   * **`skip` (3):** Wraps the read pointer back to the start of the ring.

   * **`ping` (1):** Updates the local `lk_node` struct with the peer's current
     `task_count` (used for emergent load balancing).

   * **`frag` (2):**

     * If the fragment's destination `node_id` matches the *local* node, it
       calls `:sys_mail :in` for zero-copy reassembly.

     * If routing *through* this node, it allocates a new message, copies the
       fragment out of the ring, and calls `:sys_mail :send` to forward it.

3. **Reclaim:** Sets the processed buffer's status back to `ready` and advances
   the pointer.

## 2. Fragmentation and Reassembly (`sys/mail`)

Messages larger than `lk_data_size` are broken down into Fragments by a
"Postman" task and reassembled seamlessly by the receiver.

### Fragmentation (`sys/mail/out.vp`)

When `:sys_mail :send` sees a message bound for an off-chip destination, if it
exceeds `lk_data_size`, it routes it to the local Postman task (`:sys_mail
:out`).

1. **Session Management:** The Postman increments a global `sys_mail_session`
   ID.

2. **Loop & Slice:** It iteratively allocates fragments from
   `sys_mail_msg_heap`.

3. **Header Generation:** Each fragment receives:

   * `length`: Size of this chunk.

   * `offset`: Absolute byte offset into the original unfragmented message.

   * `total`: Total length of the original message.

4. **Queueing:** The fragments are queued to the `statics_sys_mail_outbound_list`
   where Link drivers pick them up.

### Reassembly (`sys/mail/in.vp`)

Reassembly is highly optimized and resilient to out-of-order delivery.

1. **Lookup:** When a `frag` arrives for the local node, `:sys_mail :in`
   searches `statics_sys_mail_parcel_list` for a parcel matching the source
   `netid`.

2. **Allocation:** If it's a new parcel, the kernel allocates a single
   contiguous buffer matching the `total` length.

3. **Zero-Copy Insert:** Using the absolute `offset` provided in the fragment
   header, the payload is copied directly from the Link ring buffer into its
   final position in the parcel (`msg->+msg_data + rx_frag->lk_frag_offset`).

4. **Completion:** `msg_total` is decremented by the fragment `length`. When it
   reaches 0, the fully reassembled message is removed from the list and
   dispatched.

## 3. Kernel Routing and Discovery

ChrysaLisp relies on an emergent, decentralized routing algorithm to map the
network.

### Node Mapping (`node_map`)

The kernel maintains a `node_map` (`sys/statics/class.inc`), mapping a target
`node_id` to a routing tuple: `[service_set, timestamp, session, hops,
via_node_1, via_node_2...]`.

* Multipath routing is supported by appending multiple `via_node`s to the tuple.

### The Ping Cycle (`sys/kernel/class.vp`)

Every `ping_period` (5,000,000 microseconds / 5 seconds), the Kernel `:ping`
task:

1. Gathers all local services into a string.

2. Broadcasts a `+kn_msg_ping` message out to the network.

3. The message contains the sender's origin ID, session ID, hop count, and
   services.

### Flood Fill Logic

When a kernel receives a Ping (`flood_fill` in `sys/kernel/class.vp`):

1. **New Session:** If `session` > known session:

   * Clear old routes, record new session/timestamp/hops, add the incoming link
     as the `via` node, and flood the Ping to all *other* links.

2. **Better Route:** If `session` matches but `hops` < known hops:

   * Clear old routes, update hops, set the `via` node, and flood the Ping.

3. **Equal Route:** If `hops` matches known hops:

   * Add the link to the list of valid `via` nodes (enabling multipath). Do
     *not* flood.

4. **Service Update:** The service list is parsed, and the `node_map`'s `hset`
   is updated to reflect the origin's current capabilities.

## 4. Self-Healing and Garbage Collection

To maintain the "Formless" philosophy, nodes must cleanly forget state related
to disconnected or crashed peers. This runs automatically at the end of every
5-second `:ping` cycle.

### 1. Route Purging (`purge_callback`)

* Scans `statics_sys_mail_node_map`.

* If a route's `timestamp` is older than `ping_period * 2` (10 seconds), the
  node is considered dead.

* The route and its advertised services are erased.

### 2. Message Purging (`purge_mail`)

* Scans the `statics_sys_mail_outbound_list` (outgoing queue).

* Any outbound message queued for longer than `ping_period * 2` is freed,
  preventing undeliverable messages from causing memory leaks.

### 3. Parcel Purging (Reassembly Cleanup)

* Scans the `statics_sys_mail_parcel_list` (incoming reassembly queue).

* If a parcel hasn't received a fragment in `ping_period * 2`, it means the
  sender crashed or the network partitioned mid-transmission. The incomplete
  parcel is safely discarded.

## 5. Emergent Load Balancing

Load balancing ("Be like water") is inherently tied to the link protocol:

1. **Local Assessment:** Link drivers share their `task_count` via `ping` buffer
   statuses continuously.

2. **Downhill Flow:** When a task is spawned with `+kn_call_child`, the kernel
   (`sys/kernel/class.vp`) checks the `task_count` of all connected peers.

3. **Delegation:** If peer nodes have a lower `task_count`, the kernel aborts
   local creation, picks a random less-loaded peer from the best candidates, and
   forwards the creation request across the network.
