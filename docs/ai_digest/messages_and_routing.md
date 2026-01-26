# ChrysaLisp Network & IPC Architecture

ChrysaLisp implements a location-transparent, message-passing microkernel
architecture. Communication between tasks uses the same semantics whether the
tasks are on the same core, different cores, or different physical machines.
This document details the low-level Link Drivers, the fragmentation of large
messages, the routing algorithms, and the system's self-cleaning mechanisms.

## 1. The Link Drivers (`sys/link`)

The Link Driver is the bridge between the logical message queue and the physical
transport layer (Shared Memory). It provides a lock-free, ring-buffered
communication channel between two nodes.

Other link types are available, see the `ChrysaLib` github project for machine
to machine, USB and IP link drivers in use, to bridge physical machines via the
same abstraction.

### Shared Memory Structure

Defined in `sys/link/class.inc`, the shared memory region (`lk_shmem`) is mapped
into the address space of both communicating nodes. It is divided into two
unidirectional channels.

* **Size:** 4KB (`lk_page_size`)

* **Topology:**

	* `chan_1` (Write for Node A, Read for Node B)

	* `chan_2` (Write for Node B, Read for Node A)

	* `towel` (Arbitration flag)

**The "Towel" Protocol:**

To determine which node writes to `chan_1` vs `chan_2`, the driver uses a
"towel" concept (found in `sys/link/class.vp`). When a link initializes:

1. It checks `lk_shmem_towel`.

2. If 0, it writes its own `node_id`. It becomes the "Owner" (writes to
   `chan_1`).

3. If not 0, it reads the value. It becomes the "Guest" (writes to `chan_2`).

### Channel Protocol (`lk_chan`)

Each channel contains a ring of 3 buffers (`msg0`, `msg1`, `msg2`). This
triple-buffering allows for continuous throughput where one node writes while
the other reads.

**Buffer State (`lk_buf`):**

* **`lk_chan_status_ready` (0):** The buffer is empty. The Writer may write to
  it.

* **`lk_chan_status_busy` (1):** The buffer contains data. The Reader may
  process it.

### Transmission (`:out` task)

The `:out` task (in `sys/link/class.vp`):

1. Monitors the current write-buffer in its assigned channel.

2. Waits for status to become `ready`.

3. Queries the Mail system via `:sys_mail :ready` for any messages waiting for
   this link's peer.

4. If a message exists:

	* Copies the message header/payload into the shared memory buffer.

	* Sets status to `busy`.

	* Advances to the next buffer in the ring.

5. If no message exists, it sends a **Ping** (empty payload with `total_length`
   = -1) to maintain routing table aliveness and exchange load-balancing stats
   (`task_count`).

### Reception (`:in` task)

The `:in` task (in `sys/link/class.vp`):

1. Monitors the current read-buffer in its assigned channel.

2. Waits for status to become `busy`.

3. Reads the header.

4. **Routing:**

	* If it is a Ping, it updates local load-balancing stats.

	* If it is a Data Fragment destined for *this* node, it calls `:sys_mail :in`
	  to handle reassembly.

	* If it is destined for *another* node, it allocates a new message container,
	  copies the data, and reinjects it into the local mail system via `:sys_mail
	  :send` for forwarding.

5. Sets status to `ready` (signaling the sender can reuse the slot).

6. Advances to the next buffer.

## 2. Fragmentation and Reassembly (`sys/mail`)

ChrysaLisp supports messages larger than the physical link packet size
(`lk_data_size`, 984 bytes). Large messages ("Parcels") are broken down by a
"Postman" task and reassembled by the receiver kernel.

### Fragmentation (`sys/mail/out.vp`)

When `:sys_mail :send` detects a message larger than `lk_data_size`, it queues
it to the **Postman** task (`:sys_mail :out`).

1. **Session ID:** A unique session ID is incremented for the transaction.

2. **Loop:** The Postman iterates over the source payload.

3. **Packet Allocation:** It allocates small fragments (size <= 984 bytes).

4. **Header Generation:**

	* `frag_offset`: The byte offset of this chunk.

	* `total_length`: The total size of the original message.

	* `frag_length`: The size of this specific chunk.

5. **Queueing:** Fragments are pushed to the `offchip_list`, intermixed with
   other traffic to prevent a large message from blocking the network.

### Reassembly (`sys/mail/in.vp`)

The `:sys_mail :in` method handles incoming fragments from the Link Driver.

1. **Lookup:** It searches the `statics_sys_mail_parcel_list` for an existing
   reassembly buffer matching the `src` NetID.

2. **New Parcel:** If not found (and `frag_offset` is valid), it allocates a
   buffer of size `total_length`.

3. **Copy:** It copies the fragment data from the Link Shared Memory directl y
   into the Parcel heap object at `frag_offset`.

4. **Tracking:** It decrements a tracking counter on the Parcel object by
   `frag_length`.

5. **Completion:** When the tracking counter reaches 0 (all bytes received), the
   completed Parcel is removed from the reassembly list and delivered to the
   destination mailbox via `:sys_mail :send`.

## 3. Kernel Routing

ChrysaLisp uses a decentralized, flood-fill-style routing algorithm to discover
paths and services.

### Node Mapping (`node_map`)

The kernel maintains a `node_map` (`sys/statics/class.inc`), mapping a target
`node_id` to a routing tuple: `[service_set, timestamp, session, hops,
via_node_1, via_node_2]`

* **via_node:** The immediate neighbor to send messages to in order to reach the
  target.

### The Ping Cycle (`sys/kernel/class.vp`)

Every `ping_period` (approx 5 seconds + random jitter), the Kernel `:ping` task
runs:

1. **Broadcast:** Sends a `kn_msg_ping` to all immediate neighbors via the Link
   Drivers.

2. **Payload:** Contains the sender's origin ID, session ID, hop count
   (initially 0), and a list of offered services.

### Flood Fill Logic

When a kernel receives a Ping (`kn_call_ping` in `sys/kernel/class.vp`):

1. **New Session:** If the Ping `session` > known session for that Origin:

	* Update table with new session/timestamp.

	* Flood the Ping to all *other* links (incrementing `hops`).

2. **Better Route:** If the Ping `session` matches but `hops` < known hops:

	* Update table setting the "Via" to the link the Ping arrived on.

	* Flood the Ping.

3. **Equal Route:** If `hops` are equal:

	* Add the link to the list of valid "Via" nodes (multipath routing).

	* Do *not* flood (prevents storms).

4. **Worse Route:** Discard.

### Message Routing

When `:sys_mail :ready` is called by a Link Driver (looking for work):

1. It scans the `offchip_list` (outgoing queue).

2. It extracts the Destination Node ID.

3. It looks up the Destination in `node_map`.

4. It checks the "Via" entry.

5. If **Via == Peer Node of the Link**, the message is authorized for
   transmission on that link.

## 4. Mail Purging (Garbage Collection)

Because the network is formless and nodes may disappear, the system must
aggressively clean up stale state to prevent memory leaks and zombie routing
entries.

### The Purge Cycle

The Kernel `:ping` task (in `sys/kernel/class.vp`) triggers cleanup logic at the
end of every cycle.

### 1. Route Purging

* **Target:** `statics_sys_mail_node_map`

* **Logic:** A callback (`purge_callback`) checks the timestamp of every entry
  in the routing table.

* **Threshold:** If `last_seen < (current_time - ping_period * 2)`, the node is
  presumed dead/disconnected.

* **Action:** The route and associated service lists are removed from the map.

### 2. Message Purging

* **Target:** `statics_sys_mail_offchip_list` (Outgoing queue)

* **Logic:** `purge_mail` iterates the list.

* **Threshold:** If `msg_timestamp < (current_time - ping_period * 2)`.

* **Action:** The message is freed. This prevents messages destined for
  unreachable nodes from clogging the outgoing queue indefinitely.

### 3. Parcel Purging (Reassembly)

* **Target:** `statics_sys_mail_parcel_list` (Incoming reassembly)

* **Logic:** `purge_mail` iterates the list.

* **Threshold:** Same as above.

* **Action:** Incomplete parcels (where parts were lost or the sender died) are
  freed.

This architecture ensures that the system self-heals after network partitions or
node failures without manual intervention.