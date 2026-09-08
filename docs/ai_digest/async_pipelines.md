# Async Local Pipelines: Raw Lisp Tasks and Zero-Buffering Streaming

ChrysaLisp is designed from first principles as a distributed, message-passing,
MIMD (Multiple Instruction, Multiple Data) operating system. In distributed
topologies, computation naturally distributes across cores and networked nodes
using load-balanced pipelines (`|`) and resilient task pools (`Farm`).

However, another critical computing domain exists at the opposite end of the
spectrum: **local, high-bandwidth, latency-critical data streaming**.

In operations such as real-time media decoding -- decoding multi-megabyte CPM
images or streaming 60 FPS `.FLM` video animations -- traditional synchronous,
multi-pass decompression models create severe memory bloat and latency spikes.

This document details a powerful ChrysaLisp pattern: **Async Local Pipelines**.
By combining the ability to execute **raw Lisp-level source directly as an
inline task** with **local node pinning (`+kn_call_open`)** and **back-to-front
stream handshaking**, developers can construct multi-stage producer-consumer
pipelines with **zero full-frame intermediate buffers**.

## 1. The Bottleneck of Synchronous Multi-Pass Buffering

To appreciate the async pipeline model, consider the canonical CPM/FLM image
format (`lib/image/cpm.inc`). A compressed CPM image frame consists of layered
compression algorithms applied sequentially:

```code
[Raw File / Network Stream]
            |
            v
Layer 1: LZ4 Compression
            |
            v
Layer 2: RLE Compression (Token-bounded Run-Length Encoding)
            |
            v
Layer 3: Pixmap Pixel Formatting (12, 15, 16, 24, or 32-bit ARGB/RGB)
            |
            v
[Target Canvas]
```

### The Legacy Synchronous Implementation

Traditionally, a loader decompresses these layers sequentially using intermediate
in-memory buffers:

```vdu
; Legacy synchronous approach:
(defq source_stream stream)

; Stage 1: Decompress entire frame with LZ4 into intermediate buffer
(when lz4
	(defq lz4_stream (memory-stream))
	(lz4-decompress source_stream lz4_stream)
	(stream-seek lz4_stream 0 0)
	(setq source_stream lz4_stream))

; Stage 2: Decompress entire frame with RLE into a second intermediate buffer
(when rle
	(defq rle_stream (memory-stream))
	(rle-decompress source_stream rle_stream num_bits 8 (* w h))
	(stream-seek rle_stream 0 0)
	(setq source_stream rle_stream))

; Stage 3: Read from final intermediate buffer into target pixmap
(pixmap-read pixmap source_stream type)
```

### The Three Fundamental Flaws

1. **Massive Memory Footprint & Allocator Churn:**
   For an 800x600 32-bit image (~1.92 MB uncompressed), Stage 1 allocates a full
   intermediate `(memory-stream)` buffer, and Stage 2 allocates *another* 1.92 MB
   `memory-stream` buffer. The system temporarily consumes 3x to 4x the image's
   memory size. In film playback (`.FLM`) running at 30-60 FPS, constantly
   allocating, expanding, and freeing multi-megabyte buffers causes severe heap
   fragmentation and triggers frequent garbage collection pauses.

2. **Serialized Latency & CPU Starvation:**
   Stage 2 cannot process a single byte until Stage 1 has decompressed 100% of the
   frame. Stage 3 cannot draw a single pixel until Stage 2 has fully completed.
   If each stage takes 5 milliseconds, the user waits 15 milliseconds. On modern
   multi-core systems, remaining CPU cores sit completely idle while a single core
   plows through each stage in isolation.

3. **Poor Cache Locality:**
   By the time Stage 2 reads the bytes written at the start of Stage 1, those
   bytes have long been evicted from CPU L1/L2 caches.

## 2. The Core Primitive: Spawning Raw Lisp Source as a Task

In standard operating systems, spawning a process requires an executable binary
on disk or an external script file path. Passing transient parameters requires
command-line arguments, environment variables, or complex IPC setup.

In ChrysaLisp, **code is data, and data is code**.

The kernel primitive `(open-child script [flags])` inspects the `script` parameter:

* If `script` is a file path (e.g., `"cmd/player.lisp"`), the child task loads and
  evaluates that file.

* **If `script` is a string beginning with `'('`**, the kernel recognizes it as
  **raw, inline Lisp source**. The child task bypasses filesystem I/O entirely,
  instantiating its task context and evaluating the S-expression directly from
  memory!

### Metaprogramming with Quasiquote Templates

Because ChrysaLisp S-expressions evaluate cleanly, we can generate child task
definitions dynamically using quasiquote (`` ` ``) and unquote (`,`):

```vdu
(defun cpm-load-stage-lz4 (downstream_mbox stream)
	(str `(progn
		(import "lib/streams/lz4.inc")
		(lz4-decompress (obj-ref ,(weak-ref stream))
			(out-stream (hex-decode ,(hex-encode downstream_mbox)))))))
```

Notice what happens here:

* `(str `(progn ...))` formats the S-expression into a compact, single-line string
  starting with `'('`.

* The template executes immediately upon child task startup with zero disk
  overhead.

* Runtime objects -- such as communication mailboxes and parent stream handles -- are
  interpolated directly into the child task's lexical definition.

## 3. Shared Memory Safety & Node Pinning

When tasks run across a ChrysaLisp cluster, data must be serialized across link
drivers. But for local media decoding, serialization would defeat the purpose.
We need zero-copy shared memory access.

Two architectural mechanisms make this safe and fast:

### 1. Node Pinning (`+kn_call_open`)

The standard task spawn flag `+kn_call_child` delegates placement to the kernel's
emergent load balancer, which might slip the task to a neighboring core or node.

To ensure tasks share the same physical address space, we use **`+kn_call_open`**:

```vdu
(open-child (cpm-load-stage-pixmap pixmap type handshake_mbox done_mbox) +kn_call_open)
```

`+kn_call_open` strictly pins the child task to the **exact same hardware node
and memory context** as the parent. Pointers into heaps and memory streams remain
100% valid across both tasks without any cross-node proxying.

### 2. The `weak-ref` / `obj-ref` Lifecycle Pattern

Passing object pointers across tasks requires careful reference counting. If
the parent interpolates a raw pointer, how do we prevent the parent's GC from
reclaiming the object while the child is still executing? Conversely, how do we
avoid leaking reference counts?

ChrysaLisp solves this with the `weak-ref` / `obj-ref` pair:

1. **Parent side:** `(weak-ref obj)` extracts the raw memory address as a number
   without incrementing the reference count in the template string.

2. **Child side:** `(obj-ref ,(weak-ref obj))` increments the object's reference
   count (`+obj_count`) when the child task starts up, declaring formal ownership.

3. **Child exit:** When the child task finishes its `progn` and exits, its lexical
   environment unwinds (`env-pop`), automatically decrementing the reference
   count. If the parent has already finished, the object cleans up immediately
   without leaks.

### 3. Hex-Encoded Mailbox Handles

Mailboxes are represented by IDs or structures. When embedded in Lisp templates,
unquoted characters or signed representations can cause reader errors. Passing
mailbox handles as hex-encoded string tokens ensures robust serialization:

```vdu
(mail-send (hex-decode ,(hex-encode handshake_mbox)) (in-mbox in))
```

## 4. The Solution: An Inline Asynchronous Streaming Pipeline

Instead of allocating intermediate `(memory-stream)` buffers, we model decompression
as an **asynchronous pipeline of concurrent streaming workers**:

```code
[File / Network Stream]
          |
          v
[Stage 1: LZ4 Worker]
          | (IPC stream)
          v
[Stage 2: RLE Worker]
          | (IPC stream)
          v
[Stage 3: Pixmap Consumer]
          | (direct write)
          v
[Target Canvas]
```

Data flows between stages through ChrysaLisp **IPC streams**:

* `(in-stream)` allocates an IPC input stream backed by a mailbox.

* `(out-stream mbox)` creates a streaming output sink that writes chunks into
  the recipient's mailbox.

* As soon as an upstream stage writes a chunk of bytes, the downstream stage
  wakes up, consumes it, and forwards its output to the next stage.

* **No stage ever buffers more than a few kilobytes at a time.** Intermediate
  full-frame buffers are completely eliminated!

## 5. Back-to-Front Wiring & The Handshake Protocol

A key engineering challenge in streaming pipelines is initialization order:
**a consumer must create its input channel before a producer can connect to it.**

If the parent attempted to launch Stage 1 first, Stage 1 would have nowhere to
send its output because Stage 2 does not exist yet.

ChrysaLisp pipelines resolve this by **wiring back-to-front** using an ephemeral
`handshake_mbox`:

```code
Step 1: Parent creates handshake_mbox and done_mbox.

Step 2: Launch Consumer (Stage 3: Pixmap)
        Stage 3 creates (in-stream), sends its (in-mbox in) to handshake_mbox.
        Parent reads downstream_mbox from handshake_mbox.

Step 3: Launch Filter (Stage 2: RLE)
        Stage 2 connects its (out-stream) to downstream_mbox (Stage 3).
        Stage 2 creates (in-stream), sends its (in-mbox in) to handshake_mbox.
        Parent reads new downstream_mbox from handshake_mbox.

Step 4: Launch Producer (Stage 1: LZ4)
        Stage 1 connects its (out-stream) to downstream_mbox (Stage 2).
        Stage 1 reads directly from the source stream.

Step 5: Pipeline Executes Concurrently!
        Stage 1 decompresses LZ4 chunks -> Stage 2.
        Stage 2 decompresses RLE tokens -> Stage 3.
        Stage 3 writes pixels directly into Pixmap memory.

Step 6: Completion
        Stage 3 finishes reading and sends a done token to done_mbox.
        Parent reads done_mbox and returns the completed Canvas.
```

## 5. Concrete Implementation: `CPM-load` and `CPM-save`

The complete asynchronous streaming architecture in `lib/image/cpm.inc` implements
both decoding (`CPM-load`) and encoding (`CPM-save`) with matching stage names,
bidirectional streaming, and zero full-frame intermediate buffers.

### 5.1 Decoding Pipeline (`CPM-load`)

When reading a CPM image or FLM video frame:

```code
[Source Stream] -> [Stage 1: LZ4] -> [Stage 2: RLE] -> [Stage 3: Pixmap Consumer]
```

#### Stage 1: LZ4 Worker

```vdu
(defun cpm-load-stage-lz4 (downstream_mbox stream)
	(str `(progn
		(import "lib/streams/lz4.inc")
		(lz4-decompress (obj-ref ,(weak-ref stream))
			(out-stream (hex-decode ,(hex-encode downstream_mbox)))))))
```

#### Stage 2: RLE Worker

```vdu
(defun cpm-load-stage-rle (downstream_mbox upstream_stream_or_mbox num_bits max_tokens &optional handshake_mbox)
	(if handshake_mbox
		; Chained stage: reads from an upstream IPC in-stream
		(str `(progn
			(import "lib/streams/rle.inc")
			(mail-send (hex-decode ,(hex-encode handshake_mbox)) (in-mbox (defq in (in-stream))))
			(rle-decompress in (out-stream (hex-decode ,(hex-encode downstream_mbox))) ,num_bits 8 ,max_tokens)))
		; Root stage: reads directly from the source stream
		(str `(progn
			(import "lib/streams/rle.inc")
			(rle-decompress (obj-ref ,(weak-ref upstream_stream_or_mbox))
				(out-stream (hex-decode ,(hex-encode downstream_mbox)))
				,num_bits 8 ,max_tokens)))))
```

#### Stage 3: Pixmap Consumer

```vdu
(defun cpm-load-stage-pixmap (pixmap type handshake_mbox done_mbox)
	(str `(progn
		(import "gui/pixmap/lisp.inc")
		(mail-send (hex-decode ,(hex-encode handshake_mbox))
			(in-mbox (defq pixmap (obj-ref ,(weak-ref pixmap)) in (in-stream))))
		(mail-send (hex-decode ,(hex-encode done_mbox))
			(if (pixmap-read pixmap in ,type) (str (weak-ref pixmap)) "")))))
```

#### Pipeline Orchestration (`CPM-load`)

```vdu
(cond
	; Fast path: raw uncompressed CPM (neither LZ4 nor RLE)
	((not (or lz4 rle))
		(ifn (pixmap-read pixmap stream type)
			(setq canvas :nil)))

	; Async pipeline path:
	(:t
		(defq handshake_mbox (mail-mbox) done_mbox (mail-mbox))

		; 1. Launch Consumer (Stage 3: Pixmap)
		(open-child (cpm-load-stage-pixmap pixmap type handshake_mbox done_mbox) +kn_call_open)
		(defq downstream_mbox (mail-read handshake_mbox))

		; 2. Launch RLE (Stage 2) if present
		(when rle
			(open-child (cpm-load-stage-rle downstream_mbox (if lz4 :nil stream) num_bits (* w h) (if lz4 handshake_mbox)) +kn_call_open)
			(if lz4 (setq downstream_mbox (mail-read handshake_mbox))))

		; 3. Launch LZ4 (Stage 1) if present
		(when lz4
			(open-child (cpm-load-stage-lz4 downstream_mbox stream) +kn_call_open))

		; 4. Wait for consumer completion
		(defq res (mail-read done_mbox))
		(ifn (and res (eql res (str (weak-ref pixmap))))
			(setq canvas :nil))))
```

### 5.2 Encoding Pipeline (`CPM-save`)

Saving a CPM image performs the inverse multi-stage compression:

```code
[Stage 1: Pixmap Producer] -> [Stage 2: RLE Filter] -> [Stage 3: LZ4 Consumer] -> [Destination Stream]
```

#### Stage 1: Pixmap Producer

```vdu
(defun cpm-save-stage-pixmap (pixmap downstream_mbox type)
	(str `(progn
		(import "gui/pixmap/lisp.inc")
		(pixmap-write (pixmap-as-argb (obj-ref ,(weak-ref pixmap)))
			(out-stream (hex-decode ,(hex-encode downstream_mbox)))
			,type))))
```

#### Stage 2: RLE Filter

```vdu
(defun cpm-save-stage-rle (downstream_stream_or_mbox num_bits handshake_mbox &optional done_mbox)
	(if done_mbox
		(str `(progn
			(import "lib/streams/rle.inc")
			(mail-send (hex-decode ,(hex-encode handshake_mbox)) (in-mbox (defq in (in-stream))))
			(rle-compress in (obj-ref ,(weak-ref downstream_stream_or_mbox)) ,num_bits 8)
			(mail-send (hex-decode ,(hex-encode done_mbox)) :t)))
		(str `(progn
			(import "lib/streams/rle.inc")
			(mail-send (hex-decode ,(hex-encode handshake_mbox)) (in-mbox (defq in (in-stream))))
			(rle-compress in (out-stream (hex-decode ,(hex-encode downstream_stream_or_mbox))) ,num_bits 8)))))
```

#### Stage 3: LZ4 Consumer

```vdu
(defun cpm-save-stage-lz4 (stream handshake_mbox done_mbox)
	(str `(progn
		(import "lib/streams/lz4.inc")
		(mail-send (hex-decode ,(hex-encode handshake_mbox)) (in-mbox (defq in (in-stream))))
		(lz4-compress in (obj-ref ,(weak-ref stream)))
		(mail-send (hex-decode ,(hex-encode done_mbox)) :t))))
```

#### Pipeline Orchestration (`CPM-save`)

```vdu
(cond
	; Fast path: raw uncompressed CPM (neither LZ4 nor RLE)
	((not (or lz4 rle))
		(pixmap-write (pixmap-as-argb pixmap) stream type))

	; Async pipeline path:
	(:t
		(defq handshake_mbox (mail-mbox) done_mbox (mail-mbox) downstream_mbox :nil)

		; 1. Launch LZ4 (Stage 3: Consumer) if present
		(when lz4
			(open-child (cpm-save-stage-lz4 stream handshake_mbox done_mbox) +kn_call_open)
			(setq downstream_mbox (mail-read handshake_mbox)))

		; 2. Launch RLE (Stage 2) if present
		(when rle
			(open-child (cpm-save-stage-rle (if lz4 downstream_mbox stream) num_bits handshake_mbox (if lz4 :nil done_mbox)) +kn_call_open)
			(setq downstream_mbox (mail-read handshake_mbox)))

		; 3. Launch Pixmap (Stage 1: Producer)
		(open-child (cpm-save-stage-pixmap pixmap downstream_mbox type) +kn_call_open)

		; 4. Wait for consumer completion
		(mail-read done_mbox)))
```

### 5.3 On-Demand Codec Import

Because each pipeline stage dynamically imports only what it needs (`lib/streams/lz4.inc`,
`lib/streams/rle.inc`, `gui/pixmap/lisp.inc`) inside its own spawned child task, the
top-level module `lib/image/cpm.inc` requires zero unconditional compression library imports.
Loading `cpm.inc` introduces zero compression memory footprint until a compressed image
is actually loaded or saved.
```

## 6. Comparison & Real-World Impact

| Dimension | Legacy Synchronous Buffers | Async Local Pipeline |
| :--- | :--- | :--- |
| **Intermediate Memory** | **200% to 400%** of uncompressed frame size | **~0%** (bounded flyweight IPC stream chunks) |
| **Heap Allocations** | Multiple multi-MB `memory-stream` buffers per frame | Zero frame buffers; transient task mail packets only |
| **Execution Latency** | Sum of all stages (strictly serialized) | Pipelined (overlapped across stages) |
| **CPU Core Utilization** | 1 core active; all other cores idle | Concurrent execution across available local cores |
| **GC Impact** | Heavy GC pressure during 30-60 FPS video playback | Negligible heap churn; rock-solid memory stability |
| **Code Modularity** | Monolithic decompression loops in caller | Decoupled, reusable stage generators |

### Verification in ChrysaLisp

This architecture was validated directly inside the ChrysaLisp GUI environment:

* **Film Player (`apps/media/film/app.lisp`):** Plays high-framerate `.FLM`
  animations smoothly without dropped frames or stutter caused by GC collection.

* **Image Viewer (`apps/media/image/app.lisp`):** Loads large compressed `.CPM`
  images instantaneously, seamlessly handling 12-bit, 15-bit, 16-bit, and 32-bit
  pixel conversions directly into canvas memory.

## 7. Summary & Architectural Takeaways

The Async Local Pipeline demonstrates why ChrysaLisp's unified architecture is
so uniquely capable:

1. **Inline Tasks are Flyweight:** Because tasks are lightweight and the Lisp
   reader can evaluate strings directly, there is virtually zero penalty to
   spawning micro-tasks on the fly for ephemeral operations.

2. **`+kn_call_open` Enables Shared-Memory Concurrency:** Node pinning removes the
   boundary between distributed message passing and local multi-threaded
   programming, giving developers the safety of message passing with the raw
   throughput of shared memory.

3. **Back-to-Front Handshaking Solves Streaming Topologies:** Dynamic mailboxes
   allow consumers and producers to rendezvous and wire streaming channels
   dynamically with zero static configuration.

Whenever you face a multi-stage data transformation -- whether it is video decoding,
audio DSP synthesis, cryptographic hashing, or packet parsing -- consider replacing
intermediate buffers with an **Async Local Pipeline**.
