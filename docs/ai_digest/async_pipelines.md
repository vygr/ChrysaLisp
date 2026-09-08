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
   fragmentation and allocator churn.

2. **Poor Cache Locality & Memory Bottlenecks:**

   Writing full frames back and forth to intermediate memory buffers constantly
   evicts data from CPU L1/L2 caches. By the time Stage 2 reads the bytes written
   at the start of Stage 1, those bytes must be refetched from main RAM. This loss
   of data locality throttles throughput and degrades speed.

3. **Monolithic Inflexibility:**

   The synchronous model requires monolithic caller-side buffering logic. If a
   format supports optional compression layers (such as raw, RLE only, LZ4 only,
   or combined), the caller ends up with nested buffering logic and manual stream
   rewinds rather than clean, composable stages.

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

```file
lib/image/cpm.inc "(defun cpm-load-stage-lz4" ""
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

## 4. The Solution: An Inline Streaming Pipeline

Instead of allocating intermediate `(memory-stream)` buffers, we model processing
as a **streaming pipeline of compositional stages**:

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

* Data flows in small, bounded chunks directly from stage to stage. Data locality
  is preserved because chunks stay hot in CPU cache, drastically speeding up
  execution.

* **No stage ever buffers more than a few kilobytes at a time.** Intermediate
  full-frame buffers are completely eliminated, achieving extreme memory frugality.

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

Step 5: Data Streams Through the Pipeline
        Stage 1 decompresses LZ4 chunks -> Stage 2.
        Stage 2 decompresses RLE tokens -> Stage 3.
        Stage 3 writes pixels directly into Pixmap memory.

Step 6: Completion
        Stage 3 finishes reading and sends a done token to done_mbox.
        Parent reads done_mbox and returns the completed Canvas.
```

## 6. Concrete Implementation: `CPM-load` and `CPM-save`

The complete asynchronous streaming architecture in `lib/image/cpm.inc` implements
both decoding (`CPM-load`) and encoding (`CPM-save`) with matching stage names,
bidirectional streaming, and zero full-frame intermediate buffers.

### 6.1 Decoding Pipeline (`CPM-load`)

When reading a CPM image or FLM video frame:

```code
[Source Stream] -> [Stage 1: LZ4] -> [Stage 2: RLE] -> [Stage 3: Pixmap Consumer]
```

#### Stage 1: LZ4 Worker

```file
lib/image/cpm.inc "(defun cpm-load-stage-lz4" ""
```

#### Stage 2: RLE Worker

```file
lib/image/cpm.inc "(defun cpm-load-stage-rle" ""
```

#### Stage 3: Pixmap Consumer

```file
lib/image/cpm.inc "(defun cpm-load-stage-pixmap" ""
```

#### Pipeline Orchestration (`CPM-load`)

```file
lib/image/cpm.inc "(defun CPM-load" ""
```

### 6.2 Encoding Pipeline (`CPM-save`)

Saving a CPM image performs the inverse multi-stage compression:

```code
[Stage 1: Pixmap Producer]
-> [Stage 2: RLE Filter]
-> [Stage 3: LZ4 Consumer]
-> [Destination Stream]
```

#### Stage 1: Pixmap Producer

```file
lib/image/cpm.inc "(defun cpm-save-stage-pixmap" ""
```

#### Stage 2: RLE Filter

```file
lib/image/cpm.inc "(defun cpm-save-stage-rle" ""
```

#### Stage 3: LZ4 Consumer

```file
lib/image/cpm.inc "(defun cpm-save-stage-lz4" ""
```

#### Pipeline Orchestration (`CPM-save`)

```file
lib/image/cpm.inc "(defun CPM-save" ""
```

### 6.3 On-Demand Codec Import

Because each pipeline stage dynamically imports only what it needs (`lib/streams/lz4.inc`,
`lib/streams/rle.inc`, `gui/pixmap/lisp.inc`) inside its own spawned child task, the
top-level module `lib/image/cpm.inc` requires zero unconditional compression library imports.
Loading `cpm.inc` introduces zero compression memory footprint until a compressed image
is actually loaded or saved.

## 7. Comparison & Real-World Impact

| Dimension | Legacy Synchronous Buffers | Streaming Local Pipeline |
| :--- | :--- | :--- |
| **Intermediate Memory** | **200% to 400%** of uncompressed frame size | **~0%** (bounded flyweight IPC stream chunks) |
| **Data Locality** | Repeated full-frame RAM roundtrips evict CPU caches | Small stream chunks stay hot in fast L1/L2 cache |
| **Heap Allocations** | Multiple multi-MB `memory-stream` buffers per frame | Zero frame buffers; transient stream packets only |
| **Speed & Throughput** | Memory bandwidth and allocator churn throttle speed | Much faster execution due to cache locality and zero churn |
| **Code Modularity** | Monolithic loops and intermediate buffer rewinds | Clean, reusable, compositional stage generators |

### Verification in ChrysaLisp

This architecture was validated directly inside the ChrysaLisp GUI environment:

* **Film Player (`apps/media/film/app.lisp`):** Plays high-framerate `.FLM`
  animations smoothly without dropped frames or latency spikes caused by memory
  churn.

* **Image Viewer (`apps/media/image/app.lisp`):** Loads large compressed `.CPM`
  images with minimal memory footprint, seamlessly handling 12-bit, 15-bit, 16-bit,
  and 32-bit pixel conversions directly into canvas memory.

## 8. Summary & Architectural Takeaways

The Streaming Local Pipeline demonstrates key principles of efficient data processing
in ChrysaLisp:

1. **Memory Frugality Drives Speed:** Reducing the memory footprint has a massive
   impact on execution speed. By eliminating multi-megabyte intermediate buffers,
   data stays in CPU caches and the memory allocator does zero unnecessary work.

2. **Compositional Stages:** Independent stage functions can be composed cleanly
   into any pipeline topology (such as decoding with `CPM-load` or encoding with
   `CPM-save`), enabling flexible format support without rewriting compression logic.

3. **Inline Tasks are Flyweight:** Because tasks are lightweight and the Lisp
   reader can evaluate strings directly, there is virtually zero penalty to
   spawning micro-tasks on the fly for ephemeral operations.

4. **`+kn_call_open` Enables Shared Address Space Access:** Node pinning keeps child
   tasks on the exact same node, allowing direct object referencing (`weak-ref` and
   `obj-ref`) while maintaining clean stream-based communication.

5. **Back-to-Front Handshaking:** Dynamic mailboxes allow consumers and producers
   to rendezvous and wire streaming channels dynamically with zero static
   configuration.

Whenever you face a multi-stage data transformation -- whether it is video decoding,
image compression, audio DSP, cryptographic hashing, or packet parsing -- consider
replacing intermediate buffers with a **Streaming Local Pipeline**.
