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

This document details an orthogonal ChrysaLisp pattern: **Async Local
Pipelines**. By combining the ability to execute **raw Lisp-level source
directly as an inline task** with **local node pinning (`+kn_call_pin`)**, a
**unified stream-to-stream stage architecture**, and an **ordered list-based
pipeline wiring protocol**, developers can construct multi-stage
producer-consumer pipelines with **zero full-frame intermediate buffers** and
zero stage-specific special cases.

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

Traditionally, a loader decompresses these layers sequentially using
intermediate in-memory buffers:

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

	For an 800x600 32-bit image (~1.92 MB uncompressed), Stage 1 allocates a
	full intermediate `(memory-stream)` buffer, and Stage 2 allocates
	*another* 1.92 MB `memory-stream` buffer. The system temporarily consumes
	3x to 4x the image's memory size. In film playback (`.FLM`) running at
	30-60 FPS, constantly allocating, expanding, and freeing multi-megabyte
	buffers causes severe heap fragmentation and allocator churn.

2. **Poor Cache Locality & Memory Bottlenecks:**

	Writing full frames back and forth to intermediate memory buffers
	constantly evicts data from CPU L1/L2 caches. By the time Stage 2 reads
	the bytes written at the start of Stage 1, those bytes must be refetched
	from main RAM. This loss of data locality throttles throughput and
	degrades speed.

3. **Monolithic Inflexibility:**

	The synchronous model requires monolithic caller-side buffering logic. If
	a format supports optional compression layers (such as raw, RLE only, LZ4
	only, or combined), the caller ends up with nested buffering logic and
	manual stream rewinds rather than clean, composable stages.

## 2. The Core Primitive: Spawning Raw Lisp Source as a Task

In standard operating systems, spawning a process requires an executable binary
on disk or an external script file path. Passing transient parameters requires
command-line arguments, environment variables, or complex IPC setup.

In ChrysaLisp, **code is data, and data is code**.

The kernel primitive `(open-child script [flags])` inspects the `script`
parameter:

* If `script` is a file path (e.g., `"cmd/player.lisp"`), the child task loads
  and evaluates that file.

* **If `script` is a string beginning with `'('`**, the kernel recognizes it as
  **raw, inline Lisp source**. The child task bypasses filesystem I/O entirely,
  instantiating its task context and evaluating the S-expression directly from
  memory!

### Metaprogramming with Quasiquote Templates

Because ChrysaLisp S-expressions evaluate cleanly, we can generate child task
definitions dynamically using quasiquote (`` ` ``) and unquote (`,`):

```file
lib/image/cpm.inc "(defun cpm-load-stage" ""
```

Notice what happens here:

* `(str `(progn ...))` formats the S-expression into a compact string starting
  with `'('`.

* The template executes immediately upon child task startup with zero disk
  overhead.

* Runtime objects -- such as communication mailboxes and parent stream handles
  -- are interpolated directly into the child task's definition.

## 3. Shared Memory Safety & Node Pinning

When tasks run across a ChrysaLisp cluster, data must be serialized across
network drivers. But for local media decoding, serialization would defeat the
purpose. We need zero-copy shared memory access.

Two architectural mechanisms make this safe and fast:

### 1. Node Pinning (`+kn_call_pin`)

The standard task spawn flag `+kn_call_run` delegates placement to the
kernel's emergent load balancer, which might slip the task to a neighboring
core or node.

To ensure tasks share the same physical address space, we use
**`+kn_call_pin`**:

```vdu
(open-child (cpm-load-stage-pixmap pixmap type handshake_mbox) +kn_call_pin)
```

`+kn_call_pin` strictly pins the child task to the **exact same hardware node
and memory context** as the parent. Pointers into heaps and memory streams
remain 100% valid across both tasks without cross-node proxying.

### 2. The `weak-ref` / `obj-ref` Lifecycle Pattern

Passing object pointers across tasks requires careful reference counting. If
the parent interpolates a raw pointer, how do we prevent the parent's GC from
reclaiming the object while the child is executing? Conversely, how do we avoid
leaking reference counts?

ChrysaLisp solves this with the `weak-ref` / `obj-ref` pair:

1. **Parent side:** `(weak-ref obj)`

   Extracts the raw memory address as a number without incrementing the
   reference count in the template string.

2. **Child side:** `(obj-ref ,(weak-ref obj))`

   Increments the object's reference count (`+obj_count`) when the child task
   starts up, declaring formal ownership.

3. **Child exit:**

   When the child task finishes its `progn` and exits, its lexical environment
   unwinds (`env-pop`), automatically decrementing the reference count. If the
   parent has already finished, the object cleans up immediately without leaks.

### 3. Hex-Encoded Mailbox Handles

Mailboxes are represented by 24-byte `netid` tuples containing binary data.
When embedded in Lisp templates, binary characters or signed values can cause
reader errors. Passing mailbox handles as hex-encoded string tokens ensures
robust serialization:

```vdu
(mail-send (hex-decode ,(hex-encode handshake_mbox)) (in-mbox in))
```

## 4. The Orthogonal Pipeline Model: Everything is Stream-to-Stream

The fundamental insight of ChrysaLisp streaming is that **compression and
filtering algorithms are purely stream-to-stream functions**:

```code
in_stream ---> [ Filter ] ---> out_stream
```

Neither `lz4-decompress`, `rle-decompress`, `huffman-decompress`, nor any
intermediate transform knows or cares whether a stream is backed by an IPC
mailbox, a memory buffer, or a file on disk. They all operate on the uniform
`:stream` class interface.

### Stage Classification

In any streaming pipeline, stages fall into three simple categories:

1. **Sink (Terminal Consumer):**

   Reads from an IPC `(in-stream)` and consumes data directly into memory (such
   as writing into canvas pixmap memory, or writing to the destination `stream`).
   It sends its input mailbox upstream, and upon completion, writes its final
   result or completion token back to `handshake_mbox`.

2. **Intermediate Filter (`in_stream -> out_stream`):**

   Creates an IPC `(in-stream)`, publishes its mailbox upstream to
   `handshake_mbox`, reads from `in`, and writes transformed bytes to
   `(out-stream downstream_mbox)`.

3. **Source (Initial Producer):**

   Reads directly from the source `stream` (or canvas memory) and writes to
   `(out-stream downstream_mbox)`. It does not create an `in-stream` because it
   already owns the data source.

Because every filter stage performs the exact same mechanical role, we do not
need separate, hardcoded stage functions for every codec. A single
**`cpm-load-stage`** and **`cpm-save-stage`** generator handles arbitrary
algorithms uniformly.

## 5. Reverse Assembly & Single-Mailbox Handshaking

A pipeline must be initialized **back-to-front** (Sink to Source) so that each
downstream consumer can allocate its input mailbox before the upstream producer
attempts to connect to it.

Furthermore, a single ephemeral **`handshake_mbox`** coordinates both the
inter-stage wiring and final completion. There is no need for a separate
`done_mbox`.

```code
Step 1: Parent creates handshake_mbox.

Step 2: Launch Sink (Stage N: Pixmap)
        Stage N creates (in-stream) and sends (in-mbox in) to handshake_mbox.

Step 3: Launch Intermediate Filters in Reverse (Stage N-1 down to Stage 1)
        Parent reads downstream_mbox via (mail-read handshake_mbox).
        Filter creates (in-stream), sends its (in-mbox in) to handshake_mbox.
        Filter transforms in-stream -> (out-stream downstream_mbox).

Step 4: Launch Source (Stage 0: First Filter or Pixmap Producer)
        Parent reads downstream_mbox via (mail-read handshake_mbox).
        Stage 0 reads from root stream/canvas and writes to
         (out-stream downstream_mbox).

Step 5: Completion
        Data streams through all cores concurrently.
        When the sink completes, it sends its completion token to handshake_mbox.
        Parent unblocks via (mail-read handshake_mbox) and returns the Canvas.
```

## 6. Symmetrical Implementation: `lib/image/cpm.inc`

The complete implementation in `lib/image/cpm.inc` models both decompression
(`CPM-load`) and compression (`CPM-save`) as ordered lists of stages wired via
reverse traversal.

### 6.1 Universal Stage Generators

All decompression and compression filters are generated through two uniform
functions:

```file
lib/image/cpm.inc "(defun cpm-load-stage" "(defun cpm-load-stage-pixmap"
```

### 6.2 The Decompression Pipeline (`CPM-load`)

In `CPM-load`, stages are listed in natural data-flow order (`stream -> ... ->
pixmap`). Intermediate stages are assembled in reverse using `each!`:

```file
lib/image/cpm.inc "(defun CPM-load" ""
```

### 6.3 The Compression Pipeline (`CPM-save`)

In `CPM-save`, stages are listed in data-flow order (`pixmap -> ... -> stream`).
The last filter is the sink writing to `stream`, intermediate filters are wired
via `each!`, and the pixmap producer is launched as the source:

```file
lib/image/cpm.inc "(defun CPM-save" ""
```

## 7. Changing Filter Order with Zero Code Modifications

Because stage generators are completely decoupled from topology, reordering or
inserting filters requires **zero changes to stage functions or assembly loops**.

To invert the compression pipeline (compressing with LZ4 first, then RLE):

```vdu
; Save with LZ4 -> RLE:
(defq stages (list))
(if lz4 (push stages (# (cpm-save-stage "lib/streams/lz4.inc" 'lz4-compress %0 %1))))
(if rle (push stages (# (cpm-save-stage "lib/streams/rle.inc" 'rle-compress %0 %1 num_bits 8))))
```

And symmetrically on load:

```vdu
; Load with RLE -> LZ4:
(defq stages (list))
(if rle (push stages (# (cpm-load-stage "lib/streams/rle.inc" 'rle-decompress %0 %1 num_bits 8 (* w h)))))
(if lz4 (push stages (# (cpm-load-stage "lib/streams/lz4.inc" 'lz4-decompress %0 %1))))
```

To insert a 3rd, 4th, or 50th stage (such as encryption or Huffman coding),
simply push another stage function into the `stages` list. The pipeline wiring
automatically connects the mailboxes without modification.

## 8. Summary & Architectural Takeaways

The Async Local Pipeline embodies the core tenets of ChrysaLisp system design:

1. **Orthogonal Stream Abstraction:**

   Streams are streams. Whether backed by a shared memory ring buffer, an IPC
   mailbox, or a file descriptor, transforms consume `in` and produce `out`
   without knowing pipeline placement.

2. **Zero-Buffering Throughput:**

   Multi-megabyte frames stream through CPU cache in small, hot IPC chunks,
   eliminating multi-pass heap thrashing and memory bus saturation.

3. **Single-Mailbox Coordination:**

   `handshake_mbox` handles back-to-front channel rendezvous and final
   completion signaling without auxiliary tracking variables.

4. **Flyweight Inline Metaprogramming:**

   Pinning raw Lisp strings as child tasks via `+kn_call_pin` combines
   multi-core MIMD parallelism with zero-copy address-space sharing.
