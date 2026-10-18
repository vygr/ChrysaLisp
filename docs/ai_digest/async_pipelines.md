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

The pattern has three uses in the tree, each an incarnation of the one
`lib/streams/stages.inc`. **Images**, sections 1 to 7, read and written
through their codecs, the worked example. **Films**, section 8, a stream of
frames written as they are made. **Sound**, section 9, whose ends are stages
too: a microphone, a file, a speaker.

## 1. The Bottleneck of Synchronous Multi-Pass Buffering

To appreciate the async pipeline model, consider the canonical CPM/FLM image
format (`lib/image/cpm.inc`). A compressed CPM image frame consists of layered
compression algorithms applied sequentially:

```image
docs/diagrams/cpm_layers.cwb
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
lib/streams/stages.inc "(defun stage-form" "(defun stage-in"
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
(open-child (stage-form s (stage-in handshake) out) +kn_call_pin)
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

```image
docs/diagrams/stream_filter.cwb
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
need separate, hardcoded stage functions for every codec. A stage is just
what it imports, the function and its arguments, `(stage imports fn [arg
...])`, and the one `stage-form` in `lib/streams/stages.inc` makes the task
of any of them.

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

## 6. Symmetrical Implementation: `lib/streams/stages.inc`

`lib/streams/stages.inc` models both reading (`stages-load`) and writing
(`stages-save`) an image as an ordered list of stages wired via reverse
traversal. The CPM, PNG and JPEG loaders and the CPM saver are each a list of
stages handed to one of them.

### 6.1 A Stage

```file
lib/streams/stages.inc "(defun stage " "(defun stages-load"
```

### 6.2 Reading (`stages-load`)

Stages are listed in natural data-flow order (`stream -> ... -> pixmap`). The
pixmap's task is started first, then the stages in reverse using `each!`, the
first given the stream:

```file
lib/streams/stages.inc "(defun stages-load" "(defun stages-save"
```

A PNG is three stages, the IDAT chunks taken out, inflated and unfiltered:

```file
lib/image/png.inc "(defun PNG-load" ""
```

### 6.3 Writing (`stages-save`)

Stages are listed in data-flow order (`pixmap -> ... -> stream`). The last is
the sink writing to `stream`, and says when it is done; the pixmap's writer is
launched last, as the source:

```file
lib/streams/stages.inc "(defun stages-save" ";module"
```

## 7. Changing Filter Order with Zero Code Modifications

Because stage generators are completely decoupled from topology, reordering or
inserting filters requires **zero changes to stage functions or assembly loops**.

To invert the compression pipeline (compressing with LZ4 first, then RLE):

```vdu
; Save with LZ4 -> RLE:
(defq stages (list))
(if lz4 (push stages (stage "lib/streams/lz4.inc" 'lz4-compress)))
(if rle (push stages (stage "lib/streams/rle.inc" 'rle-compress num_bits 8)))
```

And symmetrically on load:

```vdu
; Load with RLE -> LZ4:
(defq stages (list))
(if rle (push stages (stage "lib/streams/rle.inc" 'rle-decompress num_bits 8 (* w h))))
(if lz4 (push stages (stage "lib/streams/lz4.inc" 'lz4-decompress)))
```

To insert a 3rd, 4th, or 50th stage (such as encryption or Huffman coding),
simply push another stage into the `stages` list. The pipeline wiring
automatically connects the mailboxes without modification.

## 8. Films: A Stream of Frames

`lib/streams/flm.inc` is the same pattern turned to writing a film. `(flm-open
stream format)` starts the encoder as a pinned task of raw Lisp, which sends
back the mailbox of its input stream, and the producer makes an `out-stream`
to it. `(flm-add film canvas)` writes the pixels of a frame to that feed, and
no more. `(flm-close film)` flushes the feed and lets it go, which is the end
of the stream for the encoder, and waits for it to say how many frames it
wrote. The file stream is the sink, handed to the task by weak reference, as
the image save does.

The `toflm` command and the Raymarch demo both use it. The demo pumps each
frame in as the GPU hands it back, with no file for a frame.

## 9. Sound: Ends That Are Stages

Sound is a stream to begin with. A microphone gives a piece every 20ms for
as long as anyone speaks; a speaker wants a piece every 20ms for as long as
there is anything to play. Holding a whole sound and then doing something to
it is the wrong shape for that, and for voice chat it is impossible: the
call has no end to wait for. So sound is the same pipeline, each stage a
task pinned to this node, wired back to front through one handshake. What it
adds are **ends that are stages too**, a source with no in stream and a sink
with no out stream, and **a source that can be told to stop**.

### 9.1 The Bottleneck of Holding the Whole Sound

The first `mic` command and the first Voice Notes held a recording as a list
of pieces, sorted them, joined them into one string, and wrote that string
to a file with a header in front:

```vdu
; Holding the whole sound:
(push pieces (list seq data))
...
(sort pieces (# (- (first %0) (first %1))))
(defq data (apply (const cat) (cat (list "") (map (const second) pieces))))
(save (wav-text data +audio_rate) file)
```

#### The Three Flaws

1. **The whole sound in memory, three times over:**

	A minute of voice is 1.9 MB of samples. The list of pieces holds it, the
	joined string holds it again, and the text of the file a third time
	before it is written. A recording has no length known in advance.

2. **Nothing reaches the file till the end:**

	If the app ends, or the machine, nothing of what was said is kept. A
	stream to the file keeps it as it comes.

3. **No place to put a filter:**

	Making it louder, taking out the hum, or sending it to another machine
	would each be a pass over the whole held sound, written into the app.

### 9.2 The Same Stage

A sound stage is the stage of an image, `(stage imports fn [arg ...])`, the
task made from the form `(fn in out arg ...)`. `sound-stage` only says the
stage is one of this module's:

```file
lib/audio/stages.inc "(defun sound-stage" "(defun sound-map"
```

Every stage reads and writes 16 bit mono at `+sound_rate`, 16000 a second,
that of a voice and of the microphone, in pieces of 20ms. A filter is a
function of a piece:

```file
lib/audio/stages.inc "(defun sound-pieces" ";sources"
```

```file
lib/audio/stages.inc "(defun sound-gain" "(defun sound-lowpass"
```

### 9.3 Ends That Are Stages: `stages-run`

An image pipeline's ends are not stages: `stages-load` starts a task of its
own at the end that reads into the pixmap, and `stages-save` one at the start
that writes from it. A sound's ends are a microphone, a file, a speaker, each
a stage like any other. `stages-run` wires a list whose first stage is given
no in stream, `:nil`, and whose last is given no out stream, back to front as
the others are. The sink, last, is told the `done` mailbox, and sends it `:t`
when it has finished:

```file
lib/streams/stages.inc "(defun stages-run" ";module"
```

It returns at once. The pipeline runs by itself, and the caller waits on
`done` as one more mailbox of its `mail-select`, as an app waits on any
other.

### 9.4 Stage Classification

| Stage | Kind | What it does |
| --- | --- | --- |
| `sound-mic ctl` | source | what the microphone hears, in order, till `ctl` is sent anything |
| `sound-tone hz ms [level ctl]` | source | a tone |
| `sound-from-file path [ctl]` | source | a sound file, by its format |
| `sound-gain percent` | filter | louder or quieter, 100 as it is |
| `sound-lowpass hz` | filter | what is above hz let through less |
| `sound-level who` | filter | each piece passed on, how loud it was sent to `who` |
| `sound-to-file path` | sink | a sound file, written as it comes |
| `sound-speaker` | sink | played on the Audio service, at the pace it is heard |

The microphone source puts the pieces back in the order they were heard, as
mail is not kept in order, and lets one go that is not there when two after
it are:

```file
lib/audio/stages.inc "(defun sound-mic" "(defun sound-stopped?"
```

The speaker sink keeps 100ms ahead of what has been played, so a source that
is quicker than the sound, a file, does not fill the stream of the Audio
service and have it let pieces go:

```file
lib/audio/stages.inc "(defun sound-speaker" ""
```

### 9.5 Formats as Modules

A sound file's format is a module of `lib/audio/`, as an image's is of
`lib/image/`, that says what it is for as it is read. Where an image module
gives `info`, `load` and `save`, a sound module gives `info`, which reads the
head and leaves the stream at the first sample, `head`, which writes one for
a file whose length is not known yet, and `tail`, which puts the length in
once it is:

```file
lib/audio/sound.inc "(defun sound-loader" "(defun sound-info"
```

```file
lib/audio/wav.inc "(defun WAV-head" ";module"
```

`sound-to-file` finds the format by the file's name, writes its head, the
samples as they come, and its tail. Another format is another module and
nothing else changes:

```file
lib/audio/stages.inc "(defun sound-to-file" "(defun sound-speaker"
```

### 9.6 Stopping a Source

An image's source ends at the end of its file. A microphone has no end, so
a source is given a mailbox, `ctl`, made into a form by `(stage-mbox mbox)`,
and stops when it is sent anything. Its out stream then closes, the stage
after it finds the end of its in stream, and so on to the sink, which says
it is done. Nothing is torn down from outside:

```vdu
(import "lib/audio/stages.inc")
(defq ctl (mail-mbox) done (mail-mbox))
(stages-run (list
		(sound-stage 'sound-mic (stage-mbox ctl))
		(sound-stage 'sound-gain 200)
		(sound-stage 'sound-to-file "note.wav"))
	done)
;... and when it is time to stop
(mail-send ctl "")
(mail-read done)
```

### 9.7 Pipelines in Use

The Voice app, `apps/media/voice/`, once Voice Notes, starts a pipeline for each
thing it does and tells a source when to stop. Recording is the microphone,
how loud, to a file; playing is the file, how loud, to the speaker. The app
holds no sound:

```file
apps/media/voice/app_impl.lisp "(defun record-start" "(defun pipe-done"
```

The `mic` command is the same two, from a terminal, and says how long and
how loud what it heard was.

### 9.8 Spot Sounds Stay on the Host Mixer

The sounds of a game, a shot, a bang, are not pipelines. `audio-add-rpc`
gives the sound to the host once, and `audio-play-rpc` has the host's mixer
start it on its own thread the moment it is asked, with no task between. A
pipeline has the time of a task between each of its stages, which is right
for what comes as it is made and wrong for what has to start at once. What a
pipeline can do for a game is make its sounds: a tone and its filters to a
file, that the game then plays as any other.

### 9.9 Not here yet

* Rates and channels other than 16000 mono through a pipeline, music.
* The encoder and decoder of voice chat, and the network stages.
* Filters of more kinds: a high pass, a gate, an echo.
* A pipeline as a command line, as `pipe` commands join.

The tests: `tests/system/test_sound_stages.lisp`, a tone through filters to
files, with no desktop; `tests/release/test_audio_mic.lisp`, the microphone
and the speaker on a hidden desktop of its own.

## 10. Summary & Architectural Takeaways

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

5. **Ends That Are Stages:**

   Where a sound's source and sink are a microphone, a file or a speaker,
   `stages-run` wires them as it wires a filter, and a source told to stop
   ends the pipeline as the end of each stream in turn.

