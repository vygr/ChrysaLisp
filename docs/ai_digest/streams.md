# The ChrysaLisp Stream System

In ChrysaLisp, streams are a fundamental abstraction for all input and output
operations. They provide a powerful and consistent interface for handling data
from various sources and sinks, such as files, in-memory strings, and the
underlying message-passing kernel. The entire system is built upon a base
`:stream` class with several specialized subclasses, all supported by a set of
low-level, high-performance Virtual Processor (VP) methods.

## The Core Abstraction: The `:stream` Class

The foundation of the I/O system is the `:stream` class, which defines a
common interface and behavior for all stream types. It is an inheritable class,
meaning specialized stream classes like `:fstream` and `:sstream` build upon
its foundation.

A key design feature is its set of `:static` VP methods. These methods are
generic and can be used with any stream subclass, providing a unified API for
common operations.

**Generic Static VP Methods:**

*   **`:avail`**: Returns the number of bytes currently available to be read
    from the stream's internal buffer.

*   **`:read_char`**: Reads a single character (byte) from the stream. It
    returns -1 if the end of the stream is reached.

*   **`:write_char`**: Writes a single character to the stream.

*   **`:read_bits` / `:write_bits` / `:fill_bits` / `:copy_bits`**: Methods for
    reading, writing, filling, or copying a specified number of bits, using a
    bit pool to handle operations that don't align to byte boundaries.

*   **`:read`**: Reads a block of bytes from the stream into a specified
    buffer.

*   **`:write`**: Writes a block of bytes from a buffer to the stream.

*   **`:read_line`**: Reads a line of text from the stream, ending at a
    newline character.

*   **`:write_cstr`**: Writes a C-style null-terminated string to the stream.

The `:stream` class also defines **virtual methods** that are intended to be
overridden by its subclasses to provide source-specific implementations:

*   **`:read_next`**: Fetches the next block of data into the stream's buffer
    (e.g., from a file or a network socket).

*   **`:write_next`**: Prepares for the next block of data to be written. For a
    file, this might mean flushing the buffer; for a memory stream, it could
    mean reallocating to a larger size.

*   **`:flush`**: Commits any buffered data to the underlying storage or
    transport layer.

*   **`:seek`**: Changes the current position within the stream.

## Concrete Stream Implementations

ChrysaLisp provides several stream classes for different I/O scenarios.

### `:fstream`: File Streams

The `:fstream` class is used for reading from and writing to files on the host
operating system.

*   **Lisp Constructor**: `(file-stream path [mode])`

    * Creates a new file stream. `path` is a string representing the file path.
      The optional `mode` specifies how the file should be opened (e.g.,
      `+file_open_read`, `+file_open_write`).

### `:sstream`: String Streams

The `:sstream` class provides a stream interface for reading from and writing
to in-memory strings, allowing string manipulation with the standard stream
API.

*   **Lisp Constructor**: `(string-stream str)`

    * Creates a new string stream associated with the given string `str`. It
      can be used to dynamically build or parse strings.

### `:mstream`: Memory Streams

The `:mstream` class operates on a dynamically sized list of memory chunks.
This is ideal for situations where the total size of the data is unknown, as it
can grow on demand without requiring large contiguous memory allocations.

*   **Lisp Constructor**: `(memory-stream)`

    * Creates a new, empty memory stream.


### `:lstream`: Streams Read From Lisp

The `:lstream` class has no data of its own. When it has nothing left to
read it calls a Lisp function, which gives it the next piece as a str, of
any length, or `:nil` at the end. It keeps the one str at a time. A second
function, if given, is called for `(stream-seek)`.

*   **Lisp Constructor**: `(lisp-stream fill [seek write])`, bound in
    `lib/streams/lisp_stream.inc`, which is imported for it.
*   `fill` is `(lambda () ...) -> str | :nil`. `seek` is
    `(lambda (offset whence) ...) -> pos`.
*   A function has only what it is given, so the state goes in the
    functions as they are made:
    ``(lisp-stream `(lambda () (,my-fill ',state)))``.
*   `:read_next` and `:seek` keep every register the file stream keeps, so
    VP that reads a stream reads one of these the same.
*   Given a third function, `(lisp-stream fill [seek write])`, it is a
    stream to write to. `write` is a function of `(data flush)`: it is
    given each buffer of what is written, a str, and `:nil`, and at a
    flush what there is, which may be nothing, and `:t`. A stream that is
    let go of is flushed first, as a file is, so `write` is called from the
    `:deinit` too.

It came with the store, and `(storage-stream path [mode])` is made of it,
an object of a store read a block at a time with the next few on their way,
or written a block at a time. But it is not the store's: it is how a stream
of any kind at all is made from Lisp, with no VP and no new class. Anything
Lisp can fetch or put a piece at a time is then a stream, to everything that
reads or writes one.

### A Stream Of Your Own

A stream is the functions it is made of, and what they are given. Three of
the smallest, each in `tests/streams/test_lisp_stream.lisp`:

The parts of a list, one after the other. `pop` takes from the end, so the
list is turned round first:

```vdu
(defun parts-fill (parts)
	;the next of them, :nil after the last
	(if (nempty? parts) (pop parts)))
(defq parts (reverse (list "one\n" "two\n" "three\n"))
	in (lisp-stream `(lambda () (,parts-fill ',parts))))
(lines! (# (print %0) :nil) in)
```

Another stream, changed as it is read. This is a filter of a pipeline with
no task of its own, read where it is read:

```vdu
(defun upper-fill (in)
	(if (defq part (read-blk in 4096)) (to-upper part)))
(defq up (lisp-stream `(lambda () (,upper-fill ',(file-stream "README.md")))))
```

What is written, summed on its way to another stream, a CRC-32 of it in the
state when it is flushed:

```vdu
(defun summed-write (state data flush)
	(bind '(out &ignore) state)
	(elem-set state 1 (crc32 data (second state)))
	(write-blk out data)
	(if flush (stream-flush out)))
(defq state (list (file-stream "out.bin" +file_open_write) 0)
	summed (lisp-stream '(lambda () :nil) :nil
		`(lambda (data flush) (,summed-write ',state data flush))))
```

Each of these is read or written by all there is: `read-line`, `lines!`,
`read-blk`, `read-bits`, `write-blk`, the pipes, a stage of
`lib/streams/stages.inc`, and the VP that reads a stream, the native inflate
for one, which takes a zlib stream given it a byte at a time as well as from
a file. So a new source or sink is a function or two, a decoder or a sum is
a stream round another, and nothing that reads streams has to know.

What is made of it now:

*   `(storage-stream path [mode])`, an object of the store, read ahead a
    few blocks, `service/storage/app.inc`.
*   The two ends of a TLS connection, `lib/net/tls.inc`: what is read is
    decrypted a record at a time as it is asked for, what is written is put
    in records and encrypted, and `http.inc` and `curl` read and write them
    as they would a socket.

A function of the module that makes the stream, put into the `lambda` with
a `,`, is the function itself, so it is there when the stream calls it, the
module or no. The state is put in with `',`, the one object, so a fill that
changes it changes what the next call is given. Work out what goes in with a
`(defq)` first: a macro called in the unquote, `(reverse)` say, is put in as
what it expands to, not run.

## Specialized IPC Streams: `:in` and `:out`

The `:in` and `:out` streams are specialized, high-performance classes that
form the bridge between the stream abstraction and ChrysaLisp's core
message-passing IPC system. They are the foundation of the pipe I/O system.

### `:out`: The Sending End of a Pipe

The `:out` stream is a write-only stream that packetizes data and sends it as
sequenced messages to a destination `netid` (a `mailbox_id` and `node_id`
pair).

*   **Lisp Constructor**: `(out-stream mbox)`

    * Creates a new `:out` stream that will send data to the specified `mbox`
        `netid`.

*   **Operation**: The `:out` stream manages the protocol for reliable,
    ordered delivery.

    1. **Packetizing**: When its internal buffer is full or `(stream-flush)` is
       called, it packages the buffered data into a `stream_msg` structure,
       which includes a sequence number (`seqnum`).

    2. **Sending**: It sends this message to the destination `netid`.

    3. **Acknowledgement**: It can be configured to wait for acknowledgements
       (`ack`) from the receiving `:in` stream. By tracking the `ack_seqnum`,
       it ensures data has been received before sending more, preventing
       buffer overruns and guaranteeing reliability.

    4. **Closing**: On deinitialization, it sends special messages with
       `stream_mail_state_stopping` and `stream_mail_state_stopped` flags to
       signal a graceful closure of the pipe.

### `:in`: The Receiving End of a Pipe

The `:in` stream is a read-only stream that receives sequenced messages from
its mailbox and reconstructs the original, ordered byte stream.

*   **Lisp Constructor**: `(in-stream)`

    * Creates a new `:in` stream, which automatically allocates a unique
      mailbox. The `netid` of this stream can then be shared with other tasks
      to establish a communication channel.

*   **Operation**:

    1. **Receiving**: The `:read_next` method performs a blocking read on the
       stream's mailbox, waiting for a message to arrive.

    2. **Reordering**: It inspects the `seqnum` of incoming messages. If a
       message arrives out of order, it is buffered internally until the
       missing packets arrive, ensuring the data presented to the reader is
       always in the correct sequence.

    3. **Acknowledgement**: It periodically sends `ack` messages back to the
       `:out` stream, confirming which sequence numbers it has successfully
       received.

    4. **End-of-Stream**: When it receives a message with the
       `stream_mail_state_stopped` flag, it signals an end-of-stream
       condition, causing subsequent reads to return -1.

## The Pipe I/O System: Tying It All Together

The `:in` and `:out` streams create the ChrysaLisp pipe system, a
location-transparent communication mechanism that allows tasks to interact as
if they were reading from and writing to a simple file.

```image
docs/diagrams/stream_pipe.cwb
```

1.  **Establishment**: A receiving task creates an `(in-stream)` and shares its
    `netid`.

2.  **Connection**: A sending task uses that `netid` to create an
    `(out-stream mbox)`.

3.  **Data Flow**: The `out-stream` packetizes and sends data; the `in-stream`
    receives, reorders, acknowledges, and presents a seamless byte stream.

4.  **Termination**: Closing the `out-stream` sends a `stopped` message, which
    the `in-stream` interprets as the end of the stream, allowing for clean
    shutdown.

## Lisp-Level Stream Functions

ChrysaLisp provides a rich set of Lisp functions for working with any stream
type.

### Paired Read/Write Functions

The library design emphasizes symmetry, offering paired functions for common
operations.

*   **Block I/O**:

    * `(read-blk stream bytes) -> str`: Reads a block of `bytes` from the
      stream.

    * `(write-blk stream str) -> bytes`: Writes the content of the string `str`
      to the stream.

*   **Line-Oriented I/O**:

    * `(read-line stream) -> str`: Reads a line of text, terminated by a
      newline.

    * `(write-line stream str) -> bytes`: Writes the string `str` followed by a
      newline.

### Low-Level I/O and Numeric Conversions

A powerful feature of the low-level character functions is the ability to
specify a byte width, which greatly simplifies the serialization of binary
numeric data.

*   `(read-char stream [width]) -> num`: Reads a numeric value of `width` bytes
    from the stream. If `width` is omitted, it reads a single byte. Positive
    width indicates signed, negative width indicates unsigned.

*   `(write-char stream list|num [width]) -> bytes`: Writes a numeric `list` or
    `num` to the stream with a specified `width`.

This allows for easy conversion of numeric values without manual byte
manipulation. For example, a 32-bit integer can be written to a stream and read
back in a single operation by specifying a width of 4 bytes:

```vdu
;; Write a 32-bit integer (256) to a stream
(write-char my_stream 256 +int_size)

;; Read it back from another stream
(defq my_integer (read-char another_stream +int_size))
```

This elegant feature, combined with the various stream classes, provides a
comprehensive and highly efficient I/O framework that is central to the
ChrysaLisp operating system.
