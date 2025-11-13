# The ChrysaLisp Stream System

In ChrysaLisp, streams are a fundamental abstraction for all input and output
operations. They provide a powerful and consistent interface for handling data
from various sources and sinks, such as files, in-memory strings, and the
underlying message-passing kernel. The entire system is built upon a base
`stream` class with several specialized subclasses, all supported by a set of
low-level, high-performance Virtual Processor (VP) methods.

## The Core Abstraction: The `stream` Class

The foundation of the I/O system is the `stream` class, which defines a common
interface and behavior for all stream types. It is an inheritable class, meaning
specialized stream classes like `fstream` and `sstream` build upon its
foundation.

A key design feature is its set of `:static` VP methods. These methods are
generic and can be used with any stream subclass, providing a unified API for
common operations.

**Generic Static VP Methods:**

*   **`:avail`**: Returns the number of bytes currently available to be read
    from the stream's internal buffer.

*   **`:read_char`**: Reads a single character (byte) from the stream. It
    returns -1 if the end of the stream is reached.

*   **`:write_char`**: Writes a single character to the stream.

*   **`:read_bits` / `:write_bits`**: Reads or writes a specified number of
    bits, using a bit pool to handle operations that don't align to byte
    boundaries.

*   **`:read`**: Reads a block of bytes from the stream into a specified buffer.

*   **`:write`**: Writes a block of bytes from a buffer to the stream.

*   **`:read_line`**: Reads a line of text from the stream, ending at a newline
    character.

*   **`:write_cstr`**: Writes a C-style null-terminated string to the stream.

The `stream` class also defines **virtual methods** that are intended to be
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

### `fstream`: File Streams

The `fstream` class is used for reading from and writing to files on the host
operating system.

*   **Lisp Constructor**: `(file-stream path [mode])`

    * Creates a new file stream. `path` is a string representing the file path.
      The optional `mode` specifies how the file should be opened (e.g.,
      `+file_open_read`, `+file_open_write`).

### `sstream`: String Streams

The `sstream` class provides a stream interface for reading from and writing to
in-memory strings, allowing string manipulation with the standard stream API.

*   **Lisp Constructor**: `(string-stream str)`

    * Creates a new string stream associated with the given string `str`. It can
      be used to dynamically build or parse strings.

### `mstream`: Memory Streams

The `mstream` class operates on a dynamically sized list of memory chunks. This
is ideal for situations where the total size of the data is unknown, as it can
grow on demand without requiring large contiguous memory allocations.

*   **Lisp Constructor**: `(memory-stream)`

    * Creates a new, empty memory stream.

## Specialized IPC Streams: `in` and `out`

The `in` and `out` streams are specialized, high-performance classes that form
the bridge between the stream abstraction and ChrysaLisp's core message-passing
IPC system. They are the foundation of the pipe I/O system.

### `out`: The Sending End of a Pipe

The `out` stream is a write-only stream that packetizes data and sends it as
sequenced messages to a destination `netid` (a `mailbox_id` and `node_id` pair).

*   **Lisp Constructor**: `(out-stream mbox)`

    * Creates a new `out` stream that will send data to the specified `mbox`
        `netid`.

*   **Operation**: The `out` stream manages the protocol for reliable, ordered
    delivery.

    1. **Packetizing**: When its internal buffer is full or `(stream-flush)` is
       called, it packages the buffered data into a `stream_msg` structure,
       which includes a sequence number (`seqnum`).

    2. **Sending**: It sends this message to the destination `netid`.

    3. **Acknowledgement**: It can be configured to wait for acknowledgements
       (`ack`) from the receiving `in` stream. By tracking the `ack_seqnum`, it
       ensures data has been received before sending more, preventing buffer
       overruns and guaranteeing reliability.

    4. **Closing**: On deinitialization, it sends special messages with
       `stream_mail_state_stopping` and `stream_mail_state_stopped` flags to
       signal a graceful closure of the pipe.

### `in`: The Receiving End of a Pipe

The `in` stream is a read-only stream that receives sequenced messages from its
mailbox and reconstructs the original, ordered byte stream.

*   **Lisp Constructor**: `(in-stream)`

    * Creates a new `in` stream, which automatically allocates a unique mailbox.
      The `netid` of this stream can then be shared with other tasks to
      establish a communication channel.

*   **Operation**:

    1. **Receiving**: The `:read_next` method performs a blocking read on the
       stream's mailbox, waiting for a message to arrive.

    2. **Reordering**: It inspects the `seqnum` of incoming messages. If a
       message arrives out of order, it is buffered internally until the missing
       packets arrive, ensuring the data presented to the reader is always in
       the correct sequence.

    3. **Acknowledgement**: It periodically sends `ack` messages back to the
       `out` stream, confirming which sequence numbers it has successfully
       received.

    4. **End-of-Stream**: When it receives a message with the
       `stream_mail_state_stopped` flag, it signals an end-of-stream condition,
       causing subsequent reads to return -1.

## The Pipe I/O System: Tying It All Together

The `in` and `out` streams create the ChrysaLisp pipe system, a
location-transparent communication mechanism that allows tasks to interact as if
they were reading from and writing to a simple file.

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
    from the stream. If `width` is omitted, it reads a single byte.

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
