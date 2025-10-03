# Library and System primitives

This document covers the FFI primitives available in the ChrysaLisp class
library and system `lisp.inc` files, excluding the core primitives already
detailed from `root.inc`.

## System Kernel and Task Management

These functions provide low-level access to the ChrysaLisp kernel and task
scheduler, allowing for introspection and control over the system's execution.

*   **`kernel-stats`**: Retrieves statistics from the kernel.

    * `(kernel-stats) -> (task_count mem_used mem_avail max_stack)`

*   **`load-path`**: Returns the base path for loading object files, specific to
    the current CPU and ABI.

    * `(load-path) -> str`

*   **`task-mbox`**: Returns the `netid` of the current task's mailbox.

    * `(task-mbox) -> netid`

*   **`task-count`**: Gets or adjusts the kernel's count of running tasks.

    * `(task-count bias) -> num`: The `bias` argument is added to the current
      count.

*   **`task-sleep`**: Pauses the current task for a specified duration in
    microseconds.

    * `(task-sleep usec) -> :t`

*   **`task-slice`**: Yields the CPU, allowing the scheduler to run other tasks.
    This is essential for cooperative multitasking.

    * `(task-slice) -> :t`

## Distributed Messaging (Mail System)

This is the core family of functions for inter-process communication, forming
the foundation of ChrysaLisp's message-passing architecture.

*   **`mail-mbox`**: Allocates a new, unique mailbox for the current task.

    * `(mail-mbox) -> netid`

*   **`mail-declare`**: Registers a service with the network, associating a name
    and info string with a specific mailbox.

    * `(mail-declare mbox name info) -> service_key`

*   **`mail-nodes`**: Returns a list of all known node IDs on the network.

    * `(mail-nodes) -> (node_id ...)`

*   **`mail-enquire`**: Searches the network for services matching a given
    prefix.

    * `(mail-enquire prefix) -> (service_entry ...)`

*   **`mail-forget`**: Unregisters a service from the network.

    * `(mail-forget service_key)`

*   **`mail-poll`**: Checks a list of mailboxes and returns the index of the
    first one with a waiting message, without blocking.

    * `(mail-poll mboxs) -> :nil | index`

*   **`mail-validate`**: Checks if a given mailbox `netid` is currently valid
    and allocated.

    * `(mail-validate mbox) -> :t | :nil`

*   **`mail-read`**: Blocks until a message is received in the specified mailbox
    and returns it.

    * `(mail-read mbox) -> msg`

*   **`mail-select`**: Blocks until a message is available in any of the
    mailboxes in a list and returns the index of that mailbox.

    * `(mail-select mboxs) -> index`

*   **`mail-send`**: Sends a message to a destination `netid`.

    * `(mail-send mbox obj)`

*   **`mail-timeout`**: Sets or clears a timeout for a mailbox. If the timeout
    is reached, a message is sent to that mailbox.

    * `(mail-timeout mbox ns id)`: Setting `ns` to 0 cancels the timeout.

## GUI System Management

These functions manage the main GUI window and event loop.

*   **`gui-info`**: Returns information about the main GUI screen.

    * `(gui-info) -> (mouse_x mouse_y screen_width screen_height)`

*   **`gui-init`**: Initializes the GUI system with a root view object.

    * `(gui-init screen) -> screen`

*   **`gui-deinit`**: Shuts down the GUI system.

    * `(gui-deinit) -> :nil`

*   **`gui-update`**: Triggers a redraw and composite of the GUI.

    * `(gui-update mouse_x mouse_y flags) -> :nil`

*   **`gui-event`**: Polls for and returns the next GUI event from the host OS.

    * `(gui-event) -> :nil | event_string`

## Font and Glyph Functions

These functions handle loading, inspecting, and rendering text using `.ctf`
(ChrysaLisp Typeface) files.

*   **`create-font`**: Loads a font file at a specific pixel size.

    * `(create-font name pixels) -> font`

*   **`font-info`**: Returns the name and size of a font object.

    * `(font-info font) -> (name pixels)`

*   **`font-glyph-paths`**: Generates vector `path` objects for the glyphs in a
    string.

    * `(font-glyph-paths font str) -> (path ...)`

*   **`font-glyph-ranges`**: Returns the Unicode ranges supported by the font.

    * `(font-glyph-ranges font) -> ((start end) ...)`

*   **`font-glyph-bounds`**: Calculates the bounding box for a rendered string.

    * `(font-glyph-bounds font str) -> (width height)`

*   **`font-sym-texture`**: Creates and caches a texture for a given symbol
    (word).

    * `(font-sym-texture font sym) -> texture`

## Canvas and Pixel Manipulation

These functions provide low-level control over raster graphics on a `canvas`
object.

*   **`canvas-to-argb32`**: Converts a color from a specific pixel format to a
    standard 32-bit ARGB value.

    * `(canvas-to-argb32 pixel type) -> argb_num`

*   **`canvas-from-argb32`**: Converts a 32-bit ARGB color to a specific pixel
    format.

    * `(canvas-from-argb32 pixel type) -> num`

## Path and Vector Graphics

These functions are used to create and manipulate `path` objects, which are
sequences of 2D coordinates for vector drawing.

*   **`path-gen-arc`**: Generates a circular or elliptical arc path.

    * `(path-gen-arc cx cy start_angle end_angle radius dst) -> dst`

*   **`path-gen-cubic`**: Generates a cubic Bézier curve.

    * `(path-gen-cubic p1x p1y p2x p2y p3x p3y p4x p4y dst) -> dst`

*   **`path-gen-quadratic`**: Generates a quadratic Bézier curve.

    * `(path-gen-quadratic p1x p1y p2x p2y p3x p3y dst) -> dst`

*   **`path-filter`**: Reduces the number of points in a polyline path based on
    a tolerance.

    * `(path-filter tol src dst) -> dst`

*   **`path-simplify`**: Simplifies a path using the Ramer-Douglas-Peucker
    algorithm.

    * `(path-simplify tol src dst) -> dst`

*   **`path-stroke-polygon`**: Converts the outline of a polygon into a new,
    renderable shape.

    * `(path-stroke-polygon path radius join) -> paths`

*   **`path-stroke-polyline`**: Converts a polyline into a renderable shape with
    a given thickness and style.

    * `(path-stroke-polyline path radius join cap1 cap2) -> path`

*   **`path-transform`**: Applies a 3x2 transformation matrix to a path.

    * `(path-transform m3x2 src dst) -> dst`

*   **`path-svg`**: Tokenizes an SVG path data string (`d="..."`).

    * `(path-svg d) -> commands`

## Vectorized Numeric Operations (`nums`, `fixeds`, `reals`)

These functions perform operations on entire vectors of numbers (`nums`, `reals`
or `fixeds` types) at once.

*   **`nums-abs` / `fixeds-frac` / `fixeds-floor`**: Perform element-wise
    operations on a vector.

    * `(nums-abs nums [out_nums]) -> nums`

*   **`nums-scale`**: Scales each element of a vector by a scalar value.

    * `(nums-scale nums scale [out_nums]) -> nums`

*   **`nums-add` / `nums-sub` / `nums-mul` / `nums-div` / `nums-mod` /
    `nums-min` / `nums-max`**: Perform element-wise arithmetic or comparison
    between two vectors.

    * `(nums-add nums1 nums2 [out_nums]) -> nums`

*   **`nums-sum`**: Returns the sum of all elements in a vector.

    * `(nums-sum nums) -> num`

*   **`nums-dot`**: Calculates the dot product of two vectors.

    * `(nums-dot nums1 nums2) -> num`

## Scalar Math Primitives

These are standard mathematical functions that operate on single `num`, `real`
or `fixed` values.

*   **`sqrt`**: Calculates the square root.

    * `(sqrt num) -> num`

*   **`sign`**: Returns the sign of a number (-1, 0, or 1).

    * `(sign num) -> -1 | 0 | 1`

*   **`sin` / `cos`**: Trigonometric sine and cosine functions.

    * `(sin fixed) -> fixed`

*   **`frac` / `floor`**: Return the fractional or integer part of a fixed-point
    number.

    * `(frac fixed) -> fixed`

*   **`recip`**: Calculates the reciprocal (1/x) of a fixed-point number.

    * `(recip fixed) -> fixed`

## XML Parsing

*   **`XML-parse`**: A low-level XML tokenizer that calls back to user-provided
    functions for handling tags, attributes, and text content.

    * `(XML-parse stream fnc_in fnc_out fnc_text)`

## Audio Service

These functions provide an RPC interface to a separate audio service for playing
sound.

*   **`audio-add-rpc`**: Loads a sound file (e.g., a `.wav`) and returns a
    handle.

    * `(audio-add-rpc file_path) -> handle`

*   **`audio-play-rpc`**: Plays a loaded sound effect.

    * `(audio-play-rpc handle)`

*   **`audio-change-rpc`**: Pauses, resumes, or stops a playing sound.

    * `(audio-change-rpc handle state)`

*   **`audio-remove-rpc`**: Unloads a sound effect from memory.

    * `(audio-remove-rpc handle)`
