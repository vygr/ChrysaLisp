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

*   **`os` / `cpu` / `abi`**: Returns the operating system, CPU architecture, and ABI.

    * `(os) -> sym`

    * `(cpu) -> sym`

    * `(abi) -> sym`

*   **`task-flags`**: Returns the flags of the current task.

    * `(task-flags) -> flags`

*   **`task-mbox`**: Returns the `netid` of the current task's mailbox.

    * `(task-mbox) -> netid`

*   **`task-count`**: Gets or adjusts the kernel's count of running tasks.

    * `(task-count bias) -> num`: The `bias` argument is added to the current
      count.

*   **`task-sleep`**: Pauses the current task for a specified duration in
    microseconds.

    * `(task-sleep usec) -> :t`

*   **`task-slice`**: Yields the CPU, allowing the scheduler to run other tasks.

    * `(task-slice) -> :t`

*   **`task-mboxes`**: Returns a list of mailboxes for a task.

    * `(task-mboxes size) -> ((task-mbox) [temp_mbox] ...)`

*   **`task-nodeid`**: Returns the node ID for a given mailbox or the current task.

    * `(task-nodeid [mbox]) -> nodeid`

*   **`task-timeout`**: Converts seconds to nanoseconds for use in timeouts.

    * `(task-timeout s) -> ns`

*   **`pii-time`**: Returns the current system time in nanoseconds.

    * `(pii-time) -> ns`

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

    * `(mail-timeout mbox ns id) -> mbox`: Setting `ns` to 0 cancels the timeout.

## Platform Implementation Interface (PII)

These functions interact directly with the underlying host operating
system.

*   **`pii-dirlist`**: Lists the contents of a directory.

    * `(pii-dirlist path) -> info`

*   **`pii-fstat`**: Retrieves file status information (modification time,
    size).

    * `(pii-fstat path) -> info`

*   **`pii-read-char` / `pii-write-char`**: Read or write a single character
    from/to a file descriptor.

    * `(pii-read-char fd) -> char`

    * `(pii-write-char fd char) -> char`

*   **`pii-remove`**: Deletes a file.

    * `(pii-remove path)`

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

*   **`gui-rpc`**: Performs a GUI remote procedure call.

    * `(gui-rpc (view cmd)) -> :nil | view`

*   **`gui-add-back-rpc` / `gui-add-front-rpc`**: Adds a view to the back or front of the GUI.

    * `(gui-add-back-rpc view) -> view`

*   **`gui-logout-rpc` / `gui-quit-rpc`**: Logs out or quits the GUI system.

    * `(gui-logout-rpc) -> :nil`

*   **`gui-sub-rpc`**: Subscribes to GUI events.

    * `(gui-sub-rpc view) -> view`

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

## Canvas and Graphics Manipulation

These functions provide control over raster graphics and image formats.

*   **`pixmap-to-argb32` / `pixmap-from-argb32`**: Converts between pixel formats.

    * `(pixmap-to-argb32 pixel type) -> argb_num`

    * `(pixmap-from-argb32 pixel type) -> num`

*   **`canvas-info`**: Returns information about a canvas or image file.

    * `(canvas-info file) -> (width height type) | (-1 -1 -1)`

*   **`canvas-load` / `canvas-save`**: Loads or saves a canvas from/to a file.

    * `(canvas-load file flags [swap_mode]) -> :nil | canvas`

    * `(canvas-save canvas file type) -> :nil | canvas`

*   **`canvas-brighter` / `canvas-darker`**: Adjusts color brightness.

    * `(canvas-brighter col) -> col`

    * `(canvas-darker col) -> col`

*   **`canvas-flush`**: Flushes any shared pixmaps from the cache.

    * `(canvas-flush)`

*   **`circle`**: Generates a circular path.

    * `(circle r) -> path`

*   **`lighting` / `lighting-at3`**: Applies basic attenuation and diffuse/specular lighting.

    * `(lighting col at)`

    * `(lighting-at3 col at sp)`

*   **`render-object-tris`**: Projects 3D vertices (triangles) to the screen.

*   **`texture-metrics`**: Returns information about a texture.

    * `(texture-metrics texture) -> (handle width height)`

*   **`CPM-info` / `CPM-load` / `CPM-save`**: Handles CPM image format.

*   **`CWB-info` / `CWB-load`**: Handles CWB image format.

*   **`TGA-info` / `TGA-load`**: Handles TGA image format.

*   **`SVG-info` / `SVG-load`**: Handles SVG format.

## Path and Vector Graphics

These functions are used to create and manipulate `:path` objects for vector drawing.

*   **`path-gen-arc` / `path-gen-cubic` / `path-gen-quadratic` / `path-gen-ellipse` / `path-gen-rect`**: Generates various path shapes.

    * `(path-gen-arc cx cy start_angle end_angle radius dst) -> dst`

*   **`path-gen-paths`**: Tokenizes an SVG path data string into open/closed paths.

    * `(path-gen-paths svg_d) -> ((:nil|:t path) ...)`

*   **`path-filter` / `path-simplify` / `path-smooth`**: Processes and simplifies paths.

    * `(path-filter tol src dst) -> dst`

*   **`path-stroke-polygon` / `path-stroke-polyline` / `path-stroke-polygons` / `path-stroke-polylines`**: Converts outlines into renderable shapes.

    * `(path-stroke-polyline path radius join cap1 cap2) -> path`

*   **`path-transform`**: Applies a transformation matrix to a path.

    * `(path-transform m3x2 src dst) -> dst`

*   **`path-svg`**: Tokenizes an SVG path data string.

    * `(path-svg d) -> commands`

*   **`vector-bounds-2d` / `vector-bounds-3d` / `vector-bounds-sphere`**: Calculates bounding volumes.

    * `(vector-bounds-3d verts [stride]) -> (min_v3 max_v3)`

*   **`vector-point-in-polygon`**: Checks if a point is within a polygon.

    * `(vector-point-in-polygon p paths winding_mode) -> :t | :nil`

## Vectorized Numeric Operations

These functions perform operations on entire vectors of numbers (`:nums`, `:reals`, `:fixeds`).

*   **`nums-abs` / `fixeds-frac` / `fixeds-floor` / `fixeds-ceil`**: Element-wise operations.

    * `(nums-abs nums [out_nums]) -> nums`

    * `(fixeds-frac fixeds [out_fixeds]) -> fixeds`

    * `(fixeds-floor fixeds [out_fixeds]) -> fixeds`

    * `(fixeds-ceil fixeds [out_fixeds]) -> fixeds`

*   **`nums-scale`**: Scales a vector by a scalar.

    * `(nums-scale nums scale [out_nums]) -> nums`

*   **`nums-add` / `nums-sub` / `nums-mul` / `nums-div` / `nums-mod` / `nums-min` / `nums-max`**: Element-wise arithmetic/comparison.

    * `(nums-add nums1 nums2 [out_nums]) -> nums`

    * `(nums-min nums1 nums2 [out_nums]) -> nums`

    * `(nums-max nums1 nums2 [out_nums]) -> nums`

*   **`nums-sum`**: Returns the sum of all elements.

    * `(nums-sum nums) -> num`

*   **`nums-dot`**: Calculates the dot product.

    * `(nums-dot nums1 nums2) -> num`

*   **`reals-quant` / `quant`**: Quantizes a real vector or single real value.

    * `(reals-quant reals tol [reals]) -> reals`

    * `(quant real tol) -> real`

*   **`mat4x4-mul` / `mat4x4-inv` / `mat4x4-vec4-mul` / `mat4x4-vec3-mul`**: Matrix operations.

    * `(mat4x4-mul reals reals [reals]) -> reals`

    * `(mat4x4-inv reals [reals]) -> reals`

    * `(mat4x4-vec4-mul reals reals [reals]) -> reals`

    * `(mat4x4-vec3-mul reals reals [reals]) -> reals`

*   **`mat3x2-mul-f`**: Multiplies two 3x2 fixed-point matrices.

*   **`opt-vector`**: Optimizes a vector.

## Math and Optimization

*   **`iso-surface`**: Generates triangles from an isosurface grid.

    * `(iso-surface grid isolevel) -> tris`

*   **`vertex-interp`**: Interpolates between two vertices based on an isolevel.

*   **`opt-mesh`**: Optimizes a 3D mesh (removes duplicate vertices).

*   **`gen-norms`**: Generates normals for a mesh.

## XML and SVG Parsing

*   **`XML-parse`**: A low-level XML tokenizer with callbacks.

    * `(XML-parse stream fnc_in fnc_out fnc_text)`

*   **`SVG-load`**: Renders an SVG stream to a canvas.

    * `(SVG-load stream [scale]) -> :nil | canvas`

*   **`SVG-info`**: Returns information about an SVG stream.

    * `(SVG-info stream) -> (width height type) | (-1 -1 -1)`

## Text and Regular Expressions

*   **`Buffer`**: A class for managing a text buffer.

    * `(Buffer &optional mode syntax) -> buffer`

*   **`escape` / `unescape` / `escape-regexp`**: Character escaping.

    * `(escape str) -> str`

*   **`char-class`**: Creates a sorted, interned character class string.

    * `(char-class key) -> str`

*   **`Dictionary`**: A class for managing a dictionary of words.

*   **`Regexp`**: A class for regular expression searching.

*   **`Search`**: Base class for search algorithms.

*   **`found?` / `match?` / `matches`**: Search and match functions.

    * `(match? text regexp) -> :t | :nil`

*   **`substr`**: Finds all occurrences of a substring.

    * `(substr text substr) -> matches`

*   **`query`**: Creates a search query.

    * `(query pattern whole_words regexp) -> (engine meta pattern)`

*   **`replace-compile`**: Compiles a replacement string.

    * `(replace-compile rep_str) -> compiled_rep`

*   **`replace-matches` / `replace-regex` / `replace-str`**: Replaces matches in text.

    * `(replace-regex text pattern compiled|rep_str) -> text`

*   **`replace-edits` / `replace-regex-edits` / `replace-str-edits`**: Generates edit operations for a buffer.

*   **`Syntax`**: A class for syntax highlighting.

*   **`reflow`**: Reflows words into lines of a given width.

    * `(reflow words line_width [indent tab_width]) -> lines`

## Tasks and Parallelism

*   **`pipe-farm`**: Runs a farm of jobs via pipes and collects results.

    * `(pipe-farm jobs [retry_timeout]) -> ((job result) ...)`

*   **`Farm` / `Global` / `Local`**: Classes for managing task pools.

*   **`Pipe` / `pipe-run` / `pipe-split`**: Pipeline creation and execution.

    * `(pipe-run cmdline [outfun])`

    * `(pipe-split cmdline) -> ((mode cmd) ...)`

*   **`open-child` / `open-pipe` / `open-remote` / `open-task`**: Low-level task creation and communication.

    * `(open-child task mode) -> net_id`

    * `(open-pipe tasks [modes]) -> ([net_id | 0] ...)`

    * `(open-remote task node mode) -> net_id`

    * `(open-task task node mode key_num reply)`

*   **`start` / `stop` / `restart`**: Controls child tasks.

## Streams and Compression

*   **`stream-diff` / `stream-patch`**: Diffs or patches streams in standard format.

    * `(stream-diff a b c)`

*   **`huffman-compress` / `huffman-decompress`**: Huffman coding.

    * `(huffman-compress in_stream out_stream token_bits)`

    * `(huffman-decompress in_stream out_stream token_bits)`

*   **`huffman-build-freq-map` / `huffman-write-codebook` / `huffman-read-codebook`**: Huffman model management.

*   **`huffman-compress-static` / `huffman-decompress-static`**: Static Huffman coding.

*   **`rle-compress` / `rle-decompress`**: Run-length encoding.

    * `(rle-compress in_stream out_stream [token_bits run_bits])`

    * `(rle-decompress in_stream out_stream [token_bits run_bits])`

*   **`lz4-compress` / `lz4-decompress`**: LZ4 frame compression and decompression.

    * `(lz4-compress in_stream out_stream [window_size])`

    * `(lz4-decompress in_stream out_stream [window_size])`

*   **`lz4-encode` / `lz4-decode`**: Low-level LZ4 block encoding and decoding.

    * `(lz4-encode ring_buf pos chunk hash_table) -> (comp_blk new_pos)`

    * `(lz4-decode ring_buf pos comp_blk) -> new_pos`

*   **`build_tree_and_codebook`**: Builds a Huffman tree and codebook.

## Command-Line Options

*   **`options`**: Parses command-line options from a stdio object.

    * `(options stdio optlist) -> :nil | args`

*   **`opt-flag` / `opt-num` / `opt-str`**: Option handlers.

*   **`options-find` / `options-print` / `options-split`**: Utilities for option parsing.

## Date and Time

*   **`date`**: Returns current date and time components.

    * `(date &optional seconds) -> (second minute hour date month year week)`

*   **`encode-date` / `decode-date`**: Converts between date components and strings.

*   **`timezone-init` / `timezone-lookup`**: Timezone management.

## Collections

*   **`Emap` / `Fmap` / `Fset` / `Lmap` / `Map` / `Set` / `Xmap` / `Xset`**: Various collection classes (hash maps, sets, etc.).

*   **`tree-load` / `tree-save`**: Loads/saves collection trees.

    * `(tree-load stream) -> tree`

*   **`gather` / `scatter`**: Gathers or scatters values into/from a map.

    * `(gather map key ...) -> (val ...)`

*   **`memoize`**: Memoizes function results.

*   **`tree-buckets` / `tree-collection?` / `tree-decode` / `tree-encode` / `tree-node` / `tree-type`**: Collection tree utilities.

## Structures, Enums, and Bitfields

*   **`structure` / `enums` / `bits`**: Defines data structures, enumerations, and bitfields.

    * `(structure name base [(byte field ...)] ...)`

*   **`getf` / `getf->` / `setf` / `setf->`**: Accesses or modifies structure fields.

    * `(getf obj field [offset]) -> value`

*   **`bits?`**: Checks if bits are set.

## Debugging

*   **`debug-brk`**: Sets a breakpoint.

    * `(debug-brk break-id &optional condition)`

*   **`profile-report`**: Generates a profile report.

    * `(profile-report name [reset])`

## Compiler and Assembler

*   **`within-compile-env` / `defcvar` / `deffvar` / `include`**: Compilation environment management.

    * `(within-compile-env lambda)`

    * `(defcvar sym val [sym val] ...)`

    * `(deffvar sym val [sym val] ...)`

    * `(include module)`

*   **`func-load` / `func-refs`**: Loads function objects and lists their references.

*   **`vp-rdef` / `emit` / `emit-vp-code`**: VP assembler primitives.

*   **`cscript` / `reset-reg-stack` / `def-reg-map`**: C-Script compiler.

*   **`def-func` / `def-func-end` / `abort` / `assert`**: Function definition and safety.

*   **`def-class` / `def-method` / `entry` / `exit` / `call` / `jump` / `v-call` / `gen-vtable`**: Low-level class/method definition for the assembler.

*   **`make` / `compile` / `make-all` / `remake` / `make-platforms`**: Build system commands.

## Audio Service

*   **`audio-add-rpc` / `audio-play-rpc` / `audio-change-rpc` / `audio-remove-rpc`**: RPC interface to the audio service.

    * `(audio-add-rpc file_path) -> handle`
