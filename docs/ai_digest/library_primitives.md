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

## SVG

These functions are used for SVG parsing.

*   **`SVG-Canvas`**: Renders an SVG stream to a canvas.

    * `(SVG-Canvas stream &optional scale) -> :nil | canvas`

*   **`SVG-info`**: Returns information about an SVG stream.

    * `(SVG-info stream) -> (width height type) | (-1 -1 -1)`

## Text

These functions and classes are used for text manipulation.

*   **`Buffer`**: A class for managing a text buffer.

    * `(Buffer &optional mode syntax) -> buffer`

*   **`escape`**: Escapes special characters in a string.

    * `(escape string) -> string`

*   **`unescape`**: Unescapes special characters in a string.

    * `(unescape string) -> string`

*   **`char-class`**: Creates a character class string.

    * `(char-class key) -> string`

*   **`Dictionary`**: A class for managing a dictionary of words.

    * `(Dictionary &optional num_buckets) -> dictionary`

*   **`Kmplps`**: A class for Knuth-Morris-Pratt string searching.

    * `(Kmplps &optional num_buckets) -> kmplps`

*   **`Regexp`**: A class for regular expression searching.

    * `(Regexp &optional num_buckets) -> regexp`

*   **`Search`**: The base class for search algorithms.

    * `(Search)`

*   **`found?`**: Checks if a substring is found in a string.

    * `(found? text substring) -> :t | :nil`

*   **`match?`**: Checks if a regular expression matches a string.

    * `(match? text regexp) -> :t | :nil`

*   **`substr`**: Finds all occurrences of a substring in a string.

    * `(substr text substring) -> matches`

*   **`matches`**: Finds all occurrences of a regular expression in a string.

    * `(matches text regexp) -> matches`

*   **`query`**: Creates a search query.

    * `(query pattern whole_words regexp) -> (engine pattern meta)`

*   **`Substr`**: A class for substring searching.

    * `(Substr &optional num_buckets) -> substr`

*   **`Syntax`**: A class for syntax highlighting.

    * `(Syntax) -> syntax`

## Tasks

These functions and classes are used for task management.

*   **`pipe-farm`**: Runs a pipe farm and collects the output.

    * `(pipe-farm jobs &optional retry_timeout) -> ((job result) ...)`

*   **`Farm`**: A class for managing a farm of tasks.

    * `(Farm fnc_create fnc_destroy size) -> farm`

*   **`Global`**: A class for managing global tasks.

    * `(Global fnc_create fnc_destroy) -> global`

*   **`Local`**: A class for managing local tasks.

    * `(Local fnc_create fnc_destroy &optional herd_max herd_init herd_growth) -> local`

*   **`Pipe`**: A class for creating and managing a pipeline of commands.

    * `(Pipe cmdline &optional user_select) -> pipe | :nil`

*   **`pipe-run`**: Runs a command pipeline.

    * `(pipe-run cmdline &optional outfun)`

*   **`pipe-split`**: Splits a command line into a list of commands.

    * `(pipe-split cmdline) -> (command ...)`

## Streams

These functions are used for stream manipulation.

### Diff and Patch

*   **`stream-diff`**: Diffs two streams.

    * `(stream-diff stream-a stream-b stream-c)`

*   **`stream-patch`**: Patches a stream with a diff.

    * `(stream-patch stream-a stream-b stream-c)`

### Huffman Coding

*   **`huffman-compress`**: Compresses a stream using adaptive Huffman coding.

    * `(huffman-compress in-stream out-stream token-bits)`

*   **`huffman-decompress`**: Decompresses a stream using adaptive Huffman coding.

    * `(huffman-decompress in-stream out-stream token-bits)`

*   **`huffman-build-freq-map`**: Builds a frequency map from a stream.

    * `(huffman-build-freq-map in-stream token-bits) -> freq-map`

*   **`huffman-write-codebook`**: Writes a codebook to a stream.

    * `(huffman-write-codebook out-stream token-bits freq-map)`

*   **`huffman-read-codebook`**: Reads a codebook from a stream.

    * `(huffman-read-codebook in-stream) -> (root codebook token-bits)`

*   **`huffman-compress-static`**: Compresses a stream using static Huffman coding.

    * `(huffman-compress-static in-stream out-stream model)`

*   **`huffman-decompress-static`**: Decompresses a stream using static Huffman coding.

    * `(huffman-decompress-static in-stream out-stream model)`

### Run-Length Encoding

*   **`rle-compress`**: Compresses a stream using run-length encoding.

    * `(rle-compress in-stream out-stream &optional token-bits run-bits)`

*   **`rle-decompress`**: Decompresses a stream using run-length encoding.

    * `(rle-decompress in-stream out-stream &optional token-bits run-bits)`

## Command-Line Options

These functions are used for parsing command-line options.

*   **`options`**: Parses command-line options.

    * `(options stdio option-list) -> :nil | args`

*   **`opt-flag`**: Creates an option handler for a flag.

    * `(opt-flag 'option-variable) -> lambda`

*   **`opt-num`**: Creates an option handler for a number.

    * `(opt-num 'option-variable) -> lambda`

*   **`opt-str`**: Creates an option handler for a string.

    * `(opt-str 'option-variable) -> lambda`

## Math

These functions are used for math operations.

### Matrix Math

*   **`Mat3x3-unity`**: Returns a 3x3 identity matrix.
*   **`Mat4x4-unity`**: Returns a 4x4 identity matrix.
*   **`Mat3x3-rotx`**: Returns a 3x3 rotation matrix around the x-axis.
*   **`Mat3x3-roty`**: Returns a 3x3 rotation matrix around the y-axis.
*   **`Mat3x3-rotz`**: Returns a 3x3 rotation matrix around the z-axis.
*   **`Mat3x3-scale`**: Returns a 3x3 scaling matrix.
*   **`Mat4x4-rotx`**: Returns a 4x4 rotation matrix around the x-axis.
*   **`Mat4x4-roty`**: Returns a 4x4 rotation matrix around the y-axis.
*   **`Mat4x4-rotz`**: Returns a 4x4 rotation matrix around the z-axis.
*   **`Mat4x4-translate`**: Returns a 4x4 translation matrix.
*   **`Mat4x4-scale`**: Returns a 4x4 scaling matrix.
*   **`Mat4x4-frustum`**: Returns a 4x4 frustum projection matrix.
*   **`mat3x3-vec3-mul`**: Multiplies a 3x3 matrix by a 3D vector.
*   **`mat4x4-vec3-mul`**: Multiplies a 4x4 matrix by a 3D vector.
*   **`mat4x4-vec4-mul`**: Multiplies a 4x4 matrix by a 4D vector.
*   **`mat3x3-mul`**: Multiplies two 3x3 matrices.
*   **`mat4x4-mul`**: Multiplies two 4x4 matrices.
*   **`mat4x4-invert`**: Inverts a 4x4 matrix.
*   **`print-mat`**: Prints a matrix.

### Mesh

*   **`Mesh`**: A class for representing 3D meshes.
*   **`Mesh-sphere`**: A class for creating a sphere mesh.
*   **`Mesh-torus`**: A class for creating a torus mesh.
*   **`Mesh-iso`**: A class for creating a mesh from an isosurface.
*   **`Mesh-data`**: A class for creating a mesh from data.

### Scene

*   **`Scene-node`**: A class for representing a node in a scene graph.
*   **`Scene-object`**: A class for representing an object in a scene.
*   **`Scene`**: A class for representing a scene.

### Surface

*   **`Iso`**: A class for representing an isosurface.
*   **`Iso-sphere`**: A class for representing a sphere isosurface.
*   **`Iso-cube`**: A class for representing a cube isosurface.
*   **`Iso-tetra`**: A class for representing a tetrahedron isosurface.
*   **`Iso-capsule`**: A class for representing a capsule isosurface.

### Vector Math

*   **`Vec2-f`**: Creates a 2D fixed-point vector.
*   **`Vec3-f`**: Creates a 3D fixed-point vector.
*   **`Vec4-f`**: Creates a 4D fixed-point vector.
*   **`Vec3-r`**: Creates a 3D real-point vector.
*   **`Vec4-r`**: Creates a 4D real-point vector.
*   **`vec-clamp`**: Clamps a vector between two other vectors.
*   **`vec-reflect`**: Reflects a vector off a surface.
*   **`vec-length-squared`**: Calculates the squared length of a vector.
*   **`vec-length`**: Calculates the length of a vector.
*   **`vec-norm`**: Normalizes a vector.
*   **`vec-sdist`**: Calculates the squared distance between two vectors.
*   **`vec-dist`**: Calculates the distance between two vectors.
*   **`vec-dist-to-line`**: Calculates the distance from a point to a line.
*   **`vec-sdist-to-line`**: Calculates the squared distance from a point to a line.
*   **`vec-manhattan-distance`**: Calculates the Manhattan distance between two vectors.
*   **`vec-euclidean-distance`**: Calculates the Euclidean distance between two vectors.
*   **`vec-squared-euclidean-distance`**: Calculates the squared Euclidean distance between two vectors.
*   **`vec-chebyshev-distance`**: Calculates the Chebyshev distance between two vectors.
*   **`vec-perp-2d`**: Calculates the perpendicular of a 2D vector.
*   **`vec-det`**: Calculates the determinant of two 2D vectors.
*   **`vec-cross-3d`**: Calculates the cross product of two 3D vectors.
*   **`vec-intersect-2d`**: Calculates the intersection of two 2D lines.
*   **`vec-intersect-lines-2d`**: Calculates the intersection of two 2D line segments.
*   **`vec-collide-lines-2d`**: Checks if two 2D line segments collide.
*   **`vec-collide-thick-lines-2d`**: Checks if two thick 2D line segments collide.
*   **`bounding-box`**: Calculates the bounding box of a set of vertices.
*   **`bounding-sphere`**: Calculates the bounding sphere of a set of vertices.

## URLs

This function is used for URL manipulation.

*   **`url-ext`**: Extends a URL.

    * `(url-ext url cursor &optional context) -> extended-url`

## Files and Directories

These functions are used for file and directory manipulation.

*   **`files-all`**: Returns a list of all files in a directory and its subdirectories.

    * `(files-all &optional root extensions cut_start cut_end) -> (file ...)`

*   **`files-dirs`**: Returns a list of all directories in a list of paths.

    * `(files-dirs paths) -> (directory ...)`

*   **`files-all-depends`**: Returns a list of all dependencies for a list of files.

    * `(files-all-depends files &optional implicit-imports) -> (file ...)`

*   **`files-depends`**: Returns a list of immediate dependencies for a file.

    * `(files-depends file) -> (file ...)`

## Debugging

These functions are used for debugging.

*   **`debug-brk`**: Sets a breakpoint.

    * `(debug-brk break-id &optional condition)`

*   **`defun`**: Redefined to include debugging instrumentation.

    * `(defun name args &rest body)`

*   **`defmethod`**: Redefined to include debugging instrumentation.

    * `(defmethod name args &rest body)`

*   **`profile-report`**: Generates a profile report.

    * `(profile-report name &optional reset)`

## Date and Time

These functions are used for date and time manipulation.

*   **`date`**: Returns the current date and time.

    * `(date &optional seconds) -> (second minute hour date month year week)`

*   **`encode-date`**: Encodes a date and time into a string.

    * `(encode-date &optional time-data) -> string`

*   **`decode-date`**: Decodes a date and time string.

    * `(decode-date date-string) -> (second minute hour date month year week)`

*   **`timezone-init`**: Initializes the local timezone.

    * `(timezone-init timezone-location)`

*   **`timezone-lookup`**: Looks up a timezone.

    * `(timezone-lookup property value) -> timezone-info`

## Constants

These are the constants defined in the `lib/consts` directory.

### `lib/consts/chars.inc`

*   `+char_backspace`
*   `+char_delete`
*   `+char_tab`
*   `+char_lf`
*   `+char_ff`
*   `+char_cr`
*   `+char_esc`
*   `+char_double_quote`
*   `+char_plusminus`
*   `+char_pound`
*   `+char_space`
*   `+char_underscore`
*   `+char_ampersand`
*   `+char_pipe`
*   `+char_hash`
*   `+char_lrb`
*   `+char_rrb`
*   `+char_lsb`
*   `+char_rsb`
*   `+char_lcb`
*   `+char_rcb`
*   `+char_quote`
*   `+char_tick`
*   `+char_comma`
*   `+char_tilde`
*   `+char_grave`
*   `+char_semi`
*   `+char_colon`
*   `+char_dot`
*   `+char_at`
*   `+char_lt`
*   `+char_gt`
*   `+char_equal`
*   `+char_plus`
*   `+char_minus`
*   `+char_multiply`
*   `+char_divide`
*   `+char_question`
*   `+char_caret`
*   `+char_0`
*   `+char_9`
*   `+char_a`
*   `+char_A`
*   `+char_b`
*   `+char_B`
*   `+char_o`
*   `+char_O`
*   `+char_x`
*   `+char_X`
*   `+char_z`
*   `+char_Z`
*   `+char_backslash`

### `lib/consts/colors.inc`

*   `+argb_black`
*   `+argb_white`
*   `+argb_blue`
*   `+argb_green`
*   `+argb_red`
*   `+argb_cyan`
*   `+argb_yellow`
*   `+argb_magenta`
*   `+argb_grey1` - `+argb_grey15`
*   `+argb_green6`
*   `+argb_orange`
*   `+argb_steel`
*   `+argb_marker_yellow`

### `lib/consts/scodes.inc`

A large number of scancode constants, from `+sc_unknown` to `+sc_sleep`.

## Collection Trees

These functions are used for loading and saving collection trees.

*   **`tree-load`**: Loads a collection tree from a stream.

    * `(tree-load stream) -> tree`

*   **`tree-save`**: Saves a collection tree to a stream.

    * `(tree-save stream tree &optional key_filters) -> tree`

## Collection Classes

These are the collection classes available in ChrysaLisp.

*   **`Emap`**: A generic environment map.

    * `(Emap &optional num_buckets) -> emap`

*   **`Fmap`**: A fast hash map.

    * `(Fmap &optional num_buckets) -> fmap`

*   **`Fset`**: A fast hash set.

    * `(Fset &optional num_buckets) -> fset`

*   **`Lmap`**: A linear map.

    * `(Lmap) -> lmap`

*   **`Map`**: The base class for maps.

    * `(Map)`

*   **`Set`**: The base class for sets.

    * `(Set)`

*   **`Xmap`**: A generic hash map with custom hash and comparison functions.

    * `(Xmap &optional num_buckets cmp_fnc hash_fnc) -> xmap`

*   **`Xset`**: A generic hash set with custom hash and comparison functions.

    * `(Xset &optional num_buckets cmp_fnc hash_fnc) -> xset`

## Collections

These functions are used for working with collections.

*   **`gather`**: Gathers a list of values from a map.

    * `(gather map &rest keys) -> (value ...)`

*   **`scatter`**: Scatters a list of key-value pairs into a map.

    * `(scatter map &rest key-value-pairs) -> map`

*   **`memoize`**: Memoizes the result of a function call.

    * `(memoize key form &optional num_buckets)`

## Structures, Enums, and Bitfields

These macros are used for defining structures, enums, and bitfields.

*   **`structure`**: Defines a new structure.

    * `(structure name base &rest lines)`

*   **`_structure`**: Helper function for `structure`.

    * `(_structure base line)`

*   **`getf`**: Gets the value of a field from a structure.

    * `(getf obj field [offset]) -> value`

*   **`setf`**: Sets the value of a field in a structure.

    * `(setf obj field value [offset]) -> obj`

*   **`setf->`**: Chains `setf` calls.

    * `(setf-> msg form ...)`

*   **`enums`**: Defines an enumeration.

    * `(enums name base &rest lines)`

*   **`bits`**: Defines a bitfield.

    * `(bits name base &rest lines)`

*   **`bits?`**: Checks if bits are set in a value.

    * `(bits? val &rest masks) -> :t | :nil`

*   **`bit-mask`**: Creates a bit mask.

    * `(bit-mask &rest masks) -> val`

## Class and Method Macros

These macros are used for defining classes and methods.

*   **`.?`**: Checks if a method exists on an object.

    * `(.? this method) -> :nil | lambda`

*   **`.super`**: Calls a superclass method.

    * `(.super this :method [arg ...])`

*   **`defmethod`**: Defines a new method for a class.

    * `(defmethod name ([arg ...]) body)`

*   **`defabstractmethod`**: Defines an abstract method.

    * `(defabstractmethod ([arg ...]) body)`

*   **`deffimethod`**: Defines a method using a foreign function interface.

    * `(deffimethod name ffi)`

*   **`defgetmethod`**: Defines a getter method for a field.

    * `(defgetmethod field)`

*   **`defsetmethod`**: Defines a setter method for a field.

    * `(defsetmethod field)`

*   **`defclass`**: Defines a new class.

    * `(defclass Name ([arg ...]) (super ...)|:nil body)`

*   **`.->`**: Chains method calls.

    * `(.-> this form ...)`

*   **`raise`**: Binds fields from an object to local variables.

    * `(raise field | (sym val) ...)`

*   **`lower`**: Sets fields on an object from local variables.

    * `(lower field | (field sym) ...)`

## Boot Image

These functions are used for creating boot images.

*   **`boot-image`**: Creates a boot image.

    * `(boot-image &optional *funcs* *abi* *cpu*)`

*   **`func-obj-path`**: Returns the object path for a function.

    * `(func-obj-path function-name) -> path`

## VP Optimizer

This function is part of the VP optimizer.

*   **`opt-emit-list`**: Optimizes a list of emitted instructions.

    * `(opt-emit-list emit_list emit_start emit_end)`

## VP Assembler

These functions are part of the VP assembler.

*   **`vp-def`**: Defines VP registers.

    * `(vp-def _ &optional l)`

*   **`emit`**: Emits an instruction.

    * `(emit &rest _)`

*   **`emit-vp-code`**: Emits VP code.

    * `(emit-vp-code emit_list) -> code`

## Stack Scopes

These functions are used for managing stack scopes.

*   **`scope-operator`**: Defines a new operator.

    * `(scope-operator name precedence &optional associativity implementation)`

*   **`scope-new`**: Creates a new scope.

    * `(scope-new)`

*   **`scope-get`**: Gets the offset of a scope.

    * `(scope-get level) -> offset`

*   **`pop-scope-checked`**: Pops a scope and checks for balance.

    * `(pop-scope-checked)`

*   **`scope-get-sym`**: Gets a symbol from a scope.

    * `(scope-get-sym name) -> symbol-info`

*   **`scope-def-sym`**: Defines a symbol in a scope.

    * `(scope-def-sym name type &rest attributes)`

*   **`scope-unwind`**: Unwinds the current scope.

    * `(scope-unwind)`

*   **`def-vars`**: Defines variables in the current scope.

    * `(def-vars &rest lines)`

*   **`push-scope`**: Pushes a new scope onto the stack.

    * `(push-scope)`

*   **`pop-scope`**: Pops a scope from the stack.

    * `(pop-scope)`

*   **`pop-scope-syms`**: Pops a scope and returns unused symbols.

    * `(pop-scope-syms) -> unused-symbols`

*   **`scope-used`**: Marks symbols as used.

    * `(scope-used &rest names)`

*   **`return`**: Returns from the current function.

    * `(return)`

## Function Definition

These functions are used for defining functions.

*   **`def-func`**: Defines a new function.

    * `(def-func *func_name* &optional *func_align* *func_stack*)`

*   **`def-func-end`**: Ends a function definition.

    * `(def-func-end)`

*   **`abort`**: Aborts the program.

    * `(abort &optional message)`

*   **`assert`**: Asserts that a condition is true.

    * `(assert condition &optional message)`

## C-Script Optimizer

This function is part of the C-Script optimizer.

*   **`opt-inst-list`**: Optimizes a list of instructions.

    * `(opt-inst-list instruction-list)`

## C-Script Compiler

These functions are part of the C-Script compiler.

*   **`cscript`**: Compiles a C-Script expression.

    * `(cscript expression)`

*   **`reset-reg-stack`**: Resets the register stack.

    * `(reset-reg-stack _)`

*   **`def-reg-map`**: Defines the register map.

    * `(def-reg-map pre spill)`

*   **`compile-deref?`**: Compiles a dereference operation if needed.

    * `(compile-deref?)`

*   **`compile-arrow`**: Compiles an arrow operation.

    * `(compile-arrow &optional _)`

## Structured Coding

These functions provide structured coding constructs for the VP assembler.

*   **`def-enum`**: Defines an enumeration.

    * `(def-enum name base &rest lines)`

*   **`def-struct`**: Defines a structure.

    * `(def-struct name base &rest lines)`

*   **`def-bit`**: Defines a bitfield.

    * `(def-bit name base &rest lines)`

*   **`errorcase`**: A macro that only executes its body if `*debug_mode*` is greater than 0.

    * `(errorcase &rest body)`

*   **`noterrorcase`**: A macro that only executes its body if `*debug_mode*` is 0 or less.

    * `(noterrorcase &rest body)`

*   **`errorif`**: Jumps to a label if a condition is true, only in debug mode.

    * `(errorif condition label)`

*   **`errorifnot`**: Jumps to a label if a condition is false, only in debug mode.

    * `(errorifnot condition label)`

*   **`errorif-lisp-args-len`**: Checks the number of Lisp arguments, only in debug mode.

    * `(errorif-lisp-args-len label args condition length)`

*   **`errorif-lisp-args-sig`**: Checks the signature of Lisp arguments, only in debug mode.

    * `(errorif-lisp-args-sig label args len1 &optional len2)`

*   **`errorif-lisp-args-match`**: Checks if Lisp arguments match a class, only in debug mode.

    * `(errorif-lisp-args-match label args class len)`

*   **`errorif-lisp-args-type`**: Checks the type of Lisp arguments, only in debug mode.

    * `(errorif-lisp-args-type label args class len)`

*   **`switch`**: Starts a switch block.

    * `(switch &optional _)`

*   **`endswitch`**: Ends a switch block.

    * `(endswitch)`

*   **`vpcase`**: A case in a switch block.

    * `(vpcase &rest expressions)`

*   **`vpcasenot`**: A case in a switch block, with a negated condition.

    * `(vpcasenot &rest expressions)`

*   **`default`**: The default case in a switch block.

    * `(default)`

*   **`nextcaseif`**: Jumps to the next case if a condition is true.

    * `(nextcaseif condition)`

*   **`nextcaseifnot`**: Jumps to the next case if a condition is false.

    * `(nextcaseifnot condition)`

*   **`vpif`**: An if statement.

    * `(vpif &rest expressions)`

*   **`vpifnot`**: An if statement with a negated condition.

    * `(vpifnot &rest expressions)`

*   **`else`**: An else block.

    * `(else)`

*   **`elseif`**: An else if block.

    * `(elseif &rest expressions)`

*   **`elseifnot`**: An else if block with a negated condition.

    * `(elseifnot &rest expressions)`

*   **`endif`**: Ends an if block.

    * `(endif)`

*   **`break`**: Breaks out of a loop.

    * `(break &optional label)`

*   **`breakif`**: Breaks out of a loop if a condition is true.

    * `(breakif &rest expressions)`

*   **`breakifnot`**: Breaks out of a loop if a condition is false.

    * `(breakifnot &rest expressions)`

*   **`goto`**: Jumps to a label.

    * `(goto label)`

*   **`gotoif`**: Jumps to a label if a condition is true.

    * `(gotoif condition label)`

*   **`gotoifnot`**: Jumps to a label if a condition is false.

    * `(gotoifnot condition label)`

*   **`exitif`**: Exits a loop if a condition is true.

    * `(exitif condition)`

*   **`exitifnot`**: Exits a loop if a condition is false.

    * `(exitifnot condition)`

*   **`loop-start`**: Starts a loop.

    * `(loop-start &optional label)`

*   **`loop-end`**: Ends a loop.

    * `(loop-end)`

*   **`loop-while`**: A while loop.

    * `(loop-while &rest expressions)`

*   **`loop-whilenot`**: A while loop with a negated condition.

    * `(loop-whilenot &rest expressions)`

*   **`loop-until`**: An until loop.

    * `(loop-until &rest expressions)`

*   **`loop-untilnot`**: An until loop with a negated condition.

    * `(loop-untilnot &rest expressions)`

*   **`continue`**: Continues to the next iteration of a loop.

    * `(continue &optional label)`

*   **`continueif`**: Continues to the next iteration of a loop if a condition is true.

    * `(continueif &rest expressions)`

*   **`continueifnot`**: Continues to the next iteration of a loop if a condition is false.

    * `(continueifnot &rest expressions)`

*   **`repeatif`**: Repeats a loop if a condition is true.

    * `(repeatif condition)`

*   **`repeatifnot`**: Repeats a loop if a condition is false.

    * `(repeatifnot condition)`

## Class and Method Definition

These functions are used for defining classes and methods.

*   **`def-class`**: Defines a new class.

    * `(def-class class super &rest lines)`

*   **`def-method`**: Defines a new method for a class.

    * `(def-method class member &optional alignment)`

*   **`entry`**: Marks the entry point of a method.

    * `(entry &rest _)`

*   **`exit`**: Marks the exit point of a method.

    * `(exit &rest _)`

*   **`call`**: Calls a method.

    * `(call class member &optional in-params out-params)`

*   **`jump`**: Jumps to a method.

    * `(jump class member &optional in-params)`

*   **`s-call`**: Calls a superclass method.

    * `(s-call class member &optional in-params out-params)`

*   **`d-call`**: Calls a method directly.

    * `(d-call class member &optional in-params out-params)`

*   **`s-jump`**: Jumps to a superclass method.

    * `(s-jump class member &optional in-params)`

*   **`v-call`**: Calls a virtual method.

    * `(v-call class member &optional in-params out-params obj-reg dispatch-reg)`

*   **`d-jump`**: Jumps to a method directly.

    * `(d-jump class member &optional in-params)`

*   **`r-call`**: Calls a method via a register.

    * `(r-call class member &optional in-params out-params dispatch-reg)`

*   **`v-bind`**: Binds a virtual method to a register.

    * `(v-bind class member &optional obj-reg dispatch-reg)`

*   **`f-entry`**: Method entry with parameters.

    * `(f-entry class member in-params)`

*   **`f-exit`**: Method exit with parameters.

    * `(f-exit class member out-params)`

*   **`l-entry`**: Method entry with parameters from a list.

    * `(l-entry in-params)`

*   **`l-exit`**: Method exit with parameters to a list.

    * `(l-exit out-params)`

*   **`f-path`**: Returns the function path for a method.

    * `(f-path class member) -> path`

*   **`f-bind`**: Binds a method to a register.

    * `(f-bind class member reg)`

*   **`method-input`**: Returns the input parameters of a method.

    * `(method-input class member &optional index) -> params`

*   **`method-output`**: Returns the output parameters of a method.

    * `(method-output class member &optional index) -> params`

*   **`method-lookup`**: Looks up a method in a class.

    * `(method-lookup class member) -> method-info`

*   **`signature`**: Emits a vtable signature.

    * `(signature _)`

*   **`gen-type`**: Generates a type function for a class.

    * `(gen-type class)`

*   **`gen-create`**: Generates a create function for a class.

    * `(gen-create class &optional create-name)`

*   **`gen-vtable`**: Generates a vtable for a class.

    * `(gen-vtable class)`

## Assignment

These functions handle assignment between different types of operands.

*   **`assign`**: Assigns values from source to destination.

    * `(assign &optional src dst _)`

*   **`assign-src-type`**: Determines the type of a source operand.

    * `(assign-src-type operand) -> type`

## Assembler and Make System

These functions are part of the VP assembler and the build system.

*   **`compile`**: Compiles a list of files.

    * `(compile files &optional *abi* *cpu*)`

*   **`make-merge`**: Merges a string into a string list if it's not already present.

    * `(make-merge list-of-strings string-to-add)`

*   **`all-vp-files`**: Returns a list of all `.vp` files.

    * `(all-vp-files) -> (file ...)`

*   **`all-class-files`**: Returns a list of all `class.inc` files.

    * `(all-class-files) -> (file ...)`

*   **`make-info`**: Creates lists of immediate dependencies and products for a file.

    * `(make-info file) -> (dependencies products)`

*   **`file-age`**: Returns the modification time of a file, cached.

    * `(file-age file) -> num`

*   **`make`**: Compiles a list of files, automatically determining dependencies.

    * `(make &optional files *abi* *cpu*)`

*   **`make-boot-all`**: Creates a boot image with all dependencies.

    * `(make-boot-all &optional *abi* *cpu*)`

*   **`make-all`**: Compiles all `.vp` files.

    * `(make-all &optional files *abi* *cpu*)`

*   **`remake`**: Re-compiles files and creates a boot image.

    * `(remake &optional files *abi* *cpu*)`

*   **`remake-all`**: Re-compiles all files and creates a boot image.

    * `(remake-all &optional files *abi* *cpu*)`

*   **`make-platforms`**: Runs `make` for all supported platforms.

    * `(make-platforms)`

*   **`make-all-platforms`**: Runs `make-all` for all supported platforms.

    * `(make-all-platforms)`

*   **`remake-platforms`**: Runs `remake` for all supported platforms.

    * `(remake-platforms)`

*   **`remake-all-platforms`**: Runs `remake-all` for all supported platforms.

    * `(remake-all-platforms)`

*   **`make-test`**: Runs a compile test and prints timing statistics.

    * `(make-test &optional iterations *abi* *cpu*)`

*   **`compile-test`**: Compiles all files individually and then all together.

    * `(compile-test &optional *abi* *cpu*)`

## Anaphoric Macros

These macros provide anaphoric variants of common control structures, where `it`
is implicitly bound to the result of the test expression.

*   **`aif`**: Anaphoric `if`.

    * `(aif test-form then-form [else-form])`

*   **`awhen`**: Anaphoric `when`.

    * `(awhen test-form &rest body)`

*   **`awhile`**: Anaphoric `while`.

    * `(awhile test-form &rest body)`

*   **`aand`**: Anaphoric `and`.

    * `(aand [form] ...)`

*   **`acond`**: Anaphoric `cond`.

    * `(acond (test-form body) ...)`

*   **`aeach`**: Anaphoric `each`.

    * `(aeach sequence body)`

*   **`asome`**: Anaphoric `some`.

    * `(asome sequence body)`

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
