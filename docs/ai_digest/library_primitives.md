# Library and System Primitives

This document covers the system interfaces, class libraries, and support
modules provided across the ChrysaLisp OS.

## System Kernel and Task Management

Low-level primitives for task concurrency, cooperative multitasking, and system
introspection.

*	**`kernel-stats`**: Retrieves runtime telemetry from the local kernel.

	*	`(kernel-stats) -> (task_count mem_used mem_avail max_stack)`

*	**`load-path`**: Returns the relative directory path where compiled object
	files are loaded for the current architecture.

	*	`(load-path) -> str`

*	**`os` / `cpu` / `abi`**: Returns the target host OS, CPU architecture, and
	ABI symbols.

	*	`(os) -> sym`

	*	`(cpu) -> sym`

	*	`(abi) -> sym`

*	**`task-flags`**: Retrieves the status and signal flags of the current task.

	*	`(task-flags) -> flags`

*	**`task-mbox`**: Returns the primary mailbox `netid` allocated for the
	current task.

	*	`(task-mbox) -> netid`

*	**`task-count`**: Adjusts or queries the task load bias on the current node.

	*	`(task-count bias) -> num`

*	**`task-sleep`**: Cooperatively suspends the task for a duration in
	microseconds (`0` yields the CPU).

	*	`(task-sleep usec) -> :t`

*	**`task-slice`**: Cooperatively yields execution if the current task
	timeslice has expired.

	*	`(task-slice) -> :t`

*	**`task-mboxes`**: Allocates an array containing the task's primary mailbox
	and `size - 1` new disposable mailboxes.

	*	`(task-mboxes size) -> ((task-mbox) [temp_mbox] ...)`

*	**`task-nodeid`**: Extracts the 16-byte `node_id` component from a mailbox
	or the current task.

	*	`(task-nodeid [mbox]) -> nodeid`

*	**`task-timeout`**: Scales a duration in seconds into architecture-scaled
	nanoseconds for timeouts.

	*	`(task-timeout s) -> ns`

*	**`open-task`**: Dispatches a raw task spawn request across the cluster.

	*	`(open-task task node mode key_num reply)`

*	**`open-child`**: Spawns a child task on the local node.

	*	`(open-child task mode) -> net_id`

*	**`open-remote`**: Spawns a task on a specific remote node.

	*	`(open-remote task node mode) -> net_id`

*	**`open-pipe`**: Spawns a series of tasks connected in an execution
	pipeline.

	*	`(open-pipe tasks [modes]) -> ([net_id | 0] ...)`

## Distributed Messaging (Mail System)

Location-transparent inter-process communication built on ephemeral, disposable
`netid` addresses.

*	**`mail-mbox`**: Allocates a locally unique, disposable mailbox `netid`.

	*	`(mail-mbox) -> netid`

*	**`mail-declare`**: Advertises a named service endpoint across the cluster.

	*	`(mail-declare mbox name info) -> service_key`

*	**`mail-nodes`**: Returns all active node IDs discovered on the network.

	*	`(mail-nodes) -> (node_id ...)`

*	**`mail-enquire`**: Queries the cluster for service entries matching a name
	prefix.

	*	`(mail-enquire prefix) -> (service_entry ...)`

*	**`mail-forget`**: Withdraws a service advertisement from the network.

	*	`(mail-forget service_key)`

*	**`mail-poll`**: Non-blocking inspection of mailboxes for pending messages.

	*	`(mail-poll mboxs) -> :nil | index`

*	**`mail-validate`**: Checks whether a mailbox ID is currently valid.

	*	`(mail-validate mbox) -> :t | :nil`

*	**`mail-read`**: Blocks until a message is received in the specified
	mailbox.

	*	`(mail-read mbox) -> msg`

*	**`mail-select`**: Blocks on a list of mailboxes, returning the index of the
	first ready mailbox.

	*	`(mail-select mboxs) -> index`

*	**`mail-send`**: Dispatches a message to a destination `netid`.

	*	`(mail-send mbox obj)`

*	**`mail-timeout`**: Schedules a timeout signal message sent to a mailbox.
	Passing `0` cancels the timeout.

	*	`(mail-timeout mbox ns id) -> mbox`

## Platform Implementation Interface (PII)

Direct primitives bridging ChrysaLisp to host operating system drivers.

*	**`pii-dirlist`**: Reads the contents of a directory on the host file
	system.

	*	`(pii-dirlist path) -> info`

*	**`pii-fstat`**: Retrieves file status metadata from the host file system.

	*	`(pii-fstat path) -> (mtime fsize mode) | :nil`

*	**`pii-read-char` / `pii-write-char`**: Unbuffered character I/O on host
	file descriptors.

	*	`(pii-read-char fd) -> char`

	*	`(pii-write-char fd char) -> char`

*	**`pii-remove`**: Deletes a file on the host file system.

	*	`(pii-remove path) -> num`

*	**`pii-time`**: Reads the host high-resolution monotonic timer in
	nanoseconds.

	*	`(pii-time) -> ns`

## File and Directory Utilities

Filesystem traversal, dependency analysis, and path resolution.

*	**`files-all`**: Recursively discovers all files matching extensions from a
	root directory.

	*	`(files-all [root exts cut_start cut_end]) -> paths`

*	**`files-dirs`**: Extracts unique directory paths from a list of file paths.

	*	`(files-dirs paths) -> paths`

*	**`files-depends`**: Parses a source file to extract its immediate `include`
	and `import` dependencies.

	*	`(files-depends path [end]) -> paths`

*	**`files-all-depends`**: Resolves the transitive closure of all dependencies
	for a set of files.

	*	`(files-all-depends paths [imps end]) -> paths`

*	**`files-scan`**: Scans files line-by-line with a user-defined processing
	callback.

	*	`(files-scan files handler [split_class comment]) -> files`

*	**`files-classes-info`**: Builds or loads the cached class database
	(`class_db.tre`).

	*	`(files-classes-info [forced]) -> :nil | class_db`

*	**`files-function-info`**: Builds or loads the cached function database
	(`func_db.tre`).

	*	`(files-function-info [class_db]) -> :nil | func_db`

*	**`files-all-vp-source`**: Returns all VP source files topologically sorted
	by dependency.

	*	`(files-all-vp-source) -> ordered_files`

*	**`url-ext`**: Path autocompletion helper for command prompts.

	*	`(url-ext url cx [ctx]) -> str`

## GUI System Management

Host window event handling, compositor interaction, and RPC management.

*	**`gui-info`**: Queries the mouse coordinates and dimensions of the display.

	*	`(gui-info) -> (mouse_x mouse_y screen_width screen_height)`

*	**`gui-init`**: Initializes the GUI compositor with a root view object.

	*	`(gui-init screen) -> screen`

*	**`gui-deinit`**: Shuts down the GUI subsystem.

	*	`(gui-deinit) -> :nil`

*	**`gui-update`**: Prompts the compositor to execute layout, damage clipping,
	and rendering.

	*	`(gui-update mouse_x mouse_y flags) -> :nil`

*	**`gui-event`**: Polls the host OS event queue for pending mouse, keyboard,
	or window events.

	*	`(gui-event) -> :nil | event_string`

*	**`gui-rpc`**: Sends an RPC message to the GUI compositor task.

	*	`(gui-rpc (view cmd)) -> :nil | view`

*	**`gui-add-front-rpc` / `gui-add-back-rpc`**: Adds a view to the front or
	back of the compositor hierarchy.

	*	`(gui-add-front-rpc view) -> view`

	*	`(gui-add-back-rpc view) -> view`

*	**`gui-sub-rpc`**: Removes a view from the compositor hierarchy.

	*	`(gui-sub-rpc view) -> view`

*	**`gui-logout-rpc` / `gui-quit-rpc`**: Terminates the desktop session.

	*	`(gui-logout-rpc) -> :nil`

	*	`(gui-quit-rpc) -> :nil`

*	**`view-locate`**: Calculates coordinates to center or dock a view.

	*	`(view-locate w h [p]) -> (x y w h)`

*	**`view-fit`**: Clamps coordinates to fit within the visible display bounds.

	*	`(view-fit x y w h) -> (x y w h)`

## Font, Typeface, and Glyph Operations

Loading, rendering, and rasterizing `.ctf` (ChrysaLisp Typeface) vector fonts.

*	**`create-font`**: Loads a `.ctf` font at a specified pixel size.

	*	`(create-font name pixels) -> font`

*	**`font-info`**: Returns the font typeface name and pixel height.

	*	`(font-info font) -> (name pixels)`

*	**`font-glyph-paths`**: Generates 2D vector path outlines for a string of
	glyphs.

	*	`(font-glyph-paths font str) -> (path ...)`

*	**`font-glyph-ranges`**: Returns the Unicode code point ranges supported by
	the font.

	*	`(font-glyph-ranges font) -> ((start end) ...)`

*	**`font-glyph-bounds`**: Calculates the pixel dimensions required to render
	a string.

	*	`(font-glyph-bounds font str) -> (width height)`

*	**`font-sym-texture`**: Renders and caches an anti-aliased GPU texture for a
	given symbol.

	*	`(font-sym-texture font sym) -> texture`

## Canvas, Pixmaps, and Raster Graphics

Raster image loading, format conversion, and 2D canvas drawing operations.

*	**`pixmap-as-argb`**: Converts a pixmap's pixels from premultiplied alpha to
	standard ARGB.

	*	`(pixmap-as-argb pixmap) -> pixmap`

*	**`pixmap-to-argb32` / `pixmap-from-argb32`**: Pixel format conversion.

	*	`(pixmap-to-argb32 pixel type) -> argb32`

	*	`(pixmap-from-argb32 pixel type) -> pixel`

*	**`pixmap-read` / `pixmap-write`**: Stream I/O for raw pixmap pixel data.

	*	`(pixmap-read pixmap stream type) -> :nil | pixmap`

	*	`(pixmap-write pixmap stream type) -> pixmap`

*	**`canvas-info`**: Inspects image headers without loading the full bitmap.

	*	`(canvas-info file) -> (width height type) | (-1 -1 -1)`

*	**`canvas-load` / `canvas-save`**: Loads or saves an image canvas.

	*	`(canvas-load file flags [swap_mode]) -> :nil | canvas`

	*	`(canvas-save canvas file type &rest optionals) -> :nil | canvas`

*	**`canvas-brighter` / `canvas-darker`**: Computes highlighted and shaded
	color variants.

	*	`(canvas-brighter col) -> col`

	*	`(canvas-darker col) -> col`

*	**`canvas-flush`**: Purges unused shared pixmaps from the cache.

	*	`(canvas-flush)`

*	**`texture-metrics`**: Returns the dimensions and handle of a GPU texture.

	*	`(texture-metrics texture) -> (handle width height)`

*	**`CPM-info` / `CPM-load` / `CPM-save`**: ChrysaLisp Pixmap format handler.

*	**`CWB-info` / `CWB-load`**: ChrysaLisp Vector Canvas format handler.

*	**`TGA-info` / `TGA-load`**: Truevision TGA image loader.

*	**`SVG-info` / `SVG-load`**: Scalable Vector Graphics loader.

## UI Declarative Tree Builders

Macros for assembling declarative widget hierarchies.

*	**`ui-root` / `ui-element`**: Base macros for defining component trees.

	*	`(ui-root name constructor [props] [body ...]) -> view`

	*	`(ui-element name constructor [props] [body ...]) -> view`

*	**`ui-window`**: Window container with shadow and drag borders.

	*	`(ui-window name [props] [body ...]) -> window`

*	**`ui-flow` / `ui-grid` / `ui-stack`**: Layout containers.

	*	`(ui-flow name [props] [body ...]) -> flow`

	*	`(ui-grid name [props] [body ...]) -> grid`

	*	`(ui-stack name tabs [props] [body ...]) -> stack`

*	**`ui-button` / `ui-buttons` / `ui-title-bar`**: Button controls.

	*	`(ui-button name [props] [body ...]) -> button`

	*	`(ui-buttons symbols event [props])`

	*	`(ui-title-bar name title symbols event [props]) -> flow`

*	**`ui-text` / `ui-label` / `ui-title` / `ui-md`**: Text rendering widgets.

	*	`(ui-text name [props]) -> text`

	*	`(ui-label name [props] [body ...]) -> label`

	*	`(ui-title name [props]) -> title`

	*	`(ui-md name [text_lines] [props] [body ...]) -> md`

*	**`ui-textfield` / `ui-slider` / `ui-scroll` / `ui-spinner`**: Interactive controls.

	*	`(ui-textfield name [props]) -> textfield`

	*	`(ui-slider name [props]) -> slider`

	*	`(ui-scroll name flags [props] [body ...]) -> scroll`

	*	`(ui-spinner name [props]) -> spinner`

*	**`ui-radio-bar` / `ui-toggle-bar` / `ui-tool-bar`**: Bar containers.

	*	`(ui-radio-bar name symbols [props]) -> radiobar`

	*	`(ui-toggle-bar name symbols [props]) -> radiobar`

	*	`(ui-tool-bar name [props] [body ...]) -> flow`

*	**`ui-canvas` / `ui-vdu` / `ui-backdrop` / `ui-stroke` / `ui-progress`**: Specialized display widgets.

	*	`(ui-canvas name width height scale [props]) -> canvas`

	*	`(ui-vdu name [props]) -> vdu`

	*	`(ui-backdrop name [props] [body ...]) -> backdrop`

	*	`(ui-stroke name [props]) -> stroke`

	*	`(ui-progress name [props]) -> progress`

*	**`ui-files`**: Hierarchical filesystem tree browser widget.

	*	`(ui-files name title event [props]) -> files`

*	**`ui-tool-tips`**: Attaches tooltip strings to children of a container.

	*	`(ui-tool-tips view tips)`

## Vector Paths and 2D Geometry

2D vector shape generation, stroke tessellation, and intersection testing.

*	**`path-gen-arc`**: Generates arc path coordinates.

	*	`(path-gen-arc cx cy start end radius dst) -> dst`

*	**`path-gen-cubic` / `path-gen-quadratic`**: Generates Bézier curves.

	*	`(path-gen-cubic p1x p1y p2x p2y p3x p3y p4x p4y dst) -> dst`

	*	`(path-gen-quadratic p1x p1y p2x p2y p3x p3y dst) -> dst`

*	**`path-gen-rect` / `path-gen-ellipse`**: Generates geometric primitives.

	*	`(path-gen-rect x y x1 y1 rx ry dst) -> dst`

	*	`(path-gen-ellipse cx cy rx ry dst) -> dst`

*	**`path-gen-paths`**: Parses an SVG path data string into open/closed paths.

	*	`(path-gen-paths svg_d) -> ((:nil|:t path) ...)`

*	**`path-filter` / `path-simplify` / `path-smooth`**: Geometric path optimization.

	*	`(path-filter tol src dst) -> dst`

	*	`(path-simplify tol src dst) -> dst`

	*	`(path-smooth src) -> dst`

*	**`path-stroke-polygon` / `path-stroke-polyline`**: Generates expanded
	polygon strokes from paths.

	*	`(path-stroke-polygon path radius join) -> paths`

	*	`(path-stroke-polyline path radius join cap1 cap2) -> path`

*	**`path-stroke-polygons` / `path-stroke-polylines`**: Batch stroke operations.

	*	`(path-stroke-polygons dst radius join src) -> dst`

	*	`(path-stroke-polylines dst radius join cap1 cap2 src) -> dst`

*	**`path-transform`**: Multiplies path vertices by a 3x2 transformation
	matrix.

	*	`(path-transform m3x2 src dst) -> dst`

*	**`path-svg`**: Tokenizes SVG path commands and coordinate tokens.

	*	`(path-svg d) -> commands`

*	**`vector-bounds-2d` / `vector-bounds-3d` / `vector-bounds-sphere`**: Bounding
	volume calculations.

	*	`(vector-bounds-2d paths) -> (min_v2 max_v2)`

	*	`(vector-bounds-3d verts [stride]) -> (min_v3 max_v3)`

	*	`(vector-bounds-sphere verts [stride]) -> (center_v3 radius)`

*	**`vector-point-in-polygon`**: Tests if a 2D point is enclosed by a polygon.

	*	`(vector-point-in-polygon p paths winding_mode) -> :t | :nil`

*	**`vector-perp-2d` / `vector-det` / `vector-cross-3d`**: Vector products.

	*	`(vector-perp-2d (x y)) -> list`

	*	`(vector-det (x1 y1) (x2 y2)) -> num`

	*	`(vector-cross-3d (x1 y1 z1) (x2 y2 z2)) -> list`

*	**`vector-intersect-2d` / `vector-intersect-lines-2d`**: 2D line
	intersection solvers.

	*	`(vector-intersect-2d l1_p1 av l2_p1 bv) -> (ix iy)`

	*	`(vector-intersect-lines-2d l1_p1 l1_p2 l2_p1 l2_p2) -> (ix iy)`

*	**`vector-collide-lines-2d` / `vector-collide-thick-lines-2d`**: Collision
	detection tests.

	*	`(vector-collide-lines-2d l1_p1 l1_p2 l2_p1 l2_p2) -> :t | :nil`

	*	`(vector-collide-thick-lines-2d l1_p1 l1_p2 l2_p1 l2_p2 r) -> :t | :nil`

*	**`vector-length` / `vector-dist` / `vector-sdist`**: Distance and norm
	calculators.

	*	`(vector-length p) -> real`

	*	`(vector-dist p1 p2) -> real`

	*	`(vector-sdist p1 p2) -> real`

*	**`vector-dist-to-line` / `vector-sdist-to-line`**: Point-to-line segment
	distance calculations.

	*	`(vector-dist-to-line p p1 p2) -> real`

	*	`(vector-sdist-to-line p p1 p2) -> real`

## 3D Mathematics, Meshes, and Isosurfaces

3D matrices, camera frustums, marching-cubes isosurfaces, and scene graphs.

*	**`Mat3x2-f` / `Mat3x2-rotz-f` / `Mat3x2-skewx-f` / `Mat3x2-skewy-f`**: 3x2
	transformation matrix constructors.

*	**`mat3x2-mul-f`**: Multiplies two 3x2 fixed-point matrices.

	*	`(mat3x2-mul-f mat3x2_a mat3x2_b) -> mat3x2-f`

*	**`Mat4x4-unity` / `Mat4x4-rotx` / `Mat4x4-roty` / `Mat4x4-rotz`**: 4x4
	rotation constructors.

	*	`(Mat4x4-unity) -> reals`

	*	`(Mat4x4-rotx a) -> reals`

	*	`(Mat4x4-roty a) -> reals`

	*	`(Mat4x4-rotz a) -> reals`

*	**`Mat4x4-translate` / `Mat4x4-scale` / `Mat4x4-frustum`**: 4x4 transformation
	and perspective frustum constructors.

	*	`(Mat4x4-translate x y z) -> reals`

	*	`(Mat4x4-scale x y z) -> reals`

	*	`(Mat4x4-frustum left right top bottom near far) -> reals`

*	**`Mesh` / `Mesh-sphere` / `Mesh-torus` / `Mesh-iso` / `Mesh-obj` / `Mesh-data`**:
	3D triangle mesh representations.

*	**`Scene` / `Scene-node` / `Scene-object`**: Hierarchical 3D scene graph.

*	**`iso-surface`**: Calculates marching-cubes triangle facets from an 8-corner
	voxel grid cell.

	*	`(iso-surface grid isolevel) -> tris`

*	**`vertex-interp`**: Interpolates coordinates along a grid edge.

	*	`(vertex-interp isolevel p1 p2 valp1 valp2) -> p`

*	**`opt-mesh` / `opt-vector`**: Welds duplicate vertices in a 3D mesh.

	*	`(opt-mesh verts norms tris) -> (new_verts new_norms new_tris)`

	*	`(opt-vector vector part) -> (new_vector new_indices)`

*	**`gen-norms`**: Generates face normals across triangle vertices.

	*	`(gen-norms verts tris) -> (norms new_tris)`

*	**`Iso` / `Iso-sphere` / `Iso-cube` / `Iso-tetra` / `Iso-capsule`**: Volumetric
	implicit field definitions.

## XML and Markup Parsing

*	**`XML-parse`**: Tokenizes an XML/SVG stream and invokes callback handlers
	for tags, attributes, and body text.

	*	`(XML-parse stream fnc_in fnc_out fnc_text)`

## Text Buffers, Highlighting, and Regex Search

Multi-cursor text engine, syntax coloring, pattern queries, and regex substitution.

*	**`Buffer` / `Document`**: Multi-cursor document buffer models.

*	**`Syntax`**: Tokenizes source text and maps tokens to syntax highlight
	styles.

*	**`Dictionary`**: Prefix-indexed dictionary with auto-complete support.

*	**`Search` / `Substr` / `Kmplps` / `Regexp`**: Text search engines.

*	**`found?` / `match?` / `matches` / `substr`**: Pattern searching.

	*	`(found? text substr) -> :t | :nil`

	*	`(match? text regexp) -> :t | :nil`

	*	`(matches text regexp) -> matches`

	*	`(substr text substr) -> matches`

*	**`query`**: Compiles a search query engine and metadata.

	*	`(query pattern whole_words regexp) -> (engine meta pattern)`

*	**`replace-compile`**: Compiles a substitution string supporting `$1`
	capture group references.

	*	`(replace-compile rep_str) -> compiled`

*	**`replace-matches` / `replace-regex` / `replace-str`**: Executes text
	replacement.

	*	`(replace-matches text match_lst compiled) -> text`

	*	`(replace-regex text pattern compiled) -> text`

	*	`(replace-str text pattern compiled) -> text`

*	**`replace-edits` / `replace-regex-edits` / `replace-str-edits`**: Generates
	`(start end rep_str)` edit operations for document buffers.

	*	`(replace-edits text match_lst compiled) -> edits`

*	**`escape-regexp`**: Escapes regex metacharacters in a literal string.

	*	`(escape-regexp str) -> str`

*	**`char-class`**: Builds and interns a sorted binary-searchable character
	class.

	*	`(char-class key) -> str`

*	**`csr-sort` / `csr-floor` / `csr-sort-top` / `csr-sort-bot` / `csr-cmp` / `csr-within`**:
	Multi-cursor geometric helpers.

*	**`csr-map-delete` / `csr-map-insert`**: Maps cursor positions across text
	mutations.

*	**`reflow`**: Reflows words across wrapped lines.

	*	`(reflow words line_width [indent tab_width]) -> lines`

## Task Pools, Pipelines, and Distributed Computing

Load balancing, parallel task execution, and child process pipelines.

*	**`Pipe`**: Spawns and manages an inter-task pipeline.

*	**`pipe-run`**: Executes a command pipeline and streams standard output to a
	callback.

	*	`(pipe-run cmdline [outfun])`

*	**`pipe-split`**: Parses a command line string into pipeline stages.

	*	`(pipe-split cmdline) -> ((mode cmd) ...)`

*	**`pipe-farm`**: Distributes job execution across a cluster of child
	workers.

	*	`(pipe-farm jobs [retry_timeout]) -> ((job result) ...)`

*	**`Farm` / `Global` / `Local`**: Distributed worker pool managers.

## Streams, Compression, and Archival Formats

Diffing, patch application, and compression codecs.

*	**`stream-diff`**: Generates a standard diff between two input streams.

	*	`(stream-diff a b c)`

*	**`stream-patch`**: Applies a diff to an input stream, writing the patched
	result.

	*	`(stream-patch a b c)`

*	**`huffman-compress` / `huffman-decompress`**: Adaptive Huffman coding.

	*	`(huffman-compress in_stream out_stream token_bits)`

	*	`(huffman-decompress in_stream out_stream token_bits)`

*	**`huffman-build-freq-map` / `huffman-write-codebook` / `huffman-read-codebook`**:
	Static Huffman model creation.

*	**`huffman-compress-static` / `huffman-decompress-static`**: Static Huffman
	coding.

*	**`rle-compress` / `rle-decompress`**: Run-Length Encoding codec.

	*	`(rle-compress in_stream out_stream [token_bits run_bits])`

	*	`(rle-decompress in_stream out_stream [token_bits run_bits max_tokens])`

*	**`lz4-compress` / `lz4-decompress`**: LZ4 Framed stream compression and
	decompression.

	*	`(lz4-compress in_stream out_stream [window_size])`

	*	`(lz4-decompress in_stream out_stream [window_size])`

*	**`lz4-encode` / `lz4-decode`**: High-performance LZ4 block encoder and
	decoder.

	*	`(lz4-encode ring_buf pos chunk hash_table) -> (comp pos)`

	*	`(lz4-decode ring_buf pos comp_chunk) -> pos`

*	**`lz4-read` / `lz4-write`**: Stream ring buffer I/O for LZ4 blocks.

	*	`(lz4-read buf pos stream in_len) -> pos`

	*	`(lz4-write buf pos stream out_len) -> pos`

## Command-Line Options Parser

*	**`options`**: Parses CLI argument flags from a `stdio` object.

	*	`(options stdio optlist) -> :nil | args`

*	**`opt-flag` / `opt-num` / `opt-str` / `opt-nums`**: Declarative option
	flag handlers.

	*	`(opt-flag 'opt_var)`

	*	`(opt-num 'opt_var)`

	*	`(opt-str 'opt_var)`

	*	`(opt-nums count 'opt_var)`

*	**`options-find` / `options-print` / `options-split`**: Option parser
	helpers.

## Date, Time, and Timezones

Calendar math, date formatting, and timezone lookups.

*	**`date`**: Returns current date and time components.

	*	`(date [seconds]) -> (second minute hour date month year week)`

*	**`encode-date` / `decode-date`**: Date string serialization and parsing.

	*	`(encode-date [td]) -> str`

	*	`(decode-date dts) -> (second minute hour date month year week)`

*	**`timezone-init` / `timezone-lookup`**: Timezone configuration.

	*	`(timezone-init tz_loc)`

	*	`(timezone-lookup prop val) -> tz_entry`

*	**`float-time`**: Returns the current time as fixed-point fractions.

	*	`(float-time) -> (seconds minutes hours)`

*	**`leapyear?`**: Leap year predicate.

	*	`(leapyear? year) -> :t | :nil`

*	**`days-in-month` / `days-in-year`**: Calendar interval calculations.

	*	`(days-in-month month year) -> days`

	*	`(days-in-year year) -> days`

*	**`day-of-the-week` / `month-of-the-year`**: Date component name lookups.

	*	`(day-of-the-week d) -> str`

	*	`(month-of-the-year m) -> str`

## Collections Framework

Object-oriented container hierarchy implementing maps, sets, and trees.

*	**`Map` / `Set`**: Abstract base classes for associative collections.

*	**`Emap`**: Environment-backed (`:hmap`) map.

	*	`(Emap [num_buckets]) -> emap`

*	**`Fmap` / `Fset`**: Fast hash map and set using bucketed `:pmap`/`:pset`
	storage.

	*	`(Fmap [num_buckets]) -> fmap`

	*	`(Fset [num_buckets]) -> fset`

*	**`Lmap` / `Lset`**: Linear property map and set backed by flat arrays.

	*	`(Lmap) -> lmap`

	*	`(Lset) -> lset`

*	**`Xmap` / `Xset`**: Hash map and set with custom equality and hashing
	functions.

	*	`(Xmap [num_buckets cmp_fnc hash_fnc]) -> xmap`

	*	`(Xset [num_buckets cmp_fnc hash_fnc]) -> xset`

*	**`gather`**: Extracts values from a collection for a list of keys.

	*	`(gather map|set [key] ...) -> (val|key|:nil ...)`

*	**`scatter`**: Bulk-inserts keys or key-value pairs into a collection.

	*	`(scatter map|set [key]|[key val] ...) -> map|set`

*	**`transfer`**: Transfers key-value pairs from a source to a destination map.

	*	`(transfer src_map dst_map [key val] ...) -> dst_map`

*	**`memoize`**: Evaluates and caches expressions in an `Fmap` or `Lmap`.

	*	`(memoize key form [num_buckets]) -> (eval form)`

*	**`tsort`**: Iterative topological sort using an explicit DFS work-stack.

	*	`(tsort roots dep_fnc) -> order`

*	**`tree-load` / `tree-save`**: Serializes and deserializes arbitrary
	collection trees to/from `.tre` format.

	*	`(tree-load stream) -> tree`

	*	`(tree-save stream tree [key_filters]) -> tree`

*	**`tree-type` / `tree-collection?` / `tree-decode` / `tree-encode` / `tree-node` / `tree-buckets`**:
	Tree serialization helpers.

## Binary Structures, Bitfields, and Enumerations

*	**`structure`**: Declares binary struct field offsets and types.

	*	`(structure name base [(type field ...)] ...)`

*	**`getf` / `setf`**: Accesses or mutates structure fields by offset metadata.

	*	`(getf obj field [offset]) -> value`

	*	`(setf obj field value [offset]) -> obj`

*	**`getf->` / `setf->`**: Batch struct field readers and writers.

	*	`(getf-> obj field|(field offset) ...) -> (val ...)`

	*	`(setf-> obj (field val [offset]) ...) -> obj`

*	**`enums`**: Declares sequential enumeration constants.

	*	`(enums name base [(enum field ...)] ...)`

*	**`bits`**: Declares bitmask constants.

	*	`(bits name base [(bit field ...)] ...)`

*	**`bits?`**: Checks if any specified bits are set in a bitmask.

	*	`(bits? val mask ...) -> :t | :nil`

*	**`bit-mask`**: Combines multiple bit masks into a single integer.

	*	`(bit-mask mask ...) -> val`

## Debugging and Profiling

Instrumentation hooks and profiling tools.

*	**`debug-brk`**: Conditional breakpoint that halts execution and signals the
	debug service.

	*	`(debug-brk break_id [condition])`

*	**`profile-report`**: Formats and transmits accumulated execution profiles
	to the profiling service.

	*	`(profile-report name [reset])`

*	**`*stack_frame*`**: Global call-stack list populated when running under
	`lib/debug/frames.inc`.

## Compiler, Assembler, and Build System

Virtual Processor compilation pipeline, code generation, and make engine.

*	**`within-compile-env`**: Executes code inside an isolated build
	environment.

	*	`(within-compile-env lambda)`

*	**`include`**: Imports a module into the active compilation environment.

	*	`(include file)`

*	**`jit`**: Triggers JIT compilation of VP sources protected by a global
	compilation lock.

	*	`(jit prefix file products)`

*	**`func-load` / `func-refs` / `func-obj-path`**: Object binary inspection.

	*	`(func-load name) -> (body links refs)`

	*	`(func-refs fobj) -> ([sym] ...)`

	*	`(func-obj-path sym) -> sym`

*	**`boot-image`**: Links compiled object files into a single boot image
	binary (`obj/<cpu>/<abi>/sys/boot_image`).

	*	`(boot-image [funcs abi cpu])`

*	**`make` / `compile` / `make-all` / `remake` / `remake-all` / `make-boot-all`**:
	Build system commands.

	*	`(make [files abi cpu])`

	*	`(compile files [abi cpu])`

	*	`(make-all [files abi cpu])`

	*	`(remake [files abi cpu])`

	*	`(remake-all [files abi cpu])`

	*	`(make-boot-all [abi cpu])`

*	**`make-platforms` / `make-all-platforms` / `remake-platforms` / `remake-all-platforms` / `remake-all-vp` / `make-app-platforms`**:
	Cross-platform build triggers.

	*	`(make-platforms [mode])`

	*	`(make-all-platforms [mode])`

	*	`(remake-platforms [mode])`

	*	`(remake-all-platforms [mode])`

	*	`(remake-all-vp [mode])`

	*	`(make-app-platforms [mode])`

*	**`make-test` / `compile-test`**: Benchmarking tools for build times.

	*	`(make-test [iterations abi cpu])`

	*	`(compile-test [abi cpu])`

*	**`def-func` / `def-func-end`**: Declares a native VP function boundary.

	*	`(def-func name [align stack])`

	*	`(def-func-end)`

*	**`def-class` / `def-method`**: Declares class vtables and methods for the
	assembler.

	*	`(def-class class super &rest lines)`

	*	`(def-method class member [align])`

*	**`gen-vtable` / `gen-create` / `gen-type`**: Auto-generates standard VP
	class methods.

	*	`(gen-vtable class)`

	*	`(gen-create class [name])`

	*	`(gen-type class)`

*	**`entry` / `exit`**: Defines function register entry and exit contracts.

	*	`(entry class method in_regs)`

	*	`(exit class method out_regs)`

*	**`call` / `jump` / `s-call` / `s-jump` / `d-call` / `d-jump` / `r-call` / `r-jump` / `v-call` / `v-jump`**:
	Method calling conventions.

*	**`f-path` / `s-path` / `f-bind` / `s-bind` / `v-bind` / `d-bind`**: Symbol
	resolution and binding macros.

*	**`method-input` / `method-output` / `method-lookup`**: Compile-time
	signature introspection.

*	**`push-scope` / `pop-scope` / `pop-scope-syms` / `scope-unwind` / `return`**:
	CScript stack frame and scope management.

*	**`def-vars`**: Declares typed local variables in CScript scopes.

	*	`(def-vars [(type var ...)] ...)`

*	**`assign` / `cscript`**: Assigns registers and compiles CScript expressions.

	*	`(assign [src] [dst] [compiler_regs])`

	*	`(cscript string)`

*	**`vp-rdef` / `vp-fdef`**: Defines local VP integer and floating-point
	register aliases.

	*	`(vp-rdef (alias ...) [regs])`

	*	`(vp-fdef (alias ...) [regs])`

*	**`emit` / `emit-vp-code` / `emit-translate`**: VP machine instruction
	assembly and translation.

*	**`abort` / `assert`**: Compilation assertions and emergency task aborts.

	*	`(abort [msg])`

	*	`(assert condition [msg])`

## Clipboard Service

RPC interface for sharing text with the host and desktop clipboards.

*	**`clip-put-rpc`**: Copies text to the system clipboard service.

	*	`(clip-put-rpc str)`

*	**`clip-get-rpc`**: Reads text from the system clipboard service.

	*	`(clip-get-rpc) -> str`