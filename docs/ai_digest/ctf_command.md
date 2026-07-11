# ChrysaLisp Vector Font (.ctf) Reference Manual

This document details the ChrysaLisp Vector Font (`.ctf`) binary file format
specification and the usage of the `ctf` command-line utility.

## 1. The .ctf Binary File Specification

The `.ctf` format is a resolution-independent vector font format designed for
lightweight, high-performance rendering on the ChrysaLisp OS. It replaces
floating-point representations with fixed-point arithmetic to optimize execution
speed within the Virtual Processor (VP).

### A. Coordinate & Precision Formats

Spatial measurements in a `.ctf` binary utilize a single, unified fixed-point
precision scale:

* **3.13 Fixed-Point**: Signed 16-bit integers (`short`) representing values
  scaled by 2^13 (8,192).

	This scale is used for overall font metrics, advance widths, bounding
	coordinates, path geometry, and negative kerning pair adjustments.

	By utilizing 16-bit signed shorts, the file size and in-memory footprint of
	outlines are reduced.

	Modern CPU architectures (ARM64, x86_64, and RISC-V) support signed 16-bit
	register loads with hardware-level sign extension (e.g., `LDRSH`, `MOVSX`, or
	`LH` instructions) with zero latency penalty compared to 32-bit loads.

* **Range and Headroom**: With 3 bits allocated to the integer part, the 3.13
  format supports a coordinate range from -4.0 to +3.999 Em.

	Since the standard Em-height of a font is normalized to exactly 1.0 (2^13
	units), this range provides ample headroom for extreme typographic glyph
	extents and diacritics.

* **Baseline Alignment**: In this coordinate space, the glyph baseline sits
  exactly on the horizontal y = 0.0 axis.

	Because ChrysaLisp’s 2D canvas and screen coordinate systems increase
	downwards, the Y-axis is inverted during conversion:

	* Outline points above the baseline (ascent) have negative Y coordinates,
	  extending up to -ascent.

	* Outline points below the baseline (descenders) have positive Y coordinates,
	  extending down to +descent.

	* The X-axis is normalized per glyph so that the left-most boundary (min_x)
	  starts at x = 0.0.

### B. File Structure

A `.ctf` file is arranged into three sequential binary blocks:

#### I. Global Font Header (8 bytes)

The header contains three signed 16-bit `short` integers in 3.13 format,
followed by 2 bytes of zero-padding to align the structure to a 4-byte boundary
(matching `+font_data_size` = 8):

* **ascent** (2 bytes, short): Maximum glyph height above the baseline.

* **descent** (2 bytes, short): Maximum glyph depth below the baseline.

* **xkern** (2 bytes, short): Default baseline spacing (default kerning value).

* **padding** (2 bytes, short): Zero-fill bytes for 4-byte natural alignment.

#### II. Page Directory Index

Following the header is a table of page structures. Each page indexes a
continuous block of character codes.

* **end** (2 bytes, ushort): The highest character code in this page.

* **start** (2 bytes, ushort): The lowest character code in this page.

* **offsets** (count * 4 bytes, uints): Absolute byte offsets from the start of
  the file to each glyph's data block.

	The number of offsets is calculated as `count = end - start + 1`.

	Each offset is written as a 32-bit unsigned `uint` to maintain 4-byte
	alignment.

The page directory index is terminated by a **sentinel** consisting of a single
32-bit unsigned `0` value.

#### III. Glyph Data Block

Each offset in the page directory points to a glyph record structured as follows
(total `+font_path` header size = 12 bytes):

* **char_code** (2 bytes, ushort): The character code of the glyph.

* **advance** (2 bytes, short): The visual advance width (3.13 format) of the
  ink bounding box, calculated as `(- max_x min_x)`. This represents the exact
  horizontal envelope, as default and pair-wise optical kerning dictate the
  typographical spacing.

* **plen** (4 bytes, uint): Length in bytes of the path elements block.

* **klen** (4 bytes, uint): Number of kerning pairs stored for this glyph.

* **Path Elements** (`plen` bytes): A packed list of vector drawing commands.

* **Kerning Pairs** (`klen * 4` bytes): A list of 4-byte kerning records.

**Natural Alignment & Padding**: To guarantee that the 32-bit `plen` and `klen`
fields of every subsequent glyph remain naturally 4-byte aligned in memory, each
glyph's data block (comprising the header, path elements, and kerning pairs) is
padded to a 4-byte boundary.

The padding size is calculated as `(align written_size 4) - written_size`.

## 2. Drawing Command Formats

The drawing commands in the path elements block consist of a 16-bit unsigned
`type` identifier followed by its specific 16-bit coordinates in 3.13 format:

* **Type 0 (Moveto)**: Moves the cursor to the target coordinate without
  drawing.

	* `type` (2 bytes, ushort) = `0`

	* `x` (2 bytes, short): Target X coordinate (3.13)

	* `y` (2 bytes, short): Target Y coordinate (3.13)

	* Total size: 6 bytes

* **Type 1 (Lineto)**: Draws a straight line to the target coordinate.

	* `type` (2 bytes, ushort) = `1`

	* `x` (2 bytes, short): Target X coordinate (3.13)

	* `y` (2 bytes, short): Target Y coordinate (3.13)

	* Total size: 6 bytes

* **Type 3 (Quadto)**: Draws a quadratic Bezier spline.

	* `type` (2 bytes, ushort) = `3`

	* `x1` (2 bytes, short): Control point X (3.13)

	* `y1` (2 bytes, short): Control point Y (3.13)

	* `x` (2 bytes, short): Destination X (3.13)

	* `y` (2 bytes, short): Destination Y (3.13)

	* Total size: 10 bytes

* **Type 2 (Curveto)**: Draws a cubic Bezier spline.

	* `type` (2 bytes, ushort) = `2`

	* `x1` (2 bytes, short): First control point X (3.13)

	* `y1` (2 bytes, short): First control point Y (3.13)

	* `x2` (2 bytes, short): Second control point X (3.13)

	* `y2` (2 bytes, short): Second control point Y (3.13)

	* `x` (2 bytes, short): Destination X (3.13)

	* `y` (2 bytes, short): Destination Y (3.13)

	* Total size: 14 bytes

## 3. Kerning Pair Structure

Each kerning pair is stored as a 4-byte record:

* **code** (2 bytes, ushort): Character code of the trailing glyph.

* **xkern** (2 bytes, short): Kerning adjustment value in 3.13 format.

Because both coordinates and kerning pairs are stored in the same 3.13 format,
the runtime layout engine does not need to perform any scaling on the kerning
values before applying them.

## 4. The `ctf` Command-Line Application

The `ctf` command-line utility provides tools to inspect, upgrade, and compile
font files on ChrysaLisp. It supports input from native `.ctf` files as well as
OpenType (`.otf`) and TrueType (`.ttf`) outlines.

### A. Command Usage

```code
ctf [options] [path] ...
```

If no file paths are specified on the command line, `ctf` reads paths
line-by-line from `stdin`, making it fully pipe-compatible.

### B. Options

* `-h`, `--help`: Prints command line help and usage guidelines.

* `-v`, `--verbosity <num>`: Sets the diagnostic output detail level (default is
  `0`):

	* `0`: Displays global font parameters (ascent, descent, default kern, page
	  index ranges, and total glyph count) formatted as decimal reals.

	* `1`: Extends output to include individual glyph parameters (advance widths,
	  local coordinate bounds, dimensions, and active kerning pairs associated with
	  the `:kerns` property).

	* `2`: Prints complete command-level geometric draw loops (Moveto, Lineto,
	  Quadto, Curveto) alongside exact coordinate values.

	* `3`: Generates and prints temporary optical kerning results during
	  compilation.

* `-c`, `--ctf`: Triggers compilation or upgrade mode:

	* **From .otf / .ttf**: Reads TrueType or OpenType outlines, parses
	  quadratic/cubic Bezier commands, translates coordinates, automatically
	  calculates optical kerning pairs, and outputs a compiled `.ctf` binary file
	  in 3.13 format.

	* **From .ctf**: Loads the existing vector font (using the new 3.13 reader),
	  discards old records, recalculates optical kerning using the latest compiler
	  rules, and saves an updated `.ctf` binary in the 3.13 format.

* `-r`, `--range <start> <end>`: Restricts compilation or diagnostic processing
  to a specific range of Unicode character codes (e.g., `-r 32 126` for standard
  printable ASCII). This is highly useful for stripping unused regional
  characters to optimize memory.

## 5. Advanced Compiler Mechanics

When converting or upgrading fonts with `-c`, the `ctf` tool executes two
significant optimization algorithms:

### A. Mathematical Curve Flattening

To construct accurate envelopes, TrueType quadratic splines (Type 3) and
OpenType CFF cubic splines (Type 2) are flattened into discrete line segments at
compile-time using ChrysaLisp's native `16.16` (`fixed`) path primitives
(`path-gen-quadratic` and `path-gen-cubic`).

Instead of rigid fixed-step interpolation, these primitives employ an adaptive
subdivision algorithm. The algorithm recursively splits the splines into
sub-curves, performing flatness tests until the deviation of each sub-segment
falls below a strict tolerance (`eps`), ensuring optimal geometric fidelity with
the minimum necessary number of vertices.

### B. Optical Auto-Kerning

The compiler runs a two-pass optical kerning algorithm that calculates spacing
based on character boundaries:

* **First Pass**: Scans the left and right contours of every glyph at 64
  vertical slices. It calculates the raw overlap of all glyph pairings relative
  to a target spacing gap (1/16th of em-height, calculated with the fixed
  constant `+opt_target_gap_divisor` set to `16.0`). The 90th percentile of
  these overlaps is chosen as the font's default global spacing (`xkern`).

* **Second Pass**: Identifies pairs whose specific contours permit closer
  spacing than the global `xkern` baseline (e.g., "AV", "Te", "C-"). It assigns
  negative kerning pairs for these deviations, provided they exceed the minimum
  visual threshold (1/80th of em-height, defined by the fixed constant
  `+opt_threshold_divisor` set to `80.0`). The matched pairs are then written
  into the `:kerns` property.

* **Cavity Protection**: During contour generation, a maximum boundary limit
  constant (`+opt_indent_limit_divisor` set to `4.0`) is enforced. This prevents
  narrow characters like hyphens from penetrating too deeply into open glyph
  cavities, maintaining proper legibility in sequences like "RISC-V".
