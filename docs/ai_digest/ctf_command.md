# ChrysaLisp Vector Font (.ctf) Reference Manual

This document details the ChrysaLisp Vector Font (`.ctf`) binary file format
specification and the usage of the `ctf` command-line utility.

## 1. The .ctf Binary File Specification

The `.ctf` format is a resolution-independent vector font format designed for
lightweight, high-performance rendering on the ChrysaLisp OS. It replaces
floating-point representations with fixed-point arithmetic to optimize execution
speed within the Virtual Processor (VP).

### A. Coordinate & Precision Formats

Spatial measurements in a `.ctf` binary utilize two distinct fixed-point
precision scales:

* **8.24 Fixed-Point**: Signed 32-bit integers representing values scaled by
  2^24 (16,777,216). This scale is used for overall font metrics, advance
  widths, bounding coordinates, and path geometry coordinates.

* **2.14 Fixed-Point**: Signed 16-bit integers representing values scaled by
  2^14 (16,384). This compact scale is used to store negative kerning pair
  adjustments, saving file space.

### B. File Structure

A `.ctf` file is arranged into three sequential binary blocks:

#### I. Global Font Header (12 bytes)

The header contains three signed 32-bit integers in 8.24 format:

* **ascent** (4 bytes): Maximum glyph height above the baseline.

* **descent** (4 bytes): Maximum glyph depth below the baseline.

* **xkern** (4 bytes): Default baseline spacing (default kerning value).

#### II. Page Directory Index

Following the header is a table of page structures. Each page indexes a
continuous block of character codes.

* **end** (4 bytes, uint): The highest character code in this page.

* **start** (4 bytes, uint): The lowest character code in this page.

* **offsets** (count * 4 bytes, uints): Absolute byte offsets from the start of
  the file to each glyph's data block. The number of offsets is calculated as
  `count = end - start + 1`.

The page directory index is terminated by a **sentinel** consisting of a single
32-bit unsigned `0` value.

#### III. Glyph Data Block

Each offset in the page directory points to a glyph record structured as
follows:

* **char_code** (4 bytes, uint): The character code of the glyph.

* **advance** (4 bytes, int): The horizontal advance width (8.24 format).

* **plen** (4 bytes, uint): Length in bytes of the path elements block.

* **klen** (4 bytes, uint): Number of kerning pairs stored for this glyph.

* **Path Elements** (`plen` bytes): A packed list of vector drawing commands.

* **Kerning Pairs** (`klen * 4` bytes): A list of 4-byte kerning records.

## 2. Drawing Command Formats

The drawing commands in the path elements block consist of a 32-bit signed
`type` identifier followed by its specific coordinates:

* **Type 0 (Moveto)**: Moves the cursor to the target coordinate without
  drawing.

    * `type` (4 bytes, int) = `0`

    * `x` (4 bytes, int): Target X coordinate (8.24)

    * `y` (4 bytes, int): Target Y coordinate (8.24)

    * Total size: 12 bytes

* **Type 1 (Lineto)**: Draws a straight line to the target coordinate.

    * `type` (4 bytes, int) = `1`

    * `x` (4 bytes, int): Target X coordinate (8.24)

    * `y` (4 bytes, int): Target Y coordinate (8.24)

    * Total size: 12 bytes

* **Type 3 (Quadto)**: Draws a quadratic Bezier spline.

    * `type` (4 bytes, int) = `3`

    * `x1` (4 bytes, int): Control point X (8.24)

    * `y1` (4 bytes, int): Control point Y (8.24)

    * `x` (4 bytes, int): Destination X (8.24)

    * `y` (4 bytes, int): Destination Y (8.24)

    * Total size: 20 bytes

* **Type 2 (Curveto)**: Draws a cubic Bezier spline.

    * `type` (4 bytes, int) = `2`

    * `x1` (4 bytes, int): First control point X (8.24)

    * `y1` (4 bytes, int): First control point Y (8.24)

    * `x2` (4 bytes, int): Second control point X (8.24)

    * `y2` (4 bytes, int): Second control point Y (8.24)

    * `x` (4 bytes, int): Destination X (8.24)

    * `y` (4 bytes, int): Destination Y (8.24)

    * Total size: 28 bytes

## 3. Kerning Pair Structure

Each kerning pair is stored as a 4-byte record:

* **code** (2 bytes, ushort): Character code of the trailing glyph.

* **xkern** (2 bytes, short): Kerning adjustment value in 2.14 format.

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
      index ranges, and total glyph count).

    * `1`: Extends output to include individual glyph parameters (advance
      widths, local coordinate bounds, dimensions, and active kerning pairs).

    * `2`: Prints complete command-level geometric draw loops (Moveto, Lineto,
      Quadto, Curveto) alongside exact 8.24 coordinate values.

    * `3`: Generates and prints temporary optical kerning results during
      compilation.

* `-c`, `--ctf`: Triggers compilation or upgrade mode:

    * **From .otf / .ttf**: Reads TrueType or OpenType outlines, parses
      quadratic/cubic Bezier commands, translates coordinates, automatically
      calculates optical kerning pairs, and outputs a compiled `.ctf` binary
      file.

    * **From .ctf**: Loads the existing vector font, discards old kerning
      records, recalculates the optical kerning using the latest compiler rules,
      and saves an updated `.ctf` binary.

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
compile-time using 8-step Bezier interpolation. This step translates control
points to flat vertices using signed-preserving arithmetic shifts (`>>>`) to
maintain coordinate accuracy.

### B. Optical Auto-Kerning

The compiler runs a two-pass optical kerning algorithm that calculates spacing
based on character boundaries:

* **First Pass**: Scans the left and right contours of every glyph at 64
  vertical slices. It calculates the raw overlap of all glyph pairings relative
  to a target spacing gap (1/16th of em-height). The 90th percentile of these
  overlaps is chosen as the font's default global spacing (`xkern`).

* **Second Pass**: Identifies pairs whose specific contours permit closer
  spacing than the global `xkern` baseline (e.g., "AV", "Te", "C-"). It assigns
  negative kerning pairs for these deviations, provided they exceed the minimum
  visual threshold (1/80th of em-height).

* **Cavity Protection**: During contour generation, a maximum boundary limit
  constant (`+opt_indent_limit_divisor`) is enforced. This prevents narrow
  characters like hyphens from penetrating too deeply into open glyph cavities,
  maintaining proper legibility in sequences like "RISC-V".
