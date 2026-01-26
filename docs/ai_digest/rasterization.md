# ChrysaLisp Vector Graphics Architecture

This document details the architecture of the ChrysaLisp vector graphics engine.
It is a high-performance, software-based rendering system written in VP
assembly. It handles the conversion of abstract geometric paths into
anti-aliased pixels.

The pipeline consists of two distinct stages:

1. **Path Stroking:** Converting 1D paths (lines/curves) into 2D polygonal
   geometry (thick lines with joints/caps).

2. **Rasterization:** Converting 2D polygons into pixels using a scanline
   edge-walker with anti-aliasing.

## Part 1: The Path Stroking System (`gui/path`)

Before a line can be drawn, it must be "stroked." A mathematical line has zero
width. The stroking system expands this line into a polygon that has width,
specific corners (joins), and endpoints (caps). This logic is primarily
contained in `gui/path/stroke_joints.vp` and `gui/path/stroke_polyline.vp`.

### 1. Vector Geometry Engine

The stroking engine relies heavily on the SIMD-like capabilities of the VP
architecture (`vp-simd` instructions) to handle X and Y coordinates in parallel.
It uses fixed-point arithmetic (16.16 format) for all geometric calculations to
ensure precision and speed.

For every segment of a path (defined by points *P_1* and *P_2*), the engine calculates:

* **Vector (V):** *P_2 - P_1*

* **Perpendicular (V_perp):** Rotated 90 degrees.

* **Normal (N):** Normalized perpendicular vector.

*	**Offset Radius (R):** *N times text{Line Width}*.

This allows the engine to generate the "left" and "right" edges of the thick
line by calculating *P + R* and *P - R*.

### 2. Join Styles

When two line segments meet at a vertex, the engine must generate a Join. The
logic in `stroke_joints.vp` calculates the angle between the incoming segment
and the outgoing segment using dot products.

* **Miter Join:** The engine calculates the intersection point of the two outer
  edge lines using `:f_intersect` (a linear algebra solver). This creates a
  sharp point. If the angle is too acute (approaching parallel), it falls back
  to Bevel to prevent infinite spikes.

* **Bevel Join:** The engine simply connects the outer corner of the incoming
  segment to the outer corner of the outgoing segment with a straight line,
  "cutting off" the corner.

* **Round Join:** The engine generates a smooth arc between the two outer
  corners using `:gen_clerp` (Circular Linear Interpolation), recursively
  subdividing the arc until it meets a flatness threshold.

### 3. Cap Styles

Handled in `stroke_polyline.vp`, caps define how the line starts and ends.

* **Butt:** The line ends exactly at the endpoint.

* **Square:** The geometry is projected forward by the line width radius,
  creating a box end.

* **Triangle:** Two points are generated to form a sharp tip extending from the
  endpoint.

* **Arrow:** A specialized cap that generates a re-entrant arrowhead shape.

* **Round:** Similar to the Round Join, generates a semi-circle at the endpoint.

## Part 2: The Rasterizer (`gui/canvas/fpoly`)

Once paths are converted into polygons (lists of points), `fpoly` is responsible
for rendering them. It uses an Active Edge List (AEL) scanline algorithm with a
unique "Virtual Scanline" approach for anti-aliasing.

### 1. Edge Generation & Clipping (`:set_edges`)

The rendering process begins by converting the polygon's vertices into a list of
"Edges". An `Edge` structure contains:

* `x`: Current X position (fixed point).

* `ys`: Start Y scanline.

* `ye`: End Y scanline.

* `w`: Inverse slope (*dx/dy*).

**Clipping Logic:**

The system performs Y-axis clipping analytically during edge generation.

* Segments completely outside the vertical bounds are discarded.

* Segments crossing the top or bottom bounds are clipped mathematically; the X
  coordinate at the intersection is calculated, and the `ys`/`ye` are clamped.

### 2. The Bucketing System

To avoid sorting all edges every frame (which would be *O(N log N)*), the
system uses a bucket sort.

* An array of pointers `canvas_edges_start` is allocated, one slot per scanline
  height.

* Edges are linked into a list at the index corresponding to their `ys`
  (starting scanline).

* This allows the rasterizer to find new edges entering the scene in *O(1)* time
  as it iterates down the image.

### 3. The Anti-Aliasing System (Coverage Mask)

ChrysaLisp uses an 8x vertical supersampling technique optimized for memory
efficiency. It does not allocate a full supersampled buffer.

**Virtual Scanlines:**

If AA is enabled, the rasterizer processes the image at 8x vertical resolution.
For every physical pixel row, it processes 8 "Virtual" rows.

**Jittered Sampling:**

It uses a look-up table (`sample_offsets`) containing 8 specific sub-pixel
offsets (e.g., 0.25, 0.875). As the edges are processed on the virtual
scanlines, their X positions are jittered by these offsets. This converts
aliasing patterns (stair-steps) into noise, which looks smoother to the human
eye.

**The Coverage Buffer:**

A single byte array `canvas_coverage` (width of the screen) acts as the
accumulator.

1. For each of the 8 virtual passes, spans are generated.

2. Instead of writing color, a specific bit index (0-7) corresponding to the
   current virtual pass is **XORed** into the coverage buffer at that pixel
   index.

3. This XOR logic automatically handles the "Odd-Even" winding rule (holes in
   polygons) without complex logic.

### 4. The Scanline Loop (AEL)

The main loop iterates from the top of the dirty region to the bottom.

1. **Update AEL:** New edges from the current scanline bucket are added to the
   Active Edge List.

2. **Sort:** The AEL is sorted by X coordinate.

3. **Fill:**

	 * The loop pairs edges (Entry Edge -> Exit Edge).

	 * It calculates the span between them.

	 * It applies the XOR bitmask to `canvas_coverage`.

4. **DDA Step:** `x = x + w` (Move X by the slope) for all active edges.

5. **Retire:** Edges where `y == ye` are removed.

### 5. The Flush / Blender (`:span_noclip`)

After 8 virtual passes are completed for a single physical scanline:

1. The system iterates over `canvas_coverage`.

2. It uses a LUT (`mask_to_coverage`) to count the set bits in the byte.

	 * 0 bits = 0% coverage.

	 * 8 bits = 100% coverage.

	 * 4 bits = 50% coverage.

3. This count becomes the **Alpha** value.

4. `:span_noclip` reads the destination pixel, blends the source color using
   this calculated Alpha, and writes the result back.

5. The `canvas_coverage` buffer is cleared for the next line.

### Summary

This architecture provides a "best of both worlds" approach.

* **High Quality:** 8x supersampling provides excellent edge smoothing.

* **Low Memory:** It only requires one line of extra memory buffer, regardless
  of image height.

* **Flexibility:** The separation of Stroking and Rasterizing allows the same
  engine to render thin lines, thick strokes, fonts, and complex SVG shapes.