# The ChrysaLisp GUI Compositor: A Detailed Look

The ChrysaLisp GUI compositor is responsible for efficiently redrawing portions of the screen that have changed. It uses a view tree, dirty regions, and opaque regions to determine what needs to be drawn and where, ultimately interacting with a host-specific graphics backend through an array of function pointers (`host_gui_funcs`).

## Core Concepts

1. **View Tree:** UI elements (`View` objects and their derivatives) are arranged in a hierarchical tree. A parent view contains child views. Coordinates are typically relative to the parent.

2. **Dirty Regions (`view_dirty_region`):** Each view can have a `view_dirty_region`. This is a `region` object (see below) representing the area *within that view's local coordinates* that needs to be redrawn. Changes to a view (e.g., text update, resize) will mark its region as dirty.

3. **Opaque Regions (`view_opaque_region`):** A view can also declare an `view_opaque_region`. This signifies areas within the view that are fully opaque, meaning nothing behind them will be visible. This is crucial for optimization.

4. **`view_flag_dirty_all`:** If this flag is set on a view, its entire area is 
   considered dirty.

5. **`view_flag_opaque`:** If this flag is set, the view's entire bounding box is 
   considered its opaque region.

6. **Clipping:** Drawing operations are clipped to specific rectangular areas to 
   ensure views don't draw outside their designated visible portions.

7. **Backbuffer:** All drawing operations are first performed on an off-screen 
   "backbuffer" (`SDL_Texture *backbuffer` in SDL, `pixel_t *backbuffer` in raw, or 
   an equivalent concept). This backbuffer is then copied (flushed) to the screen in 
   one go to prevent flickering.

8. **Host Abstraction:** The compositor interacts with the actual graphics 
   hardware/library (SDL, framebuffer, raw blitter) through an array of function 
   pointers: `host_gui_funcs`. This keeps the core compositing logic 
   platform-agnostic.

## The `region` Class (from `gui/region/class.vp`)

The `region` class is fundamental to the compositor. It represents an arbitrary 2D 
area as a collection of **non-overlapping rectangles**. This is a key 
characteristic.

* **Representation:** A `region` is internally a linked list of `rect` structures. 
  Each `rect` has `x, y, x1, y1` coordinates.

* **Key Operations:**

    * `region :copy_rect(heap, src_region, dst_region, x, y, x1, y1)`: Copies the 
      portion of `src_region` that intersects with the rectangle `(x, y, x1, y1)` 
      into `dst_region`.

    * `region :paste_rect(heap, dst_region, x, y, x1, y1)`: Adds the rectangle 
      `(x, y, x1, y1)` to `dst_region`. If the new rectangle overlaps with existing 
      rectangles in `dst_region`, they are merged to maintain the non-overlapping 
      property. This might involve splitting and combining existing rects.

    * `region :remove_rect(heap, region, x, y, x1, y1)`: Subtracts the rectangle 
      `(x, y, x1, y1)` from `region`. This can result in existing rectangles in 
      `region` being split, resized, or removed.

    * `region :cut_rect(heap, src_region, dst_region, x, y, x1, y1)`: Similar to 
      `copy_rect` but *moves* the intersecting portion from `src_region` to 
      `dst_region`, effectively removing it from `src_region`.

    * `region :clip_rect(heap, region, x, y, x1, y1)`: Modifies `region` so that 
      it only contains parts that are within the rectangle `(x, y, x1, y1)`.

    * `region :translate(region, dx, dy)`: Moves all rectangles in `region` by 
      `(dx, dy)`.

    * `region :free(heap, region)`: Deallocates all rectangles within the `region` 
      and clears it.

    * `region :bounds(region)`: Returns the overall bounding box `(bx, by, bx1, by1)` 
      that encompasses all rectangles in the `region`.

    * **Heap:** All region operations take a `heap` parameter, which is 
      `statics_gui_rect_heap`. This heap is used for allocating/deallocating the 
      `rect` nodes within the region lists.

## The Compositing Process

The compositing process is typically triggered by the `gui-update` Lisp function, 
which calls the C++ `host_gui :update` method.

**1. `host_gui :update` (service/gui/class.vp)**

* Checks if the GUI is initialized (`statics_gui_init`). If not, calls `host_gui :init`.

* If `statics_gui_flags` indicate a global change (like a resize), it calls `host_gui :resize`.

* If the `statics_gui_dirty_flag` is set (meaning something needs redrawing):

    1. `host_gui_funcs->begin_composite()`: Sets the rendering target to the backbuffer.

    2. `host_gui :composite(statics->statics_gui_screen, ...)`: This is the core compositing logic, detailed below. It calculates the exact dirty region for the entire screen.

    3. `host_gui_funcs->end_composite()`: Resets the rendering target (usually to the main window/screen).

    4. `host_gui_funcs->flush(&rect)`: Copies the relevant portion of the backbuffer (defined by the bounds of the dirty region returned by `:composite`) to the visible screen.

    5. Clears `statics_gui_dirty_flag` and frees temporary regions from `statics_gui_temps`.

**2. `host_gui :composite(root_view, ...)` (gui/ctx/class.vp)**

This is the heart of the compositor and involves multiple passes over the view tree.

* **Pass 1: Visibility and Opaque Region Calculation (Backward Tree Traversal)**

    * Uses `view :backward_tree` to traverse the view tree from the rearmost children up to the root.

    * **`visible_down_callback`:**

        * Calculates the view's absolute screen coordinates (`view_ctx_x`, `view_ctx_y`).

        * If the current view is opaque (`view_flag_opaque` or has an `view_opaque_region`):

            * If `view_flag_opaque`, its entire bounding box is considered. Otherwise, its `view_opaque_region` (translated to parent coordinates) is used.

            * This opaque area is *removed* from the `view_dirty_region` of all its ancestors up to the root using `region :remove_rect` or `region :remove_region`. This is crucial: if a child is opaque, its ancestors don't need to redraw the area it covers.

    * **`visible_up_callback`:**

        * If `view_flag_dirty_all` is set for the current view, its entire area (0, 0, width, height) is `region :paste_rect`'d into its *local* `view_dirty_region`.

        * The view's `view_dirty_region` is then clipped to its own bounds (`region :clip_rect`).

        * This (now clipped) local `view_dirty_region` is translated to its parent's coordinate system and then `region :paste_rect`'d into its parent's `view_dirty_region`.

        * The view's local `view_dirty_region` is then freed (`region :free`).

    * **Result of Pass 1:** The `root_view` (screen) now has a `view_dirty_region` that represents all areas on the screen that need redrawing, *excluding* areas covered by opaque views that themselves are not dirty.

* **Pass 2: Distribute Visible Regions and Build Draw List (Forward Tree Traversal)**

    * Uses `view :forward_tree` to traverse the view tree from the root down to the children.

    * **`distribute_down_callback`:**

        * The parent's `view_dirty_region` (which is the area *it* needs to draw) is copied (`region :copy_rect`) into the current child's `view_dirty_region`, translated and clipped to the child's local bounds. This child's `view_dirty_region` now represents the *actual visible area* it is responsible for drawing.

        * If the child has a non-empty `view_dirty_region` after this clipping:

            * The child is added to a draw list (`statics_gui_ctx_flist`).

            * If the child is opaque: its `view_opaque_region` (translated and clipped to the parent's current dirty region) is *removed* (`region :remove_rect` or `region :remove_region`) from the parent's `view_dirty_region`. This prevents siblings drawn *after* (and behind) this opaque child from redrawing the area it will cover.

        * The callback returns a flag indicating whether to descend into this child's children (typically yes, unless the child's dirty region is empty).

    * **`distribute_up_callback`:** Primarily for symmetry, no major region operations here.

    * **Result of Pass 2:** A draw list (`ctx_flist`) is populated containing only the views that have a non-empty (visible and dirty) region to draw. Each view in this list has its `view_dirty_region` correctly set to the area it needs to render.

* **Pass 3: Actual Drawing**

    * The compositor iterates through the `ctx_flist` (draw list).

    * For each view in the list:

        * Calls the view's `:draw` method.

        * Inside the view's `:draw` method (and its `ctx_...` helper methods like `ctx_box`, `ctx_filled_box`, `ctx_blit`):

            * The `host_gui_funcs->set_clip` is called with rectangles from the view's `view_dirty_region`. This ensures drawing primitives are hardware-clipped to only the necessary parts.

            * The view then uses other `host_gui_funcs` (e.g., `box`, `filled_box`, `blit`) to render its content. These drawing operations are relative to the view's `view_ctx_x`, `view_ctx_y`.

        * After drawing, the view's `view_dirty_region` is freed.

## Summary of Region Usage

* **`view_dirty_region`:**

    * Accumulates dirty areas from children upwards (Pass 1, `visible_up_callback`).

    * Gets reduced by opaque children below it (Pass 1, `visible_down_callback`).

    * Is distributed downwards to children, becoming their specific drawing area (Pass 2, `distribute_down_callback`).

    * Serves as the clip region(s) for the view's actual drawing primitives (Pass 3).

* **`view_opaque_region`:**

    * Used to subtract from ancestor's dirty regions (Pass 1, `visible_down_callback`).

    * Used to subtract from parent's dirty region before passing to subsequent siblings (Pass 2, `distribute_down_callback`).

* **`region :paste_rect`:** Used to add areas to a `view_dirty_region` (e.g., a view marking itself dirty, or a parent accumulating child dirty areas).

* **`region :remove_rect` / `region :remove_region`:** Used to subtract opaque areas from dirty regions.

* **`region :copy_rect` / `region :cut_rect`:** Used when distributing a parent's dirty region to a child.

* **`region :translate`:** Used to convert regions between parent and child coordinate systems.

* **`region :clip_rect`:** Used to ensure a view's dirty region doesn't extend beyond its own bounds or the (translated) dirty region of its parent.

## Conclusion

This compositor design is quite sophisticated. By distinguishing between a view's self-declared dirty area, its opaque area, and the final clipped visible region it needs to draw, it effectively minimizes redundant drawing operations. The `region` class, with its ability to manage complex non-overlapping rectangular areas through CSG-like operations (union via paste, subtraction via remove, intersection via clip), is the cornerstone of this efficiency.

It correctly handles overlapping opaque and transparent views, ensuring that only the truly visible and changed pixels are processed by the host graphics backend.