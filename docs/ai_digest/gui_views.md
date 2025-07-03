# The View Class

The `View` class is the abstract base and fundamental building block for every
element in the ChrysaLisp Graphical User Interface (GUI). It is the superclass
from which all other widgets—`Window`, `Button`, `Label`, `Flow`, `Canvas`,
etc.—are derived. Inheriting directly from the `hmap` class, every `View`
instance is fundamentally a hash map, embodying the system's "Recursive
Architecture" philosophy. This design allows the entire GUI to be represented as
a tree of views, known as the scene graph, which governs rendering, layout, and
property inheritance.

## Core Concepts

*   **Scene Graph:** The GUI is a tree of `View` objects. Each view can have a
    parent and a list of children. This hierarchy dictates the drawing order and
    clipping, and facilitates the propagation of events and properties.

*   **Property Inheritance:** When a property (like `:font` or `:color`) is
    accessed on a view, and that view doesn't have it defined locally, the
    system automatically performs an `hmap :search` up the scene graph to its
    parent, its parent's parent, and so on, until the property is found. This
    provides a powerful, Lisp-like lexical scoping model for visual attributes.

*   **Non-Recursive Design:** To align with ChrysaLisp's small-stack philosophy,
    the GUI's layout and drawing mechanisms are explicitly non-recursive. They
    rely on iterative traversals of a flattened representation of the view tree,
    ensuring constant and minimal stack usage regardless of the GUI's
    complexity.

## Hierarchy and Tree Management

These methods are used to construct and navigate the scene graph.

*   **`(:add_child child)` / `(:add_front child)` / `(:add_back child)`**: Adds
    a `child` view to this view's list of children. The child's `:parent`
    property is set automatically. `:add_front` and `:add_back` control the
    Z-order.

*   **`(:get_parent)`**: Returns the view's parent.

*   **`(:sub)`**: Detaches this view from its parent.

*   **`(:to_front)` / `(:to_back)`**: Modifies the view's Z-order relative to
    its siblings.

*   **`(:children)`**: Returns a `list` of the view's immediate children.

*   **`(:flatten)`**: Returns a `list` of the view's subtree, in DFS order.

## Geometry and Layout

The layout system is a highly disciplined, two-pass process triggered by either
a query for a view's preferred size (`:pref_size`) or a command to change its
geometry (`:change`).

Both these systems rely on the `view :flatten` method. This method DFS
enumerates a view tree without descending into `+view_flag_subtree` marked sub
trees. For example a `Scroll` child view.

### The Preferred Size Mechanism (Sizing Pass)

This flow is used to determine the ideal size of a view and its entire subtree
without necessarily applying any changes to the screen. It is initiated by a
call to `:pref_size`.

1.  **`:pref_size`**: The public API to query a view's ideal size. It triggers
    the sizing pass by calling `:constraints`.

2.  **`:constraints`**: This is the orchestrator of the sizing pass. It performs
    a **forward traversal** of a flattened Depth-First Search (DFS) list of the
    view subtree. Because children appear before their parents in this list, it
    ensures that a parent is processed only after all its children's preferred
    sizes have been calculated. For each view, it calls `:constraint`.

3.  **`:constraint`**: This is the workhorse method for a single view. It
    calculates its own preferred width and height (`+view_cw`, `+view_ch`) based
    on its internal properties (e.g., text length, font size, border width) and
    the already-computed, via `:get_constraint`, sizes of its children.

4.  **`:set_constraint(width, height)`**: The primitive setter that stores the
    calculated preferred size in the view's `+view_cw` and `+view_ch` fields.

This pass effectively "bubbles up" size requirements from the leaves of the GUI
tree to the root.

### The Layout Application Mechanism (Layout Pass)

This flow applies the final geometry to all views in a subtree. It is initiated
by a call to `:change`.

1.  **`:change(x, y, w, h, [flag])`**: The public API to set a view's geometry.
    It is optimized to only trigger a full re-layout if the view's `width` or
    `height` actually changes (or if the `flag` is true). If a re-layout is
    needed, it calls `:constrain`.

2.  **`:constrain`**: This orchestrator first executes the **Sizing Pass**
    described above to ensure all preferred sizes are up-to-date. It then
    immediately executes the **Layout Pass**. This method performs a **reverse
    traversal** of the flattened DFS list (parents before children).

3.  **`:layout`**: A parent's `:layout` method is responsible for calculating
    the final position and dimensions for each of its children based on their
    preferred sizes, via `:get_constraint`, and its own layout rules (e.g.,
    `Flow` vs. `Grid`). It then calls `:set_bounds` on each child.

4.  **`:set_bounds(x, y, w, h)`**: The primitive setter that commits the final,
    calculated geometry to a child view's `+view_x, +view_y, +view_w, +view_h`
    fields.

This pass effectively "trickles down" the allocated space from the root of the
GUI tree to the leaves.

## Drawing and Compositing

ChrysaLisp uses an efficient dirty-region system for rendering.

*   **`:dirty` / `:dirty_all` / `:add_dirty(...)`**: Methods used to mark a view
    or a specific rectangular area within it as needing to be redrawn. This adds
    the rectangle to a `dirty_region` list.

*   **`:draw`**: The core drawing method. Subclasses override this to render
    their content. Drawing is performed using the `ctx` (context) API, such as
    `:ctx_filled_box` or `:ctx_blit`. The GUI compositor ensures `:draw` is only
    called for views that are visible and intersect with the screen's total
    damaged area.

*   **`:opaque_region`**: A view can define an opaque region. The compositor
    uses this information to subtract that area from the dirty regions of views
    located behind it, preventing unnecessary redraws.

## Event Handling

The base `View` class provides the infrastructure for event dispatch, while
subclasses implement the specific behaviors.

*   **`:owner_id`**: A `netid` field that identifies the task owning the view.
    All events related to this view are sent as messages to this owner.

*   **`:id`**: A unique integer identifying the specific view instance, used as
    the `target_id` in event messages.

*   **`:hit_tree(x, y)`**: A crucial system method that determines which view is
    at a given screen coordinate, enabling the GUI to route mouse events to the
    correct target.

*   **`:connect(event_id)`**: Allows an application to associate a view with a
    custom action `event_id`. When triggered (e.g., a `Button` is clicked), the
    view emits a message with this ID.

*   **`:emit`**: Sends the action message to the view's owner.

## Class and Struct Definition

As defined in `gui/view/lisp.inc`, the `View` struct contains all the necessary
fields for participating in the GUI.

```vdu
(structure +view +hmap_size
	(offset start)
	(struct node +ln_node_size)       ; Node for scene graph linked list
	(struct list +lh_list_size)       ; List header for children
	(netid owner_id)                  ; Owning task's netid
	(ptr dirty_region opaque_region)  ; Regions
	(ptr ctx_node)                    ; Used by the GUI compositor
	(long id)                         ; Unique instance ID
	(uint flags)                      ; Bitmask (dirty, hidden, opaque, subtree)
	(int ctx_x ctx_y)                 ; Absolute on-screen coordinates
	(int x y w h)                     ; Relative coordinates and size
	(int cw ch))                      ; Constrained (preferred) width/height
```

## Conclusion

The `View` class is a powerful and elegant abstraction that serves as the
cornerstone of the ChrysaLisp GUI. It is more than just a widget superclass; it
is a direct embodiment of the system's core design philosophies. By inheriting
from `hmap` and using a disciplined, non-recursive, two-pass layout algorithm,
it provides a flexible and extensible system for building complex user
interfaces that remains extremely high-performance and perfectly aligned with
the cooperative, small-stack architecture of the operating system as a whole.