# The ChrysaLisp GUI Service and Widgets

ChrysaLisp provides a unique and powerful GUI system that is deeply integrated
with its Lisp environment. Unlike traditional toolkits, ChrysaLisp treats UI
widgets themselves as Lisp environments, allowing for a highly dynamic and
flexible approach to constructing user interfaces. This document details the GUI
service, the core architectural concepts, the UI builder macros, the event
system, and provides a reference for the standard widgets.

## Core Architectural Concepts

The ChrysaLisp GUI architecture is built upon a few fundamental principles:

1. **Widgets as Lisp Classes:** All UI elements are instances of Lisp classes
   (defined with `(defclass ...)` from `lib/class/class.inc`) that inherit from
   a common base `View` class (`widgets.md`).

2. **`View` Inherits `hmap`:** The `View` class, in turn, inherits from the
   Virtual Processor (VP) level `hmap` class. An `hmap` is the fundamental
   structure for Lisp environments in ChrysaLisp (`environment.md`). This
   inheritance is crucial: **every UI widget IS a Lisp environment**.

3. **The UI Tree:** Widgets are organized in a hierarchical parent-child tree. A
   `Window` typically serves as the root, with other widgets like layouts
   (`Flow`, `Grid`) and controls (`Button`, `Textfield`) nested within it.
   Parenting is managed by methods such as `(. parent_widget :add_child
   child_widget)`.

4. **Property System via Environment Inheritance:**

    * Because widgets are environments, they can store symbol bindings
      (properties) using `(def widget :property value)` or `(set widget :property
      value)`.

    * When a property is accessed using `(get :property widget_instance)`,
      ChrysaLisp searches for the property first on `widget_instance` itself. If
      not found, it recursively searches up the UI tree through its parent
      widgets until the property is found or the root is reached. If the property
      isn't found, `(get)` returns `:nil`. This is explicitly stated in
      `widgets.md`: "When you use `(get :color this)` on a widget instance a
      search will be made starting at this widget... all the way up to the parent
      widget, all the way up to the root widget if needed."

    * `(def? :property widget_instance)` checks if a property is defined directly
      on the widget, without an upward search.

    * This system allows for powerful theming (e.g., setting `:font` or
      `:ink_color` on a `Window` to be inherited by all children) and contextual
      data flow.

5. **Widget Behavior via Methods:** Widgets have methods (defined with
   `(defmethod ...)`) that control their appearance and interactivity, such as
   `:draw`, `:layout`, `:mouse_down`, `:key_down`, etc. These methods can use
   `(get ...)` to access properties that influence their behavior.

## UI Builder Macros

ChrysaLisp provides a set of Lisp macros, primarily found in `gui/lisp.inc`
(and listed in `widgets.md` and `macros.md`), to simplify the construction of UI
widget trees. These macros typically:

* Create an instance of the respective widget class.

* Set default properties (often related to appearance or behavior).

* Allow overriding default properties and adding custom ones through an optional
  `props` argument.

* Handle adding the newly created widget as a child to its parent in the macro's
  lexical scope (often an implicit `_ui` variable holding the current parent).

* Connect action events to event IDs if applicable.

The general structure for using these macros is often nested, reflecting the UI
tree:

```vdu
;; (ui-window name [props] [body])
;; (ui-element name constructor [props] [body]) are the base forms

(ui-window *my_app_window* (:title "My Application" :color +argb_grey2)
    (ui-flow *main_layout* (:flow_flags +flow_down_fill)
        (ui-label *a_label* (:text "Enter name:"))
        (ui-textfield *name_field* (:hint_text "Your name here"))
        (ui-button *submit_button* (:text "Submit" :action_event +event_submit_clicked))
    )
)
```

The `ui-props` macro can be used to merge default properties with user-supplied
ones within these builder macros.

## The Event System

The GUI event system, as described in `event_dispatch.md` and `event_loops.md`,
enables applications to respond to user interactions and system notifications.

1. **Event Source:** The GUI task (a separate ChrysaLisp task managing the
   overall GUI) sends event messages to an application's main mailbox.

2. **Event Message Structure:**

    * Events are Lisp objects (often simple lists or structures built from
      `str-alloc` and `setf->`).

    * A crucial field is `+ev_msg_target_id`, which contains the ID of the
      widget the event is intended for. User-defined action events (e.g., from
      buttons) typically have positive IDs, while internal UI widget events
      might use negative IDs.

    * Other fields include `+ev_msg_type` (e.g., `+ev_type_mouse`,
      `+ev_type_key_down`, `+ev_type_action`), and type-specific data (e.g.,
      mouse coordinates for mouse events, key codes for key events).

3. **Event Dispatch in Applications:**

    * The application's main event loop reads messages from its primary mailbox.

    * **`cond` Dispatch:** A common way to handle events is using a `(cond)`
      statement that branches based on `+ev_msg_target_id` or `+ev_msg_type`.

    * **`Fmap` Dispatch:** For more complex applications, an `Fmap` (a hash
      map) can map event IDs or event types to handler functions. This is the
      approach used in the **Editor application** (`apps/edit/actions.inc`
      defines `*event_map*`, `*key_map*`, etc., which are `Fmap`s). The main
      loop in the Editor (`apps/edit/app_impl.lisp`) then looks up the event/key
      in these maps and calls the associated handler function via
      `(dispatch-action ...)`.

    * **Widget's `:event` Method:** If an event is not handled by the
      application's primary dispatch logic, it's often passed to the root
      window's `:event` method (e.g., `(. *window* :event msg)`). The `Window`
      class (and potentially the base `View` class) then further dispatches the
      event to the specific target widget's interaction methods (e.g.,
      `:mouse_down`, `:key_down`, `:action`).

4. **The Template Application (`apps/template/app.lisp`):**

    * The Template application also uses an `*event_map*` (and key maps) defined
      in `apps/template/actions.inc`.

    * Its main loop in `apps/template/app.lisp` follows a similar pattern to
      the Editor:

```vdu
(while *running*
    (defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
    (cond
        ;; ... other mailbox handling (tip, timer) ...
        ((defq id (getf *msg* +ev_msg_target_id) action (. *event_map* :find id))
            ;; call bound event action
            (dispatch-action action))
        ;; ... key handling ...
        (:t ;gui event
            (. *window* :event *msg*))))
```

5. **Multiple Mailboxes for GUI Applications (`event_loops.md`):**

    * GUI applications often use `(alloc-select)` to create and manage multiple
      mailboxes. This allows them to partition different types of events:

        * `+select_main`: For GUI events from the GUI service.

        * `+select_timer`: For timer events generated by `(mail-timeout ...)`
          (used for animations, timed actions, or implementing custom timeouts).

        * `+select_tip`: Often used for handling tooltip display logic, also
          typically driven by `(mail-timeout ...)`.

        * Other application-specific mailboxes (e.g., `+select_reply` for farm
          workers in the Raymarch or Editor's `pipe-farm` usage).

    * The application's main loop uses `(mail-select select_list)` to block until
      a message arrives on any of the monitored mailboxes and then processes the
      message based on which mailbox (indicated by the index returned by
      `mail-select`) received it.

    * Transient mailboxes are also used for RPC-style communication, for
      instance, by `gui-rpc` for adding/removing windows, where a temporary
      mailbox is created for a single request-reply interaction.

## Widget Reference

The following sections detail the standard UI widgets available in ChrysaLisp.
They typically inherit from `View` or another widget class. Their Lisp-level
definitions are primarily in `gui/<widgetname>/lisp.inc` and they are made
available via `gui/lisp.inc`.

### `View`

* **Inherits:** `hmap` (VP Level)

* **Lisp Class:** `(View)` (from `gui/view/lisp.inc`)

* **UI Macro:** `(ui-view name [props] [body])`

* **Description:** The base class for all UI widgets. It provides fundamental
  functionality for existing within the UI tree, managing position and size,
  handling dirty regions for rendering, and basic event dispatch.

* **Key Properties (often set via `props` in UI macros or inherited):**

    * `:color`: Background color.

    * `:ink_color`: Default text/foreground color.

    * `:font`: Default font.

    * `:border`: Border style/thickness indicator.

    * `:min_width`, `:min_height`: Preferred minimum dimensions.

    * `:flow_flags`: (For layout behavior if it's a container or its parent is a
      `Flow`).

    * User-defined properties.

* **Key Methods (from `View.md` and `gui/view/lisp.inc`):**

    * `(:add_child child)`, `(:add_front child)`, `(:add_back child)`: Adds a
      child widget.

    * `(:sub)`: Removes the widget from its parent.

    * `(:get_pos)`, `(:set_pos x y)`: Get/set widget position relative to parent.

    * `(:get_size)`, `(:set_size width height)`: Get/set widget size.

    * `(:get_bounds)`, `(:set_bounds x y width height)`: Get/set position and
      size.

    * `(:change x y width height [flag])`: Sets bounds and calls `:layout` if
      changed or flag is true.

    * `(:change_dirty x y width height)`: Marks old area dirty, changes bounds,
      marks new area dirty.

    * `(:pref_size)`: Returns preferred (width height).

    * `(:layout)`: (Often overridden by subclasses) Arranges child widgets.

    * `(:draw)`: (Often overridden by subclasses) Renders the widget.

    * `(:dirty)`, `(:dirty_all)`: Marks the widget or entire hierarchy for
      redraw.

    * `(:emit)`: Sends an action event if targets are connected via `:connect`.

    * `(:connect id)`: Connects an action event from this widget to a specific
      `id`.

    * `(:find_id target_id)`: Searches the widget and its children for a widget
      with `target_id`.

    * `(:hit_tree x y)`: Finds the topmost child widget at screen coordinates `x,
      y`.

    * `(:set_flags value mask)`, `(:get_flags)`: Manage view flags like
      `+view_flag_hiden`, `+view_flag_opaque`.

    * `(:ctx_set_color col)`, `(:ctx_box x y w h)`, `(:ctx_filled_box x y w h)`,
      `(:ctx_blit tid col x y w h)`: Direct drawing context operations.

*Example:*

```vdu
(ui-window *my-window* (:title "My App" :min_width 300 :min_height 200)
    ;; ... child widgets for the window's content Flow ...
)
```

### `Window`

* **Inherits:** `View`

* **Lisp Class:** `(Window)` (from `gui/window/lisp.inc`)

* **UI Macro:** `(ui-window name [props] [body])`

    * Typically creates a `Window` with a default `Flow` child to which `body`
      elements are added.

    * Default props (from `gui/lisp.inc`): `:font *env_window_font*`,
      `:ink_color *env_ink_col*`, `:color *env_window_col*`, `:hint_color
      *env_hint_col*`, `:no_hint_color *env_no_hint_col*`, `:border
      *env_window_border*`, `:shadow *env_window_shadow*`.

* **Description:** The top-level widget for an application's UI. It manages
  window dragging, resizing (implicitly via its content), and serves as the
  root of the UI tree for event dispatch and property inheritance.

* **Key Properties:**

    * `:child`: The main content child (often a `Flow` layout).

    * `:tip_mbox`: (As seen in `event_loops.md`) Mailbox for tooltip events.

* **Key Methods (from `Window.md` and `gui/window/lisp.inc`):**

    * `(:add_child child)`: Sets the main content child.

    * `(:draw)`: Draws the window frame, border, and shadow.

    * `(:layout)`: Lays out its main child.

    * `(:event event)`: Primary event dispatcher for the window and its children.

    * `(:mouse_down event)`, `(:mouse_move event)`: Handle window dragging and
      resizing.

    * `(:drag_mode rx ry)`: Determines drag/resize mode based on mouse position.

*Example:*

```vdu
(ui-window *my-window* (:title "My App" :min_width 300 :min_height 200)
    ;; ... child widgets for the window's content Flow ...
)
```

### `Backdrop`

* **Inherits:** `View`

* **Lisp Class:** `(Backdrop)` (from `gui/backdrop/lisp.inc`)

* **UI Macro:** `(ui-backdrop name [props] [body])`

* **Description:** A simple widget used to display a colored background,
  optionally with a style like a grid, axis lines, or horizontal lines.

* **Key Properties:**

    * `:color` (inherited): Background color.

    * `:ink_color` (inherited): Color for style lines.

    * `:style`: Can be `:grid`, `:axis`, `:lines`, or `:plain` (default
      :nil/plain).

    * `:spacing`: Spacing for grid/lines.

* **Key Methods (from `Backdrop.md` and `gui/backdrop/lisp.inc`):**

    * `(:draw)`: Draws the background and any style lines based on properties.

*Example:*

```vdu
(ui-backdrop *my-grid-bg*
    (:color +argb_grey2 :ink_color +argb_grey5 :style :grid :spacing 16))
```

### `Button`

* **Inherits:** `Label`

* **Lisp Class:** `(Button)` (from `gui/button/lisp.inc`)

* **UI Macro:** `(ui-button name [props] [body])`

    * Default props (from `gui/lisp.inc`): `:flow_flags (num-intern (logior
      +flow_flag_down +flow_flag_align_hcenter +flow_flag_align_vcenter))`,
      `:border *env_button_border*`.

* **Description:** A standard clickable button. It displays text (inherited from
  `Label`) and can show a tooltip.

* **Key Properties:**

    * All `Label` properties (e.g., `:text`, `:font`, `:ink_color`).

    * `:state`: Internal state for visual feedback (pressed/normal).

    * `:tip_text`: Text for the tooltip.

* **Key Methods (from `Button.md` and `gui/button/lisp.inc`):**

    * `(:draw)`: Draws the button with 3D-like border based on its state.

    * `(:layout)`: Lays out its internal text label.

    * `(:mouse_down event)`, `(:mouse_up event)`, `(:mouse_move event)`: Handle
      mouse interactions to change state and emit action events on click.

    * `(:mouse_enter event)`, `(:mouse_exit event)`: Manage tooltip timers.

    * `(:show_tip)`: Displays the tooltip window.

*Example:*

```vdu
(ui-button *ok_button* (:text "OK" :action_event +event_ok :tip_text "Confirm action"))
```

### `Canvas` / `Canvas-pixmap`

* **Inherits:** `Canvas-base` (which inherits `View`)

* **Lisp Class:** `(Canvas width height scale)` or `(Canvas-pixmap pixmap)`
  (from `gui/canvas/lisp.inc`)

* **UI Macro:** `(ui-canvas name width height scale [props])`

* **Description:** Provides a direct drawing surface. `Canvas` creates its own
  pixel buffer, while `Canvas-pixmap` uses an existing `pixmap` object.

* **Key Properties:**

    * `:color` (for drawing operations, not background).

* **Key Methods (from `Canvas-base.md`, `canvas.md`):**

    * `(:draw)`: Typically draws the canvas's texture if it has one (e.g., from a
      `:swap`).

    * `(:fill argb)`: Fills the canvas with a color.

    * `(:set_color argb)`: Sets the current drawing color.

    * `(:plot x y)`, `(:fbox x y width height)`: Draw primitives.

    * `(:fpoly x y winding_mode paths)`, `(:ftri tri)`: Draw filled
      polygons/triangles.

    * `(:swap flags)`: Uploads the canvas content to a texture (for display) and
      returns the canvas.

    * `(:resize canvas)`: Resizes this canvas to match another canvas's content.

    * `(:next_frame)`: Used for animated pixmaps (e.g., FLM files).

    * `(:save file format)`: Saves canvas content.

    * `:set_canvas_flags flags`: e.g., `+canvas_flag_antialias`.

*Example:*

```vdu
(ui-canvas *drawing_area* 200 150 1
    (:on_draw (lambda (this)
        (.-> this (:set_color +argb_red) (:fbox 10 10 50 50) (:swap 0)))))
```

### `Edit`

* **Inherits:** `View`

* **Lisp Class:** `(Edit)` (from `gui/edit/lisp.inc`)

* **UI Macro:** Usually used as part of a more complex editor setup, often
  wrapped by a custom `Editor-edit` class as in the Editor app.

* **Description:** A powerful text editing widget. It manages a `Buffer` object
  for the text content and provides numerous methods for text manipulation,
  cursor movement, selection, and interaction.

* **Key Properties (often managed via its internal `Buffer` or set on the `Edit`
  instance):**

    * `:buffer`: The `Buffer` instance holding the text.

    * `:vdu_text`, `:vdu_paper`, `:vdu_ink`: `Vdu` instances for rendering text,
      selections, and bracket matching.

    * `:color_select`, `:color_found`, `:color_region`: Colors for selections,
      find results, region highlights.

* **Key Methods (from `Edit.md`):**

    * `(:get_buffer)`, `(:set_buffer text_buffer)`

    * `(:get_cursor)`, `(:set_cursor x y)`

    * `(:get_anchor)`, `(:set_anchor x y)`

    * `(:insert string)`, `(:backspace)`, `(:delete)`, `(:break)`

    * `(:home)`, `(:end)`, `(:left)`, `(:right)`, `(:up)`, `(:down)` (and
      `*-select` variants)

    * `(:left_bracket)`, `(:right_bracket)`: Jump to matching bracket.

    * `(:mouse_down event)`, `(:mouse_move event)`, `(:mouse_wheel event)`:
      Handle mouse input.

    * `(:underlay_paper)`, `(:underlay_ink)`: Prepare VDU overlays for
      selections/brackets.

    * `(:reflow)`: Reflows the current paragraph.

    * `(:sort)`, `(:unique)`, `(:invert)`, `(:comment)`, `(:to_lower)`,
      `(:to_upper)`: Text manipulation.

*Example (Simplified, often used within a higher-level editor class):*

```vdu
;; In the Editor app, an Editor-edit class inherits Edit
(defclass Editor-edit () (Edit)
    (def this :font *env_editor_font*))

(ui-element *my_editor_widget* (Editor-edit))
```

### `Flow`

* **Inherits:** `View`

* **Lisp Class:** `(Flow)` (from `gui/flow/lisp.inc`)

* **UI Macro:** `(ui-flow name [props] [body])`

* **Description:** A layout widget that arranges its children in a sequence
  (left, right, up, or down) and can apply fill and alignment options.

* **Key Properties:**

    * `:flow_flags`: A combination of flags (e.g., `+flow_flag_right`,
      `+flow_flag_fillw`, `+flow_flag_align_vcenter`). `gui/flow/lisp.inc`
      defines common combinations like `+flow_right_fill`.

    * `:min_width`, `:min_height`.

* **Key Methods (from `Flow.md` and `gui/flow/lisp.inc`):**

    * `(:layout)`: Positions child widgets according to `:flow_flags` and their
      preferred sizes.

    * `(:pref_size)`: Calculates its preferred size based on children and flow
      direction.

*Example:*

```vdu
(ui-flow *button_bar* (:flow_flags +flow_right :min_height 30)
    (ui-button *b1* (:text "OK"))
    (ui-button *b2* (:text "Cancel"))
)
```

### `Grid`

* **Inherits:** `View`

* **Lisp Class:** `(Grid)` (from `gui/grid/lisp.inc`)

* **UI Macro:** `(ui-grid name [props] [body])`

* **Description:** A layout widget that arranges its children in a regular grid.

* **Key Properties:**

    * `:grid_width`: Number of columns. If 0, calculated from children and
      `:grid_height`.

    * `:grid_height`: Number of rows. If 0, calculated from children and
      `:grid_width`.

* **Key Methods (from `Grid.md` and `gui/grid/lisp.inc`):**

    * `(:layout)`: Positions child widgets in grid cells, giving each equal
      space.

    * `(:pref_size)`: Calculates preferred size based on the max preferred size
      of its children and grid dimensions.

*Example:*

```vdu
(ui-grid *keypad* (:grid_width 3 :grid_height 4)
    ;; ... 12 buttons for a keypad ...
)
```

### `Hchart`

* **Inherits:** `Flow`

* **Lisp Class:** `(Hchart title num_marks)` (from `gui/hchart/lisp.inc`)

* **UI Macro:** `(ui-hchart name title num_marks [props])`

* **Description:** A horizontal bar chart widget. It includes a title, a scale,
  and dynamically added progress bars.

* **Key Properties:**

    * `:units`: Unit multiplier for scale display.

    * `:maximum`: Maximum value for the chart scale.

* **Key Methods (from `Hchart.md` and `gui/hchart/lisp.inc`):**

    * `(:add_bar)`: Adds a new `Progress` bar to the chart and returns it.

    * `(:layout_bars)`: Lays out the dynamically added bars.

    * `(:update_scale)`: Updates the chart's scale display based on current
      values.

*Example:*

```vdu
(ui-hchart *cpu_usage_chart* "CPU Usage" 10 (:units 100 :maximum 100))
(defq bar1 (. *cpu_usage_chart* :add_bar))
(def bar1 :value 50)
(. *cpu_usage_chart* :update_scale)
```

### `Label`

* **Inherits:** `View`

* **Lisp Class:** `(Label)` (from `gui/label/lisp.inc`)

* **UI Macro:** `(ui-label name [props] [body])`

    * Default props (from `gui/lisp.inc`): `:flow_flags (num-intern (logior
      +flow_flag_right +flow_flag_align_vcenter))`, `:border
      *env_label_border*`.

* **Description:** Displays a line of text. The text is actually rendered by a
  child `Text` widget, and the `Label` itself provides a border and background.
  Its internal `Flow` widget (`:label_flow`) controls text alignment.

* **Key Properties:**

    * `:text`: The string to display.

    * `:font`, `:ink_color`, `:color`, `:border`.

    * `:flow_flags` (on its internal `:label_flow`): Controls text alignment
      within the label.

* **Key Methods (from `Label.md` and `gui/label/lisp.inc`):**

    * `(:draw)`: Draws the label's panel (border and background).

    * `(:layout)`: Lays out its internal `Text` widget via the `:label_flow`.

    * `(:add_child child)`: Adds a child to its internal `Flow` (can be used to
      add icons next to text).

*Example:*

```vdu
(ui-label *status_message* (:text "Ready." :font *env_body_font*))
```

### `Node` (Interactive Text)

* **Inherits:** `Text`

* **Lisp Class:** `(Node)` (from `gui/node/lisp.inc`)

* **UI Macro:** `(ui-element name (Node) [props] [body])` (typically, or used
  directly if `Node` is `(defclass ...)` globally).

* **Description:** An interactive text widget. It's like a `Text` widget but
  responds to mouse clicks, visually changing its state. Often used in `Tree`
  widgets.

* **Key Methods (from `Node.md` and `gui/node/lisp.inc`):**

    * `(:mouse_down event)`, `(:mouse_move event)`, `(:mouse_up event)`: Handles
      mouse interactions to provide visual feedback and emit an action event on
      click.

*Example (often part of a Tree):*

```vdu
(ui-element *tree-node-label* (Node) (:text "Expandable Item"))
```

### `Progress`

* **Inherits:** `View`

* **Lisp Class:** `(Progress)` (from `gui/progress/lisp.inc`)

* **UI Macro:** `(ui-progress name [props])`

* **Description:** A simple progress bar.

* **Key Properties:**

    * `:color`: Color of the progress fill.

    * `:value`: Current progress value.

    * `:maximum`: Maximum progress value.

* **Key Methods (from `Progress.md` and `gui/progress/lisp.inc`):**

    * `(:draw)`: Draws the progress bar.

    * `(:pref_size)`: Provides default preferred size.

*Example:*

```vdu
(ui-progress *download_progress* (:value 50 :maximum 100 :color +argb_green))
```

### `Radiobar`

* **Inherits:** `Flow`

* **Lisp Class:** `(Radiobar symbols [mode])` (from `gui/radiobar/lisp.inc`)

* **UI Macro:** `(ui-radio-bar name symbols [props])` or `(ui-toggle-bar name
  symbols [props])`

    * Default props (from `gui/lisp.inc`): `:color *env_toolbar_col*`, `:font
      *env_toolbar_font*`, `:border *env_button_border*`.

* **Description:** A bar of buttons that can operate in radio mode (only one
  selected) or toggle mode (multiple can be selected).

* **Key Properties:**

    * `:mode`: `:t` for toggle mode, `:nil` (default) for radio mode.

* **Key Methods (from `Radiobar.md` and `gui/radiobar/lisp.inc`):**

    * `(:action event)`: Handles button clicks to update selection state.

    * `(:get_selected)`: Returns the index of the selected button (radio mode) or
      potentially a list of states (toggle mode, though API might focus on single
      get).

    * `(:set_selected index)`: Sets the selected button (radio mode).

    * `(:get_states)`, `(:set_states states)`: Manage selection for toggle mode.

*Example:*

```vdu
(ui-radio-bar *alignment_options* '("Left" "Center" "Right")
    (:action_event +event_alignment_change))
```

```vdu
(ui-toggle-bar *style_options* '("Bold" "Italic" "Underline")
    (:action_event +event_style_change))
```

### `Scroll`

* **Inherits:** `View`

* **Lisp Class:** `(Scroll flags)` (from `gui/scroll/lisp.inc`)

* **UI Macro:** `(ui-scroll name flags [props] [body])`

    * Default props (from `gui/lisp.inc`): `:color *env_slider_col*`.

* **Description:** A container that provides scrollbars for a single child
  widget if the child's content exceeds the scroll view's visible area.

* **Key Properties:**

    * `flags`: Combination of `+scroll_flag_vertical`, `+scroll_flag_horizontal`,
      or `+scroll_flag_both`.

    * `:vslider`, `:hslider`: Internal `Slider` widgets for scrollbars.

    * `:child`: The single child widget to be scrolled.

* **Key Methods (from `Scroll.md` and `gui/scroll/lisp.inc`):**

    * `(:action data)`: Handles events from its internal sliders to scroll the
      child.

    * `(:add_child child)`: Sets the scrollable child widget.

    * `(:layout)`: Positions the child and updates scrollbar states based on
      child size and current scroll position.

    * `(:mouse_wheel event)`: Handles mouse wheel input to scroll.

    * `(:visible descendant)`: Scrolls to make the `descendant` widget (a child
      of its main `:child`) visible.

*Example:*

```vdu
(ui-scroll *text_area_scroll* +scroll_flag_both
    (ui-edit *my_text_editor* (;;... editor props ...))
)
```

### `Slider`

* **Inherits:** `View`

* **Lisp Class:** `(Slider)` (from `gui/slider/lisp.inc`)

* **UI Macro:** `(ui-slider name [props])`

    * Default props (from `gui/lisp.inc`): `:color *env_slider_col*`.

* **Description:** A horizontal or vertical slider control.

* **Key Properties:**

    * `:value`: Current value of the slider.

    * `:maximum`: Maximum value.

    * `:portion`: Size of the slider thumb relative to its track (influences how
      much range one click/drag covers).

    * `:state`: Internal state for visual feedback.

* **Key Methods (from `Slider.md` and `gui/slider/lisp.inc`):**

    * `(:draw)`: Draws the slider track and thumb.

    * `(:mouse_down event)`, `(:mouse_move event)`, `(:mouse_up event)`: Handle
      mouse interaction to change slider value and emit action events.

*Example:*

```vdu
(ui-slider *volume_slider* (:maximum 100 :value 50 :action_event +event_volume_change))
```

### `Spinner`

* **Inherits:** `Flow`

* **Lisp Class:** `(Spinner)` (from `gui/spinner/lisp.inc`)

* **UI Macro:** `(ui-spinner name [props])`

* **Description:** A numeric input widget with up/down buttons to
  increment/decrement its value.

* **Key Properties:**

    * `:value`: Current numeric value.

    * `:maximum`, `:minimum`: Range constraints for the value.

* **Key Methods (from `Spinner.md` and `gui/spinner/lisp.inc`):**

    * `(:action event)`: Handles clicks on its internal up/down buttons to change
      the value.

    * `(:layout)`: Updates its internal label displaying the current value.

*Example:*

```vdu
(ui-spinner *quantity_spinner* (:value 1 :minimum 1 :maximum 10 :action_event +event_quantity_change))
```

### `Stack`

* **Inherits:** `Flow`

* **Lisp Class:** `(Stack tabs)` (from `gui/stack/lisp.inc`)

* **UI Macro:** `(ui-stack name tabs [props] [body])`

* **Description:** A tabbed container where only one child (page) is visible at
  a time, selected by clicking on its corresponding tab button.

* **Key Properties:**

    * `:font` (for tab buttons).

* **Key Methods (from `Stack.md` and `gui/stack/lisp.inc`):**

    * `(:action event)`: Handles tab button clicks to switch the visible child
      page.

    * `(:add_child child)`: Adds a new page to the stack.

*Example:*

```vdu
(ui-stack *settings_tabs* '("General" "Appearance" "Advanced")
    (ui-flow *general_settings_page* (;;... general settings widgets ...))
    (ui-flow *appearance_settings_page* (;;... appearance settings widgets ...))
    (ui-flow *advanced_settings_page* (;;... advanced settings widgets ...))
)
```

### `Stroke`

* **Inherits:** `View`

* **Lisp Class:** Not explicitly shown as `(defclass Stroke ...)` in provided
  Lisp snippets, but `(ui-stroke ...)` macro exists. Assume it's a Lisp class.

* **UI Macro:** `(ui-stroke name [props])`

* **Description:** A widget for capturing freehand strokes, like in a drawing
  application.

* **Key Properties:**

    * `:strokes`: A list of paths representing drawn strokes.

    * `:states`: Potentially indicating if a stroke is complete.

* **Key Methods (from `Stroke.md`):**

    * `(:clear)`: Clears completed strokes.

    * `(:mouse_down event)`, `(:mouse_move event)`, `(:mouse_up event)`: Capture
      mouse input to create stroke paths.

*Example (Conceptual, often part of a drawing app like Whiteboard):*

```vdu
(ui-stroke *drawing_pad* (:action_event +event_stroke_updated))
```

### `Text`

* **Inherits:** `View`

* **Lisp Class:** `(Text)` (from `gui/text/lisp.inc`)

* **UI Macro:** `(ui-text name [props])`

* **Description:** A widget for displaying a single line of static
  (non-editable) text.

* **Key Properties:**

    * `:text`: The string to display.

    * `:font`, `:ink_color`, `:color`.

    * `:offset`: Horizontal offset for the text within its bounds.

* **Key Methods (from `Text.md` and `gui/text/lisp.inc`):**

    * `(:draw)`: Renders the text using a cached texture if available.

    * `(:pref_size)`: Calculates preferred size based on text content and font.

*Example:*

```vdu
(ui-text *welcome_message* (:text "Welcome to ChrysaLisp!" :font *env_title_font*))
```

### `Textfield`

* **Inherits:** `Label`

* **Lisp Class:** `(Textfield)` (from `gui/textfield/lisp.inc`)

* **UI Macro:** `(ui-textfield name [props])`

    * Default props (from `gui/lisp.inc`): `:flow_flags (num-intern (logior
      +flow_flag_right +flow_flag_align_vcenter))`, `:border
      *env_textfield_border*`.

* **Description:** A single-line editable text input field.

* **Key Properties:**

    * `:text`: The current text content (bound to `:clear_text` internally for
      editing).

    * `:clear_text`: The actual editable string.

    * `:hint_text`: Placeholder text shown when the field is empty.

    * `:cursor`: Current cursor position.

    * `:anchor`: Selection anchor position.

    * `:mode`: Can be `:t` for password mode (displays asterisks).

* **Key Methods (from `Textfield.md` and `gui/textfield/lisp.inc`):**

    * `(:draw)`: Draws the textfield, including selection and cursor.

    * `(:key_down event)`: Handles keyboard input for text editing, cursor
      movement, selection, and clipboard operations.

    * `(:mouse_down event)`, `(:mouse_move event)`: Handles mouse input for
      cursor positioning and selection.

    * `(:get_text)`: Returns the current text content (`:clear_text`).

    * `(:set_text text)`: Sets the text content.

*Example:*

```vdu
(ui-textfield *password_field* (:hint_text "Password" :mode :t :action_event +event_password_entered))
```

### `Title`

* **Inherits:** `Label`

* **Lisp Class:** `(Title)` (from `gui/title/lisp.inc`)

* **UI Macro:** `(ui-title name [props])`

    * Default props (from `gui/lisp.inc`): `:font *env_title_font*`, `:border
      *env_title_border*`.

* **Description:** A specialized label typically used as part of a `Window`'s
  title bar. It's draggable to move the window.

* **Key Methods (from `Title.md` and `gui/title/lisp.inc`):**

    * `(:mouse_down event)`, `(:mouse_move event)`: Handle mouse input to
      initiate and perform window dragging.

*Example (usually created by `ui-title-bar`):*

```vdu
(ui-title *app_title* (:text "My Application"))
```

### `Tree`

* **Inherits:** `Flow`

* **Lisp Class:** `(Tree event)` (from `gui/tree/lisp.inc`)

* **UI Macro:** `(ui-tree name event [props])`

* **Description:** A widget for displaying hierarchical data, like a file system
  tree. Nodes can be expanded or collapsed.

* **Key Properties:**

    * `:action_event`: Base event ID for folder toggle actions. Leaf node actions
      are `(inc action_event)`.

* **Key Methods (from `Tree.md` and `gui/tree/lisp.inc`):**

    * `(:action event)`: Handles clicks on toggle icons to expand/collapse
      branches.

    * `(:add_route route)`: Adds a path to the tree, creating intermediate nodes
      as needed.

    * `(:populate [root exts n mode])`: Populates the tree from a file system
      directory.

    * `(:empty)`: Clears the tree.

    * `(:expand)`, `(:collapse)`: Expand/collapse all nodes.

    * `(:find_node route)`: Finds a node by its path string.

    * `(:get_route node)`: Gets the path string for a given node.

    * `(:select route)`, `(:highlight route [state])`: Manage selection and
      highlighting.

*Example:*

```vdu
(ui-tree *file_browser_tree* +event_file_folder_action
    (:font *env_medium_terminal_font*))
(. *file_browser_tree* :populate "." '(".lisp" ".inc"))
```

### `Vdu`

* **Inherits:** `View`

* **Lisp Class:** `(Vdu)` (from `gui/vdu/lisp.inc`)

* **UI Macro:** `(ui-vdu name [props])`

    * Default props (from `gui/lisp.inc`): `:font *env_terminal_font*`, `:color
      0`.

* **Description:** A text display widget that renders characters on a grid,
  similar to a terminal. It uses a font and can display colored text.

* **Key Properties:**

    * `:vdu_width`, `:vdu_height`: Dimensions in characters.

    * `:font`, `:ink_color`.

* **Key Methods (from `Vdu.md` and `gui/vdu/lisp.inc`):**

    * `(:char_size)`: Returns (width height) of a character in pixels.

    * `(:load lines offset_x offset_y cursor_x cursor_y)`: Loads an array of text
      lines (can be strings or arrays of character codes with attributes) into
      the VDU.

    * `(:layout)`: Configures the VDU based on its font and dimensions.

*Example:*

```vdu
(ui-vdu *log_output_vdu*
    (:vdu_width 80 :vdu_height 25 :ink_color +argb_green))
(. *log_output_vdu* :load '("Line 1" "Line 2") 0 0 0 0)
```

## UI Combination Macros

These macros from `gui/lisp.inc` (listed in `macros.md`) create common
combinations of widgets:

* `(ui-title-bar name title symbols event [props]) -> flow`

    * Creates a `Flow` layout containing `Button`s (for close, min, max using
      `symbols`) and a `Title` widget.

    * The `event` is the base for button actions.

* `(ui-buttons symbols event [props])`

    * Creates a series of `Button` widgets. `symbols` is a list of UTF8 strings
      or Unicode numbers for button text/icons. `event` is the base ID;
      subsequent buttons get incrementing IDs.

* `(ui-tool-bar name [props] [body]) -> flow`

    * Creates a `Flow` layout, typically styled as a toolbar, to hold buttons or
      other controls.

    * Default props (from `gui/lisp.inc`): `:flow_flags +flow_right`, `:color
      *env_toolbar_col*`, `:font *env_toolbar_font*`.

*Example using combination macros:*

```vdu
(enums +my_events 0 (enum app_close tool_action1 tool_action2))

(ui-window *main_win* (:title "Combo Example")
    (ui-title-bar *app_title_bar* "My App" (0xea19) +my_events_app_close)
    (ui-tool-bar *app_toolbar* ()
        (ui-buttons (0xe9fe 0xe99d) +my_events_tool_action1
            (:tip_text "Action One" "Action Two"))
    )
    ;; ... rest of window content ...
)
```