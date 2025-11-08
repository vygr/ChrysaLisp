# Template

The `Template` application is a simple demo GUI application that you can copy
and work from as a starting point for your own applications.

If you hover the mouse over the embedded UI below you can see the kind of
features available. There are more features available through the key bindings
which can be found in the `keys.md` documentation.

## UI

```widget
apps/template/widgets.inc *window* 512 512
```

## Implementation Study

The application in `apps/template/` is the quintessential starting point for any
developer looking to build a GUI application in ChrysaLisp. It is not a
functional application in itself but rather a well-structured, minimal skeleton.
Its primary purpose is pedagogical: to demonstrate the canonical file structure,
event handling patterns, and core architectural concepts of a ChrysaLisp GUI
application. Understanding this template is the first step to building more
complex programs.

### 1. Core Architecture and Components

The Template app exemplifies the separation of concerns that is idiomatic in
ChrysaLisp GUI development. It can be understood through the
Model-View-Controller (MVC) pattern, though the "Model" is intentionally
minimal.

*   **Model (Application State):**

    * This is the part a developer would expand most. In the template, the model
        consists only of a single global flag, `*running*` in `app.lisp`, which
        controls the main event loop.

    * A real application would replace or augment this with its own data
        structures to hold its state (e.g., a list of items, a game board, user
        settings). The template shows *where* this state should be
        managed—within the `main` function's scope or as globals in `app.lisp`.

*   **View (User Interface):**

    * **`apps/template/widgets.inc`:** This file defines the entire visual
        structure of the application using ChrysaLisp's declarative UI macros.
        It creates the main `*window*`, a `ui-title-bar`, a `ui-tool-bar`, and a
        `ui-stack` widget which contains several example `Backdrop`s and a
        `Grid` of buttons. This demonstrates a common layout pattern with a
        title, a toolbar, and a main content area.

    * The widgets are given symbolic names (e.g., `*window*`, `*stack_flow*`,
        `*b1*`), making them easily accessible from the application logic.

*   **Controller (Event Handling and Logic):**

    * **`apps/template/actions.inc`:** This file acts as the primary
        switchboard. It defines the `*event_map*`, which maps widget-generated
        event IDs (like `+event_button_1`) to handler functions (like
        `action-button-1`). It also defines the various `*key_map*`s for
        keyboard shortcuts.

    * **`apps/template/ui.inc`:** This is where the action handler functions are
        implemented. In the template, these functions are just **stubs**. They
        contain a `(debug-brk "name")` call, which will pause execution in
        the debugger and if one is attached, immediately showing the developer that
        their event was correctly dispatched. This is followed by a trivial UI
        update, like changing a button's color.

    * **`apps/template/app.lisp`:** The `main` function contains the core event
        loop. It waits for messages, determines their type, and uses the maps in
        `actions.inc` to dispatch them to the correct handler functions in
        `ui.inc`.

### 2. A Developer's Guide: Building on the Template

The template's structure provides a clear roadmap for creating a new
application. A developer would typically follow these steps:

1.  **Define the UI (`widgets.inc`):** Modify this file to build the desired
    interface. Add or remove widgets like `ui-button`, `ui-label`, `ui-slider`,
    etc. Assign a new, unique event symbol from the `+event` enum to each
    interactive widget's `:connect` property.

2.  **Define Events and Actions (`actions.inc`):** For every new event symbol
    added in `widgets.inc`, create a corresponding entry in the `*event_map*`
    that maps it to a new action function name (e.g.,
    `(def *event_map* +event_my_new_button action-my-new-button)`).

3.  **Implement Application Logic (`ui.inc`):** Create the Lisp functions for
    the new actions (e.g., `(defun action-my-new-button () ...)`). This is where
    the core logic of the application resides. These functions will interact
    with the application's model (its data) and then update the view (the UI
    widgets).

4.  **Manage State (`app.lisp`):** Define the necessary variables inside the
    `main` function to hold the application's state. The action handlers in
    `ui.inc` will modify these variables.

5.  **Organize Helper Functions:** Place general-purpose helper functions in
    `utils.inc`, clipboard logic in `clipboard.inc`, and undo/redo logic in
    `undo.inc`. This organizational structure keeps the codebase clean and
    manageable.

### 3. Key Implementation Details Walkthrough

*   **The Event Loop (`main` in `app.lisp`):** The core of the application is
    the `(while *running* ...)` loop. It uses `(mail-select select)` to wait for
    messages. The `cond` block inside the loop is the main dispatcher:

    1. It first checks if the event is a known GUI action by looking up the
        `target_id` in `*event_map*`. If found, it calls `(dispatch-action)`.

    2. If it's a keyboard event, it checks the modifier keys and looks up the
        key code in the appropriate `*key_map*`.

    3. If no specific handler is found, it passes the event to the default
        window handler `(. *window* :event msg)`.

*   **Event Mapping (`actions.inc`):**

    This file creates a direct link between a UI element and the code that runs
    when it's activated. The line `+event_button_1 action-button-1` inside the
    `(scatter (Fmap) ...)` form instructs the system that whenever an event with
    the ID `+event_button_1` is received, the function named `action-button-1`
    should be executed.

*   **Placeholder Actions (`ui.inc`):**

    The function `(defun action-button-1 () ...)` is a perfect example of a
    placeholder.

    ```vdu
    (defun action-button-1 ()
        (debug-brk "button1")
        (def (. *b1* :dirty) :color (random-color)))
    ```

    * `(debug-brk "button1")`: This is a powerful tool for a new developer.
        It sets a conditional breakpoint. If the debugger app is running, this
        line will cause the Template app to pause, confirming that the event was
        correctly wired and dispatched.

    * `(def (. *b1* :dirty) :color (random-color))`: This is the minimal "do
        something visible" action. It changes the color of the button that was
        pressed. A developer would replace this with their own logic. The use of
        `(:dirty)` is important, as it signals to the GUI system that this
        widget needs to be redrawn in the next frame.

*   **The `Stack` Widget (`widgets.inc`):**

    The template showcases the `ui-stack` widget, which is a powerful container
    for creating tabbed interfaces or card layouts.

    ```vdu
    (ui-stack *stack_flow* '("main" "settings" "status" "info")
            (:color +argb_black)
        (ui-grid *main_widget* ...)
        (ui-backdrop *settings_widget* ...)
        ...)
    ```

    The `Stack` automatically creates a `Radiobar` (the tabs) using the list of
    strings `("main" "settings" ...)` and displays only one of its child widgets
    at a time, corresponding to the selected tab. This provides a clean way to
    manage multiple views within the same window area.

### 4. Placeholder Utility Files

The inclusion of `undo.inc`, `clipboard.inc`, and `utils.inc` is a structural
guide. Their functions are stubs that simply call `(debug-brk ...)`. This
teaches the developer:

*   **Where to put undo logic:** The `action-undo` function in `undo.inc` is the
    designated place to implement undo/redo functionality.

*   **How to interact with the system clipboard:** `action-copy` and
    `action-paste` in `clipboard.inc` show where to place calls to
    `(clip-get-rpc)` and `(clip-put-rpc)`.

*   **How to organize helper functions:** `utils.inc` is intended for
    general-purpose functions specific to the application, like the
    `random-color` and `select-panel` helpers in the template.

## Conclusion

The Template application is a crucial educational tool in the ChrysaLisp
ecosystem. It is not meant to be used as-is but to be copied and modified. By
providing a clean, working, and well-organized skeleton, it demonstrates the
idiomatic way to structure a GUI application, separating UI definition
(`widgets.inc`), event mapping (`actions.inc`), and application logic (`ui.inc`,
`app.lisp`). For a new programmer, following this pattern—extending the UI,
mapping new events, and filling in the action stubs—is the most direct path to
mastering GUI development in ChrysaLisp.