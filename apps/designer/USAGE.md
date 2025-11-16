# ChrysaLisp Designer Mode - Usage Guide

## Quick Start

The Designer Mode allows you to visually create and edit ChrysaLisp GUI applications. Here's how to get started.

### 1. Understanding the System

The designer works by **executing** your UI code with tracking enabled. This means:

```lisp
;Your app in normal mode
(import "gui/lisp.inc")
(ui-window *window* ()
  (ui-button my_btn (:text "Click")))

;Your app in designer mode (auto-swapped)
(import "gui_designer/lisp.inc")  ;← Designer tracking enabled
(ui-window *window* ()
  (ui-button my_btn (:text "Click")))  ;← Same code, but tracked!
```

### 2. Running the Demo

Try the included demo to see designer tracking in action:

```bash
# From ChrysaLisp root directory
./run.sh "(import \"apps/designer/demo.lisp\")"
```

You'll see output like:

```
╔════════════════════════════════════════╗
║  Designer Mode - UI Tree Generated     ║
╚════════════════════════════════════════╝

Root Element:
  Type: ui-window
  Name: *demo_window*
  Children: 1

Total Elements: 15

This UI tree can be:
  • Edited visually in the Designer
  • Serialized back to Lisp source
  • Loaded and modified
  • Merged with imperative code
```

### 3. Testing Serialization

Test the round-trip serialization (Code → Tree → Code):

```bash
./run.sh "(import \"apps/designer/test_serialize.lisp\")"
```

This will show how UI code is:
1. Executed with tracking
2. Captured as a tree structure
3. Serialized back to Lisp code

### 4. Using the Designer Application

Launch the full designer:

```bash
./run.sh "(import \"apps/designer/app.lisp\") (main)"
```

The designer provides:
- **Widget Palette** (left) - Drag widgets to add them
- **Design Surface** (center) - Visual preview
- **Property Inspector** (right) - Edit properties
- **UI Tree** (bottom) - Hierarchical view

### 5. Creating Your First Design

#### From Scratch

1. Click "New" in the designer
2. Select widgets from the palette
3. Configure properties
4. Click "Save" to export as .lisp file

#### Editing Existing Apps

To edit an existing app (e.g., calculator):

```lisp
;Load the designer loader module
(import "gui_designer/loader.inc")

;Load an app for editing
(defq result (designer-load-app-for-editing "apps/calculator/app.lisp"))

;The app is now loaded with tracking enabled
;You can edit it in the designer
```

## Core Concepts

### The Magic of Execution-Based Design

Traditional GUI builders **parse** markup:
```
XML/HTML → Parse → Object Tree → Edit → Generate → XML/HTML
```

ChrysaLisp Designer **executes** code:
```
Lisp → Execute → Object Tree (with tracking) → Edit → Serialize → Lisp
```

The difference is profound:
- ✅ No parser needed - Lisp evaluator does the work
- ✅ No code generation - direct serialization
- ✅ Same code in designer and runtime
- ✅ Preserves imperative code alongside UI
- ✅ Perfect round-tripping guaranteed

### Import Swapping

The system automatically swaps imports when loading for editing:

```lisp
;File on disk: apps/myapp/app.lisp
(import "gui/lisp.inc")  ;← Normal mode

;Designer loads and swaps to:
(import "gui_designer/lisp.inc")  ;← Designer mode

;Executes the code (builds UI + tracks structure)

;On save, swaps back to:
(import "gui/lisp.inc")  ;← Normal mode restored
```

This is done by `designer-swap-imports` and `designer-restore-imports` in `gui_designer/loader.inc`.

### Structure Tracking

As your UI code executes, each macro call is tracked:

```lisp
(ui-button my_btn (:text "Click" :min_width 100))

;Becomes a tracked element:
{:id 42
 :type "ui-button"
 :name "my_btn"
 :constructor "(Button)"
 :props ((:text "Click" :min_width 100))
 :children ()}
```

The tracking happens in `gui_designer/lisp.inc` via macro wrapping.

### Serialization

The tree serializes back to the original format:

```lisp
;Tree element
{:type "ui-button" :name "my_btn" :props ((:text "Click"))}

;Serializes to
(ui-button my_btn (:text "Click"))
```

See `designer-serialize-tree` in `gui_designer/serialize.inc`.

## Advanced Usage

### Accessing the UI Tree Programmatically

```lisp
;Import with designer tracking
(import "gui_designer/lisp.inc")

;Build your UI
(ui-window *window* ()
  (ui-button btn (:text "Test")))

;Get the tracked tree
(defq tree (designer-get-tree))

;Inspect it
(print "Type: " (get :type tree))
(print "Children: " (str (length (get :children tree))))

;Serialize to Lisp
(import "gui_designer/serialize.inc")
(defq code (designer-serialize-tree tree))
(print code)
```

### Modifying Trees Programmatically

```lisp
(import "gui_designer/runtime.inc")

;Select an element
(designer-select my-element)

;Change a property
(designer-set-property my-element :text "New Text")

;Add a child
(defq new-btn (make-element "ui-button" "new_btn"))
(designer-add-child parent-element new-btn)

;Serialize the modified tree
(defq code (designer-serialize-tree tree))
```

### Custom Widgets

To add custom widget support:

1. Define the widget in `gui_designer/lisp.inc`:

```lisp
(defmacro ui-my-widget (n &optional p &rest x)
  (static-qqp (progn
    (designer-push-element
      (designer-make-element "ui-my-widget" ',n '(MyWidget) ',p '(~x)))
    (ui-element-original ,n (MyWidget) ,p ~x)
    (designer-pop-element))))
```

2. Add to the palette in `apps/designer/app.lisp`:

```lisp
(defq +widget-types (list
  ...
  "ui-my-widget"))
```

### Preserving Imperative Code

The designer preserves non-UI code when saving:

```lisp
;Original app file
(import "gui/lisp.inc")

(defq *state* 0)  ;← Preserved

(defun my-handler ()  ;← Preserved
  (++ *state*))

(ui-window *window* ()  ;← Edited in designer
  (ui-button btn (:text "Click")))

;After designer save, imperative code is preserved
```

This is handled by `designer-merge-ui` in `gui_designer/loader.inc`.

## File Structure

```
ChrysaLisp/
├── gui/
│   └── lisp.inc              # Standard GUI macros
├── gui_designer/             # Designer mode system
│   ├── lisp.inc             # Tracking macros (wraps gui/lisp.inc)
│   ├── serialize.inc        # Tree → Lisp serialization
│   ├── runtime.inc          # Interactive editing runtime
│   └── loader.inc           # Import swapping & app loading
└── apps/designer/           # Designer application
    ├── app.lisp            # Main designer UI
    ├── demo.lisp           # Demo showing tracking
    ├── test_app.lisp       # Simple test app
    ├── test_serialize.lisp # Serialization tests
    ├── README.md           # Architecture & philosophy
    └── USAGE.md            # This file
```

## API Reference

### gui_designer/lisp.inc

- `(designer-reset)` - Clear tracking state
- `(designer-get-tree)` - Get tracked UI tree
- `*designer-enabled*` - Enable/disable tracking

### gui_designer/serialize.inc

- `(designer-serialize-tree tree)` - Serialize tree to Lisp code
- `(designer-save-to-file tree filepath)` - Save tree to file

### gui_designer/runtime.inc

- `(designer-select element)` - Select an element
- `(designer-get-selection)` - Get selected element
- `(designer-set-property element key value)` - Modify property
- `(designer-add-child parent child)` - Add child element
- `(designer-copy)` - Copy selection to clipboard
- `(designer-paste parent)` - Paste from clipboard

### gui_designer/loader.inc

- `(designer-swap-imports source)` - Swap gui → gui_designer
- `(designer-restore-imports source)` - Swap gui_designer → gui
- `(designer-load-app-for-editing filepath)` - Load app with tracking
- `(designer-save-app tree original-source filepath)` - Save with merge

## Examples

### Example 1: Simple App with Tracking

```lisp
(import "gui_designer/lisp.inc")

(ui-window *window* ()
  (ui-flow main (:flow_flags +flow_down_fill)
    (ui-label title (:text "My App"))
    (ui-button submit (:text "Submit"))))

(defq tree (designer-get-tree))
(print "Created: " (get :type tree))
```

### Example 2: Serialize and Save

```lisp
(import "gui_designer/lisp.inc")
(import "gui_designer/serialize.inc")

;Build UI with tracking
(ui-window *window* ()
  (ui-button btn (:text "Test")))

;Serialize
(defq code (designer-serialize-tree (designer-get-tree)))

;Save to file
(designer-save-to-file (designer-get-tree) "my_ui.lisp")
```

### Example 3: Programmatic Editing

```lisp
(import "gui_designer/lisp.inc")
(import "gui_designer/runtime.inc")

;Build initial UI
(ui-window *window* ()
  (ui-button btn1 (:text "Old Text")))

(defq tree (designer-get-tree))

;Find and modify the button
(designer-walk-tree tree (lambda (elem)
  (when (eql (get :name elem) "btn1")
    (designer-set-property elem :text "New Text"))))

;Serialize modified tree
(defq code (designer-serialize-tree tree))
```

## Troubleshooting

### "designer-get-tree returns :nil"

Make sure you're importing `gui_designer/lisp.inc`, not `gui/lisp.inc`:

```lisp
;Wrong
(import "gui/lisp.inc")

;Right
(import "gui_designer/lisp.inc")
```

### "Properties not preserved"

Ensure you're using the proper property list format:

```lisp
;Right
(ui-button btn (:text "Hello" :min_width 100))

;Wrong (will not track correctly)
(ui-button btn :text "Hello")
```

### "Serialization missing children"

Make sure to call `designer-pop-element` after each UI block:

```lisp
(defmacro ui-my-widget (n &optional p &rest x)
  (static-qqp (progn
    (designer-push-element ...)
    (ui-element-original ,n ...)
    (designer-pop-element))))  ;← Don't forget this!
```

## Further Reading

- [README.md](README.md) - Architecture and philosophy
- [Paul Hammant's Blog Post](https://paulhammant.com/2013/03/28/interface-builders-alternative-lisp-timeline) - Historical context
- [GitHub Issue #262](https://github.com/vygr/ChrysaLisp/issues/262) - Original feature request

## Contributing

To extend the designer:

1. Add new widget macros to `gui_designer/lisp.inc`
2. Add to palette in `apps/designer/app.lisp`
3. Update property definitions in `gui_designer/runtime.inc`
4. Test round-tripping with `test_serialize.lisp`

## License

Part of the ChrysaLisp project. See main repository for license.
