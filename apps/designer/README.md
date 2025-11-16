# ChrysaLisp Designer Mode

A visual WYSIWYG designer for ChrysaLisp GUI applications, inspired by the revolutionary Interface Builders of the past - particularly NeXT's InterfaceBuilder and the original Lisp InterfaceBuilder.

## Overview

The Designer Mode is a groundbreaking approach to GUI development that **executes** (not parses) the same Lisp pseudo-declarative markup used by applications. This means:

- The same code runs in both the designer and the final application
- No parsing or code generation - it's pure execution with tracking
- Preserves inline imperative (non-UI) code when saving
- True WYSIWYG - what you design is exactly what runs

## Key Concepts

### Execution, Not Parsing

Unlike traditional GUI builders that parse markup (like DreamWeaver with HTML, or modern InterfaceBuilder with XIBs), the ChrysaLisp Designer **executes** the UI code. This is the same revolutionary approach used by the original Lisp InterfaceBuilder.

The designer works by:

1. Swapping `(import "gui/lisp.inc")` with `(import "gui_designer/lisp.inc")`
2. The designer macros wrap the standard UI macros
3. As the code executes, it both creates the UI AND tracks the structure
4. The tracked structure can be edited visually
5. Saving serializes the structure back to Lisp code
6. The imports are swapped back to standard `gui/lisp.inc`

### Import Swapping

The key innovation is import swapping:

```lisp
;Normal app mode
(import "gui/lisp.inc")

;Designer mode (automatic swap when loading for editing)
(import "gui_designer/lisp.inc")
```

This allows the same application code to run in both modes without modification.

## Architecture

### Components

1. **gui_designer/lisp.inc** - Tracking macros that wrap standard UI macros
2. **gui_designer/serialize.inc** - Serialization back to Lisp format
3. **gui_designer/runtime.inc** - Interactive editing runtime
4. **gui_designer/loader.inc** - Import swapping and app loading
5. **apps/designer/app.lisp** - The Designer application itself

### Workflow

```
┌─────────────────┐
│  Load App File  │
└────────┬────────┘
         │
         ▼
┌─────────────────────────┐
│ Swap Import to Designer │
└────────┬────────────────┘
         │
         ▼
┌──────────────────────┐
│  Execute App Code    │
│  (builds UI + track) │
└────────┬─────────────┘
         │
         ▼
┌────────────────────┐
│  Visual Editing    │
│  - Select elements │
│  - Modify props    │
│  - Add/remove      │
│  - Drag/drop       │
└────────┬───────────┘
         │
         ▼
┌──────────────────────┐
│ Serialize to Lisp    │
└────────┬─────────────┘
         │
         ▼
┌─────────────────────────┐
│ Restore Standard Import │
└────────┬────────────────┘
         │
         ▼
┌──────────────┐
│  Save File   │
└──────────────┘
```

## Usage

### Creating a New Design

```lisp
;Start the designer
(import "apps/designer/app.lisp")
```

1. Click "New" to start a blank design
2. Use the widget palette to add components
3. Configure properties in the property inspector
4. Save the design to a .lisp file

### Editing an Existing App

```lisp
;Load an app for editing
(defq result (designer-load-app-for-editing "apps/calculator/app.lisp"))
```

The designer will:
1. Read the app file
2. Swap the import to designer mode
3. Execute the code (building UI + tracking structure)
4. Display the UI tree for editing

### Testing the Tracking System

```lisp
;Load the test app
(import "apps/designer/test_app.lisp")

;The UI tree is automatically tracked
(defq tree (get-ui-tree))
(print "Tree: " (str tree))
```

## Features

### Widget Palette

Supports all standard ChrysaLisp widgets:
- ui-button
- ui-label
- ui-text
- ui-textfield
- ui-flow (layout container)
- ui-grid (grid layout)
- ui-slider
- ui-progress
- ui-canvas
- ui-vdu (terminal)
- ui-scroll
- ui-backdrop
- ui-title
- ui-radio-bar
- ui-toggle-bar
- ui-tree
- ui-spinner
- ui-view
- ui-stack

### Property Inspector

Edit common properties:
- :text
- :color
- :ink_color
- :font
- :min_width
- :min_height
- :border
- :flow_flags
- :grid_width
- :grid_height

### UI Tree View

Visual hierarchy showing:
- Element type
- Element name
- Number of children
- Nested structure

### Serialization

Saves back to valid Lisp code:
- Preserves structure
- Maintains property formatting
- Restores standard imports
- **Preserves non-UI code** (the revolutionary part!)

## Philosophy

This implementation honors the vision described in Paul Hammant's blog post [Interface Builders: Alternative Lisp Timeline](https://paulhammant.com/2013/03/28/interface-builders-alternative-lisp-timeline).

The key insight is that Lisp's code-as-data philosophy allows UI definitions to be both:
1. **Executable** - they run to create the interface
2. **Manipulable** - they can be edited as data structures
3. **Serializable** - they can be written back as code

This creates a perfect cycle:
```
Code → Execute → Runtime Objects → Edit → Serialize → Code
```

The same code that defines the UI in the source file is the same code that runs in the application. The designer is just a different execution environment that adds tracking and editing capabilities.

## Historical Context

From the blog post:

> "The revolutionary tech was that the Lisp version took pseudo-declarative UI building blocks in Lisp, and built the UI from that, but that was also the serialization format for the designer. Meaning you could save the UI design, and go right on coding imperatively in the UI class."

This is **exactly** what we've implemented here. The pseudo-declarative UI macros like `ui-window`, `ui-button`, etc. are both:
- The source code format
- The designer's internal representation
- The runtime execution format

## Limitations and Future Work

Current implementation:
- ✅ Import swapping
- ✅ UI structure tracking
- ✅ Serialization back to Lisp
- ✅ Basic designer UI
- ✅ Widget palette
- ⚠️ Visual selection (partial)
- ⚠️ Drag-and-drop (partial)
- ⚠️ Property editing (basic)
- ⚠️ Preservation of imperative code (basic)

Future enhancements:
- Full drag-and-drop positioning
- Live preview updates
- Undo/redo
- Grid snapping
- Alignment guides
- Copy/paste
- Complete non-UI code preservation
- Template library
- Component composition

## Examples

### Simple Button Example

```lisp
;This code works in both normal mode and designer mode
(import "gui/lisp.inc")  ;or gui_designer/lisp.inc

(ui-window *window* ()
  (ui-flow main_flow (:flow_flags +flow_down_fill)
    (ui-button my_button (:text "Click Me"
                          :min_width 100))))
```

When loaded in designer mode, this creates a visual editor where you can:
- Select the button
- Change its text
- Adjust its width
- Move it to different containers
- Save changes back to the source file

### The Magic

The **exact same code** runs in both contexts:
- In production: creates the UI
- In designer: creates the UI + tracking metadata

No code generation. No parsing. Just execution with different side effects.

This is the revolution that didn't happen in mainstream programming - until now.

## Technical Details

### Macro Wrapping

Each UI macro in `gui_designer/lisp.inc` wraps the original:

```lisp
(defmacro ui-button (n &optional p &rest x)
  (static-qqp (progn
    ;Track the element
    (designer-push-element
      (designer-make-element "ui-button" ',n '(Button) ',p '(~x)))
    ;Execute normally
    (ui-element-original ,n (Button) ,p ~x)
    ;Pop from stack
    (designer-pop-element))))
```

This creates a stack-based tracking system that mirrors the execution tree.

### Tree Structure

Each tracked element contains:

```lisp
{:id <unique-id>
 :type "ui-button"
 :name "my_button"
 :constructor "(Button)"
 :props ((:text "Click Me" :min_width 100))
 :children ()}
```

### Serialization

The tree serializes back to the original macro format:

```lisp
(ui-button my_button (:text "Click Me" :min_width 100))
```

Perfect round-tripping: Code → Tree → Code

## Credits

Inspired by:
- NeXT InterfaceBuilder
- Original Lisp InterfaceBuilder
- Paul Hammant's historical research
- The ChrysaLisp architecture

Implemented for ChrysaLisp by the AI assistant, following the principles of execution-based design.
