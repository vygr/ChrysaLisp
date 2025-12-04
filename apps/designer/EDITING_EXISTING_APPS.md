# Editing Existing Apps with Designer Mode

This guide shows how to load existing ChrysaLisp apps (like calculator, whiteboard, etc.) and make them editable in the designer.

## The Magic: Import Swapping + Execution

The designer works by:

1. **Reading** the app's source file
2. **Swapping** `(import "gui/lisp.inc")` → `(import "gui_designer/lisp.inc")`
3. **Executing** the modified code (builds UI + tracks structure)
4. **Capturing** the UI tree for editing
5. **Saving** back with imports restored and imperative code preserved

## Quick Example: Load Calculator

### Step 1: Load with Designer Tracking

```lisp
;Load the enhanced loader
(import "gui_designer/loader_enhanced.inc")

;Load calculator app
(defq result (load-app-for-designer "apps/calculator/app.lisp"))

;Extract the tree
(bind '(tree original-source swapped-source) result)
```

### Step 2: Inspect the Tree

```lisp
;See what was captured
(print "Type: " (get :type tree))          ;; "ui-window"
(print "Name: " (get :name tree))          ;; "*window*"
(print "Children: " (length (get :children tree)))  ;; Number of child elements
```

### Step 3: Edit the Tree

```lisp
(import "gui_designer/runtime.inc")

;Find an element
(defq display-elem :nil)
(designer-walk-tree tree (lambda (elem)
  (when (eql (get :name elem) "*display*")
    (setq display-elem elem))))

;Modify a property
(designer-set-property display-elem :text "Modified!")

;Add a new button
(defq new-btn (make-element "ui-button" "new_button"))
(designer-set-property new-btn :text "New Button")
(designer-add-child parent-element new-btn)
```

### Step 4: Save Changes

```lisp
;Save to a new file
(save-designer-changes tree original-source "apps/calculator/app_modified.lisp")

;Or save back to original (be careful!)
(save-designer-changes tree original-source "apps/calculator/app.lisp")
```

## Complete Demo

Run the included demo:

```bash
./run.sh "(import \"apps/designer/demo_load_calculator.lisp\")"
```

This will:
- ✅ Load calculator app
- ✅ Show UI tree structure
- ✅ Modify an element
- ✅ Serialize the modified UI
- ✅ Show how to save

## What Gets Preserved?

When you edit and save, the system preserves:

### ✅ Preserved (Imperative Code)
- Import statements (other than gui/lisp.inc)
- Enums and constants
- State variables
- Helper functions
- Event handlers
- Main loop
- Comments outside UI section

### 🔄 Replaced (UI Code)
- Everything between `(ui-window ...` and its matching `)`
- This is rebuilt from the edited tree

## Example: Calculator Structure

Original calculator app:

```lisp
(import "././login/env.inc")
(import "gui/lisp.inc")          ;← Swapped during loading
(import "lib/consts/chars.inc")
(import "lib/consts/scodes.inc")

(enums +event 0                   ;← Preserved
	(enum close max min)
	(enum base_change)
	(enum button))

(defq *config* :nil ...)          ;← Preserved

(defun config-default () ...)     ;← Preserved

(ui-window *window* ()            ;← Replaced with edited version
	(ui-title-bar _ "Calculator" ...)
	(ui-radio-bar base_bar ...)
	(ui-label *display* ...)
	(ui-grid button_grid ...))

(defun main ()                    ;← Preserved
	...)
```

After editing and saving:
- UI section is regenerated from tree
- All other code remains unchanged
- Import is restored to `gui/lisp.inc`

## Advanced: Editing Any App

### Load Any App

```lisp
(import "gui_designer/loader_enhanced.inc")

;Load any app
(defq result (load-app-for-designer "apps/whiteboard/app.lisp"))
(defq result (load-app-for-designer "apps/chess/app.lisp"))
(defq result (load-app-for-designer "apps/terminal/app.lisp"))
```

### Batch Modifications

```lisp
;Change all buttons to have min_width 100
(designer-walk-tree tree (lambda (elem)
  (when (eql (get :type elem) "ui-button")
    (designer-set-property elem :min_width 100))))
```

### Extract UI for Reuse

```lisp
;Extract just the UI to a separate file
(defq ui-code (designer-serialize-tree tree))
(when (defq stream (file-stream "calculator_ui.inc" +file_open_write))
  (write stream ui-code))

;Now you can import it from other apps
(import "calculator_ui.inc")
```

## Workflow: Visual Designer Integration

Eventually, the full designer app will integrate this:

```
1. User clicks "Open" in designer
2. File browser selects "apps/calculator/app.lisp"
3. Designer calls load-app-for-designer
4. UI tree displayed in designer
5. User edits visually (drag/drop, properties, etc.)
6. User clicks "Save"
7. Designer calls save-designer-changes
8. File updated with new UI, old code preserved
```

## How It Works Internally

### Import Swapping

```lisp
;Original line in app:
(import "gui/lisp.inc")

;Swapped for loading:
(import "gui_designer/lisp.inc")

;All ui-* macros now track structure while executing

;On save, swapped back:
(import "gui/lisp.inc")
```

### Execution with Tracking

```lisp
;Original code:
(ui-button my_btn (:text "Click"))

;With gui_designer/lisp.inc:
(ui-button my_btn (:text "Click"))
  ;↓
  ;1. Tracks: {:type "ui-button" :name "my_btn" :props ...}
  ;2. Creates: actual Button widget
  ;3. Returns: the widget (same as normal)

;Result: UI is built AND structure is captured
```

### Section Replacement

```lisp
;Original file:
[Header code]
[Functions]
(ui-window *window* ()     ;← UI starts here
  [UI code])               ;← UI section
[More functions]           ;← UI ends here
[Main loop]

;After edit:
[Header code]              ;← Preserved
[Functions]                ;← Preserved
(ui-window *window* ()     ;← Regenerated from tree
  [Modified UI code])      ;← New version
[More functions]           ;← Preserved
[Main loop]                ;← Preserved
```

## Troubleshooting

### "No UI tree captured"

The app might not use standard ui-* macros. Check if it:
- Uses `(import "gui/lisp.inc")`
- Uses `(ui-window ...)` or similar macros
- Has the UI definition at the top level (not inside functions)

### "Error: Could not read file"

Check the file path:
```lisp
;Absolute path
(load-app-for-designer "/full/path/to/app.lisp")

;Relative path (from ChrysaLisp root)
(load-app-for-designer "apps/calculator/app.lisp")
```

### "Modified app doesn't run"

The saved file might have syntax errors. Check:
- Matching parentheses
- Proper imports restored
- No duplicate definitions

Debug by comparing original vs saved:
```bash
diff apps/calculator/app.lisp apps/calculator/app_modified.lisp
```

## Limitations

Current implementation:
- ✅ Loads apps with ui-* macros
- ✅ Captures full UI tree
- ✅ Preserves imperative code
- ⚠️ Assumes UI is at top level (not in functions)
- ⚠️ Assumes one main UI window
- ⚠️ Requires app to be well-formed

## Examples

### Example 1: Change Calculator Display

```lisp
(import "gui_designer/loader_enhanced.inc")
(import "gui_designer/runtime.inc")

(defq result (load-app-for-designer "apps/calculator/app.lisp"))
(bind '(tree original-source _) result)

;Find display
(defq display :nil)
(designer-walk-tree tree (lambda (elem)
  (when (eql (get :name elem) "*display*")
    (setq display elem))))

;Change font
(designer-set-property display :font "*env_title_font*")

;Save
(save-designer-changes tree original-source "apps/calculator/app_bigfont.lisp")
```

### Example 2: Add Button to Whiteboard

```lisp
(import "gui_designer/loader_enhanced.inc")
(import "gui_designer/runtime.inc")

(defq result (load-app-for-designer "apps/whiteboard/app.lisp"))
(bind '(tree original-source _) result)

;Find main flow
(defq main-flow :nil)
(designer-walk-tree tree (lambda (elem)
  (when (and (eql (get :type elem) "ui-flow")
             (eql (get :name elem) "main_area"))
    (setq main-flow elem))))

;Add clear button
(defq clear-btn (make-element "ui-button" "clear_btn"))
(designer-set-property clear-btn :text "Clear All")
(designer-set-property clear-btn :min_width 100)
(designer-add-child main-flow clear-btn)

;Save
(save-designer-changes tree original-source "apps/whiteboard/app_with_clear.lisp")
```

## Summary

Yes, **the designer CAN load and edit existing apps like calculator!**

The process:
1. Import swapping enables tracking
2. Execution captures the UI tree
3. Visual or programmatic editing modifies the tree
4. Serialization rebuilds the UI code
5. Merging preserves imperative code
6. Save writes the updated file

Try it:
```bash
./run.sh "(import \"apps/designer/demo_load_calculator.lisp\")"
```

This demonstrates the revolutionary power of execution-based design: the same code that runs the app is the same code that defines its editable structure! 🎨✨
