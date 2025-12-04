# Visual Property Editor

A complete property editing UI for the ChrysaLisp Designer.

## Overview

The property editor displays and edits properties of the selected UI element with appropriate input types for each property kind:

```
╔════════════════════════════════════╗
║ Properties                         ║
╠════════════════════════════════════╣
║ Type: ui-button                    ║
║ Name: submit_btn                   ║
║────────────────────────────────────║
║ :text                              ║
║ [Submit____________]  ← Text field ║
║                                    ║
║ :min_width                         ║
║ [100_______________]  ← Number     ║
║                                    ║
║ :color                             ║
║ [+argb_green_______] [■]  ← Color  ║
║                                    ║
║ :font                              ║
║ [*env_window_font*_]  ← Font       ║
║────────────────────────────────────║
║ [Apply Changes]                    ║
╚════════════════════════════════════╝
```

## Property Types

### Text Properties

```lisp
:text, :tip_text, :name
```

**Input**: Text field
**Example**: `"Submit"`, `"Click me"`

### Number Properties

```lisp
:min_width, :min_height, :border, :grid_width, :grid_height
```

**Input**: Number field (validated)
**Example**: `100`, `50`, `2`

### Color Properties

```lisp
:color, :ink_color, :hint_color
```

**Input**: Text field + color preview swatch
**Values**:
- Color constants: `+argb_white`, `+argb_green`, etc.
- Numeric ARGB: `0xFFFFFFFF`

**Display**: Shows color preview box next to input

### Font Properties

```lisp
:font
```

**Input**: Text field (could be dropdown)
**Values**: `*env_window_font*`, `*env_title_font*`, etc.

### Boolean Properties

```lisp
:disabled, :hidden
```

**Input**: Checkbox
**Values**: `true` / `false`

### Flag Properties

```lisp
:flow_flags
```

**Input**: Text field (could be dropdown or multi-select)
**Values**: `+flow_down`, `+flow_right_fill`, etc.

## Implementation

### Property Metadata

```lisp
(defq +property-types (scatter (Lmap)
  :text "text"
  :min_width "number"
  :color "color"
  :font "font"
  :disabled "boolean"
  :flow_flags "flags"))
```

### Getting Element Properties

```lisp
(defun get-element-properties (element)
  ; Extracts all properties from element
  (defq props (first (get :props element)))
  ; Returns: ((:text "Submit") (:min_width 100) ...)
  ...)
```

### Formatting Values for Display

```lisp
(defun format-property-value (value type)
  (case type
    ("text" (str value))
    ("number" (str value))
    ("color"
      ; Map number to constant name if known
      (if (= value +argb_green) "+argb_green" (str value)))
    ("font" (str value))
    ("boolean" (if value "true" "false"))
    ...))
```

### Parsing User Input

```lisp
(defun parse-property-value (str-value type)
  (case type
    ("text" str-value)
    ("number" (str-as-num str-value))
    ("color"
      ; Try to resolve constant name, else parse number
      (if (eql str-value "+argb_green")
        +argb_green
        (str-as-num str-value)))
    ("boolean" (eql str-value "true"))
    ...))
```

### Applying Changes to Model

```lisp
(defun apply-property-changes ()
  (when *selected-element*
    ; Get values from input fields
    (defq text-val (get :text text_input))

    ; Update model
    (designer-set-property *selected-element* :text text-val)

    ; Model is updated!
    ; Could regenerate view here
    ))
```

## Workflow

### 1. User Selects Element

```lisp
; User clicks "submit_btn" in tree view
(setq *selected-element*
  (select-element-by-name *design-tree* "submit_btn"))

; Update property panel
(update-property-panel *selected-element*)
```

### 2. Property Panel Populates

```lisp
(defun update-property-panel (element)
  ; Show element info
  (set elem_type :text (cat "Type: " (get :type element)))
  (set elem_name :text (cat "Name: " (get :name element)))

  ; Get properties
  (defq props (get-element-properties element))

  ; Fill in text field
  (defq text-val (find-property props :text))
  (set text_input :text text-val)

  ; Fill in width field
  (defq width-val (find-property props :min_width))
  (set width_input :text width-val)

  ; ... etc for all properties
  )
```

### 3. User Edits Properties

```
User types in text field: "Click Me"
User changes width: "150"
User changes color: "+argb_blue"
```

### 4. User Clicks "Apply"

```lisp
(defun apply-property-changes ()
  ; Read from inputs
  (defq new-text (get :text text_input))
  (defq new-width (get :text width_input))
  (defq new-color (get :text color_input))

  ; Update model
  (designer-set-property *selected-element* :text new-text)
  (designer-set-property *selected-element* :min_width new-width)
  (designer-set-property *selected-element* :color new-color)

  ; Model updated! Could regenerate preview
  (regenerate-preview *design-tree*))
```

### 5. Model Updates, View Regenerates

```lisp
; Before:
(ui-button submit_btn (:text "Submit" :min_width 100))

; After applying changes:
(ui-button submit_btn (:text "Click Me" :min_width 150 :color +argb_blue))
```

## UI Layout

```
┌────────────┬──────────────┬─────────────────┐
│ Tree       │ Preview      │ Properties      │
├────────────┼──────────────┼─────────────────┤
│ • window   │              │ Type: ui-button │
│   • flow   │   Preview    │ Name: submit    │
│     • label│   of UI      │─────────────────│
│     • btn1 │   elements   │ :text           │
│     • btn2 │              │ [Submit_______] │
│            │              │                 │
│            │              │ :min_width      │
│            │              │ [100__________] │
│            │              │                 │
│            │              │ :color          │
│            │              │ [+argb_green_]  │
│            │              │ [■ Green]       │
│            │              │                 │
│            │              │ [Apply Changes] │
└────────────┴──────────────┴─────────────────┘
```

## Advanced Features

### Color Preview Swatch

```lisp
; Show color preview next to input
(ui-flow color_row (:flow_flags +flow_right)
  (ui-textfield color_input (:text "+argb_green"))
  (ui-backdrop color_preview (:color +argb_green
    :min_width 60 :min_height 30)))

; Update preview when color changes
(defun update-color-preview (color-str)
  (defq color-val (parse-color color-str))
  (set color_preview :color color-val))
```

### Property Type Detection

```lisp
(defun get-common-properties-for-type (element-type)
  ; Return relevant properties for this element type
  (cond
    ((find "button" element-type)
      '(:text :min_width :color :border))

    ((find "flow" element-type)
      '(:flow_flags :color :min_width :min_height))

    ((find "grid" element-type)
      '(:grid_width :grid_height :color))

    ...))
```

### Validation

```lisp
(defun validate-property (prop-name value type)
  (case type
    ("number"
      ; Must be numeric
      (if (str-as-num value)
        :t
        (progn (print "Error: must be a number") :nil)))

    ("color"
      ; Must be constant or hex
      (if (or (starts-with "+" value)
              (starts-with "0x" value))
        :t
        (progn (print "Error: invalid color format") :nil)))

    ...))
```

### Live Preview Updates

```lisp
; Option 1: Update on every keystroke
(defmethod :text_changed (event)
  (apply-property-changes)
  (regenerate-preview))

; Option 2: Update on "Apply" button
; (Current implementation - more controlled)
```

## Demo

Run the property editor demo:

```bash
./run.sh "(import \"apps/designer/designer_with_property_editor.lisp\") (main)"
```

You'll see:
1. **Left panel**: Tree of UI elements (clickable)
2. **Center**: Preview area
3. **Right panel**: Property editor

Try this:
1. Click "ui-button submit_btn" in tree
2. Properties populate on right
3. Change `:text` to "Click Me!"
4. Change `:min_width` to "150"
5. Change `:color` to "+argb_blue"
6. Click "Apply Changes"
7. See console output showing updated tree

## Integration

### With Designer App

```lisp
; When element selected (from tree or preview)
(defun on-element-selected (element)
  (property-editor-set-element element)
  (property-editor-refresh))

; When property changed
(defun on-property-changed (prop-name new-value)
  (property-editor-handle-change prop-name new-value)
  (regenerate-preview))

; In main loop
(while running
  ...
  (when property-changed-event
    (on-property-changed prop-name value)))
```

### With State Toggles

```lisp
; Properties might be conditional
(when (designer-get-toggle "show_advanced")
  ; Show advanced properties
  (show-property :flow_flags)
  (show-property :border))
```

### With Drag-Drop

```lisp
; After drag operation, select moved element
(defun after-drag-move (element)
  (designer-execute-move ...)
  (designer-select-element element)
  (property-editor-set-element element)
  (property-editor-refresh))
```

## Future Enhancements

### Dropdowns for Known Values

```lisp
; Color picker dropdown
(ui-flow color_picker ()
  (ui-label _ (:text ":color"))
  (ui-radio-bar color_select
    ("+argb_white" "+argb_black" "+argb_red" "+argb_green" "+argb_blue")))
```

### Font Selector

```lisp
; Font dropdown
(ui-radio-bar font_select
  ("*env_window_font*" "*env_title_font*" "*env_toolbar_font*" "*env_terminal_font*"))
```

### Number Spinner

```lisp
; Number with +/- buttons
(ui-flow width_spinner ()
  (ui-textfield width_input (:text "100"))
  (ui-button minus_btn (:text "-"))
  (ui-button plus_btn (:text "+")))
```

### Boolean Checkbox

```lisp
; Actual checkbox widget
(ui-flow disabled_row ()
  (ui-label _ (:text ":disabled"))
  (ui-checkbox disabled_check (:checked :nil)))
```

### Property Groups

```lisp
; Collapsible sections
(ui-flow layout_group ()
  (ui-title _ (:text "Layout Properties"))
  (ui-flow _ ()
    (property-row :min_width)
    (property-row :min_height)))

(ui-flow appearance_group ()
  (ui-title _ (:text "Appearance"))
  (ui-flow _ ()
    (property-row :color)
    (property-row :font)))
```

### Property Help Text

```lisp
; Tooltip or help text for each property
(ui-flow text_row ()
  (ui-label text_label (:text ":text"
    :tip_text "The text displayed on the widget")))
```

## Benefits

✅ **Visual editing** - No need to type property names
✅ **Type safety** - Appropriate inputs for each type
✅ **Live feedback** - Color previews, validation
✅ **Model-first** - Properties update tree model
✅ **Consistent** - Same property editor for all widgets
✅ **Extensible** - Easy to add new property types

## Conclusion

The property editor provides a **complete visual interface** for editing element properties, with:

- Appropriate input types for each property
- Validation and formatting
- Color previews
- Model-first updates
- Integration with selection system

This removes the need for manual property editing in code! 🎨✨
