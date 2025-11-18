# Color Library Examples

This directory contains practical examples demonstrating how to use the ChrysaLisp Color Library.

## Examples Overview

### Example 1: Basic Usage (`example1_basic_usage.lisp`)
**Purpose:** Learn the fundamentals of the color library

**Topics Covered:**
- Working with web colors by name
- Converting between RGB and HSV color spaces
- Parsing hex color strings (#RGB and #RRGGBB)
- Basic color manipulation (lighten, darken, saturate, etc.)
- Blending colors
- Finding color names from ARGB values

**Run:**
```bash
./run.sh examples/colors/example1_basic_usage.lisp
```

**Sample Output:**
```
Forest green ARGB: 4286675746
Forest green hex:  #228B22
RGB components: R=34 G=139 B=34

Red RGB(255,0,0) → HSV(0°,100%,100%)
Cyan HSV(180°,100%,100%) → RGB(0,255,255)

Blending red + blue:
  0% blue: #FF0000
  25% blue: #BF003F
  50% blue: #7F007F
  75% blue: #3F00BF
  100% blue: #0000FF
```

### Example 2: Color Theming (`example2_theming.lisp`)
**Purpose:** Generate professional color schemes for applications

**Topics Covered:**
- Monochromatic themes (variations of one color)
- Complementary themes (opposite colors on color wheel)
- Triadic themes (three evenly spaced colors)
- Analogous themes (adjacent colors on color wheel)
- Split-complementary themes
- Creating complete application themes with primary, accent, and semantic colors

**Run:**
```bash
./run.sh examples/colors/example2_theming.lisp
```

**Key Functions:**
```lisp
(defun generate-monochromatic-theme (base_color))
(defun generate-complementary-theme (base_color))
(defun generate-triadic-theme (base_color))
(defun generate-analogous-theme (base_color))
(defun generate-split-complementary-theme (base_color))
(defun create-app-theme (primary_color))
```

**Sample Output:**
```
TRIADIC THEME
Uses three colors evenly spaced (120°)

--- Triadic Palette ---
  #4169E1  HSV(225°,75%,88%)
  #41E169  HSV(345°,75%,88%)
  #E14169  HSV(105°,75%,88%)

Complete Application Theme:
Primary Colors:
  Primary:       #4B0082
  Primary Light: #6B1FA2
  Primary Dark:  #2B0062
...
```

### Example 3: Widget Integration (`example3_widget_integration.lisp`)
**Purpose:** Integrate ColorPicker widget into GUI applications

**Topics Covered:**
- Creating and configuring ColorPicker widget
- Handling color change events
- Applying colors to UI elements dynamically
- Building color-based tools (paint apps, theme editors, etc.)
- Real-time color preview

**Run:**
```bash
./run.sh examples/colors/example3_widget_integration.lisp
```

**Key Integration Pattern:**
```lisp
; Create picker
(defq *picker* (ColorPicker))
(def *picker* :action_event +event_picker)

; Handle color changes
((>= id +event_picker)
  (defq current_color (. *picker* :get_color))
  (update-ui-with-color current_color))

; Set color programmatically
(. *picker* :set_color 0xffff0000)
```

## Common Use Cases

### Use Case 1: Create a Custom Color Palette
```lisp
(import "lib/colors/colors.inc")

; Start with brand color
(defq brand_color (hex-to-argb "#3498db"))

; Create palette
(defq palette (list
  brand_color
  (color-lighten brand_color 20)
  (color-darken brand_color 20)
  (color-saturate brand_color 20)
  (color-desaturate brand_color 20)))
```

### Use Case 2: Theme Switching
```lisp
(defun apply-theme (widget theme_colors)
  ; Apply theme to widget hierarchy
  (def (. widget :dirty)
    :color (. theme_colors :find :background)
    :ink_color (. theme_colors :find :text))

  ; Recursively apply to children
  (when (defq children (get :children widget))
    (each (lambda (child)
      (apply-theme child theme_colors))
      children)))
```

### Use Case 3: Color Validation
```lisp
(defun validate-contrast (bg_color text_color)
  ; Check if text is readable on background
  (bind '(r1 g1 b1) (rgb-unpack bg_color))
  (bind '(r2 g2 b2) (rgb-unpack text_color))

  ; Simple luminance calculation
  (defq lum1 (+ (* r1 0.299) (* g1 0.587) (* b1 0.114))
        lum2 (+ (* r2 0.299) (* g2 0.587) (* b2 0.114)))

  ; Return contrast ratio
  (/ (max lum1 lum2) (min lum1 lum2)))
```

### Use Case 4: Dynamic UI Coloring
```lisp
(defun colorize-by-value (widget value min_val max_val)
  ; Color widget based on value (red=low, green=high)
  (defq ratio (/ (- value min_val) (- max_val min_val)))

  ; Interpolate hue from red (0°) to green (120°)
  (defq hue (* ratio 120))
  (bind '(r g b) (hsv-to-rgb hue 100 100))

  (def (. widget :dirty) :color (rgb-pack r g b)))
```

## Integration Patterns

### Pattern 1: Color-Aware Widget Base Class
```lisp
(defclass Themed-Widget () (View)
  (defmethod :init ()
    (bind '(this) (call :super :init))
    (def this :theme_colors (create-app-theme (hex-to-argb "#2196F3")))
    this)

  (defmethod :apply_theme ()
    (def (. this :dirty)
      :color (. (get :theme_colors this) :find :surface)
      :ink_color (. (get :theme_colors this) :find :text))
    this))
```

### Pattern 2: Live Color Preview
```lisp
(defun create-color-preview (initial_color)
  (defq preview (ui-view _ (:min_width 100 :min_height 100
                             :color initial_color)))

  (defmethod preview :set_preview_color (color)
    (def (. this :dirty) :color color)
    this)

  preview)
```

### Pattern 3: Color History
```lisp
(defq *color_history* (list))

(defun add-to-history (color)
  ; Keep last 10 colors
  (when (> (length *color_history*) 10)
    (setq *color_history* (most *color_history*)))
  (push *color_history* color))

(defun get-recent-colors ()
  *color_history*)
```

## Tips and Best Practices

1. **Use Web Colors for Consistency**: Start with named web colors for standard UI elements
2. **HSV for Manipulation**: Use HSV space for intuitive color adjustments
3. **Clamp Values**: The library automatically clamps values, but be aware of edge cases
4. **Cache Conversions**: RGB↔HSV conversions are fast but cache if used in tight loops
5. **Theme Objects**: Use Fmap to organize related colors in themes
6. **Event Handling**: ColorPicker emits events on every change - debounce if needed
7. **Contrast Checking**: Always verify text/background contrast for accessibility

## Advanced Topics

### Creating Gradients
```lisp
(defun create-gradient (start_color end_color steps)
  ; Create smooth gradient between two colors
  (map (lambda (i)
    (color-blend start_color end_color (/ i (- steps 1.0))))
    (range steps)))
```

### Color Palette Generation
```lisp
(defun generate-palette (base_color count)
  ; Generate palette by rotating hue
  (defq step (/ 360 count))
  (map (lambda (i)
    (color-rotate-hue base_color (* i step)))
    (range count)))
```

### Material Design Palette
```lisp
(defun create-material-palette (base_color)
  ; Create Material Design-style palette with shades
  (scatter (Fmap)
    :50  (color-lighten base_color 40)
    :100 (color-lighten base_color 30)
    :200 (color-lighten base_color 20)
    :300 (color-lighten base_color 10)
    :400 base_color
    :500 (color-darken base_color 10)
    :600 (color-darken base_color 20)
    :700 (color-darken base_color 30)
    :800 (color-darken base_color 40)
    :900 (color-darken base_color 50)))
```

## See Also

- [Color Library README](../../lib/colors/README.md) - Full API documentation
- [ColorPicker Widget Source](../../lib/colors/picker.inc) - Widget implementation
- [Demo Application](../../apps/colorpicker/app.lisp) - Full-featured demo
- [Edge Case Tests](../../lib/colors/test_edges.lisp) - Comprehensive test suite

## Contributing

Have a useful color example? Add it to this directory with:
1. Clear documentation of what it demonstrates
2. Commented code explaining key concepts
3. Expected output or screenshots
4. Update this README with a description
