# ChrysaLisp Color Library

A comprehensive color manipulation and selection library for ChrysaLisp, featuring HSV/RGB color conversion, web color database, and a full-featured ColorPicker widget.

## Features

### Color Library (`lib/colors/colors.inc`)

#### Web Colors Database
- 140+ named web colors (HTML/CSS standard colors)
- Access by keyword name (e.g., `:red`, `:forestgreen`, `:rebeccapurple`)
- Full ARGB values for all colors

#### Color Conversion
- **HSV ↔ RGB conversion**
  - `(hsv-to-rgb h s v)` - Convert HSV (H: 0-360, S: 0-100, V: 0-100) to RGB (0-255)
  - `(rgb-to-hsv r g b)` - Convert RGB (0-255) to HSV

#### ARGB Utilities
- `(argb-pack a r g b)` - Pack ARGB components (0-255) into integer
- `(argb-unpack argb)` - Unpack ARGB integer to (alpha red green blue)
- `(rgb-pack r g b)` - Pack RGB with full alpha
- `(rgb-unpack argb)` - Unpack to (red green blue)
- `(argb-to-hex argb)` - Convert to hex string "#RRGGBB"
- `(hex-to-argb hex_string)` - Parse "#RGB" or "#RRGGBB" to ARGB

#### Color Name Functions
- `(color-name-to-argb name_keyword)` - Get ARGB from color name
- `(color-get-names)` - Get all available color names
- `(color-find-name argb [tolerance])` - Find closest web color name

#### Color Manipulation
- `(color-lighten argb amount)` - Lighten color by amount (0-100)
- `(color-darken argb amount)` - Darken color by amount (0-100)
- `(color-saturate argb amount)` - Increase saturation (0-100)
- `(color-desaturate argb amount)` - Decrease saturation (0-100)
- `(color-rotate-hue argb degrees)` - Rotate hue (-360 to 360)
- `(color-blend argb1 argb2 ratio)` - Blend two colors (ratio: 0.0-1.0)

### ColorPicker Widget (`lib/colors/picker.inc`)

A full-featured GUI widget for interactive color selection.

#### Features
- **HSV Sliders**: Hue (0-360), Saturation (0-100), Value (0-100)
- **RGB Sliders**: Red, Green, Blue (0-255)
- **Live Color Preview**: Visual feedback of current color
- **Hex Input/Display**: Edit and display hex color codes
- **Web Color Palette**: Quick selection from 36 common web colors
- **Action Events**: Emits events when color changes

#### Usage

```lisp
(import "lib/colors/picker.inc")

; Create a color picker
(defq picker (ColorPicker))

; Set action event ID for color change notifications
(def picker :action_event +my_color_change_event)

; Add to your UI
(. my_container :add_child picker)

; Get current color
(defq current_color (. picker :get_color))

; Set color programmatically
(. picker :set_color 0xffff0000)  ; Set to red

; Handle color change events
(when (= event_id +my_color_change_event)
  (defq new_color (. picker :get_color))
  ; Use the new color...
)
```

## Demo Application

A comprehensive demo is available at `apps/colorpicker/app.lisp` showcasing:
- All color picker features
- Color information display (Hex, RGB, HSV)
- Clipboard integration (copy color values)
- Color manipulation (lighten, darken, saturate, desaturate)
- Color name lookup
- Sample text and background previews

### Running the Demo

```bash
./run.sh apps/colorpicker/app.lisp
```

## Examples

### Basic Color Conversion

```lisp
(import "lib/colors/colors.inc")

; HSV to RGB
(bind '(r g b) (hsv-to-rgb 120 100 100))  ; Pure green: (0 255 0)

; RGB to HSV
(bind '(h s v) (rgb-to-hsv 255 0 0))  ; Red: (0 100 100)

; Hex conversion
(defq hex (argb-to-hex 0xffff0000))  ; "#FF0000"
(defq color (hex-to-argb "#00FF00"))  ; 0xff00ff00
```

### Using Web Colors

```lisp
(import "lib/colors/colors.inc")

; Get color by name
(defq forest (color-name-to-argb :forestgreen))  ; 0xff228b22

; Find color name
(defq name (color-find-name 0xffff0000))  ; :red

; List all color names
(defq all_colors (color-get-names))
```

### Color Manipulation

```lisp
(import "lib/colors/colors.inc")

(defq red 0xffff0000)

; Make it lighter
(defq light_red (color-lighten red 20))

; Make it darker
(defq dark_red (color-darken red 20))

; Rotate hue to get different color
(defq shifted (color-rotate-hue red 120))  ; Shift to cyan/green

; Blend two colors
(defq purple (color-blend red 0xff0000ff 0.5))  ; Mix red and blue
```

### Creating Custom Color Pickers

```lisp
(import "lib/colors/picker.inc")

(defclass MyApp () (Window)
  (defmethod :init ()
    (bind '(this) (call :super :init))

    ; Create color picker
    (defq picker (ColorPicker))
    (def picker :action_event +my_color_event)
    (. this :add_child picker)

    ; Store reference
    (def this :color_picker picker)
    this)

  (defmethod :handle_color_change (event)
    (defq color (. (get :color_picker this) :get_color))
    ; Use the color...
    ))
```

## Architecture

### Color Conversion Algorithm
- HSV to RGB uses standard color theory conversion with sector-based calculation
- Handles edge cases (achromatic colors, boundary conditions)
- All calculations use floating-point for precision, converting to integers at the end

### Widget Design
- Inherits from `Flow` for flexible layout
- Uses slider widgets for continuous value adjustment
- Prevents feedback loops during updates with `:updating` flag
- Emits action events on color changes for integration with applications

## Performance Notes

- Color conversions are optimized for interactive use
- Web color lookups use O(1) hash map access
- Color name search is O(n) but typically fast (140 colors)
- Widget updates batch related changes to minimize redraws

## Future Enhancements

Potential additions to the library:
- Color gradient generation
- Color harmony/complementary color suggestions
- Alpha channel support in picker widget
- HSL color space support
- Pantone color matching
- Color blindness simulation
- Save/load color palettes
- Recent colors history

## License

Part of ChrysaLisp project. See main project license.
