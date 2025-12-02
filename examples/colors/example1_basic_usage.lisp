;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Example 1: Basic Color Library Usage
; Demonstrates fundamental color operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/colors/colors.inc")

(print "")
(print "═══════════════════════════════════════")
(print "  Example 1: Basic Color Usage")
(print "═══════════════════════════════════════")
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1. Working with Web Colors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "--- Web Colors ---")
(print "")

; Get a color by name
(defq forest_green (color-name-to-argb :forestgreen))
(print "Forest green ARGB: " forest_green)
(print "Forest green hex:  " (argb-to-hex forest_green))

; Get color components
(bind '(r g b) (rgb-unpack forest_green))
(print "RGB components: R=" r " G=" g " B=" b)

; List some available colors
(print "")
(print "First 10 web colors available:")
(each (lambda (name)
	(print "  " (rest (str name)) ": " (argb-to-hex (color-name-to-argb name))))
	(slice (color-get-names) 0 10))

(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2. Converting Between Color Spaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "--- Color Space Conversion ---")
(print "")

; RGB to HSV
(defq red_rgb (list 255 0 0))
(bind '(r g b) red_rgb)
(bind '(h s v) (rgb-to-hsv r g b))
(print "Red RGB(" r "," g "," b ") → HSV(" h "°," s "%," v "%)")

; HSV to RGB
(defq cyan_hsv (list 180 100 100))
(bind '(h s v) cyan_hsv)
(bind '(r g b) (hsv-to-rgb h s v))
(print "Cyan HSV(" h "°," s "%," v "%) → RGB(" r "," g "," b ")")

(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 3. Hex Color Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "--- Hex Color Parsing ---")
(print "")

; Parse different hex formats
(defq colors (list "#FF5733" "#F00" "00FF00"))

(each (lambda (hex_str)
	(defq color (hex-to-argb hex_str))
	(if color
		(progn
			(bind '(r g b) (rgb-unpack color))
			(print "'" hex_str "' → RGB(" r "," g "," b ") → "
				   (argb-to-hex color)))
		(print "'" hex_str "' → INVALID")))
	colors)

(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 4. Color Manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "--- Color Manipulation ---")
(print "")

; Start with a base color
(defq base_color (color-name-to-argb :royalblue))
(print "Base color (Royal Blue): " (argb-to-hex base_color))

; Create variations
(defq lighter (color-lighten base_color 20))
(print "  Lightened (+20):  " (argb-to-hex lighter))

(defq darker (color-darken base_color 20))
(print "  Darkened (-20):   " (argb-to-hex darker))

(defq more_sat (color-saturate base_color 20))
(print "  Saturated (+20):  " (argb-to-hex more_sat))

(defq less_sat (color-desaturate base_color 20))
(print "  Desaturated (-20):" (argb-to-hex less_sat))

(defq rotated (color-rotate-hue base_color 120))
(print "  Hue rotated (120°):" (argb-to-hex rotated))

(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 5. Color Blending
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "--- Color Blending ---")
(print "")

(defq red (color-name-to-argb :red))
(defq blue (color-name-to-argb :blue))

(print "Blending red + blue:")
(print "  Red:  " (argb-to-hex red))
(print "  Blue: " (argb-to-hex blue))

(dotimes (i 5)
	(defq ratio (/ i 4.0))
	(defq blended (color-blend red blue ratio))
	(print "  " (n2i (* ratio 100)) "% blue: " (argb-to-hex blended)))

(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 6. Finding Color Names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "--- Color Name Lookup ---")
(print "")

; Find names for some colors
(defq test_colors (list
	0xffff0000  ; Pure red
	0xff00ff00  ; Pure green
	0xff0000ff  ; Pure blue
	0xff808080  ; Gray
	0xffffa500  ; Orange
	0xff7f7f7f)) ; Unknown gray

(each (lambda (color)
	(defq hex (argb-to-hex color))
	(defq name (color-find-name color 30))
	(if name
		(print hex " is close to '" (rest (str name)) "'")
		(print hex " has no close match")))
	test_colors)

(print "")
(print "═══════════════════════════════════════")
(print "  Example complete!")
(print "═══════════════════════════════════════")
