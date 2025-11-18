;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Example 2: Creating a Color Theme
; Demonstrates building complementary color schemes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/colors/colors.inc")

(print "")
(print "═══════════════════════════════════════")
(print "  Example 2: Color Theme Generator")
(print "═══════════════════════════════════════")
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Theme Generator Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-monochromatic-theme (base_color)
	; Generate 5 shades from a base color
	(defq theme (list base_color))
	(push theme (color-lighten base_color 20))
	(push theme (color-lighten base_color 40))
	(push theme (color-darken base_color 20))
	(push theme (color-darken base_color 40))
	theme)

(defun generate-complementary-theme (base_color)
	; Generate complementary colors (opposite on color wheel)
	(list base_color
		  (color-rotate-hue base_color 180)))

(defun generate-triadic-theme (base_color)
	; Generate triadic colors (120° apart)
	(list base_color
		  (color-rotate-hue base_color 120)
		  (color-rotate-hue base_color 240)))

(defun generate-analogous-theme (base_color)
	; Generate analogous colors (adjacent on color wheel)
	(list (color-rotate-hue base_color -30)
		  base_color
		  (color-rotate-hue base_color 30)))

(defun generate-split-complementary-theme (base_color)
	; Generate split-complementary (base + 2 colors adjacent to complement)
	(list base_color
		  (color-rotate-hue base_color 150)
		  (color-rotate-hue base_color 210)))

(defun print-theme (name colors)
	; Display a theme
	(print "")
	(print "--- " name " ---")
	(each (lambda (color)
		(bind '(r g b) (rgb-unpack color))
		(bind '(h s v) (rgb-to-hsv r g b))
		(print "  " (argb-to-hex color)
			   "  HSV(" h "°," s "%," v "%)"))
		colors))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Example 1: Monochromatic Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "MONOCHROMATIC THEME")
(print "Uses variations of a single color")

(defq base (color-name-to-argb :royalblue))
(print "Base color: " (argb-to-hex base))

(defq mono_theme (generate-monochromatic-theme base))
(print-theme "Monochromatic Palette" mono_theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Example 2: Complementary Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "")
(print "")
(print "COMPLEMENTARY THEME")
(print "Uses colors opposite on the color wheel")

(defq comp_theme (generate-complementary-theme base))
(print-theme "Complementary Palette" comp_theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Example 3: Triadic Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "")
(print "")
(print "TRIADIC THEME")
(print "Uses three colors evenly spaced (120°)")

(defq tri_theme (generate-triadic-theme base))
(print-theme "Triadic Palette" tri_theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Example 4: Analogous Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "")
(print "")
(print "ANALOGOUS THEME")
(print "Uses colors adjacent on the color wheel")

(defq ana_theme (generate-analogous-theme base))
(print-theme "Analogous Palette" ana_theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Example 5: Split-Complementary Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "")
(print "")
(print "SPLIT-COMPLEMENTARY THEME")
(print "Uses base + 2 colors adjacent to its complement")

(defq split_theme (generate-split-complementary-theme base))
(print-theme "Split-Complementary Palette" split_theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Create a Full App Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "")
(print "")
(print "═══════════════════════════════════════")
(print "  Complete Application Theme")
(print "═══════════════════════════════════════")

(defun create-app-theme (primary_color)
	; Create a complete theme for an application
	(defq theme (scatter (Fmap)))

	; Primary colors
	(def theme
		:primary primary_color
		:primary_light (color-lighten primary_color 20)
		:primary_dark (color-darken primary_color 20))

	; Accent color (complementary)
	(defq accent (color-rotate-hue primary_color 180))
	(def theme
		:accent accent
		:accent_light (color-lighten accent 20)
		:accent_dark (color-darken accent 20))

	; Neutral colors
	(def theme
		:background 0xffffffff
		:surface 0xfff5f5f5
		:text 0xff212121
		:text_secondary 0xff757575
		:divider 0xffbdbdbd)

	; Success/warning/error
	(def theme
		:success (color-name-to-argb :green)
		:warning (color-name-to-argb :orange)
		:error (color-name-to-argb :red))

	theme)

; Generate theme
(defq app_theme (create-app-theme (color-name-to-argb :indigo)))

; Display theme
(print "")
(print "Primary Colors:")
(print "  Primary:       " (argb-to-hex (. app_theme :find :primary)))
(print "  Primary Light: " (argb-to-hex (. app_theme :find :primary_light)))
(print "  Primary Dark:  " (argb-to-hex (. app_theme :find :primary_dark)))

(print "")
(print "Accent Colors:")
(print "  Accent:        " (argb-to-hex (. app_theme :find :accent)))
(print "  Accent Light:  " (argb-to-hex (. app_theme :find :accent_light)))
(print "  Accent Dark:   " (argb-to-hex (. app_theme :find :accent_dark)))

(print "")
(print "Neutral Colors:")
(print "  Background:    " (argb-to-hex (. app_theme :find :background)))
(print "  Surface:       " (argb-to-hex (. app_theme :find :surface)))
(print "  Text:          " (argb-to-hex (. app_theme :find :text)))
(print "  Text Secondary:" (argb-to-hex (. app_theme :find :text_secondary)))
(print "  Divider:       " (argb-to-hex (. app_theme :find :divider)))

(print "")
(print "Semantic Colors:")
(print "  Success:       " (argb-to-hex (. app_theme :find :success)))
(print "  Warning:       " (argb-to-hex (. app_theme :find :warning)))
(print "  Error:         " (argb-to-hex (. app_theme :find :error)))

(print "")
(print "═══════════════════════════════════════")
(print "  Theme generation complete!")
(print "═══════════════════════════════════════")
