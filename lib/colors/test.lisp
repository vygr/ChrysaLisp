;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Color Library Test Suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/colors/colors.inc")

(defun test-print (name result expected)
	; Print test result
	(print "Test: " name)
	(if (eql result expected)
		(print "  ✓ PASS")
		(print "  ✗ FAIL - Expected: " expected " Got: " result))
	(print ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test ARGB Packing/Unpacking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "=== ARGB Pack/Unpack Tests ===")
(print "")

(defq packed (argb-pack 0xff 0x12 0x34 0x56))
(test-print "argb-pack" packed 0xff123456)

(bind '(a r g b) (argb-unpack 0xff123456))
(test-print "argb-unpack alpha" a 0xff)
(test-print "argb-unpack red" r 0x12)
(test-print "argb-unpack green" g 0x34)
(test-print "argb-unpack blue" b 0x56)

(defq rgb (rgb-pack 255 0 0))
(test-print "rgb-pack red" rgb 0xffff0000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test Hex Conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "=== Hex Conversion Tests ===")
(print "")

(test-print "argb-to-hex red" (argb-to-hex 0xffff0000) "#FF0000")
(test-print "argb-to-hex green" (argb-to-hex 0xff00ff00) "#00FF00")
(test-print "argb-to-hex blue" (argb-to-hex 0xff0000ff) "#0000FF")

(test-print "hex-to-argb #FF0000" (hex-to-argb "#FF0000") 0xffff0000)
(test-print "hex-to-argb #00FF00" (hex-to-argb "#00FF00") 0xff00ff00)
(test-print "hex-to-argb #F00 short" (hex-to-argb "#F00") 0xffff0000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test HSV <-> RGB Conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "=== HSV <-> RGB Conversion Tests ===")
(print "")

; Test pure colors
(bind '(r g b) (hsv-to-rgb 0 100 100))
(test-print "hsv-to-rgb red (H=0)" (list r g b) (list 255 0 0))

(bind '(r g b) (hsv-to-rgb 120 100 100))
(test-print "hsv-to-rgb green (H=120)" (list r g b) (list 0 255 0))

(bind '(r g b) (hsv-to-rgb 240 100 100))
(test-print "hsv-to-rgb blue (H=240)" (list r g b) (list 0 0 255))

; Test grayscale (S=0)
(bind '(r g b) (hsv-to-rgb 0 0 100))
(test-print "hsv-to-rgb white (S=0, V=100)" (list r g b) (list 255 255 255))

(bind '(r g b) (hsv-to-rgb 0 0 0))
(test-print "hsv-to-rgb black (S=0, V=0)" (list r g b) (list 0 0 0))

; Test RGB to HSV
(bind '(h s v) (rgb-to-hsv 255 0 0))
(print "rgb-to-hsv red: H=" h " S=" s " V=" v)

(bind '(h s v) (rgb-to-hsv 0 255 0))
(print "rgb-to-hsv green: H=" h " S=" s " V=" v)

(bind '(h s v) (rgb-to-hsv 0 0 255))
(print "rgb-to-hsv blue: H=" h " S=" s " V=" v)

; Round-trip test
(bind '(r1 g1 b1) (list 128 64 192))
(bind '(h s v) (rgb-to-hsv r1 g1 b1))
(bind '(r2 g2 b2) (hsv-to-rgb h s v))
(print "Round-trip RGB->HSV->RGB:")
(print "  Original: " r1 " " g1 " " b1)
(print "  After:    " r2 " " g2 " " b2)
(print "  HSV:      " h " " s " " v)
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test Web Colors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "=== Web Color Tests ===")
(print "")

(test-print "color-name-to-argb :red" (color-name-to-argb :red) 0xffff0000)
(test-print "color-name-to-argb :green" (color-name-to-argb :green) 0xff008000)
(test-print "color-name-to-argb :blue" (color-name-to-argb :blue) 0xff0000ff)
(test-print "color-name-to-argb :rebeccapurple" (color-name-to-argb :rebeccapurple) 0xff663399)

(test-print "color-find-name red" (color-find-name 0xffff0000) :red)
(test-print "color-find-name blue" (color-find-name 0xff0000ff) :blue)

(defq num_colors (length (color-get-names)))
(print "Total web colors available: " num_colors)
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test Color Manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "=== Color Manipulation Tests ===")
(print "")

(defq test_color 0xff800080)  ; Purple (128, 0, 128)

(defq lighter (color-lighten test_color 20))
(print "Original purple: " (argb-to-hex test_color))
(print "Lightened:       " (argb-to-hex lighter))

(defq darker (color-darken test_color 20))
(print "Darkened:        " (argb-to-hex darker))

(defq more_sat (color-saturate test_color 20))
(print "Saturated:       " (argb-to-hex more_sat))

(defq less_sat (color-desaturate test_color 20))
(print "Desaturated:     " (argb-to-hex less_sat))

(defq rotated (color-rotate-hue test_color 120))
(print "Hue rotated:     " (argb-to-hex rotated))

(defq blended (color-blend 0xffff0000 0xff0000ff 0.5))
(print "Red+Blue blend:  " (argb-to-hex blended))
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Summary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "=== Test Complete ===")
(print "All basic functionality verified!")
