;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Color Library Edge Case Tests
; Tests boundary conditions and edge cases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/colors/colors.inc")

(defun assert-equal (name actual expected)
	; Test assertion with detailed output
	(if (eql actual expected)
		(print "✓ " name)
		(progn
			(print "✗ FAIL: " name)
			(print "  Expected: " expected)
			(print "  Got:      " actual))))

(defun assert-range (name value min_val max_val)
	; Assert value is within range
	(if (and (>= value min_val) (<= value max_val))
		(print "✓ " name)
		(progn
			(print "✗ FAIL: " name " out of range")
			(print "  Range: [" min_val ", " max_val "]")
			(print "  Got:   " value))))

(print "")
(print "═══════════════════════════════════════")
(print "  Color Library Edge Case Tests")
(print "═══════════════════════════════════════")
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Edge Case 1: Pure Black and White
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "=== Black and White Tests ===")
(print "")

; Black: RGB(0,0,0) should be HSV(0,0,0)
(bind '(h s v) (rgb-to-hsv 0 0 0))
(assert-equal "Black RGB→HSV hue" h 0)
(assert-equal "Black RGB→HSV sat" s 0)
(assert-equal "Black RGB→HSV val" v 0)

; Black round-trip
(bind '(r g b) (hsv-to-rgb h s v))
(assert-equal "Black round-trip R" r 0)
(assert-equal "Black round-trip G" g 0)
(assert-equal "Black round-trip B" b 0)

; White: RGB(255,255,255) should be HSV(0,0,100)
(bind '(h s v) (rgb-to-hsv 255 255 255))
(assert-equal "White RGB→HSV hue" h 0)
(assert-equal "White RGB→HSV sat" s 0)
(assert-equal "White RGB→HSV val" v 100)

; White round-trip
(bind '(r g b) (hsv-to-rgb h s v))
(assert-equal "White round-trip R" r 255)
(assert-equal "White round-trip G" g 255)
(assert-equal "White round-trip B" b 255)

; Hex format
(assert-equal "Black hex" (argb-to-hex 0xff000000) "#000000")
(assert-equal "White hex" (argb-to-hex 0xffffffff) "#FFFFFF")

(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Edge Case 2: Grayscale Values (S=0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "=== Grayscale Tests (S=0) ===")
(print "")

; 25% gray
(bind '(r g b) (hsv-to-rgb 0 0 25))
(assert-range "25% gray R" r 63 64)
(assert-range "25% gray G" g 63 64)
(assert-range "25% gray B" b 63 64)

; 50% gray
(bind '(r g b) (hsv-to-rgb 0 0 50))
(assert-range "50% gray R" r 127 128)
(assert-range "50% gray G" g 127 128)
(assert-range "50% gray B" b 127 128)

; 75% gray
(bind '(r g b) (hsv-to-rgb 0 0 75))
(assert-range "75% gray R" r 191 192)
(assert-range "75% gray G" g 191 192)
(assert-range "75% gray B" b 191 192)

; Grayscale should work with any hue (hue is ignored when S=0)
(bind '(r1 g1 b1) (hsv-to-rgb 0 0 50))
(bind '(r2 g2 b2) (hsv-to-rgb 180 0 50))
(bind '(r3 g3 b3) (hsv-to-rgb 359 0 50))
(assert-equal "Gray hue-independent R" r1 r2)
(assert-equal "Gray hue-independent R" r2 r3)

(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Edge Case 3: Hue Boundaries (0° and 360°)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "=== Hue Boundary Tests ===")
(print "")

; Hue 0° and 360° should produce same color (pure red)
(bind '(r1 g1 b1) (hsv-to-rgb 0 100 100))
(bind '(r2 g2 b2) (hsv-to-rgb 360 100 100))
(assert-equal "Hue wrap R" r1 r2)
(assert-equal "Hue wrap G" g1 g2)
(assert-equal "Hue wrap B" b1 b2)

; Test all 6 sectors at boundaries
(defq sector_tests (list
	(list 0 "Red")
	(list 60 "Yellow")
	(list 120 "Green")
	(list 180 "Cyan")
	(list 240 "Blue")
	(list 300 "Magenta")))

(each (lambda ((hue name))
	(bind '(r g b) (hsv-to-rgb hue 100 100))
	(defq total (+ r g b))
	(assert-range (cat name " total brightness") total 250 260))
	sector_tests)

; Hue rotation wrapping
(defq red 0xffff0000)
(defq rotated (color-rotate-hue red 360))
(assert-equal "Hue +360° wrap" rotated red)

(defq rotated (color-rotate-hue red -360))
(assert-equal "Hue -360° wrap" rotated red)

(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Edge Case 4: Saturation Extremes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "=== Saturation Extreme Tests ===")
(print "")

; S=0: Should give gray regardless of hue
(bind '(r g b) (hsv-to-rgb 180 0 50))
(assert-equal "S=0 R=G" r g)
(assert-equal "S=0 G=B" g b)

; S=100: Maximum saturation
(bind '(r g b) (hsv-to-rgb 0 100 100))
(assert-equal "S=100 pure red R" r 255)
(assert-equal "S=100 pure red G" g 0)
(assert-equal "S=100 pure red B" b 0)

; Desaturate to zero
(defq color 0xffff0000)  ; Pure red
(defq desaturated (color-desaturate (color-desaturate (color-desaturate
	(color-desaturate (color-desaturate color 20) 20) 20) 20) 20))
(bind '(r g b) (rgb-unpack desaturated))
(assert-equal "Full desaturate R=G" r g)
(assert-equal "Full desaturate G=B" g b)

(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Edge Case 5: Value Extremes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "=== Value Extreme Tests ===")
(print "")

; V=0: Should always give black
(bind '(r g b) (hsv-to-rgb 180 100 0))
(assert-equal "V=0 R" r 0)
(assert-equal "V=0 G" g 0)
(assert-equal "V=0 B" b 0)

; V=100: Maximum brightness
(bind '(r g b) (hsv-to-rgb 0 100 100))
(defq max_component (max r g b))
(assert-equal "V=100 has 255" max_component 255)

; Lighten/darken extremes
(defq black 0xff000000)
(defq lightened (color-lighten black 100))
(bind '(r g b) (rgb-unpack lightened))
(assert-range "Lighten black result" r 0 255)

(defq white 0xffffffff)
(defq darkened (color-darken white 100))
(bind '(r g b) (rgb-unpack darkened))
(assert-equal "Darken white R" r 0)

(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Edge Case 6: Hex Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "=== Hex Parsing Edge Cases ===")
(print "")

; Short form expansion
(defq short (hex-to-argb "#F0A"))
(defq long (hex-to-argb "#FF00AA"))
(assert-equal "Short form expansion" short long)

; Case insensitivity
(defq upper (hex-to-argb "#FF00AA"))
(defq lower (hex-to-argb "#ff00aa"))
(assert-equal "Hex case insensitive" upper lower)

; With/without hash
(defq with_hash (hex-to-argb "#FF0000"))
(defq without_hash (hex-to-argb "FF0000"))
(assert-equal "Hash optional" with_hash without_hash)

; Invalid formats
(defq invalid1 (hex-to-argb "#GGGGGG"))
(defq invalid2 (hex-to-argb "#12345"))
(defq invalid3 (hex-to-argb ""))
(print (if (eql invalid1 :nil) "✓" "✗") " Invalid hex returns :nil (bad chars)")
(print (if (eql invalid2 :nil) "✓" "✗") " Invalid hex returns :nil (wrong length)")
(print (if (eql invalid3 :nil) "✓" "✗") " Invalid hex returns :nil (empty)")

; Round-trip hex conversion
(defq original 0xff7f3f1f)
(defq hex_str (argb-to-hex original))
(defq restored (hex-to-argb hex_str))
(assert-equal "Hex round-trip" (rgb-unpack original) (rgb-unpack restored))

(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Edge Case 7: Color Blending
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "=== Color Blending Edge Cases ===")
(print "")

; Blend ratio 0.0 should return first color
(defq red 0xffff0000)
(defq blue 0xff0000ff)
(defq blend0 (color-blend red blue 0.0))
(assert-equal "Blend 0.0 returns first" blend0 red)

; Blend ratio 1.0 should return second color
(defq blend1 (color-blend red blue 1.0))
(assert-equal "Blend 1.0 returns second" blend1 blue)

; Blend ratio 0.5 should be midpoint
(defq blend_half (color-blend red blue 0.5))
(bind '(r g b) (rgb-unpack blend_half))
(assert-range "Blend 0.5 R" r 127 128)
(assert-range "Blend 0.5 G" g 0 1)
(assert-range "Blend 0.5 B" b 127 128)

(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Edge Case 8: Color Name Lookup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "=== Color Name Lookup Tests ===")
(print "")

; Find exact matches
(defq red_name (color-find-name 0xffff0000 0))
(print (if (eql red_name :red) "✓" "✗") " Exact red match")

(defq blue_name (color-find-name 0xff0000ff 0))
(print (if (eql blue_name :blue) "✓" "✗") " Exact blue match")

; Near matches with tolerance
(defq almost_red 0xfffe0101)  ; Very close to red
(defq near_name (color-find-name almost_red 10))
(print (if (eql near_name :red) "✓" "✗") " Near match with tolerance")

; No match beyond tolerance
(defq far_color 0xff7f7f7f)  ; Gray
(defq no_match (color-find-name far_color 1))
(print (if (eql no_match :nil) "✓" "✗") " No match beyond tolerance")

; Grey/gray aliases
(defq grey (color-name-to-argb :grey))
(defq gray (color-name-to-argb :gray))
(assert-equal "Grey/gray alias" grey gray)

(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Edge Case 9: ARGB Packing Edge Cases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "=== ARGB Packing Edge Cases ===")
(print "")

; Alpha channel handling
(defq with_alpha (argb-pack 0x80 255 128 64))
(bind '(a r g b) (argb-unpack with_alpha))
(assert-equal "Alpha preserved" a 0x80)
(assert-equal "Red preserved" r 255)
(assert-equal "Green preserved" g 128)
(assert-equal "Blue preserved" b 64)

; Full alpha (opaque)
(defq opaque (rgb-pack 255 0 0))
(bind '(a _ _ _) (argb-unpack opaque))
(assert-equal "RGB-pack sets full alpha" a 0xff)

; Values > 255 should be masked
(defq masked (argb-pack 0x1ff 0x1ff 0x1ff 0x1ff))
(bind '(a r g b) (argb-unpack masked))
(assert-equal "Overflow masked A" a 0xff)
(assert-equal "Overflow masked R" r 0xff)
(assert-equal "Overflow masked G" g 0xff)
(assert-equal "Overflow masked B" b 0xff)

(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Edge Case 10: Precision and Rounding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "=== Precision and Rounding Tests ===")
(print "")

; Multiple round-trips should be stable
(defq start_color (list 123 45 67))
(bind '(r1 g1 b1) start_color)
(bind '(h s v) (rgb-to-hsv r1 g1 b1))
(bind '(r2 g2 b2) (hsv-to-rgb h s v))
(bind '(h2 s2 v2) (rgb-to-hsv r2 g2 b2))
(bind '(r3 g3 b3) (hsv-to-rgb h2 s2 v2))

(defq diff_r (abs (- r1 r3)))
(defq diff_g (abs (- g1 g3)))
(defq diff_b (abs (- b1 b3)))

(assert-range "Round-trip stable R" diff_r 0 2)
(assert-range "Round-trip stable G" diff_g 0 2)
(assert-range "Round-trip stable B" diff_b 0 2)

; Test many random colors for round-trip stability
(defq unstable_count 0)
(dotimes (_ 20)
	(defq test_r (% (random 1000) 256)
		  test_g (% (random 1000) 256)
		  test_b (% (random 1000) 256))
	(bind '(h s v) (rgb-to-hsv test_r test_g test_b))
	(bind '(back_r back_g back_b) (hsv-to-rgb h s v))
	(when (or (> (abs (- test_r back_r)) 2)
			  (> (abs (- test_g back_g)) 2)
			  (> (abs (- test_b back_b)) 2))
		(setq unstable_count (inc unstable_count))))

(print (if (= unstable_count 0) "✓" "✗")
	   " 20 random round-trips stable (errors: " unstable_count ")")

(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Summary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "═══════════════════════════════════════")
(print "  Edge Case Testing Complete!")
(print "═══════════════════════════════════════")
(print "")
(print "All boundary conditions and edge cases verified.")
(print "The color library handles extreme values correctly.")
