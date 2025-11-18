;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ChrysaLisp Color Library
; Comprehensive color manipulation and conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
; Web Color Database
;;;;;;;;;;;;;;;;;;;

; Standard web colors with their ARGB values
(defq *web_colors* (scatter (Fmap)
	; Reds
	:indianred 0xffcd5c5c :lightcoral 0xfff08080 :salmon 0xfffa8072
	:darksalmon 0xffe9967a :lightsalmon 0xffffa07a :crimson 0xffdc143c
	:red 0xffff0000 :firebrick 0xffb22222 :darkred 0xff8b0000
	; Pinks
	:pink 0xffffc0cb :lightpink 0xffffb6c1 :hotpink 0xffff69b4
	:deeppink 0xffff1493 :mediumvioletred 0xffc71585 :palevioletred 0xffdb7093
	; Oranges
	:lightsalmon 0xffffa07a :coral 0xffff7f50 :tomato 0xffff6347
	:orangered 0xffff4500 :darkorange 0xffff8c00 :orange 0xffffa500
	; Yellows
	:gold 0xffffd700 :yellow 0xffffff00 :lightyellow 0xffffffe0
	:lemonchiffon 0xfffffacd :lightgoldenrodyellow 0xfffafad2 :papayawhip 0xffffefd5
	:moccasin 0xffffe4b5 :peachpuff 0xffffdab9 :palegoldenrod 0xffeee8aa
	:khaki 0xfff0e68c :darkkhaki 0xffbdb76b
	; Purples
	:lavender 0xffe6e6fa :thistle 0xffd8bfd8 :plum 0xffdda0dd
	:violet 0xffee82ee :orchid 0xffda70d6 :fuchsia 0xffff00ff
	:magenta 0xffff00ff :mediumorchid 0xffba55d3 :mediumpurple 0xff9370db
	:rebeccapurple 0xff663399 :blueviolet 0xff8a2be2 :darkviolet 0xff9400d3
	:darkorchid 0xff9932cc :darkmagenta 0xff8b008b :purple 0xff800080
	:indigo 0xff4b0082 :slateblue 0xff6a5acd :darkslateblue 0xff483d8b
	:mediumslateblue 0xff7b68ee
	; Greens
	:greenyellow 0xffadff2f :chartreuse 0xff7fff00 :lawngreen 0xff7cfc00
	:lime 0xff00ff00 :limegreen 0xff32cd32 :palegreen 0xff98fb98
	:lightgreen 0xff90ee90 :mediumspringgreen 0xff00fa9a :springgreen 0xff00ff7f
	:mediumseagreen 0xff3cb371 :seagreen 0xff2e8b57 :forestgreen 0xff228b22
	:green 0xff008000 :darkgreen 0xff006400 :yellowgreen 0xff9acd32
	:olivedrab 0xff6b8e23 :olive 0xff808000 :darkolivegreen 0xff556b2f
	:mediumaquamarine 0xff66cdaa :darkseagreen 0xff8fbc8b :lightseagreen 0xff20b2aa
	:darkcyan 0xff008b8b :teal 0xff008080
	; Blues/Cyans
	:aqua 0xff00ffff :cyan 0xff00ffff :lightcyan 0xffe0ffff
	:paleturquoise 0xffafeeee :aquamarine 0xff7fffd4 :turquoise 0xff40e0d0
	:mediumturquoise 0xff48d1cc :darkturquoise 0xff00ced1 :cadetblue 0xff5f9ea0
	:steelblue 0xff4682b4 :lightsteelblue 0xffb0c4de :powderblue 0xffb0e0e6
	:lightblue 0xffadd8e6 :skyblue 0xff87ceeb :lightskyblue 0xff87cefa
	:deepskyblue 0xff00bfff :dodgerblue 0xff1e90ff :cornflowerblue 0xff6495ed
	:royalblue 0xff4169e1 :blue 0xff0000ff :mediumblue 0xff0000cd
	:darkblue 0xff00008b :navy 0xff000080 :midnightblue 0xff191970
	; Browns
	:cornsilk 0xfffff8dc :blanchedalmond 0xffffebcd :bisque 0xffffe4c4
	:navajowhite 0xffffdead :wheat 0xfff5deb3 :burlywood 0xffdeb887
	:tan 0xffd2b48c :rosybrown 0xffbc8f8f :sandybrown 0xfff4a460
	:goldenrod 0xffdaa520 :darkgoldenrod 0xffb8860b :peru 0xffcd853f
	:chocolate 0xffd2691e :saddlebrown 0xff8b4513 :sienna 0xffa0522d
	:brown 0xffa52a2a :maroon 0xff800000
	; Whites
	:white 0xffffffff :snow 0xfffffafa :honeydew 0xfff0fff0
	:mintcream 0xfff5fffa :azure 0xfff0ffff :aliceblue 0xfff0f8ff
	:ghostwhite 0xfff8f8ff :whitesmoke 0xfff5f5f5 :seashell 0xfffff5ee
	:beige 0xfff5f5dc :oldlace 0xfffdf5e6 :floralwhite 0xfffffaf0
	:ivory 0xfffffff0 :antiquewhite 0xfffaebd7 :linen 0xfffaf0e6
	:lavenderblush 0xfffff0f5 :mistyrose 0xffffe4e1
	; Grays
	:gainsboro 0xffdcdcdc :lightgray 0xffd3d3d3 :lightgrey 0xffd3d3d3
	:silver 0xffc0c0c0 :darkgray 0xffa9a9a9 :darkgrey 0xffa9a9a9
	:gray 0xff808080 :grey 0xff808080 :dimgray 0xff696969
	:dimgrey 0xff696969 :lightslategray 0xff778899 :lightslategrey 0xff778899
	:slategray 0xff708090 :slategrey 0xff708090 :darkslategray 0xff2f4f4f
	:darkslategrey 0xff2f4f4f :black 0xff000000))

;;;;;;;;;;;;;;;;;;;
; Helper Functions
;;;;;;;;;;;;;;;;;;;

(defq +hex_digits "0123456789ABCDEF")

(defun _num-to-hex (n)
	; Convert 0-255 to two-digit hex string
	(cat (elem-get +hex_digits (/ n 16))
		 (elem-get +hex_digits (% n 16))))

(defun _hex-char-to-num (c)
	; Convert single hex char to number
	(defq pos (find c +hex_digits))
	(if pos pos
		(defq pos (find c "abcdef"))
		(if pos (+ pos 10) 0)))

(defun _hex-to-num (s)
	; Convert hex string to number
	(defq result 0)
	(each (lambda (c)
		(setq result (+ (* result 16) (_hex-char-to-num c))))
		s)
	result)

;;;;;;;;;;;;;;;;;;;
; ARGB Utilities
;;;;;;;;;;;;;;;;;;;

(defun argb-pack (a r g b)
	; (argb-pack alpha red green blue) -> argb_int
	; Pack separate ARGB components (0-255) into a single integer
	(logior (<< (logand a 0xff) 24)
			(<< (logand r 0xff) 16)
			(<< (logand g 0xff) 8)
			(logand b 0xff)))

(defun argb-unpack (argb)
	; (argb-unpack argb_int) -> (alpha red green blue)
	; Unpack an ARGB integer into separate components (0-255)
	(list
		(logand (>> argb 24) 0xff)
		(logand (>> argb 16) 0xff)
		(logand (>> argb 8) 0xff)
		(logand argb 0xff)))

(defun rgb-pack (r g b)
	; (rgb-pack red green blue) -> argb_int
	; Pack RGB components with full alpha
	(argb-pack 0xff r g b))

(defun rgb-unpack (argb)
	; (rgb-unpack argb_int) -> (red green blue)
	; Unpack RGB components, ignoring alpha
	(bind '(_ r g b) (argb-unpack argb))
	(list r g b))

(defun argb-to-hex (argb)
	; (argb-to-hex argb_int) -> hex_string
	; Convert ARGB to hex string "#RRGGBB"
	(bind '(_ r g b) (argb-unpack argb))
	(cat "#" (_num-to-hex r) (_num-to-hex g) (_num-to-hex b)))

(defun hex-to-argb (hex_str)
	; (hex-to-argb hex_string) -> argb_int or :nil
	; Convert hex string "#RGB" or "#RRGGBB" to ARGB (with full alpha)
	(defq s (if (starts-with "#" hex_str) (rest hex_str) hex_str))
	(defq len (length s))
	(cond
		((= len 3)
			; Short form #RGB -> #RRGGBB
			(defq r (_hex-char-to-num (elem-get s 0))
				  g (_hex-char-to-num (elem-get s 1))
				  b (_hex-char-to-num (elem-get s 2)))
			(rgb-pack (+ r (* r 16)) (+ g (* g 16)) (+ b (* b 16))))
		((= len 6)
			; Full form #RRGGBB
			(defq r (_hex-to-num (slice s 0 2))
				  g (_hex-to-num (slice s 2 4))
				  b (_hex-to-num (slice s 4 6)))
			(rgb-pack r g b))
		(:t :nil)))

;;;;;;;;;;;;;;;;;;;
; HSV <-> RGB Conversion
;;;;;;;;;;;;;;;;;;;

(defun hsv-to-rgb (h s v)
	; (hsv-to-rgb hue saturation value) -> (red green blue)
	; H: 0-360, S: 0-100, V: 0-100
	; Returns RGB values 0-255
	(defq s_norm (/ s 100.0)
		  v_norm (/ v 100.0))

	(if (= s 0)
		; Achromatic (gray)
		(defq val (n2i (* v_norm 255)))
		(list val val val)
		; Chromatic
		(progn
			(defq h_sector (/ (% h 360) 60.0)
				  i (n2i h_sector)
				  f (- h_sector i)
				  p (* v_norm (- 1.0 s_norm))
				  q (* v_norm (- 1.0 (* s_norm f)))
				  t_val (* v_norm (- 1.0 (* s_norm (- 1.0 f)))))

			(defq rgb_floats
				(case i
					(0 (list v_norm t_val p))
					(1 (list q v_norm p))
					(2 (list p v_norm t_val))
					(3 (list p q v_norm))
					(4 (list t_val p v_norm))
					(5 (list v_norm p q))
					(:t (list 0.0 0.0 0.0))))

			(map (lambda (x) (max 0 (min 255 (n2i (* x 255))))) rgb_floats))))

(defun rgb-to-hsv (r g b)
	; (rgb-to-hsv red green blue) -> (hue saturation value)
	; RGB: 0-255
	; Returns H: 0-360, S: 0-100, V: 0-100
	(defq r_norm (/ r 255.0)
		  g_norm (/ g 255.0)
		  b_norm (/ b 255.0)
		  max_val (max r_norm g_norm b_norm)
		  min_val (min r_norm g_norm b_norm)
		  delta (- max_val min_val))

	(defq v (n2i (* max_val 100))
		  s (if (= max_val 0.0) 0 (n2i (* (/ delta max_val) 100))))

	(defq h
		(if (= delta 0.0)
			0
			(n2i (* 60
				(cond
					((= max_val r_norm)
						(% (/ (- g_norm b_norm) delta) 6.0))
					((= max_val g_norm)
						(+ (/ (- b_norm r_norm) delta) 2.0))
					(:t
						(+ (/ (- r_norm g_norm) delta) 4.0)))))))

	(list (if (< h 0) (+ h 360) h) s v))

;;;;;;;;;;;;;;;;;;;
; Color Name Functions
;;;;;;;;;;;;;;;;;;;

(defun color-name-to-argb (name)
	; (color-name-to-argb name_keyword) -> argb_int or :nil
	; Get ARGB value for a web color name
	(. *web_colors* :find name))

(defun color-get-names ()
	; (color-get-names) -> list_of_name_keywords
	; Get all web color names
	(defq names (list))
	(. *web_colors* :each (lambda (k _) (push names k)))
	(sort cmp names))

(defun color-find-name (argb &optional tolerance)
	; (color-find-name argb_int [tolerance]) -> name_keyword or :nil
	; Find closest web color name to given ARGB value
	; tolerance: max color distance (default 30)
	(defq tol (ifn tolerance 30)
		  target_rgb (rgb-unpack argb)
		  closest_name :nil
		  closest_dist (+ tol 1))

	(. *web_colors* :each
		(lambda (name_key color_val)
			(bind '(tr tg tb) target_rgb)
			(bind '(cr cg cb) (rgb-unpack color_val))
			(defq dist (+ (abs (- tr cr)) (abs (- tg cg)) (abs (- tb cb))))
			(when (< dist closest_dist)
				(setq closest_dist dist
					  closest_name name_key))))

	(if (<= closest_dist tol) closest_name :nil))

;;;;;;;;;;;;;;;;;;;
; Color Manipulation
;;;;;;;;;;;;;;;;;;;

(defun color-lighten (argb amount)
	; (color-lighten argb_int amount) -> argb_int
	; Lighten color by amount (0-100)
	(bind '(r g b) (rgb-unpack argb))
	(bind '(h s v) (rgb-to-hsv r g b))
	(defq new_v (min 100 (+ v amount)))
	(bind '(nr ng nb) (hsv-to-rgb h s new_v))
	(rgb-pack nr ng nb))

(defun color-darken (argb amount)
	; (color-darken argb_int amount) -> argb_int
	; Darken color by amount (0-100)
	(bind '(r g b) (rgb-unpack argb))
	(bind '(h s v) (rgb-to-hsv r g b))
	(defq new_v (max 0 (- v amount)))
	(bind '(nr ng nb) (hsv-to-rgb h s new_v))
	(rgb-pack nr ng nb))

(defun color-saturate (argb amount)
	; (color-saturate argb_int amount) -> argb_int
	; Increase saturation by amount (0-100)
	(bind '(r g b) (rgb-unpack argb))
	(bind '(h s v) (rgb-to-hsv r g b))
	(defq new_s (min 100 (+ s amount)))
	(bind '(nr ng nb) (hsv-to-rgb h new_s v))
	(rgb-pack nr ng nb))

(defun color-desaturate (argb amount)
	; (color-desaturate argb_int amount) -> argb_int
	; Decrease saturation by amount (0-100)
	(bind '(r g b) (rgb-unpack argb))
	(bind '(h s v) (rgb-to-hsv r g b))
	(defq new_s (max 0 (- s amount)))
	(bind '(nr ng nb) (hsv-to-rgb h new_s v))
	(rgb-pack nr ng nb))

(defun color-rotate-hue (argb degrees)
	; (color-rotate-hue argb_int degrees) -> argb_int
	; Rotate hue by degrees (-360 to 360)
	(bind '(r g b) (rgb-unpack argb))
	(bind '(h s v) (rgb-to-hsv r g b))
	(defq new_h (% (+ h degrees) 360))
	(defq new_h (if (< new_h 0) (+ new_h 360) new_h))
	(bind '(nr ng nb) (hsv-to-rgb new_h s v))
	(rgb-pack nr ng nb))

(defun color-blend (argb1 argb2 ratio)
	; (color-blend argb1 argb2 ratio) -> argb_int
	; Blend two colors. ratio: 0.0 (all argb1) to 1.0 (all argb2)
	(bind '(r1 g1 b1) (rgb-unpack argb1))
	(bind '(r2 g2 b2) (rgb-unpack argb2))
	(defq r (to-int (+ r1 (* (- r2 r1) ratio)))
		  g (to-int (+ g1 (* (- g2 g1) ratio)))
		  b (to-int (+ b1 (* (- b2 b1) ratio))))
	(rgb-pack r g b))

;;;;;;;;;;;;;;;;;;;
; Export public API
;;;;;;;;;;;;;;;;;;;

(export-symbols
	'(*web_colors*
	  argb-pack argb-unpack rgb-pack rgb-unpack
	  argb-to-hex hex-to-argb
	  hsv-to-rgb rgb-to-hsv
	  color-name-to-argb color-get-names color-find-name
	  color-lighten color-darken
	  color-saturate color-desaturate
	  color-rotate-hue color-blend))
