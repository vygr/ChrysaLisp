;;;;;;;;;;;;
; svg loader
;;;;;;;;;;;;

(import "././xml/parse.inc")
(import "././consts/colors.inc")
(import "gui/path/lisp.inc")
(import "gui/canvas/lisp.inc")
(import "././math/matrix.inc")

;module
(env-push)

(defun deg-to-rad (d)
	(/ (* d +fp_pi) 180.0))

;the CSS colors by name, which SVG has
(defq +svg_colors (scatter (Fmap 101)
	"aliceblue" 0xfff0f8ff "antiquewhite" 0xfffaebd7 "aqua" 0xff00ffff
	"aquamarine" 0xff7fffd4 "azure" 0xfff0ffff "beige" 0xfff5f5dc
	"bisque" 0xffffe4c4 "black" 0xff000000 "blanchedalmond" 0xffffebcd
	"blue" 0xff0000ff "blueviolet" 0xff8a2be2 "brown" 0xffa52a2a
	"burlywood" 0xffdeb887 "cadetblue" 0xff5f9ea0 "chartreuse" 0xff7fff00
	"chocolate" 0xffd2691e "coral" 0xffff7f50 "cornflowerblue" 0xff6495ed
	"cornsilk" 0xfffff8dc "crimson" 0xffdc143c "cyan" 0xff00ffff
	"darkblue" 0xff00008b "darkcyan" 0xff008b8b "darkgoldenrod" 0xffb8860b
	"darkgray" 0xffa9a9a9 "darkgreen" 0xff006400 "darkgrey" 0xffa9a9a9
	"darkkhaki" 0xffbdb76b "darkmagenta" 0xff8b008b
	"darkolivegreen" 0xff556b2f "darkorange" 0xffff8c00
	"darkorchid" 0xff9932cc "darkred" 0xff8b0000 "darksalmon" 0xffe9967a
	"darkseagreen" 0xff8fbc8f "darkslateblue" 0xff483d8b
	"darkslategray" 0xff2f4f4f "darkslategrey" 0xff2f4f4f
	"darkturquoise" 0xff00ced1 "darkviolet" 0xff9400d3 "deeppink" 0xffff1493
	"deepskyblue" 0xff00bfff "dimgray" 0xff696969 "dimgrey" 0xff696969
	"dodgerblue" 0xff1e90ff "firebrick" 0xffb22222 "floralwhite" 0xfffffaf0
	"forestgreen" 0xff228b22 "fuchsia" 0xffff00ff "gainsboro" 0xffdcdcdc
	"ghostwhite" 0xfff8f8ff "gold" 0xffffd700 "goldenrod" 0xffdaa520
	"gray" 0xff808080 "green" 0xff008000 "greenyellow" 0xffadff2f
	"grey" 0xff808080 "honeydew" 0xfff0fff0 "hotpink" 0xffff69b4
	"indianred" 0xffcd5c5c "indigo" 0xff4b0082 "ivory" 0xfffffff0
	"khaki" 0xfff0e68c "lavender" 0xffe6e6fa "lavenderblush" 0xfffff0f5
	"lawngreen" 0xff7cfc00 "lemonchiffon" 0xfffffacd "lightblue" 0xffadd8e6
	"lightcoral" 0xfff08080 "lightcyan" 0xffe0ffff
	"lightgoldenrodyellow" 0xfffafad2 "lightgray" 0xffd3d3d3
	"lightgreen" 0xff90ee90 "lightgrey" 0xffd3d3d3 "lightpink" 0xffffb6c1
	"lightsalmon" 0xffffa07a "lightseagreen" 0xff20b2aa
	"lightskyblue" 0xff87cefa "lightslategray" 0xff778899
	"lightslategrey" 0xff778899 "lightsteelblue" 0xffb0c4de
	"lightyellow" 0xffffffe0 "lime" 0xff00ff00 "limegreen" 0xff32cd32
	"linen" 0xfffaf0e6 "magenta" 0xffff00ff "maroon" 0xff800000
	"mediumaquamarine" 0xff66cdaa "mediumblue" 0xff0000cd
	"mediumorchid" 0xffba55d3 "mediumpurple" 0xff9370db
	"mediumseagreen" 0xff3cb371 "mediumslateblue" 0xff7b68ee
	"mediumspringgreen" 0xff00fa9a "mediumturquoise" 0xff48d1cc
	"mediumvioletred" 0xffc71585 "midnightblue" 0xff191970
	"mintcream" 0xfff5fffa "mistyrose" 0xffffe4e1 "moccasin" 0xffffe4b5
	"navajowhite" 0xffffdead "navy" 0xff000080 "oldlace" 0xfffdf5e6
	"olive" 0xff808000 "olivedrab" 0xff6b8e23 "orange" 0xffffa500
	"orangered" 0xffff4500 "orchid" 0xffda70d6 "palegoldenrod" 0xffeee8aa
	"palegreen" 0xff98fb98 "paleturquoise" 0xffafeeee
	"palevioletred" 0xffdb7093 "papayawhip" 0xffffefd5 "peachpuff" 0xffffdab9
	"peru" 0xffcd853f "pink" 0xffffc0cb "plum" 0xffdda0dd
	"powderblue" 0xffb0e0e6 "purple" 0xff800080 "rebeccapurple" 0xff663399
	"red" 0xffff0000 "rosybrown" 0xffbc8f8f "royalblue" 0xff4169e1
	"saddlebrown" 0xff8b4513 "salmon" 0xfffa8072 "sandybrown" 0xfff4a460
	"seagreen" 0xff2e8b57 "seashell" 0xfffff5ee "sienna" 0xffa0522d
	"silver" 0xffc0c0c0 "skyblue" 0xff87ceeb "slateblue" 0xff6a5acd
	"slategray" 0xff708090 "slategrey" 0xff708090 "snow" 0xfffffafa
	"springgreen" 0xff00ff7f "steelblue" 0xff4682b4 "tan" 0xffd2b48c
	"teal" 0xff008080 "thistle" 0xffd8bfd8 "tomato" 0xffff6347
	"turquoise" 0xff40e0d0 "violet" 0xffee82ee "wheat" 0xfff5deb3
	"white" 0xffffffff "whitesmoke" 0xfff5f5f5 "yellow" 0xffffff00
	"yellowgreen" 0xff9acd32))

;white space, as XML has it
(defq +svg_space (char-class " \t\r\n"))

;what a length is in pixels by its unit, and what SVG passes over
(defq +svg_units (scatter (Fmap 11) "" 1.0 "px" 1.0 "pt" 1.33333 "pc" 16.0 "mm" 3.77953
		"cm" 37.79528 "in" 96.0 "em" 16.0)
	+svg_skip ''("defs" "symbol" "clipPath" "mask" "marker" "pattern" "linearGradient"
		"radialGradient" "title" "desc" "metadata" "style" "script" "filter" "font")
	;what holds nothing: what is in one is not drawn
	+svg_shapes ''("circle" "ellipse" "line" "rect" "polygon" "polyline" "path"))

(defun read-num (val)
	; (read-num str) -> num
	;a number as it is written, an exponent too, which (str-to-num) stops at
	(cond
		((and (defq e (or (find "e" val) (find "E" val)))
				(defq x (str-to-num (slice val (inc e) -1))) (/= x 0))
			(defq m (n2d (str-to-num (slice val 0 e))))
			(times (abs x) (setq m (if (> x 0) (* m (const (n2d 10))) (/ m (const (n2d 10))))))
			m)
		((str-to-num val))))

(defun read-length (val)
	; (read-length str) -> :nil | :percent | fixed
	;a length in pixels, of its unit; :percent for a %, :nil for a unit not
	;known
	(defq at (bskip (const (char-class "+-.0123456789eE")) val 0) unit (trim (slice val at -1)))
	(cond
		((eql unit "%") :percent)
		((defq k (. +svg_units :find unit))
			(if (eql unit "") (n2q (read-num val)) (* (n2q (read-num (slice val 0 at))) k)))))

(defun read-opacity (val)
	; (read-opacity str) -> :inherit | fixed
	;0 to 1, or a %; with anything else after it, not known
	(defq at (bskip (const (char-class "+-.0123456789eE")) val 0) unit (trim (slice val at -1) +svg_space))
	(cond
		((eql unit "") (max 0.0 (min 1.0 (n2q (read-num val)))))
		((eql unit "%") (max 0.0 (min 1.0 (/ (n2q (read-num (slice val 0 at))) 100.0))))
		(:inherit)))

(defun read-col (val)
	;#rgb or #rrggbb, of hex digits only
	(and (some! (# (not (find %0 "0123456789abcdefABCDEF"))) (list val) :nil 1 -1) (setq val ""))
	(case (length val)
		(4 (pixmap-to-argb32 (str-to-num (cat "0xf" (rest val))) 12))
		(7 (pixmap-to-argb32 (str-to-num (cat "0x" (rest val))) 24))))

(defun read-rgb (val)
	;rgb() and rgba(), each of 0 to 255 or a %, the alpha 0 to 1
	(bind '(r g b &optional a) (filter! (# (nempty? %0)) (split val (const (char-class "(), \t"))) 1 -1))
	(bind '(r g b) (map (lambda (v)
			(if (ends-with "%" v) (n2i (/ (* (n2q (read-num v)) 255.0) 100.0)) (read-num v)))
		(list r g b)))
	(defq alpha (if (and a (num? (defq o (read-opacity a)))) (n2i (* o 255.0)) 0xff))
	(apply (const +) (map (# (<< (max 0 (min 255 (n2i %0))) %1)) (list alpha r g b) '(24 16 8 0))))

(defun read-plain-color (entry val)
	; (read-plain-color entry str) -> :inherit | argb
	;a color as SVG and CSS write it, 0 for none, :inherit for one not known
	;names and functions of no case, as CSS has them
	(defq low (to-lower val))
	(cond
		((find low '("none" "transparent")) 0)
		((eql low "currentcolor") (. entry :find "color"))
		((starts-with "#" val) (ifn (read-col val) :inherit))
		((or (starts-with "rgb(" low) (starts-with "rgba(" low)) (read-rgb low))
		((. +svg_colors :find low))
		(:inherit)))

(defun read-color (entry val)
	; (read-color entry str) -> :inherit | argb | (:url id fall)
	;a color, (read-plain-color), or a paint server, a gradient, by its id,
	;with the color after it if there is one, to fall back on, (svg-paint)
	(setq val (trim val +svg_space))
	(cond
		((starts-with "url(" val)
			(defq at (find ")" val) id (trim (slice val 4 (ifn at -1)) (const (char-class " '\q")))
				fall (if at (trim (slice val (inc at) -1))))
			(if (starts-with "#" id) (setq id (slice id 1 -1)))
			(list :url id (if (nempty? fall) (read-plain-color entry fall))))
		((read-plain-color entry val))))
(defun svg-fit (w h vw vh par)
	; (svg-fit w h vw vh par) -> (sx sy tx ty)
	;a viewBox of vw by vh into a viewport of w by h as preserveAspectRatio
	;says, by default xMidYMid meet: none stretches it, meet fits all of it
	;in, slice fills the viewport with it, and the align places what is
	;left over
	(defq words (split (ifn par "xMidYMid meet") (const (char-class " \t,")))
		align (ifn (first words) "xMidYMid") sx (/ w vw) sy (/ h vh))
	(cond
		((eql align "none") (list sx sy 0.0 0.0))
		(:t (defq k (if (eql (second words) "slice") (max sx sy) (min sx sy))
				dx (- w (* vw k)) dy (- h (* vh k)))
			;an align is x then Y, each of Min, Mid or Max, xMinYMax
			(defq ax (slice align 1 4) ay (slice align 5 8))
			(list k k
				(cond ((eql ax "Min") 0.0) ((eql ax "Max") dx) ((* dx 0.5)))
				(cond ((eql ay "Min") 0.0) ((eql ay "Max") dy) ((* dy 0.5)))))))

(defun svg-attr (keys vals key)
	; (svg-attr keys vals key) -> :nil | str
	;an attribute, or a property of the style attribute
	(cond
		((defq at (find key keys)) (elem-get vals at))
		((defq at (find "style" keys))
			(some (lambda (p)
					(defq kv (split p ":"))
					(and (= (length kv) 2) (eql (trim (first kv)) key) (trim (second kv))))
				(split (elem-get vals at) ";")))))

(defun svg-geom (keys vals names viewport)
	; (svg-geom keys vals names viewport) -> (fixed | :nil ...)
	;a shape's own geometry, never its parent's, SVG does not inherit it: a
	;size not given or not known none, a place 0, a % of the viewport's
	;width, its height, or of a radius its diagonal over root 2
	(bind '(vw vh) viewport)
	(map (lambda (name)
			(defq size (find name '("width" "height" "r" "rx" "ry")))
			(cond
				((not (defq v (svg-attr keys vals name))) (if size :nil 0.0))
				((eql (defq l (read-length (setq v (trim v +svg_space)))) :percent)
					(* (/ (n2q (read-num v)) 100.0)
						(cond
							((find name '("x" "width" "cx" "rx" "x1" "x2")) vw)
							((find name '("y" "height" "cy" "ry" "y1" "y2")) vh)
							((n2q (sqrt (/ (+ (* (n2d vw) (n2d vw)) (* (n2d vh) (n2d vh))) (const (n2d 2)))))))))
				((num? l) l)
				(size :nil)
				(0.0)))
		names))

(defun svg-alpha (argb opacity)
	; (svg-alpha argb opacity) -> argb
	;the color's alpha by the opacity, a color of full alpha as it was
	(if (/= (>> argb 24) 0xff) (setq opacity (* opacity (/ (n2q (>> argb 24)) 255.0))))
	(+ (<< (n2i (* opacity 0xff.0)) 24) (logand argb 0xffffff)))

(defun svg-coord (val size)
	; (svg-coord str size) -> fixed
	;a coordinate of a gradient, a % of the size, 1 of a box, the
	;viewport's of the user's space
	(cond
		((ends-with "%" val) (* (/ (n2q (read-num val)) 100.0) size))
		((num? (defq l (read-length val))) l)
		(0.0)))

(defun read-transform (entry val)
	(defq stack (reverse (split val ")")) ma (. entry :find "transform"))
	(while (defq transform (pop stack))
		(defq args (split transform (const (char-class "(, \t\n")))
			key (first args)
			;spaces after the last ) are a transform of no args, the (rest)
			;check off: of which is (), where a (map!) from 1 is an error
			vals (map (# (n2q (str-to-num %0))) (rest args))
			mb (case key
				("matrix"
					(bind '(m00 m01 m10 m11 tx ty) vals)
					(mat3x2 m00 m01 tx m10 m11 ty))
				("translate"
					(bind '(tx &optional ty) vals)
					(mat3x2-translate tx (ifn ty 0.0)))
				("scale"
					(bind '(sx &optional sy) vals)
					(mat3x2-scale sx sy))
				("rotate"
					;about a point, default the origin
					(bind '(a &optional cx cy) vals)
					(mat3x2-rot (deg-to-rad a) cx cy))
				("skewX"
					(bind '(a) vals)
					(mat3x2-skew (deg-to-rad a)))
				("skewY"
					(bind '(a) vals)
					(mat3x2-skew 0.0 (deg-to-rad a)))
				;one not known is no change
				(:t +mat3x2_unity)))
		(setq ma (mat3x2-mul ma mb)))
	ma)


(defun svg-bounds (paths)
	; (svg-bounds paths) -> (x y w h)
	;the box of what the paths, ((closed path) ...), cover, of their points
	(defq x0 32767.0 y0 32767.0 x1 -32767.0 y1 -32767.0)
	(each (lambda ((& p))
			(defq i 0 n (length p))
			(while (< i n)
				(defq x (elem-get p i) y (elem-get p (inc i)))
				(setq x0 (min x0 x) x1 (max x1 x) y0 (min y0 y) y1 (max y1 y) i (+ i 2))))
		paths)
	(if (> x1 x0) (list x0 y0 (- x1 x0) (- y1 y0)) (list 0.0 0.0 0.0 0.0)))

(defun svg-offset (val)
	; (svg-offset str) -> fixed
	;0 to 1, or a %; one not known 0
	(if (num? (defq o (read-opacity val))) o 0.0))

(defun svg-stop (keys vals)
	; (svg-stop keys vals) -> (offset argb)
	;a stop of a gradient, its color with its stop-opacity
	(defq argb (read-color (Lmap) (ifn (svg-attr keys vals "stop-color") "black")))
	(unless (num? argb) (setq argb +argb_black))
	(and (defq op (svg-attr keys vals "stop-opacity")) (num? (setq op (read-opacity op)))
		(setq argb (svg-alpha argb op)))
	(list (svg-offset (ifn (svg-attr keys vals "offset") "0")) argb))

(defun svg-gradient (id log ids)
	; (svg-gradient id log ids) -> :nil | (kind attrs stops)
	;a linearGradient or a radialGradient kept in the log, (SVG-load), its
	;attributes over those of the one its href names, and so on, eight at
	;most, and its stops, or those of the nearest of them that has any
	(defq chain (list) attrs (Fmap 11) stops (list))
	(while (and id (< (length chain) 8) (defq at (. ids :find id)) (defq e (elem-get log at))
			(find (second e) '("linearGradient" "radialGradient")))
		(push chain at)
		(bind '(& & keys vals) e)
		(setq id (and (defq href (or (svg-attr keys vals "href") (svg-attr keys vals "xlink:href")))
			(starts-with "#" href) (slice href 1 -1))))
	(when (nempty? chain)
		(reach (lambda (at)
				(bind '(& & keys vals) (elem-get log at))
				(each (# (. attrs :insert %0 %1)) keys vals)
				(defq own (list) i (inc at) d 1)
				(while (and (< i (length log)) (> d 0))
					(bind '(ek &optional ea eb ec) (elem-get log i))
					(case ek
						(:in (and (= d 1) (eql ea "stop") (push own (svg-stop eb ec))) (++ d))
						(:out (-- d)))
					(++ i))
				(if (nempty? own) (setq stops own)))
			chain)
		(list (second (elem-get log (first chain))) attrs stops)))
(defun svg-paint (canvas val opacity paths m defs)
	; (svg-paint canvas val opacity paths m defs) -> :nil | :t
	;the canvas told to draw with a gradient, (:url id fall), defs the log
	;and the ids, (SVG-load): :t if it is to be drawn, a gradient or a color,
	;:nil if not. Of the gradient's box, objectBoundingBox by default, the
	;box of the paths, its gradientTransform, then m, the element's
	(bind '(& id fall) val)
	(bind '(log ids (vw vh)) defs)
	(cond
		((not (defq g (svg-gradient id log ids)))
			(when (and (num? fall) (/= fall 0))
				(. canvas :set_color (svg-alpha fall opacity))
				:t))
		(:t (bind '(kind attrs stops) g)
			(defq stops (map (lambda ((o c)) (list o (svg-alpha c opacity))) stops)
				units (ifn (. attrs :find "gradientUnits") "objectBoundingBox")
				tr (. attrs :find "gradientTransform")
				frame (if tr (read-transform (scatter (Lmap) "transform" +mat3x2_unity) tr) +mat3x2_unity)
				spread (case (. attrs :find "spreadMethod")
					("reflect" +spread_reflect) ("repeat" +spread_repeat) (:t +spread_pad))
				user (eql units "userSpaceOnUse")
				;a % of 1 of a box, of the viewport of the user's space, its
				;width, its height, or for a radius its diagonal over root 2
				sizes (if user
					(list vw vh (n2q (sqrt (/ (+ (* (n2d vw) (n2d vw)) (* (n2d vh) (n2d vh))) (const (n2d 2))))))
					'(1.0 1.0 1.0))
				at (lambda (key default size)
					(svg-coord (ifn (. attrs :find key) default) (elem-get sizes size))))
			(cond
				;of no stops, its fallback, or nothing
				((empty? stops)
					(when (and (num? fall) (/= fall 0))
						(. canvas :set_color (svg-alpha fall opacity))
						:t))
				((= (length stops) 1)
					(. canvas :set_color (second (first stops)))
					:t)
				((and (nql units "userSpaceOnUse")
						(bind '(bx by bw bh) (svg-bounds paths))
						(or (<= bw 0.0) (<= bh 0.0)))
					;a box of no width or height, nothing drawn
					:nil)
				(:t (if (nql units "userSpaceOnUse")
						(setq frame (mat3x2-mul (mat3x2 bw 0.0 bx 0.0 bh by) frame)))
					(setq frame (mat3x2-mul m frame))
					(if (eql kind "linearGradient")
						(. canvas :set_linear (at "x1" "0%" 0) (at "y1" "0%" 1) (at "x2" "100%" 0) (at "y2" "0%" 1) stops spread frame)
						(. canvas :set_radial (at "cx" "50%" 0) (at "cy" "50%" 1) (at "r" "50%" 2) stops spread frame))
					:t)))))

(defun read-attribute (entry key val)
	; (read-attribute entry key val) -> :inherit | val
	;:inherit for one that is to be its parent's: inherit, or a value not known
	(if (eql (setq val (trim val +svg_space)) "inherit") (setq key :inherit))
	(case key
		(:inherit :inherit)
		(("x" "y" "x1" "y1" "x2" "y2" "rx" "ry" "cx" "cy" "r" "width" "height")
			;not inherited in SVG: one that can not be read is none, 0, but
			;a % is left as it was, an svg of 100% is its default size
			(cond
				((eql (defq l (read-length val)) :percent) :inherit)
				((num? l) l)
				(0.0)))
		(("fill-opacity" "stroke-opacity" "opacity")
			(read-opacity val))
		("viewBox"
			(map (# (n2q (str-to-num %0))) (split val (const (char-class ", ")))))
		("points"
			;pairs of numbers, a last one alone left out: a path of an odd
			;length takes the fill and the stroke past its end
			(defq p (reduce (# (push %0 (n2q (read-num %1))))
				(filter (# (nempty? %0)) (split val (const (char-class "\t\n\r ,")))) (path)))
			(if (odd? (length p)) (slice p 0 -2) p))
		("transform"
			(read-transform entry val))
		(("stroke" "fill" "color")
			(read-color entry val))
		("visibility"
			(if (find val '("visible" "hidden" "collapse")) val :inherit))
		("stroke-dasharray"
			;none, or lengths, an odd number of them said twice; one less
			;than 0 or one not known is none, as is a pattern of no length
			(defq l (map (const read-length) (filter (# (nempty? %0)) (split val (const (char-class "\t ,"))))))
			(cond
				((or (eql val "none") (empty? l) (some (# (or (not (num? %0)) (< %0 0.0))) l)) :nil)
				((= 0.0 (reduce (const +) l 0.0)) :nil)
				((odd? (length l)) (cat l l))
				(l)))
		("stroke-dashoffset"
			(if (num? (defq l (read-length val))) l :inherit))
		("stroke-linecap"
			(case val
				("butt" +cap_butt)
				("round" +cap_round)
				("square" +cap_square)
				(:t :inherit)))
		("stroke-linejoin"
			(case val
				("miter" +join_miter)
				("bevel" +join_bevel)
				("round" +join_round)
				("miter-clip" +join_miter)
				("arcs" +join_miter)
				(:t :inherit)))
		("fill-rule"
			(case val
				("nonzero" +winding_none_zero)
				("evenodd" +winding_odd_even)
				(:t :inherit)))
		("font-weight"
			(case val
				("normal" "Regular")
				("bold" "Bold")
				("bolder" "Bolder")
				("lighter" "Lighter")
				(:t :inherit)))
		(("font-size" "stroke-width")
			;inherited: one that can not be read is the parent's
			(if (num? (defq l (read-length val))) l :inherit))
		("font-family"
			"OpenSans")
		("text-anchor"
			(case val
				("start" "start")
				("middle" "middle")
				("end" "end")
				(:t :inherit)))
		("dominant-baseline"
			(case val
				("auto" "auto")
				("middle" "middle")
				("hanging" "hanging")
				(:t :inherit)))
		("d"
			;one that can not be read draws nothing, not the picture
			(catch (path-gen-paths val) (list)))
		(:t val)))

(defun push-attributes (stack keys vals)
	;a copy of the parent's, its attributes and its style's over it. A key
	;that is there already keeps its place: the defaults block that is in
	;order, (SVG-load), stays so
	(push stack (defq entry (. (last stack) :copy)))
	;color first, what a currentColor of the same element is
	(and (defq at (find "color" keys))
		(nql (defq c (read-attribute entry "color" (elem-get vals at))) :inherit)
		(. entry :insert "color" c))
	(each (lambda (key val)
			(cond
				((eql key "style")
					(each (lambda (p)
							(when (= (length (defq kv (split p ":"))) 2)
								(defq k (trim (first kv)) v (read-attribute entry k (second kv)))
								(unless (eql v :inherit) (. entry :insert k v))))
						(split val ";")))
				((nql (setq val (read-attribute entry key val)) :inherit)
					(. entry :insert key val)))) keys vals)
	entry)

(defun path-fill-and-stroke (canvas entry paths &optional defs)
	(bind '(& stroke & fill & stroke_width & stroke_linecap & stroke_linejoin
			& fill_rule & fill_opacity & stroke_opacity & transform & opacity & visibility
			& dasharray & dashoffset &ignore)
		(get :bucket entry))
	;cheaky bind of the front of the Pmap :bucket !
	(unless (eql visibility "visible") (setq fill 0 stroke 0))
	(setq fill_opacity (* fill_opacity opacity) stroke_opacity (* stroke_opacity opacity))
	;a gradient's or a color's, :t if there is any to draw
	(defq paint (lambda (val alpha)
			(cond
				((list? val) (and defs (svg-paint canvas val alpha paths transform defs)))
				(:t (. canvas :set_color (svg-alpha val alpha)) :t))))
	(and (nql fill 0) (/= fill_opacity 0.0) (paint fill fill_opacity)
		(.-> canvas
			(:fpoly 0.0 0.0 fill_rule
				(map (lambda ((f p)) (mat3x2-vec2-mul transform p (cat p))) paths))))
	(and (nql stroke 0) (/= stroke_opacity 0.0) (paint stroke stroke_opacity)
		(.-> canvas
			(:fpoly 0.0 0.0 fill_rule
				(map (lambda (p) (mat3x2-vec2-mul transform p p))
					(reduce (lambda (l (f p))
							(cond
								(dasharray
									;each dash a line of its own, with its caps
									(each (# (push l (path-stroke-polyline %0 (* 0.5 stroke_width)
											stroke_linejoin stroke_linecap stroke_linecap)))
										(path-dashes p dasharray dashoffset f))
									l)
								(f	;closed
									(bind '(p1 p2) (path-stroke-polygon p (* 0.5 stroke_width)
											stroke_linejoin))
									(push l p1 p2))
								(:t ;open
									(push l (path-stroke-polyline p (* 0.5 stroke_width)
											stroke_linejoin stroke_linecap stroke_linecap)))))
						paths (list)))))))

(defun SVG-load (stream &optional scale most_w most_h)
	; (SVG-load stream [scale most_w most_h]) -> :nil | canvas
	;the picture drawn at scale times its pixels and made its own size, or
	;given most_w, and most_h, drawn straight at the size that fits, no
	;larger than its own, as a preview wants it: no pixels made to be
	;thrown away
	(setq scale (n2q (ifn scale 1)))
	(defq canvas :nil skip 0 log (list) ids (Fmap 31) viewport (list 100.0 100.0) defs (list log ids viewport) replaying 0 stack
			(list (scatter (Lmap)
					;this block of defaults are in order !
					"stroke" 0
					"fill" +argb_black
					"stroke-width" 1.0
					"stroke-linecap" +cap_butt
					"stroke-linejoin" +join_miter
					"fill-rule" +winding_none_zero
					"fill-opacity" 1.0
					"stroke-opacity" 1.0
					"transform" (mat3x2-scale scale)
					"opacity" 1.0
					"visibility" "visible"
					"stroke-dasharray" :nil
					"stroke-dashoffset" 0.0
					;this block can be any order
					"color" +argb_black
					"d" (path) "points" (path)
					"x" 0.0 "y" 0.0 "x1" 0.0 "y1" 0.0 "x2" 0.0 "y2" 0.0
					"cx" 0.0 "cy" 0.0 "r" 0.0
					"text-anchor" "start" "dominant-baseline" "auto"
					"font-size" 16.0 "font-family" "OpenSans" "font-weight" "Regular")))
	;each element as it came is kept, and where each with an id is, for a
	;use of it to draw it again, (replay)
	(defq on-in (lambda (command keys vals)
			;of the SVG namespace by a prefix, the element itself
			(if (starts-with "svg:" command) (setq command (slice command 4 -1)))
			(when (= replaying 0)
				(push log (list :in command (cat keys) (cat vals)))
				(if (defq at (find "id" keys)) (. ids :insert (elem-get vals at) (dec (length log)))))
			;what draws nothing is passed over with all that is in it, so
			;is what is not shown: skip counts the depth in it
			(cond
				((> skip 0) (++ skip))
				((or (find command +svg_skip) (find ":" command)
						(eql (svg-attr keys vals "display") "none"))
					(setq skip 1))
				(:t
					(case command
						("svg"
							(bind '(w h vb par) (gather (defq entry (push-attributes stack keys vals))
									"width" "height" "viewBox" "preserveAspectRatio"))
							;a size not more than 0 is none given, and a viewBox
							;of no area none
							(or (num? w) (setq w :nil))
							(or (num? h) (setq h :nil))
							(and w (<= w 0.0) (setq w :nil))
							(and h (<= h 0.0) (setq h :nil))
							(and vb (or (/= (length vb) 4) (<= (elem-get vb 2) 0.0) (<= (elem-get vb 3) 0.0))
								(setq vb :nil))
							;the viewBox fitted to the width and height if they
							;are other than it, else they are its
							(defq x 0.0 y 0.0 fit :nil)
							(when vb
								(bind '(vx vy vw vh) vb)
								(setq x vx y vy)
								(if (and w h (> vw 0.0) (> vh 0.0) (or (/= w vw) (/= h vh)))
									(setq fit (svg-fit w h vw vh par))
									(setq w vw h vh)))
							(setd w 256.0 h 256.0)
							;the user's space a % of a gradient's is of
							(elem-set viewport 0 (if vb (elem-get vb 2) w))
							(elem-set viewport 1 (if vb (elem-get vb 3) h))
							(cond
								(most_w
									;the size that fits, the picture's shape kept
									(defq k (min 1.0 (/ (n2q most_w) w)))
									(if most_h (setq k (min k (/ (n2q most_h) h))))
									(setq scale k canvas (Canvas (max 1 (n2i (+ (* w k) 0.5))) (max 1 (n2i (+ (* h k) 0.5))) 1)))
								((setq canvas (Canvas (n2i w) (n2i h) (n2i scale)))))
							(. canvas :set_canvas_flags +canvas_flag_antialias)
							(. entry :insert "transform"
								(cond
									(fit (bind '(sx sy tx ty) fit)
										(mat3x2 (* scale sx) 0.0 (* scale (- tx (* sx x)))
											0.0 (* scale sy) (* scale (- ty (* sy y)))))
									((mat3x2 scale 0.0 (* scale x -1.0) 0.0 scale (* scale y -1.0))))))
						("circle"
							(defq entry (push-attributes stack keys vals))
							(bind '(cx cy r) (svg-geom keys vals '("cx" "cy" "r") viewport))
							;a radius not more than 0 draws nothing
							(and (num? r) (> r 0.0)
								(path-fill-and-stroke canvas entry
									(list (list :t (path-gen-arc cx cy 0.0 +fp_2pi r (path)))) defs)))
						("ellipse"
							(defq entry (push-attributes stack keys vals))
							(bind '(cx cy rx ry) (svg-geom keys vals '("cx" "cy" "rx" "ry") viewport))
							(setd rx ry ry rx)
							(and (num? rx) (num? ry) (> rx 0.0) (> ry 0.0)
								(path-fill-and-stroke canvas entry
									(list (list :t (path-gen-ellipse cx cy rx ry (path)))) defs)))
						("line"
							(defq entry (push-attributes stack keys vals))
							(bind '(x1 y1 x2 y2) (svg-geom keys vals '("x1" "y1" "x2" "y2") viewport))
							(defq d (path x1 y1 x2 y2))
							(path-fill-and-stroke canvas entry (list (list :nil d)) defs))
						("rect"
							(defq entry (push-attributes stack keys vals))
							(bind '(x y rx ry x1 y1) (svg-geom keys vals '("x" "y" "rx" "ry" "width" "height") viewport))
							;a size not more than 0 draws nothing, a corner less than 0
							;is none, one more than half a side half of it
							(when (and (num? x1) (num? y1) (> x1 0.0) (> y1 0.0))
								(and (num? rx) (< rx 0.0) (setq rx :nil))
								(and (num? ry) (< ry 0.0) (setq ry :nil))
								(setd rx ry ry rx)
								(if rx (setq rx (min rx (* x1 0.5)) ry (min ry (* y1 0.5))))
								(setq x1 (+ x x1) y1 (+ y y1))
								(path-fill-and-stroke canvas entry
									(list (list :t (path-gen-rect x y x1 y1 rx ry (path)))) defs)))
						("polygon"
							(defq entry (push-attributes stack keys vals)
								d (. entry :find "points"))
							(path-fill-and-stroke canvas entry (list (list :t d)) defs))
						("polyline"
							(defq entry (push-attributes stack keys vals)
								d (. entry :find "points"))
							(path-fill-and-stroke canvas entry (list (list :nil d)) defs))
						("path"
							(defq entry (push-attributes stack keys vals)
								d (. entry :find "d"))
							(path-fill-and-stroke canvas entry d defs))
						("use"
							;its own attributes over what it uses, but x and y,
							;which are a move after its transform
							(defq href (or (svg-attr keys vals "href") (svg-attr keys vals "xlink:href"))
								k (list) v (list))
							(each (lambda (key val)
									(unless (find key '("x" "y" "width" "height" "href" "xlink:href" "transform"))
										(push k key) (push v val)))
								keys vals)
							(push k "transform")
							(push v (cat (ifn (svg-attr keys vals "transform") "")
								" translate(" (ifn (svg-attr keys vals "x") "0") "," (ifn (svg-attr keys vals "y") "0") ")"))
							(push-attributes stack k v)
							(when (and href (starts-with "#" href) (< replaying 8)
									(defq at (. ids :find (slice href 1 -1))))
								(++ replaying)
								(replay at)
								(-- replaying)))
						(("text" "g" "a" "switch")
							(push-attributes stack keys vals)))
					;a shape is drawn, its attributes let go of, and what
					;is in it passed over with its end, by skip
					(when (find command +svg_shapes)
						(pop stack)
						(setq skip 1))))))
	(defq on-out (lambda (command)
			(if (starts-with "svg:" command) (setq command (slice command 4 -1)))
			(if (= replaying 0) (push log (list :out command)))
			(if (> skip 0) (-- skip)
				(case command
					(("svg" "g" "a" "switch" "use" "text")
						(pop stack))))))
	(defq on-text (lambda (text)
			(if (= replaying 0) (push log (list :text text)))
			;render text (skip in TUI mode where fonts unavailable), its
			;white space as SVG has it by default: each run one space, none
			;at either end
			(setq text (join (split text (const (char-class " \t\r\n"))) " "))
			(when (and (= skip 0) (get 'create-font) (nempty? text)
					(eql (. (last stack) :find "visibility") "visible"))
				(bind '(x y family weight size anchor baseline)
					(gather (defq entry (last stack))
						"x" "y" "font-family" "font-weight" "font-size"
						"text-anchor" "dominant-baseline"))
				(defq font (create-font (cat "fonts/" family "-" weight ".ctf") (n2i size)))
				(setd font (create-font (cat "fonts/" family "-Regular.ctf") (n2i size)))
				(setd font (create-font "fonts/OpenSans-Regular.ctf" (n2i size)))
				(bind '(w h) (font-glyph-bounds font text))
				(defq ox (case anchor
						("middle" (* (n2q w) -1.0))
						("end" (* (n2q w) -2.0))
						(:t 0.0))
					oy (case baseline
						("middle" (* (n2q h) 0.5))
						("hanging" (n2q h))
						(:t 0.0))
					d (map (# (list :t (mat3x2-vec2-mul
									(mat3x2-translate (+ x ox) (+ y oy)) %0 %0)))
						(font-glyph-paths font text)))
				(path-fill-and-stroke canvas entry d defs))))
	(defq replay (lambda (at)
			;what was kept from at, it and all in it, drawn again; a symbol,
			;or another of what draws nothing, as a group
			(defq root (second (elem-get log at)) i at depth 0)
			(if (find root +svg_skip) (setq root "g"))
			(while (and (< i (length log)) (or (= i at) (> depth 0)))
				(bind '(kind &optional a b c) (elem-get log i))
				(case kind
					(:in (on-in (if (= i at) root a) b c) (++ depth))
					(:out (-- depth) (on-out (if (= depth 0) root a)))
					(:text (on-text a)))
				(++ i))))
	(XML-parse stream on-in on-out on-text)
	;made its own size, if drawn at more than its pixels
	(cond
		((or most_w (<= scale 1.0)) canvas)
		(:t (bind '(w h) (. canvas :pref_size))
			(. (Canvas w h 1) :resize canvas))))

(defun SVG-info (stream)
	; (SVG-info stream) -> (width height type) | (-1 -1 -1)
	(defq stack (list (Lmap)) width -1 height -1)
	(XML-parse stream
		(lambda (command keys vals)
			(when (eql command "svg")
				(bind '(w h vb) (gather (defq entry (push-attributes stack keys vals))
						"width" "height" "viewBox"))
				(setd w 256.0 h 256.0)
				(if vb (bind '(& & w h) vb))
				(setq width w height h)))
		(lambda (command))
		(lambda (text)))
	(list width height 32))

;the canvas is told what this reads
(canvas-loader ".svg" SVG-info SVG-load)

;module
(export-symbols '(SVG-load SVG-info))
(env-pop)
