;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/font/symbols.inc - the symbol font, made by the system
;
; A symbol is drawn on a grid of 24, with strokes, all of one weight,
; and a few shapes that are filled. It is data, a list of those, and the
; kit below makes them: a line, a loop, an arc, a ring, an arrow, a
; document, a folder. The symbols themselves are lib/font/symbol_set.inc.
;
; To make the font each stroke is given to the stroker of the path
; library, (path-stroke-polyline), which gives its outline, and the
; outlines of a symbol are its glyph. Where two strokes cross their
; outlines overlap, and a glyph is filled by the non zero rule, so what
; overlaps is filled the once, there is no working out of where they
; meet. A theme is the same symbols with another weight of stroke, or
; another end and corner.
;
; cmd/symbols.lisp is the command, it writes the fonts and the names,
; lib/consts/symbols.inc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui/path/lisp.inc")

;the grid, the font's ascent and descent as parts of its height, the first
;code, and how many times bigger than the grid the outlines are worked in,
;the stroker makes its curves of lines that are fine at that size
(defq +sf_grid 24.0 +sf_ascent 0.9375 +sf_descent 0.0625 +sf_base 0xe000 +sf_work 8.0)

;;;;;;;;;
; the kit
;;;;;;;;;

;a thing drawn is a list of items, so that things are put together with
;cat. An item is (kind numbers ...), the numbers on the grid
(defun s-line (&rest xy)
	; (s-line x y x y ...) -> items
	;a stroke from point to point
	(list (list :line (map (const n2f) xy))))

(defun s-loop (&rest xy)
	; (s-loop x y x y ...) -> items
	;a stroke that comes back to where it began
	(list (list :loop (map (const n2f) xy))))

(defun s-ring (cx cy r)
	; (s-ring cx cy r) -> items
	;a circle, its line
	(list (list :ring (n2f cx) (n2f cy) (n2f r))))

(defun s-disc (cx cy r)
	; (s-disc cx cy r) -> items
	;a circle, filled
	(list (list :disc (n2f cx) (n2f cy) (n2f r))))

(defun s-box (x y x1 y1)
	; (s-box x y x1 y1) -> items
	;a rectangle, filled, its corners as round as a stroke's
	(list (list :box (n2f x) (n2f y) (n2f x1) (n2f y1))))

(defun s-fill (&rest xy)
	; (s-fill x y x y ...) -> items
	;a shape, filled, its corners a little round
	(list (list :fill (map (const n2f) xy))))

(defun s-deg (a)
	;an angle in degrees as the system has angles
	(* (n2f a) (const (/ +fp_pi 180.0))))

(defun s-arc (cx cy r a0 a1 &optional steps)
	; (s-arc cx cy r a0 a1 [steps]) -> items
	;part of a circle, a stroke, from one angle to another in degrees,
	;anticlockwise as they grow
	(setd steps 32)
	(defq cx (n2f cx) cy (n2f cy) r (n2f r) a0 (s-deg a0) da (/ (- (s-deg a1) a0) (n2f steps)) pts (list))
	(each (lambda (i)
		(defq a (+ a0 (* da (n2f i))))
		(push pts (+ cx (* r (cos a))) (- cy (* r (sin a)))))
		(range 0 (inc steps)))
	(list (list :line pts)))

(defun s-rrect (x y x1 y1 &optional r)
	; (s-rrect x y x1 y1 [r]) -> items
	;a rectangle with round corners, its line
	(setd r 2)
	(defq x (n2f x) y (n2f y) x1 (n2f x1) y1 (n2f y1) r (n2f r) pts (list))
	(each (lambda ((cx cy a))
		(each (lambda (i)
			(defq t (s-deg (+ a (* i 15))))
			(push pts (+ cx (* r (cos t))) (- cy (* r (sin t)))))
			(range 0 7)))
		(list (list (- x1 r) (+ y r) 0) (list (+ x r) (+ y r) 90)
			(list (+ x r) (- y1 r) 180) (list (- x1 r) (- y1 r) 270)))
	(list (list :loop pts)))

(defun s-head (x y dx dy &optional h)
	; (s-head x y dx dy [h]) -> items
	;the head of an arrow, at x y, for a line that comes in along dx dy
	(setd h 3.6)
	(defq dx (n2f dx) dy (n2f dy) l (sqrt (+ (* dx dx) (* dy dy))) dx (/ dx l) dy (/ dy l)
		c (const (cos (* 40.0 (/ +fp_pi 180.0)))) s (const (sin (* 40.0 (/ +fp_pi 180.0))))
		x (n2f x) y (n2f y) h (n2f h))
	(cat (s-line x y (- x (* h (- (* dx c) (* dy s)))) (- y (* h (+ (* dx s) (* dy c)))))
		(s-line x y (- x (* h (+ (* dx c) (* dy s)))) (- y (* h (- (* dy c) (* dx s)))))))

(defun s-arrow (x y x1 y1 &optional h)
	; (s-arrow x y x1 y1 [h]) -> items
	;a line with a head on its far end
	(cat (s-line x y x1 y1) (s-head x1 y1 (- (n2f x1) (n2f x)) (- (n2f y1) (n2f y)) h)))

(defun s-arc-arrow (cx cy r a0 a1 &optional h)
	; (s-arc-arrow cx cy r a0 a1 [h]) -> items
	;an arc with a head on its end
	(defq e (s-deg a1) way (if (> a1 a0) 1.0 -1.0))
	(cat (s-arc cx cy r a0 a1)
		(s-head (+ (n2f cx) (* (n2f r) (cos e))) (- (n2f cy) (* (n2f r) (sin e)))
			(* way (neg (sin e))) (* way (neg (cos e))) h)))

(defun s-corners (&optional a b l)
	; (s-corners [a b l]) -> items
	;the four corners of a frame
	(setd a 3 b 21 l 4)
	(cat (s-line a (+ a l) a a (+ a l) a) (s-line (- b l) a b a b (+ a l))
		(s-line b (- b l) b b (- b l) b) (s-line (+ a l) b a b a (- b l))))

(defun s-map (items fx fy &optional fr)
	; (s-map items fx fy [fr]) -> items
	;every point of a thing moved, x by fx and y by fy, and a radius by fr
	(setd fr (const identity))
	(map (lambda ((kind &rest data))
		(case kind
			((:line :loop :fill)
				(list kind (reduce! (# (push %0 (if (= (logand (!) 1) 0) (fx %1) (fy %1))))
					(list (first data)) (list))))
			((:ring :disc)
				(list kind (fx (first data)) (fy (second data)) (fr (third data))))
			(:t (defq x (fx (first data)) y (fy (second data)) x1 (fx (third data)) y1 (fy (elem-get data 3)))
				(list kind (min x x1) (min y y1) (max x x1) (max y y1)))))
		items))

(defun s-mirror (items)
	; (s-mirror items) -> items
	;left for right
	(s-map items (# (- +sf_grid %0)) (const identity)))

(defun s-flip (items)
	; (s-flip items) -> items
	;top for bottom
	(s-map items (const identity) (# (- +sf_grid %0))))

(defun s-turn (items)
	; (s-turn items) -> items
	;across for down, a thing that lies along is made to stand
	(map (lambda ((kind &rest data))
		(case kind
			((:line :loop :fill)
				(list kind (reduce (# (push %0 (second %1) (first %1))) (partition (first data) 2) (list))))
			((:ring :disc) (list kind (second data) (first data) (third data)))
			(:t (list kind (second data) (first data) (elem-get data 3) (third data)))))
		items))

(defun s-small (items &optional s)
	; (s-small items [s]) -> items
	;a thing made smaller, kept to the top left, its strokes as thick as
	;ever. It leaves room for a badge
	(setd s 0.74)
	(s-map items (# (+ 2.0 (* (- %0 2.0) s))) (# (+ 2.0 (* (- %0 2.0) s))) (# (* %0 s))))

(defun s-heavy (items &optional f)
	; (s-heavy items [f]) -> items
	;the strokes of a thing made f times the weight of the theme's, twice
	;if not said. For a symbol that is only ever drawn small
	(setd f 2.0)
	(map (lambda ((kind &rest data))
		(if (eql kind :line) (list kind (first data) (n2f f)) (cat (list kind) data)))
		items))

;the badges. All of them, three dots along the bottom. Everywhere, a ring
;at the bottom right
(defun s-all (items)
	(cat (s-small items) (s-disc 13 21 1.3) (s-disc 17 21 1.3) (s-disc 21 21 1.3)))

(defun s-global (items)
	(cat (s-small items) (s-ring 19 19 3) (s-line 16 19 22 19)))

;;;;;;;;;;;;;;;;;
; making the font
;;;;;;;;;;;;;;;;;

(defun sym-wind (paths)
	; (sym-wind paths) -> paths
	;the outlines of one item, all turned if they must be so that the
	;biggest goes round the one way. A glyph is filled by the non zero
	;rule, and two outlines that go round opposite ways cut a hole where
	;they overlap, a disc on the end of a stroke did
	(defq big 0.0)
	(each (lambda (p)
		(defq pts (partition p 2) a 0.0)
		(each (lambda ((x y))
			(bind '(x1 y1) (elem-get pts (% (inc (!)) (length pts))))
			(setq a (+ a (- (* x y1) (* x1 y))))) pts)
		(if (> (abs a) (abs big)) (setq big a))) paths)
	(if (>= big 0.0) paths
		(map (# (apply (const path) (reduce (# (push %0 (first %1) (second %1)))
			(reverse (partition %0 2)) (list)))) paths)))

(defun sym-paths (items radius joint cap)
	; (sym-paths items radius joint cap) -> paths
	;the outlines of a thing, at the size they are worked in. radius is
	;half the weight of a stroke, on the grid
	(defq out (list) r (* radius +sf_work)
		scaled (lambda (pts) (apply (const path) (map (# (* %0 +sf_work)) pts)))
		whole (lambda (cx cy cr) (path-gen-arc (* cx +sf_work) (* cy +sf_work) 0.0 +fp_2pi (* cr +sf_work) (path))))
	(each (lambda ((kind &rest data))
		(defq ps (list))
		(case kind
			(:line (defq lr (if (> (length data) 1) (* r (second data)) r))
				(push ps (path-stroke-polyline (scaled (first data)) lr joint cap cap)))
			(:loop (path-stroke-polygons ps r joint (list (scaled (first data)))))
			(:ring (path-stroke-polygons ps r +join_bevel (list (apply whole data))))
			(:disc (push ps (apply whole data)))
			(:box (push ps (path-gen-rect (* (first data) +sf_work) (* (second data) +sf_work)
				(* (third data) +sf_work) (* (elem-get data 3) +sf_work) +sf_work +sf_work (path))))
			(:fill (each (# (push out %0)) (sym-wind (list (scaled (first data)))))
				(path-stroke-polygons ps (* 0.6 +sf_work) +join_round (list (scaled (first data))))))
		(each (# (push out %0)) (sym-wind ps)))
		items)
	;a curve is more points than it wants, those nearer than half a step
	;of the work size are let go
	(map (# (path-filter 0.5 %0 %0)) out))

(defun sym-glyph (code paths)
	; (sym-glyph code paths) -> str
	;a glyph of a .ctf, docs/ai_digest/ctf_command.md. Its left edge is at
	;0, and its advance is how wide its ink is
	(defq lo 30000.0 hi -30000.0 cmds (list) size (const (* +sf_grid +sf_work)))
	(each (lambda (p) (each (lambda ((x &)) (setq lo (min lo x) hi (max hi x))) (partition p 2))) paths)
	(defq short (# (char (logand (n2i (* %0 8192.0)) 0xffff) +short_size)))
	(each (lambda (p)
		(defq pts (partition p 2))
		(each (lambda ((x y))
			(push cmds (char (if (= (!) 0) 0 1) +short_size)
				(short (/ (- x lo) size)) (short (- (/ y size) +sf_ascent))))
			(cat pts (list (first pts)))))
		paths)
	(defq body (apply (const cat) (cat (list "") cmds))
		rec (cat (char code +short_size) (short (/ (- hi lo) size))
			(char (length body) +int_size) (char 0 +int_size) body))
	(cat rec (slice (const (char 0 4)) 0 (- (align (length rec) +int_size) (length rec)))))

(defun sym-font (symbols radius joint cap)
	; (sym-font symbols radius joint cap) -> str
	;the whole of a .ctf, of symbols each (name items), in the order they
	;are given, from +sf_base
	(defq glyphs (map (# (sym-glyph (+ +sf_base (!)) (sym-paths (second %0) radius joint cap))) symbols)
		short (# (char (logand (n2i (* %0 8192.0)) 0xffff) +short_size))
		at (+ 8 4 (* 4 (length glyphs)) 4) offsets (list))
	(each (# (push offsets (char at +int_size)) (setq at (+ at (length %0)))) glyphs)
	(apply (const cat) (cat
		(list (short +sf_ascent) (short +sf_descent) (char 0 4)
			(char (+ +sf_base (dec (length glyphs))) +short_size) (char +sf_base +short_size))
		offsets (list (char 0 +int_size)) glyphs)))
