;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/motion/morph.inc - one outline into another
;
; An outline is a list of (closed path), as (path-gen-paths) gives it: the
; lines of a shape, each closed or open, a path being x y x y in fixeds. The
; letters of a word are one, an outer line and any holes for each letter.
;
; To morph, the lines of the one outline are paired with the lines of the
; other, each pair is made the same number of points, evenly along their
; lengths, and turned to start where they lie closest. A line with no partner
; grows out of a point, or shrinks into one. All that is done once, and each
; frame is then a sum over arrays, in native code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui/path/lisp.inc")
(import "gui/font/lisp.inc")

;the fewest points a line of a morph is made of, a short line morphing to a
;long one still needs some to bend with, and the most
(defq +morph_min 24 +morph_max 512 +morph_far 1000000.0)

(defun outline-svg (d)
	; (outline-svg d) -> outline
	;an outline from the d of an SVG path
	(path-gen-paths d))

(defun outline-text (text font_name size)
	; (outline-text text font_name size) -> outline
	;the letters of some words, the left of them at 0 and the line they sit
	;on at their foot. The outlines a font gives are twice its size
	(map (# (list :t (path-transform (const (fixeds 0.5 0.0 0.0 0.0 0.5 0.0)) %0 %0)))
		(if (nempty? text) (font-glyph-paths (create-font font_name size) text) (list))))

(defun outline-box (outline)
	; (outline-box outline) -> :nil | (x y x1 y1)
	;the box round all of its lines
	(when (nempty? outline)
		(defq x +morph_far y +morph_far x1 (neg +morph_far) y1 (neg +morph_far))
		(each (lambda ((closed p))
				(defq i 0 n (length p))
				(while (< i n)
					(defq px (elem-get p i) py (elem-get p (inc i)))
					(setq x (min x px) y (min y py) x1 (max x1 px) y1 (max y1 py) i (+ i 2))))
			outline)
		(if (<= x x1) (list x y x1 y1))))

(defun outline-centred (outline)
	; (outline-centred outline) -> outline
	;a new one, the middle of its box at 0 0
	(cond
		((defq box (outline-box outline))
			(bind '(x y x1 y1) box)
			(defq m (fixeds 1.0 0.0 (neg (* 0.5 (+ x x1))) 0.0 1.0 (neg (* 0.5 (+ y y1)))))
			(map (lambda ((closed p)) (list closed (path-transform m p (cat p)))) outline))
		(outline)))

(defun morph-length (p closed)
	; (morph-length p closed) -> fixed
	;how long a line is, with the way back to its start if it is closed
	(defq n (length p) i 2 len 0.0)
	(when (>= n 4)
		(while (< i n)
			(defq dx (- (elem-get p i) (elem-get p (- i 2)))
				dy (- (elem-get p (inc i)) (elem-get p (dec i))))
			(setq len (+ len (sqrt (+ (* dx dx) (* dy dy)))) i (+ i 2)))
		(when closed
			(defq dx (- (elem-get p 0) (elem-get p (- n 2)))
				dy (- (elem-get p 1) (elem-get p (dec n))))
			(setq len (+ len (sqrt (+ (* dx dx) (* dy dy)))))))
	len)

(defun morph-area (p)
	; (morph-area p) -> fixed
	;the area inside a closed line, by the way it goes round, one way
	;round less than 0, the other more
	(defq n (length p) i 0 sum 0.0)
	(while (< i n)
		(defq j (if (>= (+ i 2) n) 0 (+ i 2)))
		(setq sum (+ sum (- (* (/ (elem-get p i) 16.0) (elem-get p (inc j)))
				(* (/ (elem-get p j) 16.0) (elem-get p (inc i)))))
			i (+ i 2)))
	(* sum 8.0))

(defun morph-centre (p)
	; (morph-centre p) -> (x y)
	;the mean of its points, near enough the middle of a line
	(defq n (/ (length p) 2) sx 0.0 sy 0.0 i 0)
	(while (< i (* n 2))
		(setq sx (+ sx (/ (elem-get p i) (n2f (max n 1))))
			sy (+ sy (/ (elem-get p (inc i)) (n2f (max n 1)))) i (+ i 2)))
	(list sx sy))

(defun morph-resample (p closed count)
	; (morph-resample p closed count) -> path
	;count points, evenly along the line. A closed line's come back round
	;to one step short of the first, an open line's end on its last
	(defq n (length p) out (path) total (morph-length p closed))
	(cond
		((or (< n 4) (<= total 0.0))
			;a point, or nothing
			(defq x (if (>= n 2) (elem-get p 0) 0.0) y (if (>= n 2) (elem-get p 1) 0.0))
			(times count (push out x y)))
		(:t (defq step (/ total (n2f (if closed count (max 1 (dec count)))))
				x (elem-get p 0) y (elem-get p 1) at 2 want 0.0 gone 0.0 seg 0.0
				nx x ny y made 0)
			(while (< made count)
				;on along the line till the next point lies on this part of it
				(while (and (>= want (+ gone seg)) (or (< at n) (and closed (= at n))))
					(setq gone (+ gone seg) x nx y ny)
					(if (< at n)
						(setq nx (elem-get p at) ny (elem-get p (inc at)))
						(setq nx (elem-get p 0) ny (elem-get p 1)))
					(setq at (+ at 2))
					(defq dx (- nx x) dy (- ny y))
					(setq seg (sqrt (+ (* dx dx) (* dy dy)))))
				(defq f (if (> seg 0.0) (min 1.0 (/ (- want gone) seg)) 0.0))
				(push out (+ x (* f (- nx x))) (+ y (* f (- ny y))))
				(setq want (+ want step) made (inc made)))))
	out)

(defun morph-reverse (p)
	; (morph-reverse p) -> path
	;the line going the other way round
	(defq out (path) i (- (length p) 2))
	(while (>= i 0)
		(push out (elem-get p i) (elem-get p (inc i)))
		(setq i (- i 2)))
	out)

(defun morph-align (a b)
	; (morph-align a b) -> path
	;b, a closed line of as many points as a, turned to start where it lies
	;closest to a, looked for by a few of its points at a few starts
	(defq n (/ (length a) 2) stride (max 1 (/ n 32)) tries (max 1 (/ n 64))
		best 0 best_sum +morph_far s 0)
	(while (< s n)
		(defq sum 0.0 i 0)
		(while (< i n)
			(defq j (* 2 (% (+ i s) n))
				dx (- (elem-get a (* i 2)) (elem-get b j))
				dy (- (elem-get a (inc (* i 2))) (elem-get b (inc j))))
			(setq sum (+ sum (abs dx) (abs dy)) i (+ i stride)))
		(if (< sum best_sum) (setq best_sum sum best s))
		(setq s (+ s tries)))
	(cond
		((= best 0) b)
		(:t (defq out (path) i 0)
			(while (< i n)
				(defq j (* 2 (% (+ i best) n)))
				(push out (elem-get b j) (elem-get b (inc j)))
				(setq i (inc i)))
			out)))

(defun morph-marks (outline)
	; (morph-marks outline) -> ((closed path x y sign) ...)
	;each line, its centre as far across and down the box of all of them,
	;and which way round it goes, so a hole looks for a hole
	(defq box (or (outline-box outline) '(0.0 0.0 1.0 1.0)))
	(bind '(bx by bx1 by1) box)
	(defq bw (max 1.0 (- bx1 bx)) bh (max 1.0 (- by1 by)))
	(map (lambda ((closed p))
			(bind '(cx cy) (morph-centre p))
			(list closed p (/ (- cx bx) bw) (/ (- cy by) bh)
				(if (and closed (< (morph-area p) 0.0)) -1 1)))
		outline))

(defun morph-pair (from to)
	; (morph-pair from to) -> ((mark_a | :nil mark_b | :nil) ...)
	;the lines of the two paired, the nearest first as a line of the one
	;takes its pick of the lines of the other. Those left over have none
	(defq used (list) out (list)
		smaller (if (<= (length from) (length to)) from to)
		larger (if (<= (length from) (length to)) to from)
		flip (not (eql smaller from)))
	(each (lambda (mark)
			(bind '(& & x y sign) mark)
			(defq best :nil best_d +morph_far)
			(each (lambda (other)
					(unless (find other used)
						(bind '(& & ox oy osign) other)
						;one that goes the other way round is a long way off
						(defq d (+ (abs (- x ox)) (abs (- y oy)) (if (= sign osign) 0.0 1.0)))
						(if (< d best_d) (setq best_d d best other))))
				larger)
			(when best
				(push used best)
				(push out (if flip (list best mark) (list mark best)))))
		smaller)
	(each (lambda (other)
			(unless (find other used)
				(push out (if flip (list other :nil) (list :nil other)))))
		larger)
	out)

(defun morph-point (mark marks)
	; (morph-point mark marks) -> (x y)
	;where a line with no partner grows from: the centre of the nearest of
	;the lines on the other side, or if there are none its own
	(bind '(& p x y &ignore) mark)
	(defq best p best_d +morph_far)
	(each (lambda ((& op ox oy &ignore))
			(defq d (+ (abs (- x ox)) (abs (- y oy))))
			(if (< d best_d) (setq best_d d best op)))
		marks)
	(morph-centre best))

(defun morph-point-path (at count)
	; (morph-point-path (x y) count) -> path
	;a line of count points, all the one point
	(bind '(x y) at)
	(defq out (path))
	(times count (push out x y))
	out)

(defun Morph (from to)
	; (Morph from to) -> morph
	;the two outlines made ready, a list of (closed start change) for each
	;line. (morph-at) gives the outline at a part of the way
	(defq marks_a (morph-marks from) marks_b (morph-marks to))
	(map (lambda ((ma mb))
			(defq pa (if ma (second ma)) pb (if mb (second mb))
				closed (first (or ma mb))
				count (min +morph_max (max +morph_min
					(/ (length (or pa pb)) 2) (/ (length (or pb pa)) 2)))
				a (if pa (morph-resample pa closed count)
					(morph-point-path (morph-point mb marks_a) count))
				b (if pb (morph-resample pb closed count)
					(morph-point-path (morph-point ma marks_b) count)))
			(when (and ma mb closed (first mb))
				;both go the same way round, each from where the other is
				;closest
				(if (/= (elem-get ma 4) (elem-get mb 4)) (setq b (morph-reverse b)))
				(setq b (morph-align a b)))
			(list closed a (nums-sub b a (cat a))))
		(morph-pair marks_a marks_b)))

(defun morph-at (morph u)
	; (morph-at morph u) -> outline
	;the outline at that part of the way, 0.0 the one, 1.0 the other. It
	;can go past either, for an ease that does
	(map (lambda ((closed a change))
			(defq p (nums-scale change u (cat a)))
			(list closed (nums-add a p p)))
		morph))

(defun morph-trim (p closed u)
	; (morph-trim p closed u) -> path
	;the first part of a line, as far along it as u says, as an open line.
	;A closed line goes on round to its start
	(defq n (length p) out (path) want (* (morph-length p closed) u))
	(when (>= n 2)
		(push out (elem-get p 0) (elem-get p 1))
		(defq at 2 gone 0.0 x (elem-get p 0) y (elem-get p 1) going :t)
		(while (and going (or (< at n) (and closed (= at n))))
			(defq nx (if (< at n) (elem-get p at) (elem-get p 0))
				ny (if (< at n) (elem-get p (inc at)) (elem-get p 1))
				dx (- nx x) dy (- ny y) seg (sqrt (+ (* dx dx) (* dy dy))))
			(cond
				((>= (+ gone seg) want)
					(defq f (if (> seg 0.0) (/ (- want gone) seg) 0.0))
					(push out (+ x (* f dx)) (+ y (* f dy)))
					(setq going :nil))
				(:t (push out nx ny)
					(setq gone (+ gone seg) x nx y ny at (+ at 2))))))
	out)
