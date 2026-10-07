(report-header "System: the stroker, a sharp turn of short lines")

(import "gui/lisp.inc")
(import "lib/math/vector.inc")

;the glyph r has a notch where its arm leaves its stem, a sharp turn of
;short lines. On the inside of such a turn the two edges of a stroke meet
;a long way back, further than the lines are long, and the outline used
;to throw a spike out to that point, four times the radius away. It must
;fall back to a bevel there. So no point of a stroked outline may be much
;further from the glyph than the radius.
(defq font (create-font "fonts/Hack-Regular.ctf" 34) radius 5.5)

(defun stroke-reach (text join)
	;how far past the bounds of the glyphs their stroked outline goes
	(defq raw (font-glyph-paths font text) reach 0.0)
	(bind '((min_x min_y) (max_x max_y)) (vector-bounds-2d raw))
	(bind '((sx sy) (sx1 sy1)) (vector-bounds-2d (path-stroke-polygons (list) radius join raw)))
	(max (- min_x sx) (- min_y sy) (- sx1 max_x) (- sy1 max_y)))

(assert-true "font" font)
(each (lambda (text)
	(assert-true (cat "round joins, " text) (<= (stroke-reach text +join_round) (* radius 1.2)))
	(assert-true (cat "bevel joins, " text) (<= (stroke-reach text +join_bevel) (* radius 1.2))))
	'("r" "sqrt-ff" "cpy-dr" "mkv"))

;a mitre on the outside of a turn is still a mitre, a square comes out square
(defq box (list (path 0.0 0.0 100.0 0.0 100.0 100.0 0.0 100.0)))
(bind '((bx by) (bx1 by1)) (vector-bounds-2d (path-stroke-polygons (list) 10.0 +join_miter box)))
(assert-true "a mitred square" (every (# (< (abs (- %0 %1)) 0.05))
	'(-10.0 -10.0 110.0 110.0) (list bx by bx1 by1)))
