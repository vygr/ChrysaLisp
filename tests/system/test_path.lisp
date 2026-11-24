(report-header "Path: the filter and simplify of a path, onto itself and into another")

(import "gui/lisp.inc")

(defun pa-box (p)
	;the bounds of a path, as whole numbers
	(defq lo_x 100000.0 hi_x -100000.0 lo_y 100000.0 hi_y -100000.0)
	(each (lambda ((x y)) (setq lo_x (min lo_x x) hi_x (max hi_x x) lo_y (min lo_y y) hi_y (max hi_y y)))
		(partition p 2))
	(map (const n2i) (list lo_x hi_x lo_y hi_y)))

(defq pa_circle (path-gen-arc 96.0 96.0 0.0 +fp_2pi 80.0 (path))
	pa_count (length pa_circle)
	pa_line (path 0.0 0.0 10.0 0.1 20.0 0.0 30.0 10.0 40.0 0.0))

;the filter, points nearer than the tolerance to the one before are let go
(defq pa_self (path-filter 0.5 (cat pa_circle) (cat pa_circle)))
(assert-true "a filter lets go of points" (< (length pa_self) pa_count))
(assert-list-eq "into an empty path is the same as onto itself" pa_self (path-filter 0.5 pa_circle (path)))
(assert-list-eq "and into a smaller one" pa_self (path-filter 0.5 pa_circle (path 1.0 2.0)))
(assert-list-eq "and into a bigger one" pa_self (path-filter 0.5 pa_circle (cat pa_circle pa_circle)))
(assert-eq "the source is as it was" pa_count (length pa_circle))
(assert-list-eq "one point is that point" '(1.0 2.0) (path-filter 0.5 (path 1.0 2.0) (path 9.0 9.0 8.0 8.0)))
(assert-list-eq "no points is none" '() (path-filter 0.5 (path) (path 9.0 9.0)))
(assert-list-eq "a point on top of the last goes" '(0.0 0.0 5.0 0.0)
	(path-filter 0.5 (path 0.0 0.0 0.1 0.0 5.0 0.0) (path)))
;many times over, a write past the end of a path is found by what is next to it
(each! (lambda () (path-filter 0.5 pa_circle (path))) '() 0 200)
(assert-eq "and it does not write past the end" pa_count (length pa_circle))

;simplify, a point is kept if it is further than the tolerance from the line without it
(defq pa_simple (path-simplify 0.5 pa_circle (path)))
(assert-true "simplify lets go of points" (< (length pa_simple) pa_count))
(assert-list-eq "and keeps the whole of a round shape" '(16 176 16 176) (pa-box pa_simple))
(assert-list-eq "a point off the line is kept, one on it is not" '(0.0 0.0 20.0 0.0 30.0 10.0 40.0 0.0)
	(path-simplify 1.0 pa_line (path)))
(assert-list-eq "with a tolerance bigger than all of it only the ends are" '(0.0 0.0 40.0 0.0)
	(path-simplify 20.0 pa_line (path)))
(assert-list-eq "two points are those two" '(1.0 2.0 3.0 4.0) (path-simplify 1.0 (path 1.0 2.0 3.0 4.0) (path)))
;a big shape, the squares of its distances do not fit in 32 bits of fixed
(defq pa_big (path-gen-arc 2000.0 2000.0 0.0 +fp_2pi 1500.0 (path)))
(assert-list-eq "a big shape is kept whole" '(500 3500 500 3500) (pa-box (path-simplify 0.5 pa_big (path))))

;a line as dashes: 100 long, dashes of 10 with gaps of 5, from its start
(defq pa_dashes (path-dash (path 0.0 0.0 100.0 0.0) 10.0 5.0))
(assert-eq "a line 100 long is 7 dashes of 10 with gaps of 5, the last cut by its end" 7 (length pa_dashes))
(assert-list-eq "the first from the start, 10 long" '(0 0 10 0) (map (# (n2i (+ %0 0.5))) (first pa_dashes)))
(assert-list-eq "the next after a gap" '(15 0 25 0) (map (# (n2i (+ %0 0.5))) (second pa_dashes)))
(assert-list-eq "the last cut by the end" '(90 0 100 0) (map (# (n2i (+ %0 0.5))) (last pa_dashes)))
;a dash goes round a corner, its points the corner's too
(defq pa_corner (path-dash (path 0.0 0.0 8.0 0.0 8.0 8.0) 10.0 5.0))
(assert-list-eq "a dash round a corner keeps the corner" '(0 0 8 0 8 2) (map (# (n2i (+ %0 0.5))) (first pa_corner)))
;closed, round back to the start: a square of side 10, dashes of 10 and gaps of 10
(defq pa_square (path-dash (path 0.0 0.0 10.0 0.0 10.0 10.0 0.0 10.0) 10.0 10.0 :t))
(assert-eq "a square closed, 40 round, is two dashes of 10 and two gaps" 2 (length pa_square))
(assert-list-eq "and the last side is dashed too" '(10 10 0 10) (map (# (n2i (+ %0 0.5))) (second pa_square)))
(assert-eq "a single point is no dashes" 0 (length (path-dash (path 1.0 1.0) 4.0 4.0)))

;what took a node down, 2026-11-23, now refused by the bindings: an arc of
;a radius less than 0 divided without end, and an odd number of numbers
;read or wrote past the path's end
(assert-error "an arc of a radius less than 0 is refused" (path-gen-arc 10.0 10.0 0.0 +fp_2pi -4.0 (path)) {(path-gen-arc cx cy start end radius dst) wrong_types})
(assert-true "and of 0, a point, is made" (> (length (path-gen-arc 10.0 10.0 0.0 +fp_2pi 0.0 (path))) 0))
(assert-error "a matrix over an odd path is refused" (mat3x2-vec2-mul (mat3x2 1.0 0.0 10.0 0.0 1.0 20.0) (path 1.0 2.0 3.0) (path 1.0 2.0 3.0)) {(mat3x2-vec2-mul m3x2 src dst) wrong_types})
(assert-error "and into a dst shorter than its src" (mat3x2-vec2-mul (mat3x2 1.0 0.0 10.0 0.0 1.0 20.0) (path 1.0 2.0 3.0 4.0) (path 1.0 2.0)) {(mat3x2-vec2-mul m3x2 src dst) wrong_types})
(assert-error "simplify of an odd path is refused" (path-simplify 0.1 (path 0.0 0.0 1.0 3.0 2.0 0.0 3.0 3.0 4.0 0.0 9.0) (path)) {(path-simplify tol src dst) wrong_types})
(assert-error "a filter of one" (path-filter 0.1 (path 0.0 0.0 1.0 3.0 2.0 0.0 9.0) (path)) {(path-filter tol src dst) wrong_types})

;an SVG path's d as SVG has it, 2026-11-23: its numbers of every form, an
;arc's flags of one character each, and drawn up to its first error
(defun pa-d (d) (str (path-svg d)))
(assert-eq "a number from a point" (str '("M" 0.5 0.5 "x")) (pa-d "M.5.5"))
(assert-eq "a second point begins the next" (pa-d "M 5.5 .3") (pa-d "M5.5.3"))
(assert-eq "a sign begins the next" (pa-d "M 10 -20") (pa-d "M10-20"))
(assert-eq "a plus" (pa-d "M 3 4") (pa-d "M+3 4"))
(assert-eq "an exponent" (pa-d "M 100 -25 0.1") (pa-d "M1e2-2.5E+1 1e-1"))
(assert-eq "arc flags written together" (pa-d "a 25 25 0 1 1 25 25") (pa-d "a25 25 0 1125 25"))
(assert-eq "an arc of all of them" (pa-d "M 10 -20 A 5.5 .3 -4 0 1 0 -.1") (pa-d "M10-20A5.5.3-4 010-.1"))
(assert-eq "a flag not 0 or 1 ends it" (pa-d "a 25 25 0") (pa-d "a 25 25 0 6 0 -25 25"))
(assert-eq "as does a character not of a path" (pa-d "M 0 0 L 5") (pa-d "M 0 0 L 5 # 7"))
(defun pa-paths (d) (str (path-gen-paths d)))
(assert-eq "a set that is not whole is not drawn, nor what is after it"
	(pa-paths "M 10 10 L 20 20") (pa-paths "M 10 10 L 20 20 30 L 40 40"))
(assert-eq "nor a command of no set" (pa-paths "M 10 10 L 20 20") (pa-paths "M 10 10 L 20 20 L"))
(assert-eq "a path not begun by a move is nothing" "()" (pa-paths "L 5 5"))
(assert-eq "numbers after a close end it" (pa-paths "M 0 0 L 9 9 Z") (pa-paths "M 0 0 L 9 9 Z 4 4 L 1 1"))
(assert-eq "after a close, a new path from where it began"
	(str (list (list :t (path 0.0 0.0 10.0 0.0 0.0 0.0)) (list :nil (path 0.0 0.0 0.0 10.0))))
	(pa-paths "M 0 0 L 10 0 Z L 0 10"))
