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
(each (lambda (_) (path-filter 0.5 pa_circle (path))) (range 0 200))
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
