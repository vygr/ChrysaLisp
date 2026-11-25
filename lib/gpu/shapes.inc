;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; shapes for a render to draw, (. render :mesh id verts [vfile]), each
; straight into the doubles a mesh is, three vertices a triangle
;
; (sky-verts) -> doubles
; (globe-verts segs rings) -> doubles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/math/vector.inc")

(defun sky-verts ()
	; (sky-verts) -> doubles
	;two triangles over all of the frame, by its corners
	(doubles +double_-1 +double_-1 +double_1 +double_-1 +double_1 +double_1
		+double_-1 +double_-1 +double_1 +double_1 +double_-1 +double_1))

(defun globe-verts (segs rings)
	; (globe-verts segs rings) -> doubles
	;a ball of radius 1 as triangles, three vertices each, a vertex its
	;place, 3 numbers, and where on the map that is, 2. North is up, and
	;east goes round to the right as seen from outside, as a map has it.
	;A triangle goes round counter clockwise as seen from outside
	(defq out (list) rows (map! (lambda () (defq r (!)) (defq theta (/ (* +double_pi (n2d r)) (n2d rings)) st (sin theta) ct (cos theta))
				(map! (lambda () (defq phi (/ (* +double_2pi (n2d (!))) (n2d segs)))
						(list (* st (sin phi)) ct (* st (cos phi))
							(/ (n2d (!)) (n2d segs)) (/ (n2d r) (n2d rings)))) '() 0 (inc segs))) '() 0 (inc rings)))
	(each! (lambda () (defq r (!)) (each! (lambda () (defq s (!)) (defq a (elem-get (elem-get rows r) s) b (elem-get (elem-get rows (inc r)) s)
						c (elem-get (elem-get rows (inc r)) (inc s)) d (elem-get (elem-get rows r) (inc s)))
					;at a pole a quad is a triangle, the other has no area
					(if (< r (dec rings)) (each (lambda (v) (each (lambda (n) (push out n)) v)) (list a b c)))
					(if (> r 0) (each (lambda (v) (each (lambda (n) (push out n)) v)) (list a c d)))) '() 0 segs)) '() 0 rings)
	(apply (const doubles) out))
