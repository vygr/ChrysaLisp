;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Water, a kernel: the light at each place of a coarse grid over a
; chunk, two numbers a place, field.shader. The sun's, a soft shadow
; of the rock toward it, of what breaks the surface too; and the
; sky's, how open the water is straight up, which the caves have
; little of. A vertex of the chunk has the light of where it is in
; the grid, between its places. An index is a place, x fastest.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "apps/demos/water/field.shader")

(defbuffer light :float :write)

;the first place of the grid, how far apart its places are, and how
;many there are each way
(definput origin :vec3)
(definput step :float 2.125)
(definput side :int 9)

(defun main :void ((i :int))
	(defq z (/ i (* side side)) rest (- i (* z side side)) y (/ rest side) x (- rest (* y side))
		p (+ origin (* (vec3 (float x) (float y) (float z)) step)))
	(elem-set light (* i 2) (lit_by p sun_way 6.0))
	(elem-set light (+ (* i 2) 1) (lit_by p (vec3 0.0 1.0 0.0) 2.0)))
