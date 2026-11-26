;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Water, a kernel: how much water there is at each place of a grid,
; field.shader, a chunk of the reef and one more all round it, so the
; slope at each place of the chunk can be had from its neighbours.
; An index is a place, x fastest, then y, then z.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "apps/demos/water/field.shader")

(defbuffer grid :float :write)

;the first place of the grid, how far apart its places are, and how
;many there are each way
(definput origin :vec3)
(definput step :float 0.5)
(definput side :int 37)

(defun main :void ((i :int))
	(defq z (/ i (* side side)) rest (- i (* z side side)) y (/ rest side) x (- rest (* y side)))
	(elem-set grid i (water (+ origin (* (vec3 (float x) (float y) (float z)) step)))))
