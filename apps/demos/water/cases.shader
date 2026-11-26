;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Water, a kernel: the case of each cube of a chunk for marching
; cubes, which of its eight corners are in the rock, a bit each, of
; the grid grid.shader made. Its cubes are those of the grid less the
; one more all round, kept for the slopes. 0 and 255 have no surface in
; them.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defbuffer grid :float :read)
(defbuffer cases :int :write)

;how many places each way of the grid, and how many cubes are marched
(definput side :int 37)
(definput cubes :int 34)

;1 if the place of the grid is in the rock
(defun solid :int ((x :int) (y :int) (z :int))
	(if (<= (elem-get grid (+ x (* side (+ y (* side z))))) 0.0) 1 0))

;the corners in the order the tables have them, surface.inc: 0 to 3
;round the near face, 4 to 7 round the far
(defun main :void ((i :int))
	(defq z (/ i (* cubes cubes)) rest (- i (* z cubes cubes)) y (/ rest cubes) x (- rest (* y cubes))
		x0 (+ x 1) y0 (+ y 1) z0 (+ z 1) x1 (+ x 2) y1 (+ y 2) z1 (+ z 2))
	(elem-set cases i (logior (solid x0 y0 z0) (<< (solid x1 y0 z0) 1) (<< (solid x1 y1 z0) 2)
		(<< (solid x0 y1 z0) 3) (<< (solid x0 y0 z1) 4) (<< (solid x1 y0 z1) 5)
		(<< (solid x1 y1 z1) 6) (<< (solid x0 y1 z1) 7))))
