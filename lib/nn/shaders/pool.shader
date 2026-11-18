;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a layer of average pooling forward over a batch, each output the
; average of its q by q block, q apart, what is left over at the edges in
; none. An index for each output of each sample
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defbuffer x :float :read)
(defbuffer a :float :write)
(definput planes :int 6)
(definput rows :int 28)
(definput cols :int 28)
(definput q :int 2)
(definput oh :int 14)
(definput ow :int 14)
(defconst max_q 64)

(defun main :void ((i :int))
	(defq per (* oh ow) sc (/ i per) p (- i (* sc per)) oy (/ p ow) ox (- p (* oy ow))
		xp (* sc rows) sum 0.0)
	(for (qy 0 max_q)
		(when (>= qy q) (break))
		(defq xr (* (+ xp (* oy q) qy) cols))
		(for (qx 0 max_q)
			(when (>= qx q) (break))
			(setq sum (+ sum (elem-get x (+ xr (* ox q) qx))))))
	(elem-set a i (* sum (/ 1.0 (float (* q q))))))
