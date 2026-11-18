;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a layer of convolutions forward over a batch, each output the bias of
; its filter and the filter times the patch of the planes under it, k by
; k across all the planes, stride apart from pad before the edges, those
; outside the planes 0, then the activation. A filter's weights plane by
; plane, row by row, summed in that order, as the CPU's are. An index for
; each output of each sample, the batch sample by sample, filter by
; filter, row by row
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defbuffer x :float :read)
(defbuffer w :float :read)
(defbuffer b :float :read)
(defbuffer a :float :write)
;the planes in, the filters and their size, and the places out
(definput planes :int 1)
(definput rows :int 28)
(definput cols :int 28)
(definput k :int 5)
(definput stride :int 1)
(definput pad :int 2)
(definput filters :int 6)
(definput oh :int 28)
(definput ow :int 28)
;0 none, 1 ReLU, 2 sigmoid, 3 tanh
(definput act :int 3)
(defconst max_planes 1024)
(defconst max_k 64)

(defun main :void ((i :int))
	(defq per (* filters oh ow) s (/ i per) r (- i (* s per)) f (/ r (* oh ow)) p (- r (* f oh ow))
		oy (/ p ow) ox (- p (* oy ow)) acc (elem-get b f))
	(for (c 0 max_planes)
		(when (>= c planes) (break))
		(defq xp (* (+ (* s planes) c) rows) wp (* (+ (* f planes) c) k))
		(for (ky 0 max_k)
			(when (>= ky k) (break))
			(defq iy (- (+ (* oy stride) ky) pad))
			(when (and (>= iy 0) (< iy rows))
				(for (kx 0 max_k)
					(when (>= kx k) (break))
					(defq ix (- (+ (* ox stride) kx) pad))
					(when (and (>= ix 0) (< ix cols))
						(setq acc (+ acc (* (elem-get w (+ (* (+ wp ky) k) kx))
							(elem-get x (+ (* (+ xp iy) cols) ix))))))))))
	(cond
		((= act 1) (when (< acc 0.0) (setq acc 0.0)))
		((= act 2) (setq acc (/ 1.0 (+ 1.0 (exp (- acc))))))
		((= act 3) (setq acc (tanh acc))))
	(elem-set a i acc))
