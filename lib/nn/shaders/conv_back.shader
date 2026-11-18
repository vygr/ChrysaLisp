;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the error of a layer of convolutions taken back to its inputs, each
; input the sum of the error of each output whose patch it is in times
; the weight it met there, then the slope of the activation of the layer
; before, whose outputs a are those inputs. An index for each input of
; each sample
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defbuffer dz :float :read)
(defbuffer w :float :read)
(defbuffer a :float :read)
(defbuffer dx :float :write)
(definput planes :int 6)
(definput rows :int 14)
(definput cols :int 14)
(definput k :int 5)
(definput stride :int 1)
(definput pad :int 0)
(definput filters :int 16)
(definput oh :int 10)
(definput ow :int 10)
;the activation of the layer before, 0 none, 1 ReLU, 2 sigmoid, 3 tanh
(definput act :int 0)
(defconst max_filters 4096)
(defconst max_k 64)

(defun main :void ((i :int))
	(defq per (* planes rows cols) s (/ i per) r (- i (* s per)) c (/ r (* rows cols)) p (- r (* c rows cols))
		iy (/ p cols) ix (- p (* iy cols)) acc 0.0 v (elem-get a i))
	(for (f 0 max_filters)
		(when (>= f filters) (break))
		(defq zp (* (+ (* s filters) f) oh) wp (* (+ (* f planes) c) k))
		(for (ky 0 max_k)
			(when (>= ky k) (break))
			;the output whose patch has this input at ky, if a whole one
			(defq ty (- (+ iy pad) ky) oy (/ ty stride))
			(when (and (>= ty 0) (= (* oy stride) ty) (< oy oh))
				(for (kx 0 max_k)
					(when (>= kx k) (break))
					(defq tx (- (+ ix pad) kx) ox (/ tx stride))
					(when (and (>= tx 0) (= (* ox stride) tx) (< ox ow))
						(setq acc (+ acc (* (elem-get dz (+ (* (+ zp oy) ow) ox))
							(elem-get w (+ (* (+ wp ky) k) kx))))))))))
	(cond
		((= act 1) (when (<= v 0.0) (setq acc 0.0)))
		((= act 2) (setq acc (* acc v (- 1.0 v))))
		((= act 3) (setq acc (* acc (- 1.0 (* v v))))))
	(elem-set dx i acc))
