;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a layer of convolutions' weights and biases moved down their gradients
; over a batch, from the ones of the step into the ones of the next. The
; gradient of a weight is the sum over the samples and the places out of
; the error there times the input the weight met, of a bias the sum of
; the error of its filter. An index for each weight, and then each bias
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defbuffer w0 :float :read)
(defbuffer b0 :float :read)
(defbuffer dz :float :read)
(defbuffer x :float :read)
(defbuffer w1 :float :write)
(defbuffer b1 :float :write)
(definput planes :int 1)
(definput rows :int 28)
(definput cols :int 28)
(definput k :int 5)
(definput stride :int 1)
(definput pad :int 2)
(definput filters :int 6)
(definput oh :int 28)
(definput ow :int 28)
(definput batch :int 512)
;the rate over the batch
(definput step :float 0.0002)
(defconst max_batch 65536)
(defconst max_places 1048576)
(defconst max_side 1024)

(defun main :void ((i :int))
	(defq nw (* filters planes k k) g 0.0)
	(if (< i nw)
		(progn
			(defq f (/ i (* planes k k)) r (- i (* f planes k k)) c (/ r (* k k)) kk (- r (* c k k))
				ky (/ kk k) kx (- kk (* ky k)))
			(for (s 0 max_batch)
				(when (>= s batch) (break))
				(defq zp (* (+ (* s filters) f) oh) xp (* (+ (* s planes) c) rows))
				;a row of the places out at a time, and along it: no divide
				;for each place, and a row the weight met nothing of is passed
				(for (oy 0 max_side)
					(when (>= oy oh) (break))
					(defq iy (- (+ (* oy stride) ky) pad))
					(when (and (>= iy 0) (< iy rows))
						(defq zr (* (+ zp oy) ow) xr (- (+ (* (+ xp iy) cols) kx) pad))
						(for (ox 0 max_side)
							(when (>= ox ow) (break))
							(defq ix (- (+ (* ox stride) kx) pad))
							(when (and (>= ix 0) (< ix cols))
								(setq g (+ g (* (elem-get dz (+ zr ox)) (elem-get x (+ xr (* ox stride)))))))))))
			(elem-set w1 i (- (elem-get w0 i) (* step g))))
		(progn
			(defq f (- i nw))
			(for (s 0 max_batch)
				(when (>= s batch) (break))
				(defq zp (* (+ (* s filters) f) oh ow))
				(for (p 0 max_places)
					(when (>= p (* oh ow)) (break))
					(setq g (+ g (elem-get dz (+ zp p))))))
			(elem-set b1 f (- (elem-get b0 f) (* step g))))))
