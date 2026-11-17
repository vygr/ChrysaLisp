;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a dense layer's weights and biases moved down their gradients over a
; batch, from the ones of the step into the ones of the next, as a kernel
; writes no buffer it reads. The gradient of a weight is the sum over the
; samples of the error at its output times its input, of a bias the sum
; of the error. An index for each weight, and then each bias
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defbuffer w0 :float :read)
(defbuffer b0 :float :read)
(defbuffer dz :float :read)
(defbuffer x :float :read)
(defbuffer w1 :float :write)
(defbuffer b1 :float :write)
(definput ins :int 784)
(definput outs :int 128)
(definput batch :int 512)
;the rate over the batch
(definput step :float 0.0002)
(defconst max_batch 65536)

(defun main :void ((i :int))
	(defq nw (* outs ins) g 0.0)
	(if (< i nw)
		(progn
			(defq j (/ i ins) k (- i (* j ins)))
			(for (s 0 max_batch)
				(when (>= s batch) (break))
				(setq g (+ g (* (elem-get dz (+ (* s outs) j)) (elem-get x (+ (* s ins) k))))))
			(elem-set w1 i (- (elem-get w0 i) (* step g))))
		(progn
			(defq j (- i nw))
			(for (s 0 max_batch)
				(when (>= s batch) (break))
				(setq g (+ g (elem-get dz (+ (* s outs) j)))))
			(elem-set b1 j (- (elem-get b0 j) (* step g))))))
