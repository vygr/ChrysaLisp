;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a dense layer forward over a batch, a = act(b + W x), W a row for each
; output. An index for each output of each sample, the batch row by row
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defbuffer x :float :read)
(defbuffer w :float :read)
(defbuffer b :float :read)
(defbuffer a :float :write)
(definput ins :int 784)
(definput outs :int 128)
;0 none, 1 ReLU, 2 sigmoid, 3 tanh
(definput act :int 1)
(defconst max_ins 8192)

(defun main :void ((i :int))
	(defq s (/ i outs) j (- i (* s outs)) xr (* s ins) wr (* j ins) acc (elem-get b j))
	(for (k 0 max_ins)
		(when (>= k ins) (break))
		(setq acc (+ acc (* (elem-get w (+ wr k)) (elem-get x (+ xr k))))))
	(if (= act 1)
		(when (< acc 0.0) (setq acc 0.0))
		(if (= act 2)
			(setq acc (/ 1.0 (+ 1.0 (exp (- acc)))))
			(when (= act 3) (setq acc (- (/ 2.0 (+ 1.0 (exp (* -2.0 acc)))) 1.0)))))
	(elem-set a i acc))
