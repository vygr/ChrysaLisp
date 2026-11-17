;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the error of a dense layer's outputs taken back to its inputs, through
; its weights and the slope of the activation of the layer before, whose
; outputs a are those inputs. An index for each input of each sample
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defbuffer dz :float :read)
(defbuffer w :float :read)
(defbuffer a :float :read)
(defbuffer dx :float :write)
(definput ins :int 128)
(definput outs :int 10)
;the activation of the layer before, 0 none, 1 ReLU, 2 sigmoid, 3 tanh
(definput act :int 1)
(defconst max_outs 8192)

(defun main :void ((i :int))
	(defq s (/ i ins) k (- i (* s ins)) zr (* s outs) acc 0.0 v (elem-get a i))
	(for (j 0 max_outs)
		(when (>= j outs) (break))
		(setq acc (+ acc (* (elem-get dz (+ zr j)) (elem-get w (+ (* j ins) k))))))
	(cond
		((= act 1) (when (<= v 0.0) (setq acc 0.0)))
		((= act 2) (setq acc (* acc v (- 1.0 v))))
		((= act 3) (setq acc (* acc (- 1.0 (* v v))))))
	(elem-set dx i acc))
