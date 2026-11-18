;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the outputs of a batch as chances, softmax, the error of each against
; its label, the chances less the label's one, the loss of each, the
; cross entropy, and the output each takes it for. An index a sample
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defbuffer z :float :read)
(defbuffer y :int :read)
(defbuffer dz :float :write)
(defbuffer loss :float :write)
(defbuffer guess :int :write)
(definput outs :int 10)
(defconst max_outs 4096)

(defun main :void ((s :int))
	(defq r (* s outs) label (elem-get y s) top (elem-get z r) best 0 sum 0.0)
	;the biggest score taken from all first, so none overflows
	(for (j 1 max_outs)
		(when (>= j outs) (break))
		(when (> (elem-get z (+ r j)) top) (setq top (elem-get z (+ r j)) best j)))
	(for (j 0 max_outs)
		(when (>= j outs) (break))
		(setq sum (+ sum (exp (- (elem-get z (+ r j)) top)))))
	(for (j 0 max_outs)
		(when (>= j outs) (break))
		(defq p (/ (exp (- (elem-get z (+ r j)) top)) sum) e p)
		(when (= j label)
			;no log of a chance less than 1e-37, the least a model of floats
			;takes one of, lib/nn/model.inc. A str, a number here is 4 places
			(elem-set loss s (- (log (max p "0.0000000000000000000000000000000000001"))))
			(setq e (- p 1.0)))
		(elem-set dz (+ r j) e))
	(elem-set guess s best))
