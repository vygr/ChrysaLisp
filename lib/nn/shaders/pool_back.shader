;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the error of a layer of average pooling taken back to its inputs, each
; the error of its block's average over q^2, 0 where no block is, then
; the slope of the activation of the layer before, whose outputs a are
; those inputs. An index for each input of each sample
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defbuffer dz :float :read)
(defbuffer a :float :read)
(defbuffer dx :float :write)
(definput planes :int 6)
(definput rows :int 28)
(definput cols :int 28)
(definput q :int 2)
(definput oh :int 14)
(definput ow :int 14)
;the activation of the layer before, 0 none, 1 ReLU, 2 sigmoid, 3 tanh
(definput act :int 3)

(defun main :void ((i :int))
	(defq per (* rows cols) sc (/ i per) p (- i (* sc per)) iy (/ p cols) ix (- p (* iy cols))
		oy (/ iy q) ox (/ ix q) acc 0.0 v (elem-get a i))
	(when (and (< oy oh) (< ox ow))
		(setq acc (* (elem-get dz (+ (* (+ (* sc oh) oy) ow) ox)) (/ 1.0 (float (* q q))))))
	(cond
		((= act 1) (when (<= v 0.0) (setq acc 0.0)))
		((= act 2) (setq acc (* acc v (- 1.0 v))))
		((= act 3) (setq acc (* acc (- 1.0 (* v v))))))
	(elem-set dx i acc))
