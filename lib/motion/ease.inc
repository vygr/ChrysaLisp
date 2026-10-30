;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/motion/ease.inc - how a change goes over its time
;
; An ease takes how far through its time a change is, a fixed of 0.0 to 1.0,
; and gives how far through the change it should be. Each gives 0.0 at 0.0
; and 1.0 at 1.0, between them some go past the end and come back.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ease-linear (u)
	; (ease-linear u) -> fixed
	;the same speed all the way
	u)

(defun ease-in (u)
	; (ease-in u) -> fixed
	;slow in, fast out
	(* u u))

(defun ease-out (u)
	; (ease-out u) -> fixed
	;fast in, slow out
	(defq v (- 1.0 u))
	(- 1.0 (* v v)))

(defun ease-in-out (u)
	; (ease-in-out u) -> fixed
	;slow in, slow out
	(* u u (- 3.0 (* 2.0 u))))

(defun ease-in3 (u)
	; (ease-in3 u) -> fixed
	;slower in, faster out
	(* u u u))

(defun ease-out3 (u)
	; (ease-out3 u) -> fixed
	;faster in, slower out
	(defq v (- 1.0 u))
	(- 1.0 (* v v v)))

(defun ease-back (u)
	; (ease-back u) -> fixed
	;goes past the end and settles back
	(defq v (- u 1.0))
	(+ 1.0 (* 2.70158 v v v) (* 1.70158 v v)))

(defun ease-anticipate (u)
	; (ease-anticipate u) -> fixed
	;draws back first, then goes
	(- (* 2.70158 u u u) (* 1.70158 u u)))

(defun ease-bounce (u)
	; (ease-bounce u) -> fixed
	;drops to the end and bounces on it, each bounce smaller
	(cond
		((< u 0.3636) (* 7.5625 u u))
		((< u 0.7272) (setq u (- u 0.5454)) (+ (* 7.5625 u u) 0.75))
		((< u 0.9090) (setq u (- u 0.8181)) (+ (* 7.5625 u u) 0.9375))
		(:t (setq u (- u 0.9545)) (+ (* 7.5625 u u) 0.9844))))

(defun ease-elastic (u)
	; (ease-elastic u) -> fixed
	;shoots past and rings about the end, dying away
	(defq v (- 1.0 u))
	(- 1.0 (* v v v (cos (* u (* 3.0 +fp_2pi))))))
