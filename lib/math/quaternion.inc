;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; quaternions, of doubles, w x y z: a turn about an axis held as one, no
; gimbal lock, turns made one of two by their product, and the way from
; one to another along the shortest arc, slerp. The product and the turn
; of a buffer of vec3s are natives of the doubles class, as the matrices'
; are; bound here and not in root.inc till a release
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/consts/numbers.inc")
(import "lib/math/trig.inc")

(ffi "class/doubles/lisp_quat_mul" quat-mul)
; (quat-mul qa qb [out]) -> doubles
(ffi "class/doubles/lisp_quat_v3_rot" quat-vec3-rot)
; (quat-vec3-rot q v [out]) -> doubles

;module
(env-push)

;nearer 1 than this two turns are as one, and the way between them is
;straight; less than this a way has no length
(defq +quat_close (n2d 0.9995) +quat_tiny (/ +double_1 (n2d 1000000)))

(defun quat-unity ()
	; (quat-unity) -> doubles
	;the turn that is none
	(doubles +double_1 +double_0 +double_0 +double_0))

(defun quat-unit (q)
	; (quat-unit q) -> doubles
	;q made of length 1 again, as sums drift
	(nums-scale q (recip (sqrt (nums-dot q q)))))

(defun quat-axis-angle (axis angle)
	; (quat-axis-angle axis angle) -> doubles
	;a turn of angle radians about an axis, a vec3 of length 1, made of
	;length 1 itself, as sin and cos are near, not exact
	(defq h (* angle +double_1/2) s (sin h))
	(bind '(x y z) axis)
	(quat-unit (doubles (cos h) (* x s) (* y s) (* z s))))

(defun quat-conj (q)
	; (quat-conj q) -> doubles
	;the turn back, of a quaternion of length 1
	(bind '(w x y z) q)
	(doubles w (neg x) (neg y) (neg z)))

(defun quat-between (a b)
	; (quat-between a b) -> doubles
	;the shortest turn that takes the way a to the way b, each a vec3 of
	;length 1; of ways opposite, a half turn about any axis across them
	(bind '(ax ay az) a)
	(bind '(bx by bz) b)
	(defq d (+ (* ax bx) (* ay by) (* az bz)))
	(cond
		((< d (neg +quat_close))
			;a half turn: an axis across a, from x, or from y if a is near x
			(defq c (if (< (abs ax) +double_1/2)
					(doubles +double_0 az (neg ay))
					(doubles (neg az) +double_0 ax)))
			(defq w (nums-scale c (recip (sqrt (nums-dot c c)))))
			(bind '(wx wy wz) w)
			(doubles +double_0 wx wy wz))
		((quat-unit (doubles (+ +double_1 d)
			(- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx)))))))

(defun quat-slerp (a b t)
	; (quat-slerp a b t) -> doubles
	;the turn t of the way from a to b, 0 to 1, along the shortest arc, at
	;an even rate; of turns nearly one, straight and made of length 1
	(defq d (nums-dot a b))
	(when (< d +double_0)
		(setq b (nums-scale b +double_-1) d (neg d)))
	(if (> d +quat_close)
		(quat-unit (nums-add a (nums-scale (nums-sub b a) t)))
		(defq th (acos d) s (recip (sin th)))
		(nums-add (nums-scale a (* (sin (* (- +double_1 t) th)) s))
			(nums-scale b (* (sin (* t th)) s)))))

(defun quat-angle (a b)
	; (quat-angle a b) -> double
	;how far apart two turns are, radians, the shortest way
	(* +double_2 (acos (min +double_1 (abs (nums-dot a b))))))

(defun quat-toward (q b most)
	; (quat-toward q b most) -> doubles
	;q turned toward b by no more than most radians, all the way if it is
	;nearer; for a step a frame of a turn held to a rate
	(defq apart (quat-angle q b))
	(if (<= apart (max most +quat_tiny)) b
		(quat-slerp q b (/ most apart))))

(defun quat-mat4x4 (q)
	; (quat-mat4x4 q) -> doubles
	;the matrix of the turn, as lib/math/matrix.inc has them, rows, the
	;columns the turned axes
	(bind '(w x y z) q)
	(defq xx (* x x) yy (* y y) zz (* z z) xy (* x y) xz (* x z) yz (* y z) wx (* w x) wy (* w y) wz (* w z))
	(doubles (- +double_1 (* +double_2 (+ yy zz))) (* +double_2 (- xy wz)) (* +double_2 (+ xz wy)) +double_0
		(* +double_2 (+ xy wz)) (- +double_1 (* +double_2 (+ xx zz))) (* +double_2 (- yz wx)) +double_0
		(* +double_2 (- xz wy)) (* +double_2 (+ yz wx)) (- +double_1 (* +double_2 (+ xx yy))) +double_0
		+double_0 +double_0 +double_0 +double_1))

(defun quat-axes (q)
	; (quat-axes q) -> (x y z)
	;the three axes of the turn, the turned x, y and z, each a vec3
	(partition (quat-vec3-rot q (doubles +double_1 +double_0 +double_0 +double_0 +double_1 +double_0
		+double_0 +double_0 +double_1)) 3))

(defun quat-frame (forward up)
	; (quat-frame forward up) -> doubles
	;the turn whose -z is forward and whose y is up, near enough, as a
	;view is held: the turn of -z to forward, then about forward to bring
	;its y round to up across it
	(defq q (quat-between (doubles +double_0 +double_0 +double_-1) forward)
		y (quat-vec3-rot q (doubles +double_0 +double_1 +double_0))
		f forward up (nums-sub up (nums-scale f (nums-dot up f))) uu (nums-dot up up))
	(cond
		((< uu +quat_tiny) q)
		;up the other way: a half turn about forward itself
		((< (nums-dot y (setq up (nums-scale up (recip (sqrt uu))))) (neg +quat_close))
			(quat-mul (quat-axis-angle f +double_pi) q))
		((quat-mul (quat-between y up) q))))

;module
(export-symbols
	'(quat-unity quat-axis-angle quat-conj quat-unit quat-between quat-slerp
	quat-angle quat-toward quat-mat4x4 quat-axes quat-frame))
(env-pop)
