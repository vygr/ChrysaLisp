(report-header "Quaternions")

(import "lib/math/quaternion.inc")
(import "lib/math/matrix.inc")

;near, every element, and the ways and turns the tests use
(defq +q_eps (/ +double_1 (n2d 1000000000))
	q_near (lambda (a b) (< (reduce max (map (lambda (x y) (abs (- x y))) a b) +double_0) +q_eps))
	q_x (doubles +double_1 +double_0 +double_0) q_y (doubles +double_0 +double_1 +double_0)
	q_z (doubles +double_0 +double_0 +double_1)
	q_quarter_z (quat-axis-angle q_z (/ +double_pi +double_2))
	q_quarter_x (quat-axis-angle q_x (/ +double_pi +double_2))
	q_odd (quat-unit (doubles (n2d 3) (n2d 1) (n2d -2) (n2d 5)))
	q_v (doubles (n2d 2) (n2d -1) (n2d 3)))

;the turn that is none, and a turn and its way back
(assert-true "unity times q is q" (q_near (quat-mul (quat-unity) q_odd) q_odd))
(assert-true "q times its conj is unity" (q_near (quat-mul q_odd (quat-conj q_odd)) (quat-unity)))
(assert-true "unity turns nothing" (q_near (quat-vec3-rot (quat-unity) q_v) q_v))

;a quarter turn about z takes x to y, y to -x
(assert-true "quarter z of x is y" (q_near (quat-vec3-rot q_quarter_z q_x) q_y))
(assert-true "quarter z of y is -x" (q_near (quat-vec3-rot q_quarter_z q_y) (nums-scale q_x +double_-1)))
(assert-true "a buffer of two, into an out"
	(q_near (quat-vec3-rot q_quarter_z (cat q_x q_y) (doubles +double_0 +double_0 +double_0 +double_0 +double_0 +double_0))
		(doubles +double_0 +double_1 +double_0 +double_-1 +double_0 +double_0)))

;the product turns by the second then the first
(assert-true "product is one turn then the other"
	(q_near (quat-vec3-rot (quat-mul q_quarter_x q_quarter_z) q_v)
		(quat-vec3-rot q_quarter_x (quat-vec3-rot q_quarter_z q_v))))
(assert-true "product into its own first"
	(q_near (quat-mul (cat q_quarter_x) q_quarter_z (defq q_out (cat q_quarter_x))) (quat-mul q_quarter_x q_quarter_z)))

;the matrix turns as the quaternion does
(assert-true "matrix of q turns as q"
	(q_near (mat4x4-vec3-mul (quat-mat4x4 q_odd) q_v) (quat-vec3-rot q_odd q_v)))

;the shortest turn from one way to another, and of opposite ways
(assert-true "between x and y takes x to y" (q_near (quat-vec3-rot (quat-between q_x q_y) q_x) q_y))
(defq q_w (doubles (/ (n2d 3) (n2d 5)) +double_0 (/ (n2d -4) (n2d 5))))
(assert-true "between v and w" (q_near (quat-vec3-rot (quat-between (nums-scale q_v (recip (sqrt (nums-dot q_v q_v)))) q_w)
		(nums-scale q_v (recip (sqrt (nums-dot q_v q_v))))) q_w))
(assert-true "between x and -x" (q_near (quat-vec3-rot (quat-between q_x (nums-scale q_x +double_-1)) q_x)
	(nums-scale q_x +double_-1)))

;slerp, its ends and the middle of a quarter turn
(assert-true "slerp 0" (q_near (quat-slerp (quat-unity) q_quarter_z +double_0) (quat-unity)))
(assert-true "slerp 1" (q_near (quat-slerp (quat-unity) q_quarter_z +double_1) q_quarter_z))
(assert-true "slerp half a quarter" (q_near (quat-slerp (quat-unity) q_quarter_z +double_1/2)
	(quat-axis-angle q_z (/ +double_pi +double_4))))
(assert-true "angle of a quarter" (< (abs (- (quat-angle (quat-unity) q_quarter_z) (/ +double_pi +double_2))) +q_eps))

;toward, held to a rate, and all the way when near
(assert-true "toward no more than most" (< (abs (- (quat-angle (quat-unity)
	(quat-toward (quat-unity) q_quarter_z (/ +double_1 (n2d 10)))) (/ +double_1 (n2d 10)))) +q_eps))
(assert-true "toward all the way" (q_near (quat-toward (quat-unity) q_quarter_z +double_2) q_quarter_z))

;a frame: -z to forward, y round to up across it, and up the other way
(defq q_f (doubles (/ (n2d 3) (n2d 5)) +double_0 (/ (n2d -4) (n2d 5))) q_frame (quat-frame q_f q_y))
(assert-true "frame forward" (q_near (quat-vec3-rot q_frame (nums-scale q_z +double_-1)) q_f))
(assert-true "frame up" (q_near (quat-vec3-rot q_frame q_y) q_y))
(defq q_frame (quat-frame (nums-scale q_z +double_-1) (nums-scale q_y +double_-1)))
(assert-true "frame upside down" (q_near (quat-vec3-rot q_frame q_y) (nums-scale q_y +double_-1)))
(assert-true "frame upside down forward" (q_near (quat-vec3-rot q_frame (nums-scale q_z +double_-1)) (nums-scale q_z +double_-1)))

;the natives refuse what is not theirs
(assert-error "mul of three" (quat-mul q_x q_odd) {(quat-mul qa qb [out]) wrong_types})
(assert-error "rot of four" (quat-vec3-rot q_odd (doubles +double_0 +double_0 +double_0 +double_0)) {(quat-vec3-rot q v [out]) wrong_types})
(assert-error "rot into short" (quat-vec3-rot q_odd q_v (doubles +double_0)) {(quat-vec3-rot q v [out]) wrong_types})
