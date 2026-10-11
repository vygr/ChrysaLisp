(report-header "Matrix: the 3x2 of the plane, boxes, lines against them")

(import "lib/math/matrix.inc")

(defun mx-near? (a b) (every (# (< (abs (- %0 %1)) 0.001)) a b))

;:nil is the matrix of no change
(assert-eq "no change and no change is :nil" :nil (Mat3x2-mul :nil :nil))
(defq mx_t (Mat3x2-translate 10 20))
(assert-true "no change and a matrix is that matrix" (eql (Mat3x2-mul :nil mx_t) mx_t))
(assert-true "a matrix and no change is that matrix" (eql (Mat3x2-mul mx_t :nil) mx_t))
(assert-eq "no change has none to undo it" :nil (Mat3x2-invert :nil))
(assert-eq "a point is where it was" "(3.00000 4.00000)" (str (Mat3x2-point :nil 3 4)))
(defq mx_b (list 1.0 2.0 3.0 4.0))
(assert-true "a box is the same box" (eql (Mat3x2-box mx_b :nil) mx_b))

;each kind
(assert-eq "with nothing given it is 1 0 0 0 1 0" "(1.00000 0.00000 0.00000 0.00000 1.00000 0.00000)" (str (Mat3x2)))
(assert-eq "moved" "(13.00000 24.00000)" (str (Mat3x2-point mx_t 3 4)))
(assert-eq "scaled about a point" "(5.00000 7.00000)" (str (Mat3x2-point (Mat3x2-scale 2 3 1 1) 3 3)))
(assert-true "a quarter turn is from x to y" (mx-near? (Mat3x2-point (Mat3x2-rot +fp_hpi) 1 0) (list 0.0 1.0)))
(assert-true "about a point" (mx-near? (Mat3x2-point (Mat3x2-rot +fp_hpi 10 10) 20 10) (list 10.0 20.0)))
(assert-eq "mul does the second given first" "(26.00000 48.00000)"
	(str (Mat3x2-point (Mat3x2-mul (Mat3x2-scale 2) mx_t) 3 4)))
(defq mx_m (Mat3x2-mul (Mat3x2-rot 0.7 3 4) (Mat3x2-scale 2.5 0.5 1 1)))
(assert-true "invert undoes it" (mx-near? (apply (const Mat3x2-point)
	(cat (list (Mat3x2-invert mx_m)) (Mat3x2-point mx_m 7 9))) (list 7.0 9.0)))
(assert-eq "one that flattens has none, no change is given" (str (Mat3x2)) (str (Mat3x2-invert (Mat3x2 1.0 1.0 0.0 1.0 1.0 0.0))))
(assert-true "the box round a box turned an eighth" (mx-near?
	(Mat3x2-box (list -1.0 -1.0 1.0 1.0) (Mat3x2-rot (/ +fp_pi 4.0))) (list -1.4142 -1.4142 1.4142 1.4142)))

;boxes
(assert-eq "join" "(0.00000 1.00000 9.00000 8.00000)" (str (vector-box-join-2d (list 0.0 2.0 5.0 8.0) (list 3.0 1.0 9.0 4.0))))
(assert-true "join with :nil is the other" (eql (vector-box-join-2d :nil mx_b) mx_b))
(assert-true "two that overlap" (vector-collide-boxes-2d (list 0.0 0.0 5.0 5.0) (list 4.0 4.0 9.0 9.0)))
(assert-true "two that only share a side do not" (not (vector-collide-boxes-2d (list 0.0 0.0 5.0 5.0) (list 5.0 0.0 9.0 5.0))))
(assert-true "nor does one and :nil" (not (vector-collide-boxes-2d mx_b :nil)))

;a line against a box
(assert-true "inside it" (vector-collide-line-box-2d 2.0 2.0 3.0 3.0 0.0 0.0 10.0 10.0))
(assert-true "across it" (vector-collide-line-box-2d -5.0 5.0 15.0 5.0 0.0 0.0 10.0 10.0))
(assert-true "ends in it" (vector-collide-line-box-2d -5.0 -5.0 1.0 1.0 0.0 0.0 10.0 10.0))
(assert-true "past its corner" (not (vector-collide-line-box-2d -5.0 3.0 3.0 -5.0 0.0 0.0 10.0 10.0)))
(assert-true "beside it" (not (vector-collide-line-box-2d 11.0 0.0 11.0 10.0 0.0 0.0 10.0 10.0)))

;the nearest point of a line
(assert-eq "part way" "(5.00000 0.00000 0.50000)" (str (vector-nearest-on-line-2d 5.0 3.0 0.0 0.0 10.0 0.0)))
(assert-eq "before its start" "(0.00000 0.00000 0.00000)" (str (vector-nearest-on-line-2d -4.0 3.0 0.0 0.0 10.0 0.0)))
(assert-eq "past its end" "(10.00000 0.00000 1.00000)" (str (vector-nearest-on-line-2d 14.0 3.0 0.0 0.0 10.0 0.0)))
(assert-eq "a line of no length" "(2.00000 2.00000 0.00000)" (str (vector-nearest-on-line-2d 4.0 3.0 2.0 2.0 2.0 2.0)))

;angles
(assert-true "three half turns is one the other way" (mx-near? (list (vector-angle-wrap (* 3.0 +fp_hpi))) (list (neg +fp_hpi))))
(assert-eq "pi stays" (str +fp_pi) (str (vector-angle-wrap +fp_pi)))
(assert-eq "minus pi is pi" (str +fp_pi) (str (vector-angle-wrap (neg +fp_pi))))
