(report-header "Math: the Pcb router's native line test, against its Lisp, apps/science/pcb/layer.inc")

;hit-line is native, with the test of (hit-line?) done in it, the same
;fixed point. Each record in a bucket of its own, so a record and a line
;are asked of both
(defq *app_root* "apps/science/pcb/")
(import "apps/science/pcb/layer.inc")

(defq ph_id 0)
(defun ph-native (record line)
	(hit-line (list (list record)) 0 0 0 0 1 (setq ph_id (inc ph_id)) line))
(defun ph-lisp (record line)
	(if (hit-line? record line) :t :nil))

(defq ph_pad (Record 0 (Layer-line 5.68099 136.48699 5.68099 136.48699 0.77198 0.09898)))
(assert-eq "a pad beside a line" :t (ph-native ph_pad (Layer-line 5.32499 136.96798 5.61196 135.40095 0.37399 0.15858)))
(assert-eq "a pad on a pad" :t (ph-native ph_pad (Layer-line 5.68099 136.48699 5.68099 136.48699 0.1 0.1)))
(assert-eq "far apart" :nil (ph-native ph_pad (Layer-line 50.0 50.0 60.0 50.0 0.5 0.1)))
(assert-eq "two lines that cross" :t (ph-native (Record 0 (Layer-line 0.0 0.0 10.0 10.0 0.1 0.1)) (Layer-line 0.0 10.0 10.0 0.0 0.1 0.1)))
(assert-eq "side by side, further apart than both radii and the gap, 0.3" :nil (ph-native (Record 0 (Layer-line 0.0 0.0 10.0 0.0 0.1 0.1)) (Layer-line 0.0 1.0 10.0 1.0 0.1 0.1)))
(assert-eq "side by side, closer than the gap" :t (ph-native (Record 0 (Layer-line 0.0 0.0 10.0 0.0 0.1 0.1)) (Layer-line 0.0 0.25 10.0 0.25 0.1 0.1)))

;random lines, points as pads are, along each axis and any way, and
;lines near each, at sixteenths and between
(defun ph-c () (n2f (/ (random 3200) 16)))
(defun ph-f () (/ (n2f (random 1000)) 1000.0))
(defun ph-line ()
	(defq x (+ (ph-c) (ph-f)) y (+ (ph-c) (ph-f)) r (ph-f) g (* 0.2 (ph-f)))
	(case (random 4)
		(0 (Layer-line x y x y r g))
		(1 (Layer-line x y (+ x (ph-c)) y r g))
		(2 (Layer-line x y x (+ y (ph-c)) r g))
		(:t (Layer-line x y (+ x (- (ph-c) 100.0) (ph-f)) (+ y (- (ph-c) 100.0) (ph-f)) r g))))
(defun ph-near (line)
	(Layer-line (+ (elem-get line 0) (- (ph-f) 0.5)) (+ (elem-get line 1) (- (ph-f) 0.5))
		(+ (elem-get line 2) (- (* 3.0 (ph-f)) 1.5)) (+ (elem-get line 3) (- (* 3.0 (ph-f)) 1.5))
		(ph-f) (* 0.2 (ph-f))))
(defq ph_same 0 ph_hits 0 ph_n 2000)
(times ph_n
	(defq line (ph-line) record (Record 0 line)
		query (if (= (random 2) 0) (ph-line) (ph-near line))
		want (ph-lisp record query))
	(if want (++ ph_hits))
	(if (eql want (ph-native record query)) (++ ph_same)))
(assert-eq "random lines, the same as the Lisp each time" ph_n ph_same)
(assert-true "and some of them hit, some not" (< 0 ph_hits ph_n))
