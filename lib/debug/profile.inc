;;;;;;;;;;;
; Profiling
;;;;;;;;;;;

(import "sys/lisp.inc")

(structure '+prof 0
	(byte 'cnt+ 'acc+))

(defq *profile* (env -401) *profile_ret* (list)
	profile-dump (lambda (n)
		(print n)
		(each (lambda ((k v))
			(print "Fun: " (pad k 16)
				" Cnt: " (pad (elem +prof_cnt+ v) 6)
				" Total ns: " (pad (elem +prof_acc+ v) 8)))
			(sort (lambda ((k1 v1) (k2 v2))
				(- (elem +prof_cnt+ v2) (elem +prof_cnt+ v1))) (tolist *profile*)))
		(print)))

(defmacro-bind defun (n a &rest _)
	`(defq ,n (lambda ,a
		(if (defq _e (get ',n *profile*))
			(set *profile* ',n (list (inc (elem +prof_cnt+ _e)) (elem +prof_acc+ _e) (time)))
			(def *profile* ',n (list 1 0 (time))))
		(push *profile_ret* (progn ~_))
		(push (defq _e (get ',n *profile*)) (+ (- (time) (pop _e)) (pop _e)))
		(pop *profile_ret*))))

(defmacro-bind defun-bind (n a &rest _)
	`(defq ,n (lambda ,a
		(if (defq _e (get ',n *profile*))
			(set *profile* ',n (list (inc (elem +prof_cnt+ _e)) (elem +prof_acc+ _e) (time)))
			(def *profile* ',n (list 1 0 (time))))
		(push *profile_ret* (progn ~(prebind (macroexpand _))))
		(defq _e (get ',n *profile*))
		(push (defq _e (get ',n *profile*)) (+ (- (time) (pop _e)) (pop _e)))
		(pop *profile_ret*))))
