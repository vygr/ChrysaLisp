;;;;;;;;;;;
; Profiling
;;;;;;;;;;;

(import "sys/lisp.inc")

(structure '+prof 0
	(byte 'cnt+ 'acc+))

(defq *profile* (env -401) *profile_ret* (list)
	profile-dump (lambda (n)
		(print n)
		(defq e (tolist *profile*) m (reduce (lambda (a (k v)) (max a (length k))) e 0))
		(each (lambda ((k v))
			(print "Fun: " (pad k m)
				" Cnt: " (pad (elem +prof_cnt+ v) 6)
				" Total ns: " (pad (elem +prof_acc+ v) 8)))
			(sort (lambda ((k1 v1) (k2 v2))
				(- (elem +prof_acc+ v2) (elem +prof_acc+ v1))) e))
		(print)))

(defmacro-bind defun (n a &rest _)
	`(defq ,n (lambda ,a
		(cond
			((defq _e (get ',n *profile*))
				(elem-set +prof_cnt+ _e (inc (elem +prof_cnt+ _e)))
				(if (< (length _e) 3) (push _e (time))))
			(t (def *profile* ',n (list 1 0 (time)))))
		(push *profile_ret* (progn ~_))
		(if (> (length (setq _e (get ',n *profile*))) 2)
			(push _e (+ (- (time) (pop _e)) (pop _e))))
		(pop *profile_ret*))))

(defmacro-bind defun-bind (n a &rest _)
	`(defq ,n (lambda ,a
		(cond
			((defq _e (get ',n *profile*))
				(elem-set +prof_cnt+ _e (inc (elem +prof_cnt+ _e)))
				(if (< (length _e) 3) (push _e (time))))
			(t (def *profile* ',n (list 1 0 (time)))))
		(push *profile_ret* (progn ~(prebind (macroexpand _))))
		(if (> (length (setq _e (get ',n *profile*))) 2)
			(push _e (+ (- (time) (pop _e)) (pop _e))))
		(pop *profile_ret*))))
