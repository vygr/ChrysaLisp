;;;;;;;;;;;
; Profiling
;;;;;;;;;;;

(import "sys/lisp.inc")

(structure '+prof 0
	(byte 'cnt+ 'rcnt+ 'acc+))

(defq *profile* (env -401) *profile_ret* (list))

(defun-bind profile-print (n &optional s)
	(setd s (io-stream 'stdout))
	(defq e (tolist *profile*) m (reduce (lambda (a (k v)) (max a (length k))) e 0))
	(write s (cat (char (task-mailbox) long_size) n (ascii-char 10)))
	(each (lambda ((k v))
		(write s (cat
			(pad k m)
			" cnt: " (pad (elem +prof_cnt+ v) 6)
			" ns: " (pad (elem +prof_acc+ v) 8)
			(ascii-char 10))))
		(sort (lambda ((k1 v1) (k2 v2))
			(- (elem +prof_acc+ v2) (elem +prof_acc+ v1))) e)) s)

(defun-bind profile-report (n)
	(when (/= 0 (length (defq services (mail-enquire "PROFILE_SERVICE"))))
		(mail-send (str (profile-print n (string-stream (cat ""))))
			(str-to-num (elem 1 (split (pop services) ","))))))

(defmacro-bind defun (n a &rest _)
	`(defq ,n (lambda ,a
		(cond
			((defq _e_ (get ',n *profile*))
				(elem-set +prof_cnt+ _e_ (inc (elem +prof_cnt+ _e_)))
				(elem-set +prof_rcnt+ _e_ (inc (elem +prof_rcnt+ _e_)))
				(if (= (elem +prof_rcnt+ _e_) 1) (push _e_ (time))))
			(t	(def *profile* ',n (setq _e_ (list 1 1 0 (time))))))
		(push *profile_ret* (progn ~_))
		(elem-set +prof_rcnt+ _e_ (dec (elem +prof_rcnt+ _e_)))
		(if (= (elem +prof_rcnt+ _e_) 0) (push _e_ (+ (- (time) (pop _e_)) (pop _e_))))
		(pop *profile_ret*))))

(defmacro-bind defun-bind (n a &rest _)
	`(defq ,n (lambda ,a
		(cond
			((defq _e_ (get ',n *profile*))
				(elem-set +prof_cnt+ _e_ (inc (elem +prof_cnt+ _e_)))
				(elem-set +prof_rcnt+ _e_ (inc (elem +prof_rcnt+ _e_)))
				(if (= (elem +prof_rcnt+ _e_) 1) (push _e_ (time))))
			(t	(def *profile* ',n (setq _e_ (list 1 1 0 (time))))))
		(push *profile_ret* (progn ~(prebind (macroexpand _))))
		(elem-set +prof_rcnt+ _e_ (dec (elem +prof_rcnt+ _e_)))
		(if (= (elem +prof_rcnt+ _e_) 0) (push _e_ (+ (- (time) (pop _e_)) (pop _e_))))
		(pop *profile_ret*))))
