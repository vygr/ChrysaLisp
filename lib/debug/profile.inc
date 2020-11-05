;;;;;;;;;;;
; Profiling
;;;;;;;;;;;

(import "sys/lisp.inc")

(structure '+prof 0
	(byte 'cnt+ 'rcnt+ 'acc+))

(defq *profile* (env -401) *profile_ret* (list))

(defun profile-print (n &optional s)
	;(profile-print name [stream]) -> stdout|stream
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

(defun profile-report (n)
	;(profile-report name)
	(when (/= 0 (length (defq services (mail-enquire "PROFILE_SERVICE"))))
		(mail-send (str (profile-print n (string-stream (cat ""))))
			(str-to-num (elem 1 (split (pop services) ","))))))

(defmacro profile-lambda (n a &rest _)
	`(lambda ,a
		(cond
			((defq _e_ (get ',n *profile*))
				(elem-set +prof_cnt+ _e_ (inc (elem +prof_cnt+ _e_)))
				(elem-set +prof_rcnt+ _e_ (inc (elem +prof_rcnt+ _e_)))
				(if (= (elem +prof_rcnt+ _e_) 1) (push _e_ (time))))
			(t	(def *profile* ',n (setq _e_ (list 1 1 0 (time))))))
		(push *profile_ret* (progn ~_))
		(elem-set +prof_rcnt+ _e_ (dec (elem +prof_rcnt+ _e_)))
		(if (= (elem +prof_rcnt+ _e_) 0) (push _e_ (+ (- (time) (pop _e_)) (pop _e_))))
		(pop *profile_ret*)))

(defmacro defun (n a &rest _)
	`(defq ,n (profile-lambda ,n ,a ~(prebind (macroexpand _)))))

(defmacro class (n p &rest _)
	`(defun ,n ,p (defq *profile_class* ,n this (env -1)) ~_ this))

(defmacro method (n a &rest _)
	`(def this ,n (profile-lambda ,(sym (cat *profile_class* n)) ,a ~(prebind (macroexpand _)))))
