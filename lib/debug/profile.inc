;;;;;;;;;;;
; Profiling
;;;;;;;;;;;

(import "sys/lisp.inc")

(enums +prof 0
	(enum cnt rcnt acc))

(defq *profile* (env -401) *profile_ret* (list))

(defun profile-print (n &optional s)
	; (profile-print name [stream]) -> stdout | stream
	(setd s (io-stream 'stdout))
	(defq e (tolist *profile*) m (reduce (lambda (a (k v)) (max a (length k))) e 0))
	(write s (cat (task-mailbox) n (ascii-char 10)))
	(each (lambda ((k v))
		(write s (cat
			(pad k m)
			" cnt: " (pad (elem +prof_cnt v) 6)
			" ns: " (pad (elem +prof_acc v) 8)
			(ascii-char 10))))
		(sort (lambda ((k1 v1) (k2 v2))
			(- (elem +prof_acc v2) (elem +prof_acc v1))) e)) s)

(defun profile-report (n &optional reset)
	; (profile-report name [reset])
	(when (/= 0 (length (defq services (mail-enquire "PROFILE_SERVICE"))))
		(mail-send (to-net-id (elem 1 (split (pop services) ",")))
			(str (profile-print n (string-stream (cat "")))))
		(if reset (setq *profile* (env -401)))))

(defmacro profile-lambda (n a &rest _)
	`(lambda ,a
		(cond
			((defq _e_ (get ',n *profile*))
				(elem-set +prof_cnt _e_ (inc (elem +prof_cnt _e_)))
				(elem-set +prof_rcnt _e_ (inc (elem +prof_rcnt _e_)))
				(if (= (elem +prof_rcnt _e_) 1) (push _e_ (pii-time))))
			(t	(def *profile* ',n (setq _e_ (list 1 1 0 (pii-time))))))
		(push *profile_ret* (progn ~_))
		(elem-set +prof_rcnt _e_ (dec (elem +prof_rcnt _e_)))
		(if (= (elem +prof_rcnt _e_) 0) (push _e_ (+ (- (pii-time) (pop _e_)) (pop _e_))))
		(pop *profile_ret*)))

(defmacro defun (n a &rest _)
	`(defq ,n (profile-lambda ,n ,a ~(macroexpand _))))

(defmacro defmethod (n a &rest _)
	`(def (def? :vtable this) ,n (profile-lambda ,(sym (cat *class* " " n)) ,a ~(macroexpand _))))
