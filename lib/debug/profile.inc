;;;;;;;;;;;
; Profiling
;;;;;;;;;;;

(import "sys/lisp.inc")
(defq *profile_map* (env 401) *profile_rvals* (list))

;module
(env-push)

(enums +prof 0
	(enum cnt rcnt acc))

(defun profile-print (n &optional s)
	; (profile-print name [stream]) -> stdout | stream
	(setd s (io-stream 'stdout))
	(defq e (tolist *profile_map*) m (reduce (lambda (a (k v)) (max a (length k))) e 0))
	(write s (cat (task-netid) n (ascii-char 10)))
	(each (lambda ((k v))
		(write s (cat
			(pad k m)
			" cnt: " (pad (elem-get +prof_cnt v) 8)
			" ns: " (pad (elem-get +prof_acc v) 8)
			(ascii-char 10))))
		(sort (lambda ((k1 v1) (k2 v2))
			(- (elem-get +prof_acc v2) (elem-get +prof_acc v1))) e)) s)

(defun profile-report (n &optional reset)
	; (profile-report name [reset])
   (when (/= 0 (length (defq services (mail-enquire "*Profile"))))
		(mail-send (to-net-id (elem-get 1 (split (pop services) ",")))
			(str (profile-print n (string-stream (cat "")))))
		(if reset (setq *profile_map* (env 401)))))

(defun profile-fun (name form)
	(static-qq
		((cond
			((defq _e_ (get ',name *profile_map*))
				(elem-set +prof_cnt _e_ (inc (elem-get +prof_cnt _e_)))
				(elem-set +prof_rcnt _e_ (inc (elem-get +prof_rcnt _e_)))
				(if (= (elem-get +prof_rcnt _e_) 1) (push _e_ (pii-time))))
			(:t (def *profile_map* ',name (setq _e_ (list 1 1 0 (pii-time))))))
		(push *profile_rvals* (progn ~_))
		(elem-set +prof_rcnt _e_ (dec (elem-get +prof_rcnt _e_)))
		(if (= (elem-get +prof_rcnt _e_) 0) (push _e_ (+ (- (pii-time) (pop _e_)) (pop _e_))))
		(pop *profile_rvals*))))

(defmacro defun (n a &rest _)
	(static-qq (defq ,n (lambda ,a ~(macroexpand
		(profile-fun n _))))))

(defmacro defmethod (n a &rest _)
	(static-qq (def (def? :vtable this) ,n (lambda (this ~a) ~(macroexpand
		(profile-fun (sym (cat *class* " " n)) _))))))

;module
(export-symbols defun defmethod profile-report)
(env-pop)
