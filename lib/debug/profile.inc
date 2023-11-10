;;;;;;;;;;;
; Profiling
;;;;;;;;;;;

(import "sys/lisp.inc")

;module
(env-push)

(enums +prof 0
	(enum cnt rcnt acc))

(defun profile-service ()
	(when (/= 0 (length (defq services (mail-enquire "*Profile"))))
		(to-net-id (elem-get 1 (split (pop services) ",")))))

(defun profile-ipc (service s)
	(mail-send service (cat (task-netid) s)))

(defun profile-info (n)
	(defq e (tolist *profile_map*) n (str n " (" (length e) ")")
		m (reduce (lambda (a (k v)) (max a (length k))) e 0)
		msg (cap (+ (* (length e) 6) 16) (list))
		u (pad "" (+ (length n) 4) ";;;;;;;;;;;;;;;;;;;;;;;;"))
	(push msg u (ascii-char 10)
		"; " n " ;" (ascii-char 10)
		u (ascii-char 10) (ascii-char 10))
	(each (lambda ((k v))
		(push msg
			(pad k m) " cnt: " (pad (elem-get +prof_cnt v) 8)
			" ns: " (pad (elem-get +prof_acc v) 8) (ascii-char 10)))
		(sort (lambda ((k1 v1) (k2 v2))
			(- (elem-get +prof_acc v2) (elem-get +prof_acc v1))) e))
	(apply (const cat) (push msg (ascii-char 10))))

(defun profile-report (n &optional reset)
	(when (defq service (profile-service))
		(profile-ipc service (profile-info n)))
	(if reset (setq *profile_map* (env 101))))

(defun profile-instrument (name form)
	(static-qq
		((cond
			((defq _e_ (get ',name *profile_map*))
				(elem-set +prof_cnt _e_ (inc (elem-get +prof_cnt _e_)))
				(elem-set +prof_rcnt _e_ (inc (elem-get +prof_rcnt _e_)))
				(if (= (elem-get +prof_rcnt _e_) 1) (push _e_ (pii-time))))
			(:t (def *profile_map* ',name (setq _e_ (list 1 1 0 (pii-time))))))
		(push *profile_rvals* (progn ~form))
		(elem-set +prof_rcnt _e_ (dec (elem-get +prof_rcnt _e_)))
		(if (= (elem-get +prof_rcnt _e_) 0) (push _e_ (+ (- (pii-time) (pop _e_)) (pop _e_))))
		(pop *profile_rvals*))))

(defmacro defun (n a &rest _)
	(static-qq (defq ,n (lambda ,a ~(macroexpand
		(profile-instrument n _))))))

(defmacro defmethod (n a &rest _)
	(static-qq (def (def? :vtable this) ,n (lambda (this ~a) ~(macroexpand
		(profile-instrument (sym (cat *class* " " n)) _))))))

;module
(export-symbols '(defun defmethod profile-report))
(env-pop)
