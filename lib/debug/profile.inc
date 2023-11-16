;;;;;;;;;;;
; Profiling
;;;;;;;;;;;

;module
(env-push)

(setq *profile_map* (env 101))
(defq +LF (ascii-char 10))

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
	(push msg u +LF
		"; " n " ;" +LF
		u +LF +LF)
	(each (lambda ((k v))
		(push msg
			(pad k m) " cnt: " (pad (elem-get +prof_cnt v) 8)
			" ns: " (pad (elem-get +prof_acc v) 8) +LF))
		(sort (lambda ((k1 v1) (k2 v2))
			(- (elem-get +prof_acc v2) (elem-get +prof_acc v1))) e))
	(apply (const cat) (push msg +LF)))

(defun profile-report (n &optional reset)
	(when (defq service (profile-service))
		(profile-ipc service (profile-info n)))
	(if reset (setq *profile_map* (env 101))))

(defun profile-instrument (name form)
	(static-qq
		((cond
			((defq __e__ (get ',name *profile_map*))
				(elem-set +prof_cnt __e__ (inc (elem-get +prof_cnt __e__)))
				(elem-set +prof_rcnt __e__ (inc (elem-get +prof_rcnt __e__)))
				(if (= (elem-get +prof_rcnt __e__) 1) (push __e__ (pii-time))))
			(:t (def *profile_map* ',name (setq __e__ (list 1 1 0 (pii-time))))))
		(defq __r__ (progn ~form))
		(elem-set +prof_rcnt __e__ (dec (elem-get +prof_rcnt __e__)))
		(if (= (elem-get +prof_rcnt __e__) 0) (push __e__ (+ (- (pii-time) (pop __e__)) (pop __e__))))
		__r__)))

(defmacro defun (n a &rest _)
	(static-qq (defq ,n (lambda ,a ~(macroexpand
		(profile-instrument n _))))))

(defmacro defmethod (n a &rest _)
	(static-qq (def (def? :vtable this) ,n (lambda (this ~a) ~(macroexpand
		(profile-instrument (sym (cat *class* " " n)) _))))))

;module
(export-symbols '(defun defmethod profile-report))
(env-pop)
