(import 'class/obj/obj.inc)
(import 'sys/mail/mail.inc)

(def-class 'mailbox 'obj)
(def-method 'create 'class/mailbox/create 'static '() '(r0))
(def-method 'new 'class/mailbox/new 'static nil '(r0))
(def-method 'init 'class/mailbox/init 'static '(r0 r1))
(def-method 'read 'class/mailbox/read 'static '(r0) '(r0 r1 r2))

(def-method 'deinit 'class/mailbox/deinit 'override '(r0) '(r0))

(def-struct 'mailbox 'obj)
	(struct 'mbox 'mbox)
(def-struct-end)

;;;;;;;;;;;;;;;
;inline methods
;;;;;;;;;;;;;;;

(defcfun class/mailbox/init ()
	;inputs
	;r0 = mailbox object (ptr)
	;r1 = vtable (pptr)
	;outputs
	;r0 = mailbox object (ptr)
	;r1 = 0 if error, else ok
	(vp-lea-i r0 mailbox_mbox r2)
	(ml-init r2 r3 r4)
	(s-call 'mailbox 'init '(r0 r1) '(r0 r1)))
