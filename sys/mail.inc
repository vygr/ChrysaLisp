(import 'sys/class.inc)
(import 'sys/code.inc)
(import 'sys/list.inc)
(import 'sys/heap.inc)

;;;;;;;;;;;;;;;;;;;
; mail static class
;;;;;;;;;;;;;;;;;;;

(def-class 'sys_mail)
(def-method 'statics 'sys/mail_statics)
(def-method 'init 'sys/mail_init 'static '(r1))
(def-method 'deinit 'sys/mail_deinit)
(def-method 'alloc 'sys/mail_alloc 'static nil '(r0))
(def-method 'alloc_parcel 'sys/mail_alloc_parcel 'static '(r0) '(r0))
(def-method 'send 'sys/mail_send 'static '(r0))
(def-method 'read 'sys/mail_read 'static '(r0) '(r0))
(def-method 'try_read 'sys/mail_try_read 'static '(r0) '(r0))
(def-method 'select 'sys/mail_select 'static '(r0 r1) '(r0))
(def-method 'mymail 'sys/mail_mymail 'static nil '(r0))
(def-method 'init_mailbox 'sys/mail_init_mailbox 'static '(r0) '(r0))
(def-method 'in 'sys/mail_in)
(def-method 'out 'sys/mail_out)

;;;;;;;;;;;;;;;;;
; mail structures
;;;;;;;;;;;;;;;;;

(defcvar 'mail_data_size 512)

(def-struct 'mailbox 'lh_list)
	(struct 'parcel_list 'lh_list)
	(ptr 'tcb)
(def-struct-end)

(def-struct 'id)
	(ulong 'mbox)
	(ulong 'cpu)
(def-struct-end)

(def-struct 'msg 'ln_node)
	(ulong 'length)
	(struct 'dest 'id)
	(struct 'parcel_id 'id)
	(ulong 'parcel_size)
	(ulong 'parcel_total)
	(ulong 'parcel_frag)
	(offset 'header_size)
	(struct 'data 'mail_data)
(def-struct-end)

(def-struct 'ml_statics)
	(struct 'offchip_list 'lh_list)
	(ptr 'kernel_mailbox)
	(ptr 'in_mailbox)
	(ptr 'out_mailbox)
	(ulong 'parcel_id)
(def-struct-end)

;;;;;;;;;;;;;
; mail macros
;;;;;;;;;;;;;

(defcfun ml-init (_1 _2 _3)
	;inputs
	;_1 = mailbox
	;_2 = temp
	;_3 = temp
	;outputs
	;_1 = mailbox
	;trashes
	;_2, _3

	(vp-xor-rr _2 _2)
	(vp-cpy-ri _2 _1 mailbox_tcb)
	(lh-init _1 _2)
	(vp-lea-i _1 mailbox_parcel_list _2)
	(lh-init _2 _3))

(defcfun ml-temp-create (_1 _2)
	;inputs
	;_1 = temp
	;_2 = temp
	;outputs
	;rsp = mailbox
	;trashes
	;_1, _2

	(vp-alloc mailbox_size)
	(ml-init rsp _1 _2))

(defcfun ml-temp-destroy ()
	(vp-free mailbox_size))
