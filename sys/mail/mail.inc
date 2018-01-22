(import 'sys/class.inc)
(import 'sys/code.inc)
(import 'sys/list/list.inc)
(import 'sys/heap/heap.inc)

;;;;;;;;;;;;;;;;;;;
; mail static class
;;;;;;;;;;;;;;;;;;;

(def-class 'sys_mail)
(def-method 'statics 'sys/mail/statics)
(def-method 'init 'sys/mail/init 'static '(r1))
(def-method 'alloc 'sys/mail/alloc 'static '(r0) '(r0 r1))
(def-method 'free 'sys/mail/free 'static '(r0))
(def-method 'alloc_obj 'sys/mail/alloc_obj 'static '(r0 r1 r2) '(r0))
(def-method 'free_obj 'sys/mail/free_obj 'static '(r0) '(r0 r1 r2))
(def-method 'send 'sys/mail/send 'static '(r0))
(def-method 'read 'sys/mail/read 'static '(r0) '(r0 r1))
(def-method 'try_read 'sys/mail/try_read 'static '(r0) '(r0 r1))
(def-method 'select 'sys/mail/select 'static '(r0 r1) '(r0))
(def-method 'mymail 'sys/mail/mymail 'static nil '(r0 r1))
(def-method 'init_mailbox 'sys/mail/init_mailbox 'static '(r0) '(r0))
(def-method 'in 'sys/mail/in 'static '(r0))
(def-method 'out 'sys/mail/out)

(def-method 'lisp_mymail 'sys/mail/lisp_mymail 'static '(r0 r1) '(r0 r1))
(def-method 'lisp_send 'sys/mail/lisp_send 'static '(r0 r1) '(r0 r1))

;;;;;;;;;;;;;;;;;
; mail structures
;;;;;;;;;;;;;;;;;

(def-struct 'mailbox 'lh_list)
	(struct 'parcel_list 'lh_list)
	(ptr 'tcb)
(def-struct-end)

(def-struct 'id)
	(ulong 'mbox)
	(ulong 'cpu)
(def-struct-end)

(def-struct 'msg 'ln_node)
	(uint 'length)
	(uint 'parcel_length)
	(uint 'parcel_offset)
	(uint 'parcel_id)
	(ulong 'parcel_src)
	(struct 'dest 'id)
	(offset 'stamp_size)
	(ptr 'parcel_obj)
	(ptr 'parcel_data)
(def-struct-end)

(def-struct 'ml_statics)
	(struct 'offchip_list 'lh_list)
	(struct 'msg_heap 'hp_heap)
	(ptr 'kernel_mailbox)
	(ptr 'out_mailbox)
	(ptr 'links_array)
	(uint 'parcel_id)
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
