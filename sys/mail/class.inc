(include 'sys/list/class.inc)
(include 'sys/heap/class.inc)

;;;;;;;;;;;;;;;;;;;
; mail static class
;;;;;;;;;;;;;;;;;;;

(def-class 'sys_mail)
(dec-method 'statics 'sys/mail/statics)
(dec-method 'statics_init 'sys/mail/statics_init 'static)
(dec-method 'statics_init1 'sys/mail/statics_init1 'static)
(dec-method 'alloc_mbox 'sys/mail/alloc_mbox 'static nil '(r0 r1))
(dec-method 'free_mbox 'sys/mail/free_mbox 'static '(r0))
(dec-method 'mbox_addr 'sys/mail/mbox_addr 'static '(r0) '(r0))
(dec-method 'alloc 'sys/mail/alloc 'static '(r0) '(r0 r1))
(dec-method 'free 'sys/mail/free 'static '(r0))
(dec-method 'alloc_obj 'sys/mail/alloc_obj 'static '(r0 r1 r2) '(r0))
(dec-method 'free_obj 'sys/mail/free_obj 'static '(r0) '(r0 r1 r2))
(dec-method 'send 'sys/mail/send 'static '(r0))
(dec-method 'read 'sys/mail/read 'static '(r0) '(r0 r1))
(dec-method 'poll 'sys/mail/poll 'static '(r0) '(r0 r1 r2 r3))
(dec-method 'select 'sys/mail/select 'static '(r0) '(r0))
(dec-method 'mymail 'sys/mail/mymail 'static nil '(r0 r1))
(dec-method 'declare 'sys/mail/declare 'static '(r0 r1))
(dec-method 'enquire 'sys/mail/enquire 'static '(r0) '(r0))
(dec-method 'forget 'sys/mail/forget 'static '(r0))
(dec-method 'in 'sys/mail/in 'static '(r0))
(dec-method 'out 'sys/mail/out)

(dec-method 'lisp_read 'sys/mail/lisp_read 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_poll 'sys/mail/lisp_poll 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_select 'sys/mail/lisp_select 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_send 'sys/mail/lisp_send 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_declare 'sys/mail/lisp_declare 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_enquire 'sys/mail/lisp_enquire 'static '(r0 r1) '(r0 r1))

;;;;;;;;;;;;;;;;;
; mail structures
;;;;;;;;;;;;;;;;;

(def-struct 'mbox 'lh_list)
	(struct 'parcel_list 'lh_list)
	(ptr 'tcb)
(def-struct-end)

(def-struct 'id)
	(union
		'(ulong 'id)
		'(uint 'mbox 'cpu))
(def-struct-end)

(def-struct 'msg 'ln_node)
	(uint 'length 'parcel_length 'parcel_offset 'parcel_id)
	(ulong 'parcel_src)
	(struct 'dest 'id)
	(offset 'stamp_size)
	(ptr 'parcel_obj 'parcel_data)
(def-struct-end)

(def-struct 'ml_statics)
	(struct 'offchip_list 'lh_list)
	(struct 'msg_heap 'hp_heap)
	(struct 'mbox_heap 'hp_heap)
	(ptr 'mbox_array 'mbox_free_array 'links_array 'declare_map 'out_mbox_addr)
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
	(vp-cpy-ri _2 _1 mbox_tcb)
	(lh-init _1 _2)
	(vp-lea-i _1 mbox_parcel_list _2)
	(lh-init _2 _3))
