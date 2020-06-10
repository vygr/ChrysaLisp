(include 'sys/list/class.inc)
(include 'sys/heap/class.inc)

;;;;;;;;;;;;;;;;;;;
; mail static class
;;;;;;;;;;;;;;;;;;;

(def-class 'sys_mail)
(dec-method :statics 'sys/mail/statics)
(dec-method :statics_init 'sys/mail/statics_init :static)
(dec-method :statics_init1 'sys/mail/statics_init1 :static)
(dec-method :alloc_mbox 'sys/mail/alloc_mbox :static nil '(r0 r1))
(dec-method :free_mbox 'sys/mail/free_mbox :static '(r0))
(dec-method :mbox_addr 'sys/mail/mbox_addr :static '(r0) '(r0))
(dec-method :alloc 'sys/mail/alloc :static '(r0) '(r0 r1))
(dec-method :free 'sys/mail/free :static '(r0))
(dec-method :alloc_obj 'sys/mail/alloc_obj :static '(r0 r1 r2) '(r0))
(dec-method :free_obj 'sys/mail/free_obj :static '(r0) '(r0 r1 r2))
(dec-method :send 'sys/mail/send :static '(r0))
(dec-method :read 'sys/mail/read :static '(r0) '(r0 r1))
(dec-method :poll 'sys/mail/poll :static '(r0) '(r0 r4 r5))
(dec-method :select 'sys/mail/select :static '(r0) '(r0))
(dec-method :mymail 'sys/mail/mymail :static nil '(r0 r1))
(dec-method :declare 'sys/mail/declare :static '(r0 r1))
(dec-method :enquire 'sys/mail/enquire :static '(r0) '(r0))
(dec-method :forget 'sys/mail/forget :static '(r0))
(dec-method :junk_mail 'sys/mail/junk_mail :static '(r3))
(dec-method :in 'sys/mail/in :static '(r0))
(dec-method :out 'sys/mail/out)

(dec-method :lisp_read 'sys/mail/lisp_read :static '(r0 r1) '(r0 r1))
(dec-method :lisp_poll 'sys/mail/lisp_poll :static '(r0 r1) '(r0 r1))
(dec-method :lisp_select 'sys/mail/lisp_select :static '(r0 r1) '(r0 r1))
(dec-method :lisp_send 'sys/mail/lisp_send :static '(r0 r1) '(r0 r1))
(dec-method :lisp_declare 'sys/mail/lisp_declare :static '(r0 r1) '(r0 r1))
(dec-method :lisp_enquire 'sys/mail/lisp_enquire :static '(r0 r1) '(r0 r1))
(dec-method :lisp_alloc_mbox 'sys/mail/lisp_alloc_mbox :static '(r0 r1) '(r0 r1))
(dec-method :lisp_free_mbox 'sys/mail/lisp_free_mbox :static '(r0 r1) '(r0 r1))

;;;;;;;;;;;;;;;;;
; mail structures
;;;;;;;;;;;;;;;;;

(def-struct 'id)
	(union
		(ulong 'id)
		(uint 'mbox 'cpu))
(def-struct-end)

(def-struct 'mbox 'ln_node)
	(struct 'list 'lh_list)
	(ptr 'tcb)
	(uint 'id)
(def-struct-end)

(def-struct 'msg 'ln_node)
	(uint 'length 'parcel_length 'parcel_offset 'parcel_id)
	(ulong 'parcel_src)
	(struct 'dest 'id)
	(offset 'stamp_size)
	(ptr 'parcel_obj 'parcel_data)
(def-struct-end)

;must be power 2
(defcvar 'num_mbox_buckets 8)

(def-struct 'ml_statics)
	(uint 'parcel_id 'next_mbox_id)
	(ptr 'links_array 'declare_map 'out_mbox_addr)
	(struct 'msg_heap 'hp_heap)
	(struct 'mbox_heap 'hp_heap)
	(struct 'offchip_list 'lh_list)
	(struct 'parcel_list 'lh_list)
	(struct 'mbox_buckets (* num_mbox_buckets lh_list_size))
(def-struct-end)
