(include "sys/list/class.inc")
(include "sys/heap/class.inc")

;;;;;;;;;;;;;;;;;;;
; mail static class
;;;;;;;;;;;;;;;;;;;

(def-class sys_mail nil
	(dec-method :statics_init sys/mail/statics_init :static)
	(dec-method :statics_init1 sys/mail/statics_init1 :static)
	(dec-method :alloc_mbox sys/mail/alloc_mbox :static nil (:r0 :r1))
	(dec-method :free_mbox sys/mail/free_mbox :static (:r0))
	(dec-method :validate sys/mail/validate :static (:r0) (:r0))
	(dec-method :alloc sys/mail/alloc :static (:r0) (:r0 :r1))
	(dec-method :free sys/mail/free :static (:r0))
	(dec-method :alloc_obj sys/mail/alloc_obj :static (:r0 :r1 :r2) (:r0))
	(dec-method :free_obj sys/mail/free_obj :static (:r0) (:r0 :r1 :r2))
	(dec-method :send sys/mail/send :static (:r0))
	(dec-method :read sys/mail/read :static (:r0) (:r0 :r1))
	(dec-method :poll sys/mail/poll :static (:r0) (:r0 :r4 :r5))
	(dec-method :select sys/mail/select :static (:r0) (:r0))
	(dec-method :mymail sys/mail/mymail :static nil (:r0 :r1))
	(dec-method :service sys/mail/service :static (:r0 :r1 :r2) (:r0))
	(dec-method :ping sys/mail/ping :static)
	(dec-method :declare sys/mail/declare :static (:r0 :r1 :r2) (:r0))
	(dec-method :forget sys/mail/forget :static (:r0))
	(dec-method :enquire sys/mail/enquire :static (:r0) (:r0))
	(dec-method :devices sys/mail/devices :static () (:r0))
	(dec-method :junk_mail sys/mail/junk_mail :static (:r3))
	(dec-method :in sys/mail/in :static (:r0))
	(dec-method :out sys/mail/out)
	(dec-method :ready sys/mail/ready :static (:r0 :r1 :r2) (:r0))
	(dec-method :lisp_validate sys/mail/lisp_validate :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_read sys/mail/lisp_read :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_poll sys/mail/lisp_poll :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_select sys/mail/lisp_select :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_send sys/mail/lisp_send :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_declare sys/mail/lisp_declare :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_enquire sys/mail/lisp_enquire :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_forget sys/mail/lisp_forget :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_devices sys/mail/lisp_devices :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_alloc_mbox sys/mail/lisp_alloc_mbox :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_free_mbox sys/mail/lisp_free_mbox :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_timeout sys/mail/lisp_timeout :static (:r0 :r1) (:r0 :r1)))

;;;;;;;;;;;;;;;;;
; mail structures
;;;;;;;;;;;;;;;;;

(def-struct node_id 0
	(ulong node1 node2))

(def-struct net_id 0
	(ulong mbox_id)
	(struct node_id node_id_size))

(def-struct mbox ln_node_size
	(struct list lh_list_size)
	(ptr tcb)
	(uint id))

(def-struct msg ln_node_size
	(ulong timestamp)
	(offset stamp_start)
	(struct dest net_id_size)
	(struct src net_id_size)
	(uint frag_length frag_offset total_length)
	(align)
	(offset stamp_end)
	(ptr obj frag_data))

;must be power 2
(defcvar 'num_mbox_buckets 8)

;usefull helpers for net_id and node_id

(defun load-node-id (src so &optional t1 t2)
	(setd src :r0 t1 :r3 t2 :r4)
	(vp-cpy-ir src (+ so node_id_node1) t1)
	(vp-cpy-ir src (+ so node_id_node2) t2))

(defun save-node-id (dst do &optional t1 t2)
	(setd dst :r1 t1 :r3 t2 :r4)
	(vp-cpy-ri t1 dst (+ do node_id_node1))
	(vp-cpy-ri t2 dst (+ do node_id_node2)))

(defun assign-node-id (src so dst do &optional t1 t2)
	(load-node-id src so t1 t2)
	(save-node-id dst do t1 t2))

(defun load-net-id (src so &optional t1 t2 t3)
	(setd src :r0 t1 :r2)
	(vp-cpy-ir src (+ so net_id_mbox_id) t1)
	(load-node-id src (+ so net_id_node_id) t2 t3))

(defun save-net-id (dst do &optional t1 t2 t3)
	(setd dst :r1 t1 :r2)
	(vp-cpy-ri t1 dst (+ do net_id_mbox_id))
	(save-node-id dst (+ do net_id_node_id) t2 t3))

(defun assign-net-id (src so dst do &optional t1 t2 t3)
	(load-net-id src so t1 t2 t3)
	(save-net-id dst do t1 t2 t3))
