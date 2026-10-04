(include "././list/class.inc")
(include "././heap/class.inc")

;;;;;;;;;;;;;;;;;;;
; mail static class
;;;;;;;;;;;;;;;;;;;

;a node's entry in the node map is an array, these are its slots. The time
;is when the node is purged, if not heard from before then. The
;system is the id of the machine the node is on, 0 till its ping is heard. The
;routes to the node, the node ids of the peers to send by, follow the hops.
(defq +node_set 0 +node_time 1 +node_session_cluster 2 +node_session_sys 3
	+node_system1 4 +node_system2 5 +node_hops 6 +node_vias 7)

(def-class :sys_mail :nil
	(dec-method :statics_init sys/mail/statics_init :static)
	(dec-method :statics_init1 sys/mail/statics_init1 :static)
	(dec-method :alloc_mbox sys/mail/alloc_mbox :static :nil (:r0 :r1))
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
	(dec-method :mymail sys/mail/mymail :static :nil (:r0 :r1))
	(dec-method :service sys/mail/service :static (:r0 :r1 :r2) (:r0))
	(dec-method :ping sys/mail/ping :static (:r0))
	(dec-method :kick sys/mail/kick :static)
	(dec-method :declare sys/mail/declare :static (:r0 :r1 :r2) (:r0))
	(dec-method :forget sys/mail/forget :static (:r0))
	(dec-method :enquire sys/mail/enquire :static (:r0) (:r0))
	(dec-method :devices sys/mail/devices :static () (:r0))
	(dec-method :junk_mail sys/mail/junk_mail :static (:r3))
	(dec-method :in sys/mail/in :static (:r0 :r1 :r2 :r3))
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
	(dec-method :lisp_netid sys/mail/lisp_netid :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_alloc_mbox sys/mail/lisp_alloc_mbox :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_free_mbox sys/mail/lisp_free_mbox :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_timeout sys/mail/lisp_timeout :static (:r0 :r1) (:r0 :r1)))

;must be power 2
;the ping backs off, from the min period to the max, till a kick starts it
;again. A kick is a ping from a node that saw a link come up or go down.
;Each node answers one at some time within the spread, which is the stagger
;for each node it knows of, and no less than the min stagger, so the rate
;of the answers does not grow with the network. A node not heard from in
;the spread plus the window is purged. A node is also purged if not heard
;from in twice the period its last ping gave, plus the slack. An emulated
;node runs some 10 times slower, so the window and the slack are longer.
(defcvar 'ping_period_min 1000000 'ping_period_max 64000000 'ping_tick 100000
	'ping_stagger 50000 'ping_stagger_min 1000000
	'ping_window (if (eql *cpu* 'vp64) 30000000 3000000)
	'ping_slack (if (eql *cpu* 'vp64) 20000000 2000000) 'ping_stall 1000000)
;how late a node is in running a task that is ready counts as load, one
;more task for each this many us. See sys/statics/class.inc.
(defcvar 'lag_per_task 32000)
(defcvar 'num_mbox_buckets 8)

;useful helpers for net_id and node_id

(defun load-node-id (src so &optional t1 t2)
	(setd t1 :r3 t2 :r4)
	(load-fields src (list (+ so +node_id_node1) (+ so +node_id_node2)) (list t1 t2)))

(defun save-node-id (dst do &optional t1 t2)
	(setd t1 :r3 t2 :r4)
	(save-fields dst (list (+ do +node_id_node1) (+ do +node_id_node2)) (list t1 t2)))

(defun assign-node-id (src so dst do &optional t1 t2)
	(load-node-id src so t1 t2)
	(save-node-id dst do t1 t2))

(defun load-net-id (src so &optional t1 t2 t3)
	(setd t1 :r2 t2 :r3 t3 :r4)
	(load-fields src
		(list (+ so +net_id_mbox_id) (+ so +net_id_node_id +node_id_node1) (+ so +net_id_node_id +node_id_node2))
		(list t1 t2 t3)))

(defun save-net-id (dst do &optional t1 t2 t3)
	(setd t1 :r2 t2 :r3 t3 :r4)
	(save-fields dst
		(list (+ do +net_id_mbox_id) (+ do +net_id_node_id +node_id_node1) (+ do +net_id_node_id +node_id_node2))
		(list t1 t2 t3)))

(defun assign-net-id (src so dst do &optional t1 t2 t3)
	(load-net-id src so t1 t2 t3)
	(save-net-id dst do t1 t2 t3))
