;;;;;;;;;;;;;;;;;;
; farm task object
;;;;;;;;;;;;;;;;;;

;Children started on the nodes, kept, and started again when one has
;gone. Which nodes a child is started on is the farm's to say, (:nodes)
;and (:place key nodes), and what it does after a refresh, (:grown): a
;Farm asks any node that has not refused, lib/task/local.inc keeps a herd
;on the nodes of this machine and grows it as they join.

;module
(env-push)

(defun stop ()
	;stop a child
	(callback destroy e key val)
	(. this :erase key))

(defun start ()
	;start a child, where the farm says
	(defq key (def this :key (inc (get :key this))) val (env 1))
	(. this :insert key val)
	(def val :timestamp now)
	(callback create e key val (. this :place key network_nodes)))

(defun restart ()
	;restart a child
	(setq mutated :t)
	(stop) (start))

(defun nodes-to-ask (refused)
	; (nodes-to-ask refused) -> nodes
	;the nodes there are, less those that could not start the task. If
	;that is every one of them, the nodes of this machine: a task that no
	;node can start is asked for there, as it was, and not of nobody
	(if (empty? (defq nodes (filter (# (not (find %0 refused))) (lisp-nodes))))
		(lisp-nodes :t) nodes))

(defclass Farm (create destroy size) (Fmap)
	; (Farm fnc_create fnc_destroy size) -> farm
	;the nodes that were asked for a task and could not start it. They are
	;not asked again while this farm lasts: a node need not be one that
	;runs what it is sent, see docs/ai_digest/alien_nodes.md
	(def this :create create :destroy destroy :key -1 :refused (list))
	; merge local node to bypass eventual consistency delay
	(defq network_nodes (lisp-nodes) now (pii-time) e (penv))
	(times size (start))

	(defmethod :nodes ()
		; (. farm :nodes) -> nodes
		;the nodes a child may be started on, all those that have not refused
		(nodes-to-ask (get :refused this)))

	(defmethod :place (key nodes)
		; (. farm :place key nodes) -> nodes
		;where the child of key is asked for, any of them
		nodes)

	(defmethod :grown ()
		; (. farm :grown) -> farm
		;after a refresh, a farm stays the size it was made
		this)

	(defmethod :grow_to (want)
		; (. farm :grow_to want) -> farm
		;children started till there are want
		(while (< (. this :size) want)
			(setq mutated :t)
			(start))
		this)

	(defmethod :refresh (&optional timeout)
		; (. farm :refresh [timeout]) -> :t | :nil
		;scan known nodes and update map, merge local node here as well
		;a child on a node that refused is not there to be found, it is
		;all the nodes there are that a child is looked for in
		(defq all_nodes (lisp-nodes) network_nodes (. this :nodes)
			old_keys (list) old_vals (list)
			create (get :create this) destroy (get :destroy this)
			mutated :nil now (pii-time) e (penv))
		(. this :each (# (push old_keys %0) (push old_vals %1)))
		;test for vanished and timeout nodes
		(each (lambda (key val)
			(cond
				((or (not (defq child (get :child val)))
					(find (task-nodeid child) all_nodes))
					;waiting on child launching, or found child in network_nodes
					;so check timestamp
					(and timeout
						(defq then (get :timestamp val))
						(> (- now then) timeout)
						(restart)))
				(:t ;not found child
					(restart)))) old_keys old_vals)
		(. this :grown)
		mutated)

	(defmethod :restart (key val)
		; (. farm :restart key val) -> farm
		;restart task
		(defq network_nodes (. this :nodes) mutated :nil now (pii-time) e (penv)
			create (get :create this) destroy (get :destroy this))
		(restart) this)

	(defmethod :reply (msg)
		; (. farm :reply msg) -> :nil | val
		;what a kernel answers when it is asked to start a child. If it
		;did, the entry of that child, to keep the id in. If it could not,
		;the id is 0: that node is not asked again, and the child is asked
		;for at once of another, if there is one that has not refused
		(defq key (getf msg +kn_msg_key) val (. this :find key))
		(cond
			((/= (get-long (getf msg +kn_msg_reply_id) 0) 0) val)
			((>= (length msg) +kn_msg_reply_size)
				;the answer says who could not, an id of 0 does not
				(defq node (getf msg +kn_msg_reply_node) refused (get :refused this))
				(unless (find node refused) (push refused (cat node)))
				(and val (some (# (not (find %0 refused))) (lisp-nodes))
					(. this :restart key val))
				:nil)))

	(defmethod :refused ()
		; (. farm :refused) -> nodes
		;the nodes that could not start a child
		(get :refused this))

	(defmethod :close ()
		; (. farm :close) -> farm
		;close tasks
		(defq e (penv) destroy (get :destroy this))
		(.-> this (:each (lambda (key val) (callback destroy e key val))) :empty))
	)

;module
(export-classes '(Farm))
(env-pop)
