;;;;;;;;;;;;;;;;;;;;
; global task object
;;;;;;;;;;;;;;;;;;;;

(defclass Global (fnc_create fnc_destroy) (Fmap)
	; (Global fnc_create fnc_destroy) -> global
	;the nodes that were asked for a task and could not start it. They are
	;not asked again: a node need not be one that runs what it is sent, see
	;docs/ai_digest/alien_nodes.md
	(def this :create fnc_create :destroy fnc_destroy :refused (list))

	(defmethod :refresh (&optional timeout)
		; (. global :refresh [timeout]) -> :t | :nil
		;scan known nodes and update node map
		;merge local node to bypass eventual consistency UI lag
		(defq refused (get :refused this)
			network_nodes (filter (# (not (find %0 refused))) (lisp-nodes))
			old_keys (list) old_vals (list)
			create (get :create this) destroy (get :destroy this)
			mutated :nil now (pii-time) e (penv))
		(. this :each (lambda (key val) (push old_keys key) (push old_vals val)))
		;test for vanished and timeout nodes
		(each (lambda (key val)
			(cond
				((find key network_nodes)
					;found, so check timestamp
					(and timeout
						(defq then (get :timestamp val))
						(> (- now then) timeout)
						(progn
							(setq mutated :t)
							(callback destroy e key val)
							(. this :insert key (create key now)))))
				(:t ;not found
					(setq mutated :t)
					(callback destroy e key val)
					(. this :erase key)))) old_keys old_vals)
		;test for new nodes
		(each (lambda (key)
			(unless (find key old_keys)
				(setq mutated :t)
				;must (cat key) to convert to pure string key !
				(. this :insert (cat key) (callback create e key now)))) network_nodes)
		mutated)

	(defmethod :reply (msg)
		; (. global :reply msg) -> :nil | val
		;what a kernel answers when it is asked to start the task of a
		;node. If it did, the entry of that node, to keep the id in. If it
		;could not, the id is 0, the entry is destroyed, and that node is
		;not asked again
		(defq child (getf msg +kn_msg_reply_id))
		(cond
			((/= (get-long child 0) 0)
				(. this :find (slice child +long_size -1)))
			((>= (length msg) +kn_msg_reply_size)
				;the answer says who could not, an id of 0 does not
				(defq key (getf msg +kn_msg_reply_node) refused (get :refused this))
				(unless (find key refused) (push refused (cat key)))
				(when (defq val (. this :find key))
					(defq e (penv) destroy (get :destroy this))
					(callback destroy e key val)
					(. this :erase key))
				:nil)))

	(defmethod :refused ()
		; (. global :refused) -> nodes
		;the nodes that could not start the task
		(get :refused this))

	(defmethod :close ()
		; (. global :close) -> global
		;close tasks
		(defq e (penv) destroy (get :destroy this))
		(.-> this (:each (lambda (key val) (callback destroy e key val))) :empty))
	)
