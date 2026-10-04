;;;;;;;;;;;;;;;;;;;;
; local tasks object
;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun stop ()
	;stop a child
	(callback destroy e key val)
	(. this :erase key))

(defun start ()
	;start a child, on the worker nodes in turn
	(defq key (def this :key (inc (get :key this))) val (env 1))
	(. this :insert key val)
	(def val :timestamp now)
	(callback create e key val
		(list (elem-get worker_nodes (% key (length worker_nodes))))))

(defun restart ()
	;restart a child
	(setq mutated :t)
	(stop) (start))

(defun grow ()
	;start children till the herd is the size the worker nodes call for
	(defq want (max 1 (min herd_max
		(+ herd_init (* herd_growth (dec (length worker_nodes)))))))
	(while (< (. this :size) want)
		(setq mutated :t)
		(start)))

(defclass Local (create destroy &optional herd_max herd_init herd_growth) (Fmap)
	; (Local fnc_create fnc_destroy [herd_max herd_init herd_growth]) -> local
	;a herd of worker tasks on the nodes of this machine, those that share
	;its file system. The herd is herd_init workers, and herd_growth more
	;for each node other than this one, and no more than herd_max. They are
	;started at once, on each node in turn.
	(setd herd_max +max_int herd_init 1 herd_growth 1)
	(lower :create :destroy :herd_max :herd_init :herd_growth (:key -1))
	(defq worker_nodes (lisp-nodes :t) mutated :nil now (pii-time) e (penv))
	(grow)

	(defmethod :refresh (&optional timeout)
		; (. local :refresh [timeout]) -> :t | :nil
		;scan known nodes and update map
		(raise :create :destroy :herd_max :herd_init :herd_growth
			(network_nodes (lisp-nodes) worker_nodes (lisp-nodes :t)
			old_keys (list) old_vals (list) mutated :nil now (pii-time) e (penv)))
		(. this :each (# (push old_keys %0) (push old_vals %1)))
		;test for vanished and timeout nodes
		(each (lambda (key val)
			(cond
				((or (not (defq child (get :child val)))
					(find (task-nodeid child) network_nodes))
					;waiting on child launching, or found child in network_nodes
					;so check timestamp
					(and timeout
						(defq then (get :timestamp val))
						(> (- now then) timeout)
						(restart)))
				(:t ;not found child node on network !
					(restart)))) old_keys old_vals)
		;more nodes may have joined
		(grow)
		mutated)

	(defmethod :restart (key val)
		; (. local :restart key val) -> local
		;restart task
		(raise :create :destroy
			(worker_nodes (lisp-nodes :t) mutated :nil now (pii-time) e (penv)))
		(restart) this)

	(defmethod :close ()
		; (. local :close) -> local
		;close tasks
		(raise :destroy (e (penv)))
		(.-> this (:each (lambda (key val) (callback destroy e key val))) :empty))
	)

;module
(export-classes '(Local))
(env-pop)
