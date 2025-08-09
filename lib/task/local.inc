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
	;start a child
	(defq key (def this :key (inc (get :key this))) val (env 1))
	(. this :insert key val)
	(def val :timestamp now)
	(callback create e key val nodes))

(defun restart ()
	;restart a child
	(setq mutated :t)
	(stop) (start))

(defclass Local (create destroy &optional herd_max herd_init herd_growth) (Fmap)
	; (Local fnc_create fnc_destroy [herd_max herd_init herd_growth]) -> local
	(setd herd_max +max_int herd_init 1 herd_growth 1)
	(lower :create :destroy :herd_max :herd_growth
		(:key -1 :nodes (list (task-nodeid))))
	(defq nodes (get :nodes this) now (pii-time) e (penv))
	(times (min herd_max (max herd_init 1)) (start))

	(defmethod :refresh (&optional timeout)
		; (. local :refresh [timeout]) -> :t | :nil
		;scan known nodes and update map
		(defq known_nodes (map (const cat) (mail-nodes))
			nodes (filter (# (find %0 known_nodes)) (get :nodes this))
			old_keys (list) old_vals (list)
			create (get :create this) destroy (get :destroy this)
			mutated :nil now (pii-time) e (penv))
		(def this :nodes nodes)
		(. this :each (lambda (key val) (push old_keys key) (push old_vals val)))
		;test for vanished and timeout nodes
		(each (lambda (key val)
			(cond
				((or (not (defq child (get :child val)))
					(find (slice child +mailbox_id_size -1) nodes))
					;waiting on child launching, or found child in nodes, so check timestamp
					(and timeout
						(defq then (get :timestamp val))
						(> (- now then) timeout)
						(restart)))
				(:t ;not found child
					(restart)))) old_keys old_vals)
		mutated)

	(defmethod :restart (key val)
		; (. local :restart key val)
		;restart task
		(raise :nodes :create :destroy (mutated :nil now (pii-time) e (penv)))
		(restart))

	(defmethod :add_node (node)
		; (. local :add_node node)
		;add new node
		(raise :nodes :herd_max)
		(unless (find node nodes)
			(lower (:nodes (push nodes node)))
			(when (> (-- herd_max (length nodes)) 0)
				(raise :create :herd_growth (now (pii-time) e (penv)))
				(times (max 1 (min herd_max herd_growth)) (start)))))

	(defmethod :close ()
		; (. local :close)
		;close tasks
		(raise :destroy (e (penv)))
		(.-> this (:each (lambda (key val) (callback destroy e key val))) :empty))
	)

;module
(export-classes '(Local))
(env-pop)
