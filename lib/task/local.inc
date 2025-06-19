;;;;;;;;;;;;;;;;;;;;
; local tasks object
;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun stop ()
	;stop a child
	(eval `(,destroy ',key ',val) e)
	(. this :erase key))

(defun start ()
	;start a child
	(defq key (def this :key (inc (get :key this))) val (env 1))
	(. this :insert key val)
	(def val :timestamp now)
	(eval `(,create ',key ',val ',nodes) e))

(defun restart ()
	;restart a child
	(setq mutated :t)
	(stop) (start))

(defclass Local (create destroy &optional max_size init_size size) (Fmap)
	; (Local fnc_create fnc_destroy [max_size init_size size]) -> local
	(setd max_size +max_int init_size 1 size 1)
	(def this :create create :destroy destroy
		:nodes (list (task-nodeid)) :key -1 :size size :max_size max_size)
	(defq nodes (get :nodes this) now (pii-time) e (penv))
	(times (min max_size (max init_size 1)) (start))

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
		(defq nodes (get :nodes this) mutated :nil now (pii-time)
			create (get :create this) destroy (get :destroy this)
			e (penv))
		(restart))

	(defmethod :add_node (node)
		; (. local :add_node node)
		;add new node
		(defq nodes (get :nodes this) e (penv)
			headroom (- (get :max_size this) (length nodes)))
		(unless (or (<= headroom 0) (find node nodes))
			(def this :nodes (push nodes node))
			(defq now (pii-time) create (get :create this))
			(times (min (get :size this) headroom) (start))))

	(defmethod :close ()
		; (. local :close)
		;close tasks
		(defq e (penv))
		(.-> this (:each (get :destroy this)) :empty))
	)

;module
(export-classes '(Local))
(env-pop)
