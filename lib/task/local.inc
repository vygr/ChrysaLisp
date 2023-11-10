;;;;;;;;;;;;;;;;;;;;
; local tasks object
;;;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")

;module
(env-push)

(defun stop ()
	;stop a child
	(_destroy _key _val)
	(. this :erase _key))

(defun start ()
	;start a child
	(defq _key (def this :key (inc (get :key this))) _val (env 1))
	(. this :insert _key _val)
	(def _val :timestamp _now)
	(_create _key _val _nodes))

(defun restart ()
	;restart a child
	(setq _mutated :t)
	(stop) (start))

(defclass Local (_create _destroy &optional max_size init_size size) (Fmap)
	; (Local fnc_create fnc_destroy &optional max_size init_size size) -> local
	(setd max_size +max_int init_size 1 size 1)
	(def this :create _create :destroy _destroy
		:nodes (list (task-nodeid)) :key -1 :size size :max_size max_size)
	(defq _nodes (get :nodes this) _now (pii-time))
	(times (min max_size (max init_size 1)) (start))

	(defmethod :refresh (&optional _timeout)
		; (. local :refresh [timeout]) -> :t | :nil
		;scan known nodes and update map
		(defq _known_nodes (map cat (mail-nodes))
			_nodes (filter (# (find %0 _known_nodes)) (get :nodes this))
			_old_keys (list) _old_vals (list)
			_create (get :create this) _destroy (get :destroy this)
			_mutated :nil _now (pii-time))
		(def this :nodes _nodes)
		(. this :each (lambda (key val) (push _old_keys key) (push _old_vals val)))
		;test for vanished and timeout nodes
		(each (lambda (_key _val)
			(cond
				((or (not (defq _child (get :child _val)))
					(find (slice +mailbox_id_size -1 _child) _nodes))
					;waiting on child launching, or found child in nodes, so check timestamp
					(and _timeout
						(defq _then (get :timestamp _val))
						(> (- _now _then) _timeout)
						(restart)))
				(:t ;not found child
					(restart)))) _old_keys _old_vals)
		_mutated)

	(defmethod :restart (_key _val)
		; (. local :restart key val)
		;restart task
		(defq _nodes (get :nodes this) _mutated :nil _now (pii-time)
			_create (get :create this) _destroy (get :destroy this))
		(restart))

	(defmethod :add_node (node)
		; (. local :add_node node)
		;add new node
		(defq _nodes (get :nodes this)
			_headroom (- (get :max_size this) (length _nodes)))
		(unless (or (<= _headroom 0) (find node _nodes))
			(def this :nodes (push _nodes node))
			(defq _now (pii-time) _create (get :create this))
			(times (min (get :size this) _headroom) (start))))

	(defmethod :close ()
		; (. local :close)
		;close tasks
		(.-> this (:each (get :destroy this)) :empty))
	)

;module
(export-classes '(Local))
(env-pop)
