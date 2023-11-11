;;;;;;;;;;;;;;;;;;
; farm task object
;;;;;;;;;;;;;;;;;;

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

(defclass Farm (_create _destroy size) (Fmap)
	; (Farm fnc_create fnc_destroy size) -> farm
	(def this :create _create :destroy _destroy :key -1)
	(defq _nodes (map cat (lisp-nodes)) _now (pii-time))
	(times size (start))

	(defmethod :refresh (&optional _timeout)
		; (. farm :refresh [timeout]) -> :t | :nil
		;scan known nodes and update map
		(defq _nodes (map cat (lisp-nodes))
			_old_keys (list) _old_vals (list)
			_create (get :create this) _destroy (get :destroy this)
			_mutated :nil _now (pii-time))
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
		; (. farm :restart key val)
		;restart task
		(defq _nodes (map cat (lisp-nodes))
			_create (get :create this) _destroy (get :destroy this)
			_mutated :nil _now (pii-time))
		(restart))

	(defmethod :close ()
		; (. farm :close)
		;close tasks
		(.-> this (:each (get :destroy this)) :empty))
	)

;module
(export-classes '(Farm))
(env-pop)
