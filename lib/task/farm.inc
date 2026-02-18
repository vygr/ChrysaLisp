;;;;;;;;;;;;;;;;;;
; farm task object
;;;;;;;;;;;;;;;;;;

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
	(callback create e key val network_nodes))

(defun restart ()
	;restart a child
	(setq mutated :t)
	(stop) (start))

(defclass Farm (create destroy size) (Fmap)
	; (Farm fnc_create fnc_destroy size) -> farm
	(def this :create create :destroy destroy :key -1)
	(defq network_nodes (map (const cat) (lisp-nodes)) now (pii-time) e (penv))
	(times size (start))

	(defmethod :refresh (&optional timeout)
		; (. farm :refresh [timeout]) -> :t | :nil
		;scan known nodes and update map
		(defq network_nodes (map (const cat) (lisp-nodes))
			old_keys (list) old_vals (list)
			create (get :create this) destroy (get :destroy this)
			mutated :nil now (pii-time) e (penv))
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
				(:t ;not found child
					(restart)))) old_keys old_vals)
		mutated)

	(defmethod :restart (key val)
		; (. farm :restart key val)
		;restart task
		(defq network_nodes (map (const cat) (lisp-nodes))
			create (get :create this) destroy (get :destroy this)
			mutated :nil now (pii-time) e (penv))
		(restart))

	(defmethod :close ()
		; (. farm :close)
		;close tasks
		(defq e (penv) destroy (get :destroy this))
		(.-> this (:each (lambda (key val) (callback destroy e key val))) :empty))
	)

;module
(export-classes '(Farm))
(env-pop)
