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
	(callback create e key val nodes))

(defun restart ()
	;restart a child
	(setq mutated :t)
	(stop) (start))

(defclass Farm (create destroy size) (Fmap)
	; (Farm fnc_create fnc_destroy size) -> farm
	(def this :create create :destroy destroy :key -1)
	(defq nodes (map (const cat) (lisp-nodes)) now (pii-time) e (penv))
	(times size (start))

	(defmethod :refresh (&optional timeout)
		; (. farm :refresh [timeout]) -> :t | :nil
		;scan known nodes and update map
		(defq nodes (map (const cat) (lisp-nodes))
			old_keys (list) old_vals (list)
			create (get :create this) destroy (get :destroy this)
			mutated :nil now (pii-time) e (penv))
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
		; (. farm :restart key val)
		;restart task
		(defq nodes (map (const cat) (lisp-nodes))
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
