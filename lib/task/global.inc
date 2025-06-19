;;;;;;;;;;;;;;;;;;;;
; global task object
;;;;;;;;;;;;;;;;;;;;

(defclass Global (fnc_create fnc_destroy) (Fmap)
	; (Global fnc_create fnc_destroy) -> global
	(def this :size 0 :create fnc_create :destroy fnc_destroy)

	(defmethod :refresh (&optional timeout)
		; (. global :refresh [timeout]) -> :t | :nil
		;scan known nodes and update node map
		(defq keys (map (const cat) (lisp-nodes))
			old_keys (list) old_vals (list)
			create (get :create this) destroy (get :destroy this)
			mutated :nil now (pii-time) e (penv))
		(. this :each (lambda (key val) (push old_keys key) (push old_vals val)))
		;test for vanished and timeout nodes
		(each (lambda (key val)
			(cond
				((find key keys)
					;found, so check timestamp
					(and timeout
						(defq then (get :timestamp val))
						(> (- now then) timeout)
						(progn
							(setq mutated :t)
							(eval `(,destroy ',key ',val) e)
							(. this :insert key (create key now)))))
				(:t ;not found
					(setq mutated :t)
					(eval `(,destroy ',key ',val) e)
					(. this :erase key)))) old_keys old_vals)
		;test for new nodes
		(each (lambda (key)
			(unless (find key old_keys)
				(setq mutated :t)
				;must (cat key) to convert to pure string key !
				(. this :insert (cat key) (eval `(,create ',key ',now) e)))) keys)
		(def this :size (length keys))
		mutated)

	(defmethod :close ()
		; (. global :close)
		;close tasks
		(defq e (penv) destroy (get :destroy this))
		(.-> this (:each (lambda (key val) (eval `(,destroy ',key ',val) e))) :empty)
		(def this :size 0))

	(defmethod :size ()
		; (. global :size) -> size
		;size of tasks
		(get :size this))
	)
