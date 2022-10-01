;;;;;;;;;;;;;;;;;;
; farm task object
;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")

(defclass Farm (fnc_create fnc_destroy size) (fmap)
	; (Farm fnc_create fnc_destroy size) -> farm
	(def this :create fnc_create :destroy fnc_destroy :key -1)
	(defq _nodes (map (# (cat %0)) (mail-nodes)) _now (pii-time))
	(times size
		(defq _key (def this :key (inc (get :key this))) _val (emap))
		(. this :insert _key _val)
		(. _val :insert :timestamp _now)
		(fnc_create _key _val _nodes))

	(defmethod :refresh (this &optional _timeout)
		; (. farm :refresh [_timeout]) -> :t | :nil
		;scan known nodes and update map
		(defq _nodes (map (# (cat %0)) (mail-nodes)) _old_keys (list) _old_vals (list)
			_create (get :create this) _destroy (get :destroy this)
			_mutated :nil _now (pii-time))
		(. this :each (lambda (key val) (push _old_keys key) (push _old_vals val)))
		;restart a child
		(defun _restart ()
			(setq _mutated :t)
			(_destroy _key _val)
			(. this :erase _key)
			(setq _key (def this :key (inc (get :key this))) _val (emap))
			(. this :insert _key _val)
			(. _val :insert :timestamp _now)
			(_create _key _val _nodes))
		;test for vanished and timeout nodes
		(each (lambda (_key _val)
			(cond
				((or (not (defq _child (. _val :find :child)))
					(find (slice +long_size -1 _child) _nodes))
					;waiting on child launching, or found child in nodes, so check timestamp
					(and _timeout
						(defq _then (. _val :find :timestamp))
						(> (- _now _then) _timeout)
						(_restart)))
				(:t	;not found child
					(_restart)))) _old_keys _old_vals)
		_mutated)

	(defmethod :close (this)
		; (. farm :close)
		;close tasks
		(.-> this (:each (get :destroy this)) :empty))
	)
