;;;;;;;;;;;;;;;;;;
; farm task object
;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")

(defclass Farm (fnc_create fnc_destroy size) (xmap)
	; (Farm fnc_create fnc_destroy size) -> farm
	(def this :create fnc_create :destroy fnc_destroy)
	(defq _keys (mail-nodes))
	(times size (fnc_create _keys))

	(defmethod :refresh (this &optional _timeout)
		; (. farm :refresh [_timeout]) -> t | nil
		;scan known nodes and update map
		(defq _keys (mail-nodes) _old_keys (list) _old_vals (list)
			_create (get :create this) _destroy (get :destroy this)
			_mutated nil _now (pii-time))
		(. this :each (lambda (key val) (push _old_keys key) (push _old_vals val)))
		;test for vanished and timeout nodes
		(each (lambda (_key _val)
			(cond
				((find (slice (const long_size) -1 _key) _keys)
					;found, so check timestamp
					(and _timeout
						(defq _then (. _val :find :timestamp))
						(> (- _now _then) _timeout)
						(progn
							(setq _mutated t)
							(_destroy _key _val)
							(. this :erase _key)
							(_create _keys))))
				(t	;not found
					(setq _mutated t)
					(_destroy _key _val)
					(. this :erase _key)
					(_create _keys)))) _old_keys _old_vals)
		_mutated)

	(defmethod :close (this)
		; (. farm :close)
		;close tasks
		(.-> this (:each (get :destroy this)) :empty))
	)
