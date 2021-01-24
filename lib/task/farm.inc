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
		(defq _keys (mail-nodes) _old_keys (list) _mutated nil _now (pii-time))
		(. this :each (lambda (key val) (push _old_keys key)))
		;test for vanished and timeout nodes
		(each (lambda (_key)
			(cond
				((find (slice (const long_size) -1 _key) _keys)
					;found, so check timestamp
					(when _timeout
						(defq _val (. this :find _key) _then (. _val :find :timestamp))
						(when _then
							;there is a timestamp field
							(when (> (- _now _then) _timeout)
								(setq _mutated t)
								((get :destroy this) _key _val)
								(. this :erase _key)
								((get :create this) _keys)))))
				(t	;not found
					(setq _mutated t)
					((get :destroy this) _key (. this :find _key))
					(. this :erase _key)
					((get :create this) _keys)))) _old_keys)
		_mutated)

	(defmethod :close (this)
		; (. farm :close)
		;close tasks
		(. this :each (get :destroy this)))
	)
