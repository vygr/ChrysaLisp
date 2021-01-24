;;;;;;;;;;;;;;;;;;;;
; global task object
;;;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")

(defclass Global (fnc_create fnc_destroy) (xmap)
	; (Global fnc_create fnc_destroy) -> global
	(def this :size 0 :create fnc_create :destroy fnc_destroy)

	(defmethod :refresh (this &optional _timeout)
		; (. global :refresh [_timeout]) -> t | nil
		;scan known nodes and update node map
		(defq _keys (mail-nodes) _old_keys (list) _mutated nil _now (pii-time))
		(. this :each (lambda (key val) (push _old_keys key)))
		;test for vanished and timeout nodes
		(each (lambda (_key)
			(cond
				((find _key _keys)
					;found, so check timestamp
					(when _timeout
						(defq _val (. this :find _key) _then (. _val :find :timestamp))
						(when _then
							;there is a timestamp field
							(when (> (- _now _then) _timeout)
								(setq _mutated t)
								((get :destroy this) _key _val)
								(. this :insert _key ((get :create this) _key _now))))))
				(t	;not found
					(setq _mutated t)
					((get :destroy this) _key (. this :find _key))
					(. this :erase _key)))) _old_keys)
		;test for new nodes
		(each (lambda (_key)
			(unless (find _key _old_keys)
				(setq _mutated t)
				;must (cat _key) to convert to pure string key !
				(. this :insert (cat _key) ((get :create this) _key _now)))) _keys)
		(def this :size (length _keys))
		_mutated)

	(defmethod :close (this)
		; (. global :close)
		;close tasks
		(. this :each (get :destroy this)))

	(defmethod :size (this)
		; (. global :size) -> size
		;size of tasks
		(get :size this))
	)
