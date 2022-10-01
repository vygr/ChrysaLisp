;;;;;;;;;;;;;;;;;;;;
; global task object
;;;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")

(defclass Global (fnc_create fnc_destroy) (fmap)
	; (Global fnc_create fnc_destroy) -> global
	(def this :size 0 :create fnc_create :destroy fnc_destroy)

	(defmethod :refresh (this &optional _timeout)
		; (. global :refresh [_timeout]) -> :t | :nil
		;scan known nodes and update node map
		(defq _keys (map (# (cat %0)) (mail-nodes)) _old_keys (list) _old_vals (list)
			_create (get :create this) _destroy (get :destroy this)
			_mutated :nil _now (pii-time))
		(. this :each (lambda (key val) (push _old_keys key) (push _old_vals val)))
		;test for vanished and timeout nodes
		(each (lambda (_key _val)
			(cond
				((find _key _keys)
					;found, so check timestamp
					(and _timeout
						(defq _then (. _val :find :timestamp))
						(> (- _now _then) _timeout)
						(progn
							(setq _mutated :t)
							(_destroy _key _val)
							(. this :insert _key (_create _key _now)))))
				(:t	;not found
					(setq _mutated :t)
					(_destroy _key _val)
					(. this :erase _key)))) _old_keys _old_vals)
		;test for new nodes
		(each (lambda (_key)
			(unless (find _key _old_keys)
				(setq _mutated :t)
				;must (cat _key) to convert to pure string key !
				(. this :insert (cat _key) (_create _key _now)))) _keys)
		(def this :size (length _keys))
		_mutated)

	(defmethod :close (this)
		; (. global :close)
		;close tasks
		(.-> this (:each (get :destroy this)) :empty)
		(def this :size 0))

	(defmethod :size (this)
		; (. global :size) -> size
		;size of tasks
		(get :size this))
	)
