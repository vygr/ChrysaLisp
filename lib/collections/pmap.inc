;;;;;;;;;;;;;;;;;;
; plist map object
;;;;;;;;;;;;;;;;;;

(import "./map.inc")

;module
(env-push)

(defclass Pmap () (Map)
	; (Pmap) -> pmap
	(def this :plist (plist))

	(defmethod :find (key)
		; (. pmap :find key) -> :nil | val
		(pfind (get :plist this) key))

	(defmethod :insert (key val)
		; (. pmap :insert key val) -> pmap
		(pinsert (get :plist this) key val)
		this)

	(defmethod :update (key _fnc)
		; (. pmap :update key lambda) -> val
		(pinsert (defq _p (get :plist this)) key (_fnc (pfind _p key))))

	(defmethod :memoize (key _fnc)
		; (. pmap :memoize key lambda) -> val
		(defq _p (get :plist this))
		(cond
			((pfind _p key))
			((pinsert _p key (_fnc)))))

	(defmethod :erase (key)
		; (. pmap :erase key) -> pmap
		(raise :plist)
		(when (defq _si (pfindi plist key))
			(defq _li (- (length plist) 2))
			(elem-set plist _si (elem-get plist _li))
			(elem-set plist (inc _si) (elem-get plist (inc _li)))
			(pop plist) (pop plist))
		this)

	(defmethod :each (fnc)
		; (. pmap :each lambda) -> pmap
		(defq e (penv))
		(each (lambda ((k v)) (callback fnc e k v)) (. this :tolist))
		this)

	(defmethod :tolist ()
		; (. pmap :tolist) -> list
		(partition (get :plist this) 2))

	(defmethod :copy ()
		; (. pmap :copy) -> pmap
		(defq that ((get 'Pmap)))
		(set that :plist (copy (get :plist this)))
		that)

	(defmethod :deep_copy ()
		; (. pmap :deep_copy) -> pmap
		(defq that ((get 'Pmap)) that_plist (get :plist that))
		(each (lambda ((key val)) (pinsert that_plist (copy key) (copy val))) (. this :tolist))
		that)

	(defmethod :empty ()
		; (. pmap :empty) -> pmap
		(set this :plist (plist))
		this)

	(defmethod :move ()
		; (. pmap :move) -> pmap
		(defq that (. this :copy))
		(. this :empty)
		that)

	(defmethod :resize (num_buckets)
		; (. pmap :resize num_buckets) -> pmap
		this)

	(defmethod :empty? ()
		; (. pmap :empty?) -> :t | :nil
		(= (length (get :plist this)) 0))

	(defmethod :size ()
		; (. pmap :size) -> size
		(/ (length (get :plist this)) 2))
	)

;module
(export-classes '(Pmap))
(env-pop)
