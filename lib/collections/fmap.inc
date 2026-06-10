;;;;;;;;;;;;;;;;;
; fast map object
;;;;;;;;;;;;;;;;;

(import "./map.inc")

;module
(env-push)

(defclass Fmap (&optional num_buckets) (Map)
	; (Fmap [num_buckets]) -> fmap
	(def this :num_buckets (setq num_buckets (ifn num_buckets 1))
		:buckets (map! (const plist) '() 0 num_buckets))

	(defmethod :find (key)
		; (. fmap :find key) -> :nil | val
		(pfind (elem-get (get :buckets this) (% (hash key) (get :num_buckets this))) key))

	(defmethod :insert (key val)
		; (. fmap :insert key val) -> fmap
		(pinsert (elem-get (get :buckets this) (% (hash key) (get :num_buckets this))) key val)
		this)

	(defmethod :update (key fnc)
		; (. fmap :update key lambda) -> val
		(defq p (elem-get (get :buckets this) (% (hash key) (get :num_buckets this)))
			val (callback fnc (penv) (pfind p key)))
		(pinsert p key val)
		val)

	(defmethod :memoize (key fnc)
		; (. fmap :memoize key lambda) -> val
		(defq p (elem-get (get :buckets this) (% (hash key) (get :num_buckets this))))
		(if (nql (defq idx (pfindi p key)) :nil)
			(elem-get p (inc idx))
			(progn
				(defq val (callback fnc (penv)))
				(pinsert p key val)
				val)))

	(defmethod :erase (key)
		; (. fmap :erase key) -> fmap
		(defq _p (elem-get (get :buckets this) (% (hash key) (get :num_buckets this))))
		(when (defq _si (pfindi _p key))
			(elem-set _p _si (elem-get _p -3))
			(elem-set _p (inc _si) (elem-get _p -2))
			(pop _p) (pop _p))
		this)

	(defmethod :each (fnc)
		; (. fmap :each lambda) -> fmap
		(defq e (penv))
		(each (lambda (bucket)
			(each (lambda ((k v)) (callback fnc e k v)) (partition bucket 2)))
			(get :buckets this))
		this)

	(defmethod :tolist ()
		; (. fmap :tolist) -> list
		(defq out (list))
		(each (lambda (bucket)
			(each (lambda (pair) (push out pair)) (partition bucket 2)))
			(get :buckets this))
		out)

	(defmethod :copy ()
		; (. fmap :copy) -> fmap
		(defq that ((get 'Fmap) (get :num_buckets this))
			that_buckets (get :buckets that))
		(each (lambda (this_bucket that_bucket)
			(each (lambda (x) (push that_bucket x)) this_bucket))
			(get :buckets this) that_buckets)
		that)

	(defmethod :deep_copy ()
		; (. fmap :deep_copy) -> fmap
		(defq that ((get 'Fmap) (get :num_buckets this))
			that_buckets (get :buckets that))
		(each (lambda (this_bucket that_bucket)
			(each (lambda ((k v)) (pinsert that_bucket (copy k) (copy v))) (partition this_bucket 2)))
			(get :buckets this) that_buckets)
		that)

	(defmethod :empty ()
		; (. fmap :empty) -> fmap
		(each (# (clear %0)) (get :buckets this))
		this)

	(defmethod :move ()
		; (. fmap :move) -> fmap
		(defq that ((get 'Fmap) (get :num_buckets this))
			this_buckets (get :buckets this) that_buckets (get :buckets that))
		(set this :buckets that_buckets)
		(set that :buckets this_buckets)
		that)

	(defmethod :resize (num_buckets)
		; (. fmap :resize num_buckets) -> fmap
		(raise :buckets (new_buckets (map! (const plist) '() 0 num_buckets)))
		(lower :num_buckets (:buckets new_buckets))
		(raise :num_buckets)
		(each (lambda (old_bucket)
			(each (lambda ((k v)) (pinsert (elem-get new_buckets (% (hash k) num_buckets)) k v)) (partition old_bucket 2)))
			buckets)
		this)

	(defmethod :empty? ()
		; (. fmap :empty?) -> :t | :nil
		(every (# (= (length %0) 0)) (get :buckets this)))

	(defmethod :size ()
		; (. fmap :size) -> size
		(/ (reduce (# (+ %0 (length %1))) (get :buckets this) 0) 2))
	)

;module
(export-classes '(Fmap))
(env-pop)
