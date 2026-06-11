;;;;;;;;;;;;;;;;;
; fast set object
;;;;;;;;;;;;;;;;;

(import "./set.inc")

;module
(env-push)

(defclass Fset (&optional num_buckets) (Set)
	; (Fset [num_buckets]) -> fset
	(def this :num_buckets (setq num_buckets (ifn num_buckets 1))
		:buckets (map! (const plist) '() 0 num_buckets))

	(defmethod :find (key)
		; (. fset :find key) -> :nil | key
		(defq _bucket (elem-get (get :buckets this) (% (hash key) (get :num_buckets this))))
		(if (defq i (pfindi _bucket key))
			(elem-get _bucket i)))

	(defmethod :insert (key)
		; (. fset :insert key) -> fset
		(pinsert (elem-get (get :buckets this) (% (hash key) (get :num_buckets this))) key :t)
		this)

	(defmethod :inserted (key)
		; (. fset :inserted key) -> :nil | fset
		(defq _bucket (elem-get (get :buckets this) (% (hash key) (get :num_buckets this))))
		(cond
			((pfind _bucket key) :nil)
			((pinsert _bucket key :t) this)))

	(defmethod :intern (key)
		; (. fset :intern key) -> key
		(defq _bucket (elem-get (get :buckets this) (% (hash key) (get :num_buckets this))))
		(if (defq i (pfindi _bucket key))
			(elem-get _bucket i)
			(progn (pinsert _bucket key :t) key)))

	(defmethod :erase (key)
		; (. fset :erase key) -> fset
		(defq _bucket (elem-get (get :buckets this) (% (hash key) (get :num_buckets this))))
		(when (defq i (pfindi _bucket key))
			(elem-set _bucket i (elem-get _bucket -3))
			(elem-set _bucket (inc i) (elem-get _bucket -2))
			(pop _bucket) (pop _bucket))
		this)

	(defmethod :each (fnc)
		; (. fset :each lambda) -> fset
		(defq e (penv))
		(each (lambda (bucket)
			(each (lambda ((k v)) (callback fnc e k)) (partition bucket 2)))
			(get :buckets this))
		this)

	(defmethod :tolist ()
		; (. fset :tolist) -> list
		(defq out (list))
		(each (lambda (bucket)
			(each (lambda ((k v)) (push out k)) (partition bucket 2)))
			(get :buckets this))
		out)

	(defmethod :copy ()
		; (. fset :copy) -> fset
		(defq that ((get 'Fset) (get :num_buckets this))
			that_buckets (get :buckets that))
		(each (lambda (this_bucket that_bucket)
			(each (lambda (x) (push that_bucket x)) this_bucket))
			(get :buckets this) that_buckets)
		that)

	(defmethod :deep_copy ()
		; (. fset :deep_copy) -> fset
		(defq that ((get 'Fset) (get :num_buckets this))
			that_buckets (get :buckets that))
		(each (lambda (this_bucket that_bucket)
			(each (lambda ((k v)) (pinsert that_bucket (copy k) (copy v))) (partition this_bucket 2)))
			(get :buckets this) that_buckets)
		that)

	(defmethod :union (that)
		; (. fset :union set) -> fset
		(unless (eql this that)
			(. that :each (# (. this :insert %0))))
		this)

	(defmethod :difference (that)
		; (. fset :difference set) -> fset
		(cond
			((eql this that)
				(. this :empty))
			(:t (. that :each (# (. this :erase %0)))
				this)))

	(defmethod :intersect (that)
		; (. fset :intersect set) -> fset
		(unless (eql this that)
			(defq new_buckets (map! (const plist) '() 0 (get :num_buckets this)))
			(each (lambda (bucket new_bucket)
				(each (lambda ((k v)) (if (. that :find k) (pinsert new_bucket k v))) (partition bucket 2)))
				(get :buckets this) new_buckets)
			(set this :buckets new_buckets))
		this)

	(defmethod :not_intersect (that)
		; (. fset :not_intersect set) -> fset
		(cond
			((eql this that)
				(. this :empty))
			(:t (. (defq other (. that :copy)) :difference this)
				(. this :difference that)
				(. this :union other))))

	(defmethod :empty ()
		; (. fset :empty) -> fset
		(each (# (clear %0)) (get :buckets this))
		this)

	(defmethod :move ()
		; (. fset :move) -> fset
		(defq that ((get 'Fset) (get :num_buckets this))
			this_buckets (get :buckets this) that_buckets (get :buckets that))
		(set this :buckets that_buckets)
		(set that :buckets this_buckets)
		that)

	(defmethod :resize (num_buckets)
		; (. fset :resize num_buckets) -> fset
		(raise :buckets (new_buckets (map! (const plist) '() 0 num_buckets)))
		(lower :num_buckets (:buckets new_buckets))
		(raise :num_buckets)
		(each (lambda (old_bucket)
			(each (lambda ((k v)) (pinsert (elem-get new_buckets (% (hash k) num_buckets)) k v)) (partition old_bucket 2)))
			buckets)
		this)

	(defmethod :empty? ()
		; (. fset :empty?) -> :t | :nil
		(every (# (= (length %0) 0)) (get :buckets this)))

	(defmethod :size ()
		; (. fset :size) -> size
		(/ (reduce (# (+ %0 (length %1))) (get :buckets this) 0) 2))
	)

;module
(export-classes '(Fset))
(env-pop)
