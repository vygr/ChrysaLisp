;;;;;;;;;;;;;;;;;
; fast set object
;;;;;;;;;;;;;;;;;

(import "./set.inc")

;module
(env-push)

(defclass Fset (&optional num_buckets) (Set)
	; (Fset [num_buckets]) -> fset
	(def this :num_buckets (setq num_buckets (ifn num_buckets 1))
		:buckets (map! (const pset) '() 0 num_buckets))

	(defmethod :find (key)
		; (. fset :find key) -> :nil | key
		(pfind (elem-get (get :buckets this) (% (hash key) (get :num_buckets this))) key))

	(defmethod :insert (key)
		; (. fset :insert key) -> fset
		(pinsert (elem-get (get :buckets this) (% (hash key) (get :num_buckets this))) key)
		this)

	(defmethod :inserted (key)
		; (. fset :inserted key) -> :nil | fset
		(defq bucket (elem-get (get :buckets this) (% (hash key) (get :num_buckets this))))
		(cond
			((pfind bucket key) :nil)
			((pinsert bucket key) this)))

	(defmethod :intern (key)
		; (. fset :intern key) -> key
		(defq bucket (elem-get (get :buckets this) (% (hash key) (get :num_buckets this))))
		(or (pfind bucket key)
			(progn (pinsert bucket key) key)))

	(defmethod :erase (key)
		; (. fset :erase key) -> fset
		(perase (elem-get (get :buckets this) (% (hash key) (get :num_buckets this))) key)
		this)

	(defmethod :each (fnc)
		; (. fset :each lambda) -> fset
		(defq e (penv))
		(each (lambda (bucket)
			(each (lambda (k) (callback fnc e k)) bucket))
			(get :buckets this))
		this)

	(defmethod :tolist ()
		; (. fset :tolist) -> list
		(defq out (list))
		(each (lambda (bucket)
			(each (lambda (k) (push out k)) bucket))
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
			(each (lambda (k) (pinsert that_bucket (copy k))) this_bucket))
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
			(defq new_buckets (map! (const pset) '() 0 (get :num_buckets this)))
			(each (lambda (bucket new_bucket)
				(each (lambda (k) (if (. that :find k) (pinsert new_bucket k))) bucket))
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
		(raise :buckets (new_buckets (map! (const pset) '() 0 num_buckets)))
		(lower :num_buckets (:buckets new_buckets))
		(raise :num_buckets)
		(each (lambda (old_bucket)
			(each (lambda (k) (pinsert (elem-get new_buckets (% (hash k) num_buckets)) k)) old_bucket))
			buckets)
		this)

	(defmethod :empty? ()
		; (. fset :empty?) -> :t | :nil
		(every (# (= (length %0) 0)) (get :buckets this)))

	(defmethod :size ()
		; (. fset :size) -> size
		(reduce (# (+ %0 (length %1))) (get :buckets this) 0))
	)

;module
(export-classes '(Fset))
(env-pop)