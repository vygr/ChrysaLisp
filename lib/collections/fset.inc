;;;;;;;;;;;;;;;;;;;;;;
; fast hash set object
;;;;;;;;;;;;;;;;;;;;;;

(import "./set.inc")

;module
(env-push)

(defmacro slot ()
	(static-qq (defq _kl (get :buckets this)
		_kl (elem-get _kl (% (hash key) (get :num_buckets this)))
		_si (find key _kl))))

(defclass Fset (&optional num_buckets) (Set)
	; (Fset [num_buckets]) -> fset
	(def this :num_buckets (setq num_buckets (ifn num_buckets 1))
		:buckets (lists num_buckets))

	(defmethod :find (key)
		; (. fset :find key) -> :nil | key
		(if (slot) (elem-get _kl _si)))

	(defmethod :insert (key)
		; (. fset :insert key) -> fset
		(if (slot) :nil (push _kl key))
		this)

	(defmethod :inserted (key)
		; (. fset :inserted key) -> :nil | fset
		(cond
			((slot) :nil)
			((push _kl key) this)))

	(defmethod :intern (key)
		; (. fset :intern key) -> key
		(cond
			((slot) (elem-get _kl _si))
			((push _kl key) key)))

	(defmethod :erase (key)
		; (. fset :erase key) -> fset
		(when (slot)
			(elem-set _kl _si (last _kl))
			(pop _kl))
		this)

	(defmethod :each (fnc)
		; (. fset :each lambda) -> fset
		(defq e (penv))
		(each (# (each (lambda (i) (callback fnc e i)) %0)) (get :buckets this))
		this)

	(defmethod :copy ()
		; (. fset :copy) -> fset
		(defq that ((get 'Fset) (get :num_buckets this)))
		(each (lambda (this_bucket that_bucket)
			(each (lambda (key)
				(push that_bucket key)) this_bucket)) (get :buckets this) (get :buckets that))
		that)

	(defmethod :deep_copy ()
		; (. fset :deep_copy) -> fset
		(defq that ((get 'Fset) (get :num_buckets this)))
		(each (lambda (this_bucket that_bucket)
			(each (lambda (key)
				(push that_bucket (copy key))) this_bucket)) (get :buckets this) (get :buckets that))
		that)

	(defmethod :union (that)
		; (. fset :union fset) -> fset
		(unless (eql this that)
			(. that :each (# (. this :insert %0))))
		this)

	(defmethod :difference (that)
		; (. fset :difference fset) -> fset
		(cond
			((eql this that)
				(. this :empty))
			(:t (. that :each (# (. this :erase %0)))
				this)))

	(defmethod :intersect (that)
		; (. fset :intersect fset) -> fset
		(unless (eql this that)
			(each (# (elem-set _b (!) (filter (# (. that :find %0)) %0)))
				(defq _b (get :buckets this))))
		this)

	(defmethod :not_intersect (that)
		; (. fset :not_intersect fset) -> fset
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
		(raise :buckets (new_buckets (lists num_buckets)))
		(lower :num_buckets (:buckets new_buckets))
		(raise :num_buckets)
		(each (lambda (old_bucket)
			(while (defq key (pop old_bucket))
				(push (elem-get new_buckets (% (hash key) num_buckets)) key))) buckets)
		this)

	(defmethod :empty? ()
		; (. fset :empty?) -> :t | :nil
		(every (# (= (length %0) 0)) (get :buckets this)))
	)

;module
(export-classes '(Fset))
(env-pop)
