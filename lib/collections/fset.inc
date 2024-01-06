;;;;;;;;;;;;;;;;;;;;;;
; fast hash set object
;;;;;;;;;;;;;;;;;;;;;;

(import "./set.inc")

;module
(env-push)

(defmacro slot ()
	`(defq x (get :buckets this)
		b (% (hash key) (get :num_buckets this))
		e (find key (elem-get b x))))

(defclass Fset (&optional num_buckets) (Set)
	; (Fset [num_buckets]) -> fset
	(def this :num_buckets (setq num_buckets (opt num_buckets 1))
		:buckets (lists num_buckets))

	(defmethod :find (key)
		; (. fset :find key) -> :nil | key
		(if (slot) (elem-get e (elem-get b x))))

	(defmethod :insert (key)
		; (. fset :insert key) -> fset
		(cond
			((slot))
			(:t (push (elem-get b x) key)))
		this)

	(defmethod :intern (key)
		; (. fset :intern key) -> key
		(cond
			((slot) (elem-get e (elem-get b x)))
			(:t (push (elem-get b x) key) key)))

	(defmethod :erase (key)
		; (. fset :erase key) -> fset
		(when (slot)
			(elem-set e (defq b (elem-get b x)) (last b))
			(pop b))
		this)

	(defmethod_ :each (_fnc)
		; (. fset :each lambda)
		(each (lambda (_) (each _fnc _)) (get :buckets _this)))

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
			(each (# (elem-set _ b (filter (# (. that :find %0)) %0)))
				(defq b (get :buckets this))))
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
				(push (elem-get (% (hash key) num_buckets) new_buckets) key))) buckets)
		this)

	(defmethod :empty? ()
		; (. fset :empty?) -> :t | :nil
		(every (# (= (length %0) 0)) (get :buckets this)))
	)

;module
(export-classes '(Fset))
(env-pop)

(defun Fset-k (&rest _)
	; (Fset-k [key ...]) -> fset
	(reduce (# (. %0 :insert %1)) _ (Fset)))
