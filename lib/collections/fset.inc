;;;;;;;;;;;;;;;;;;;;;;
; fast hash set object
;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defmacro slot ()
	`(defq x (get :buckets this)
		b (% (hash key) (get :num_buckets this))
		e (find key (elem b x))))

(defclass fset (&optional num_buckets) nil
	; (fset [num_buckets]) -> fset
	(def this :buckets (list) :num_buckets (setq num_buckets (opt num_buckets 1)))
	(while (>= (setq num_buckets (dec num_buckets)) 0)
		(push (get :buckets this) (list)))

	(defmethod :find (this key)
		; (. fset :find key) -> nil | key
		(slot)
		(if e (elem e (elem b x))))

	(defmethod :insert (this key)
		; (. fset :insert key) -> fset
		(slot)
		(cond
			(e (elem-set e (elem b x) key))
			(t (push (elem b x) key)))
		this)

	(defmethod :erase (this key)
		; (. fset :erase key) -> fset
		(slot)
		(when e
			(elem-set e (defq b (elem b x)) (elem -2 b))
			(pop b))
		this)

	(defmethod :each (_this _fnc)
		; (. fset :each lambda)
		(each (lambda (_) (each _fnc _)) (get :buckets _this)))

	(defmethod :copy (this)
		; (. fset :copy) -> fset
		(defq that ((get 'fset) (get :num_buckets this)))
		(each (lambda (this_bucket that_bucket)
			(each (lambda (key)
				(push that_bucket key)) this_bucket)) (get :buckets this) (get :buckets that))
		that)

	(defmethod :deep_copy (this)
		; (. fset :deep_copy) -> fset
		(defq that ((get 'fset) (get :num_buckets this)))
		(each (lambda (this_bucket that_bucket)
			(each (lambda (key)
				(push that_bucket (copy key))) this_bucket)) (get :buckets this) (get :buckets that))
		that)

	(defmethod :union (this that)
		; (. fset :union fset) -> fset
		(unless (eql this that)
			(. that :each (# (. this :insert %0))))
		this)

	(defmethod :difference (this that)
		; (. fset :difference fset) -> fset
		(cond
			((eql this that)
				(. this :empty))
			(t  (. that :each (# (. this :erase %0)))
				this)))

	(defmethod :intersect (this that)
		; (. fset :intersect fset) -> fset
		(unless (eql this that)
			(each (# (elem-set _ b (filter (# (. that :find %0)) %0)))
				(defq b (get :buckets this))))
		this)

	(defmethod :not_intersect (this that)
		; (. fset :not_intersect fset) -> fset
		(cond
			((eql this that)
				(. this :empty))
			(t  (. (defq other (. that :copy)) :difference this)
				(. this :difference that)
				(. this :union other))))

	(defmethod :empty (this)
		; (. fset :empty) -> fset
		(each (# (clear %0)) (get :buckets this))
		this)

	(defmethod :move (this)
		; (. fset :move) -> fset
		(defq that ((get 'fset) (get :num_buckets this))
			this_buckets (get :buckets this) that_buckets (get :buckets that))
		(set this :buckets that_buckets)
		(set that :buckets this_buckets)
		that)

	(defmethod :resize (this num_buckets)
		; (. fset :resize num_buckets) -> fset
		(raise :buckets (new_buckets (list)))
		(lower :num_buckets (:buckets new_buckets))
		(while (>= (setq num_buckets (dec num_buckets)) 0)
			(push new_buckets (list)))
		(raise :num_buckets)
		(each (lambda (old_bucket)
			(while (defq key (pop old_bucket))
				(push (elem (% (hash key) num_buckets) new_buckets) key))) buckets)
		this)
	)

;module
(export-classes
	fset)
(env-pop)

(defun fset-k (&rest _)
	; (fset-k [key ...]) -> fset
	(defq this (fset))
	(each (# (. this :insert %0)) _)
	this)
