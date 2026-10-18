;;;;;;;;;;;;
; set object
;;;;;;;;;;;;

(defclass Set () :nil
	; (Set) -> set
	(defabstractmethod :find (key))
		; (. set :find key) -> :nil | key
	(defabstractmethod :insert (key))
		; (. set :insert key) -> set
	(defabstractmethod :erase (key))
		; (. set :erase key) -> set
	(defabstractmethod :each (fnc))
		; (. set :each lambda) -> set
	(defabstractmethod :tolist ())
		; (. set :tolist) -> list
	(defabstractmethod :copy ())
		; (. set :copy) -> set
	(defabstractmethod :deep_copy ())
		; (. set :deep_copy) -> set
	(defabstractmethod :empty ())
		; (. set :empty) -> set
	(defabstractmethod :move ())
		; (. set :move) -> set
	(defabstractmethod :resize (num_buckets))
		; (. set :resize num_buckets) -> set
	(defabstractmethod :size ())
		; (. set :size) -> size
	(defabstractmethod :empty? ())
		; (. set :empty?) -> :t | :nil
	(defabstractmethod :inserted (key))
		; (. set :inserted key) -> :nil | set
	(defabstractmethod :intern (key))
		; (. set :intern key) -> key
	(defabstractmethod :intersect (that))
		; (. set :intersect set) -> set

	;the same for every set, made of what each set does its own way

	(defmethod :union (that)
		; (. set :union set) -> set
		(unless (eql this that)
			(. that :each (# (. this :insert %0))))
		this)

	(defmethod :difference (that)
		; (. set :difference set) -> set
		(cond
			((eql this that)
				(. this :empty))
			(:t (. that :each (# (. this :erase %0)))
				this)))

	(defmethod :not_intersect (that)
		; (. set :not_intersect set) -> set
		(cond
			((eql this that)
				(. this :empty))
			(:t (. (defq other (. that :copy)) :difference this)
				(. this :difference that)
				(. this :union other))))
	)
