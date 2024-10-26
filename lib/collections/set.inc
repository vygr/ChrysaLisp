;;;;;;;;;;;;
; set object
;;;;;;;;;;;;

(defclass Set () :nil
	; (Set) -> set
	(defabstractmethod :find (key))
		; (. set :find key) -> :nil | key
	(defabstractmethod :insert (key))
		; (. set :insert key) -> set
	(defabstractmethod :inserted (key))
		; (. set :inserted key) -> :nil | set
	(defabstractmethod :intern (key))
		; (. set :intern key) -> key
	(defabstractmethod :erase (key))
		; (. set :erase key) -> set
	(defabstractmethod :each (fnc))
		; (. set :each lambda)
	(defabstractmethod :copy ())
		; (. set :copy) -> set
	(defabstractmethod :deep_copy ())
		; (. set :deep_copy) -> set
	(defabstractmethod :union (that))
		; (. set :union set) -> set
	(defabstractmethod :difference (that))
		; (. set :difference set) -> set
	(defabstractmethod :intersect (that))
		; (. set :intersect set) -> set
	(defabstractmethod :not_intersect (that))
		; (. set :not_intersect set) -> set
	(defabstractmethod :empty ())
		; (. set :empty) -> set
	(defabstractmethod :move ())
		; (. set :move) -> set
	(defabstractmethod :resize (num_buckets))
		; (. set :resize num_buckets) -> set
	(defabstractmethod :empty? ())
		; (. set :empty?) -> :t | :nil
	)
