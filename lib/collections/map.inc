;;;;;;;;;;;;
; map object
;;;;;;;;;;;;

(defclass Map () :nil
	; (Map) -> map
	(defabstractmethod :find (key))
		; (. map :find key) -> :nil | val
	(defabstractmethod :insert (key val))
		; (. map :insert key val) -> map
	(defabstractmethod :erase (key))
		; (. map :erase key) -> map
	(defabstractmethod :each (fnc))
		; (. map :each lambda) -> map
	(defabstractmethod :copy ())
		; (. map :copy) -> map
	(defabstractmethod :deep_copy ())
		; (. map :deep_copy) -> map
	(defabstractmethod :empty ())
		; (. map :empty) -> map
	(defabstractmethod :move ())
		; (. map :move) -> map
	(defabstractmethod :resize (num_buckets))
		; (. map :resize num_buckets) -> map
	(defabstractmethod :empty? ())
		; (. map :empty?) -> :t | :nil
	(defabstractmethod :update (key func))
		; (. map :update key lambda) -> val
	(defabstractmethod :memoize (key func))
		; (. map :memoize key lambda) -> val
	)
