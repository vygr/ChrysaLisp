;;;;;;;;;;;;;
; collections
;;;;;;;;;;;;;

(import "./emap.inc")
(import "./fmap.inc")
(import "./fset.inc")
(import "./xmap.inc")
(import "./xset.inc")
(import "./lmap.inc")
(import "./tree.inc")

(defun gather (m &rest _)
	; (gather map key ...) -> (val ...)
	; gather a list of values
	(map (# (. m :find %0)) _))

(defun scatter (m &rest _)
	; (scatter map [key val] ...) -> map
	; scatter a list of values
	(reduce (lambda (m (k v)) (. m :insert k v)) (partition _ 2) m))

;;;;;;;;;;;;;;;;;;;;;;;
; Fmap/Lmap memoization
;;;;;;;;;;;;;;;;;;;;;;;

(defmacro memoize (k f &optional num_buckets)
	; (memoize key form [num_buckets]) -> (eval form)
	(if num_buckets
		(static-qq (. ,(Fmap num_buckets) :memoize ,k (# ,f)))
		(static-qq (. ,(Lmap) :memoize ,k (# ,f)))))
