;;;;;;;;;;;;;
; collections
;;;;;;;;;;;;;

(import "./emap.inc")
(import "./fmap.inc")
(import "./fset.inc")
(import "./lset.inc")
(import "./xmap.inc")
(import "./xset.inc")
(import "./lmap.inc")
(import "./tree.inc")

(defun gather (m &rest _)
	; (gather map|set [key] ...) -> (val|key|:nil ...)
	; gather a list of [key|val|:nil]
	(if (list? (first _)) (setq _ (first _)))
	(map (# (. m :find %0)) _))

(defun scatter (m &rest _)
	; (scatter map|set [key]|[key val] ...) -> map|set
	; scatter a list of [key]|[key val]
	(if (list? (first _)) (setq _ (first _)))
	(if (rfind :Set (. m :type_of))
		(reduce (lambda (m k) (. m :insert k)) _ m)
		(reduce (lambda (m (k v)) (. m :insert k v)) (partition _ 2) m)))

;;;;;;;;;;;;;;;;;;;;;;;
; Fmap/Lmap memoization
;;;;;;;;;;;;;;;;;;;;;;;

(defmacro memoize (k f &optional num_buckets)
	; (memoize key form [num_buckets]) -> (eval form)
	(if num_buckets
		(static-qq (. ,(Fmap num_buckets) :memoize ,k (# ,f)))
		(static-qq (. ,(Lmap) :memoize ,k (# ,f)))))
