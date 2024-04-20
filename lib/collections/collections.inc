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
	(defq c (gensym) e (gensym))
	(if num_buckets
		`(progn
			(unless (defq ,c (const (Fmap ,num_buckets)) ,e (. ,c :find ,k))
				(. ,c :insert ,k (setq ,e ,f))) ,e)
		`(progn
			(unless (defq ,c (const (Lmap)) ,e (. ,c :find ,k))
				(. ,c :insert ,k (setq ,e ,f))) ,e)))
