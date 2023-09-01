;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Knuth-Morris-Pratt substr search class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Substr (&optional num_buckets) :nil
	; (Substr) -> substr
	(def this :meta_cache (fmap num_buckets))

	(defmethod :compile (this pattern)
		; (. substr :compile pattern) -> :nil | meta
		(raise :meta_cache)
		(unless (defq lps (. meta_cache :find pattern))
			(cond
				((= (defq pl (length pattern)) 0)
					(defq lps :nil))
				(:t	(defq lps (cap pl (list 0)) i 0 j 1)
					(times (dec pl) (push lps -1))
					(while (< j (length lps))
						(cond
							((eql (elem-get i pattern) (elem-get j pattern))
								(elem-set j lps (inc i))
								(setq i (inc i) j (inc j)))
							(:t	(cond
									((= (elem-get i lps) 0)
										(elem-set j lps 0)
										(setq j (inc j)))
									((/= (elem-get i lps) 0)
										(elem-set i lps (elem-get (dec i) lps))
										(setq j (inc j)))))))))
			(. meta_cache :insert pattern lps))
		lps)

	(defmethod :search (this text pattern &optional lps)
		; (. substr :search text pattern &optional meta) -> indexes
		(defq out (list) lps (opt lps (. this :compile pattern)))
		(when lps
			(defq j 0 k 0)
			(while (< k (length text))
				(if (defq match (eql (elem-get j pattern) (elem-get k text)))
					(setq j (inc j) k (inc k)))
				(cond
					((= j (length pattern))
						(push out (list (- k j) k))
						(setq j 0))
					((and (not match) (< k (length text)))
						(cond
							((/= j 0)
								(setq j (elem-get (dec j) lps)))
							(:t	(setq k (inc k))))))))
		out)
	)
