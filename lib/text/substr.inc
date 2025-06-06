;;;;;;;;;;;;;;;;;;;;;
; substr search class
;;;;;;;;;;;;;;;;;;;;;

(import "./search.inc")

(defclass Substr (&optional num_buckets) (Search)
	; (Substr [num_buckets]) -> substr
	(def this :meta_cache (Fmap num_buckets))

	(defmethod :compile (pattern)
		; (. substr :compile pattern) -> :nil | meta
		(raise :meta_cache)
		(unless (defq lps (. meta_cache :find pattern))
			(. meta_cache :insert pattern (setq lps pattern)))
		lps)

	(defmethod :search (text pattern &optional lps)
		; (. substr :search text pattern [meta]) -> matches
		(defq out (list))
		(and (>= (defq l (length text)) (length pattern))
			(defq j 0 lps (ifn lps (. this :compile pattern)))
			(while (and (defq i (find (first lps) text j))
						(<= (setq j (+ i (length lps))) l))
				(if (eql (slice text i j) lps)
					(push out (list (list i j)))
					(setq j (inc i)))))
		out)

	(defmethod :match? (text pattern &optional lps)
		; (. substr :match? text pattern [meta]) -> :t | :nil
		(defq out :nil)
		(and (>= (defq l (length text)) (length pattern))
			(defq j 0 lps (ifn lps (. this :compile pattern)))
			(while (and (defq i (find (first lps) text j))
						(<= (setq j (+ i (length lps))) l))
				(if (eql (slice text i j) lps)
					(setq j -1 out :t)
					(setq j (inc i)))))
		out)
	)
