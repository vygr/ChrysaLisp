;;;;;;;;;;;;;;;;;;;;;
; substr search class
;;;;;;;;;;;;;;;;;;;;;

(defclass Substr (&optional num_buckets) :nil
	; (Substr) -> substr
	(def this :meta_cache (Fmap num_buckets))

	(defmethod :compile (pattern)
		; (. substr :compile pattern) -> :nil | meta
		(raise :meta_cache)
		(unless (defq lps (. meta_cache :find pattern))
			(defq j 0 i 1 l (length pattern) lps (cap l (list 0)))
			(while (< (length lps) l)
				(cond
					((eql (elem-get i pattern) (elem-get j pattern))
						(push lps (setq i (inc i) j (inc j))))
					((= j 0) (push lps 0) (setq i (inc i)))
					((setq j (elem-get (dec j) lps)))))
			(. meta_cache :insert pattern lps))
		lps)

	(defmethod :search (text pattern &optional lps)
		; (. substr :search text pattern &optional meta) -> (matches submatches)
		(defq out (list))
		(when (>= (defq n (length text)) (defq m (length pattern)))
			(when (defq lps (opt lps (. this :compile pattern))))
				(defq j 0 k 0 n (- n m))
				(while (<= j n)
					(cond
						((eql (elem-get j text) (elem-get k pattern))
							(when (= m (setq j (inc j) k (inc k)))
								(push out (list (- j k) j))
								(setq k 0)))
						((= k 0) (setq j (inc j)))
						((setq k (elem-get (dec k) lps))))))
		(list out '()))

	(defmethod :match? (text pattern &optional lps)
		; (. substr :match? text pattern &optional meta) -> :t | :nil
		(when (>= (defq n (length text)) (defq m (length pattern)))
			(when (defq lps (opt lps (. this :compile pattern))))
				(defq j 0 k 0 n (- n m))
				(while (<= j n)
					(cond
						((eql (elem-get j text) (elem-get k pattern))
							(when (= m (setq j (inc j) k (inc k)))
								(setq n -1)))
						((= k 0) (setq j (inc j)))
						((setq k (elem-get (dec k) lps)))))
				(= n -1)))
	)
