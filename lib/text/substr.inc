;;;;;;;;;;;;;;;;;;;;;
; substr search class
;;;;;;;;;;;;;;;;;;;;;

(defclass Substr (&optional num_buckets) :nil
	; (Substr) -> substr
	(def this :meta_cache (Fmap num_buckets))

	(defmethod :compile (pattern)
		; (. substr :compile pattern) -> :nil | meta
		:t)

	(defmethod :search (text pattern &optional lps)
		; (. substr :search text pattern &optional meta) -> (matches submatches)
		(defq out (list) lps (opt lps (. this :compile pattern)))
		(when lps
			(when (>= (defq l (length text)) (defq j (length pattern)))
				(defq i 0)
				(while (<= j l)
					(if (every (const eql) (slice i j text) pattern)
						(push out (list i j)))
					(setq i (inc i) j (inc j))))
		(list out '())))

	(defmethod :match? (text pattern &optional lps)
		; (. substr :match? text pattern &optional meta) -> :t | :nil
		(when (>= (defq l (length text)) (defq j (length pattern)))
			(defq i 0)
			(while (<= j l)
				(if (every (const eql) (slice i j text) pattern)
					(setq l -1)
					(setq i (inc i) j (inc j))))
			(= l -1)))
	)
