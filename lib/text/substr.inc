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
			(. meta_cache :insert pattern (setq lps (map (const identity) pattern))))
		lps)

	(defmethod :search (text pattern &optional lps)
		; (. substr :search text pattern &optional meta) -> matches
		(defq out (list))
		(when (>= (defq l (length text)) (defq j (length pattern)))
			(when (defq i 0 lps (opt lps (. this :compile pattern)))
				(while (<= j l)
					(cond
						((every (const eql) (slice i j text) lps)
							(push out (list (list i j)))
							(setq i j j (+ j (length lps))))
						((setq i (inc i) j (inc j)))))))
		out)

	(defmethod :match? (text pattern &optional lps)
		; (. substr :match? text pattern &optional meta) -> :t | :nil
		(when (>= (defq l (length text)) (defq j (length pattern)))
			(when (defq i 0 lps (opt lps (. this :compile pattern)))
				(while (<= j l)
					(if (every (const eql) (slice i j text) lps)
						(setq l -1)
						(setq i (inc i) j (inc j))))
				(= l -1))))
	)
