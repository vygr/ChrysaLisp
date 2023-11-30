;;;;;;;;;;;;;;;;;;;;;
; kmplps search class
;;;;;;;;;;;;;;;;;;;;;

(defclass Kmplps (&optional num_buckets) :nil
	; (Kmplps) -> kmplps
	(def this :meta_cache (Fmap num_buckets))

	(defmethod :compile (pattern)
		; (. kmplps :compile pattern) -> :nil | meta
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
		; (. kmplps :search text pattern &optional meta) -> matches
		(defq out (list))
		(when (>= (defq n (length text)) (defq m (length pattern)))
			(when (defq j 0 k 0 lps (opt lps (. this :compile pattern)))
				(while (< j n)
					(cond
						((eql (elem-get j text) (elem-get k pattern))
							(when (= m (setq j (inc j) k (inc k)))
								(push out (list (list (list (- j k) j) (slice (- j k) j text))))
								(setq k 0)))
						((= k 0) (setq j (inc j)))
						((setq k (elem-get (dec k) lps)))))))
		out)

	(defmethod :match? (text pattern &optional lps)
		; (. kmplps :match? text pattern &optional meta) -> :t | :nil
		(when (>= (defq n (length text)) (defq m (length pattern)))
			(when (defq j 0 k 0 lps (opt lps (. this :compile pattern)))
				(while (< j n)
					(cond
						((eql (elem-get j text) (elem-get k pattern))
							(when (= m (setq j (inc j) k (inc k)))
								(setq n -1)))
						((= k 0) (setq j (inc j)))
						((setq k (elem-get (dec k) lps)))))
				(= n -1))))
	)
