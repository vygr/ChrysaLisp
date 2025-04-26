;;;;;;;;;;;;;;;;;;;;;
; kmplps search class
;;;;;;;;;;;;;;;;;;;;;

(import "./search.inc")

(defclass Kmplps (&optional num_buckets) (Search)
	; (Kmplps [num_buckets]) -> kmplps
	(def this :meta_cache (Fmap num_buckets))

	(defmethod :compile (pattern)
		; (. kmplps :compile pattern) -> :nil | meta
		(raise :meta_cache)
		(unless (defq lps (. meta_cache :find pattern))
			(defq j 0 i 1 l (length pattern) lps (cap l (list 0)))
			(while (< (length lps) l)
				(cond
					((eql (elem-get pattern i) (elem-get pattern j))
						(push lps (setq i (inc i) j (inc j))))
					((= j 0) (push lps 0) (setq i (inc i)))
					((setq j (elem-get lps (dec j))))))
			(. meta_cache :insert pattern lps))
		lps)

	(defmethod :search (text pattern &optional lps)
		; (. kmplps :search text pattern [meta]) -> matches
		(defq out (list))
		(when (>= (defq n (length text)) (defq m (length pattern)))
			(when (defq j 0 k 0 lps (opt lps (. this :compile pattern)))
				(while (< j n)
					(cond
						((eql (elem-get text j) (elem-get pattern k))
							(when (= m (setq j (inc j) k (inc k)))
								(push out (list (list (- j k) j)))
								(setq k 0)))
						((= k 0) (setq j (inc j)))
						((setq k (elem-get lps (dec k))))))))
		out)

	(defmethod :match? (text pattern &optional lps)
		; (. kmplps :match? text pattern [meta]) -> :t | :nil
		(when (>= (defq n (length text)) (defq m (length pattern)))
			(when (defq j 0 k 0 lps (opt lps (. this :compile pattern)))
				(while (< j n)
					(cond
						((eql (elem-get text j) (elem-get pattern k))
							(when (= m (setq j (inc j) k (inc k)))
								(setq n -1)))
						((= k 0) (setq j (inc j)))
						((setq k (elem-get lps (dec k))))))
				(= n -1))))
	)
