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
		(unless (defq meta (. meta_cache :find pattern))
			(defq j 0 i 1 l (length pattern) meta (cap l (list 0)))
			(while (< (length meta) l)
				(cond
					((eql (elem-get pattern i) (elem-get pattern j))
						(push meta (setq i (inc i) j (inc j))))
					((= j 0) (push meta 0) (++ i))
					((setq j (elem-get meta (dec j))))))
			(. meta_cache :insert pattern meta))
		meta)

	(defmethod :search (text meta)
		; (. kmplps :search text meta) -> matches
		(if (str? meta) (setq meta (list meta)))
		(defq out (list) sstr (first meta))
		(when (>= (defq n (length text)) (defq m (length sstr)))
			(defq j 0 k 0)
			(while (< j n)
				(cond
					((eql (elem-get text j) (elem-get sstr k))
						(when (= m (setq j (inc j) k (inc k)))
							(push out (list (list (- j k) j)))
							(setq k 0)))
					((= k 0) (++ j))
					((setq k (elem-get meta (dec k)))))))
		out)

	(defmethod :match? (text meta)
		; (. kmplps :match? text meta) -> :t | :nil
		(if (str? meta) (setq meta (list meta)))
		(defq sstr (first meta))
		(when (>= (defq n (length text)) (defq m (length sstr)))
			(defq j 0 k 0)
			(while (< j n)
				(cond
					((eql (elem-get text j) (elem-get sstr k))
						(when (= m (setq j (inc j) k (inc k)))
							(setq n -1)))
					((= k 0) (++ j))
					((setq k (elem-get meta (dec k))))))
			(= n -1)))
	)
