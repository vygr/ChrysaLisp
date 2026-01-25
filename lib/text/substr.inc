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
		(unless (defq meta (. meta_cache :find pattern))
			(. meta_cache :insert pattern (setq meta (list pattern))))
		meta)

	(defmethod :search (text meta)
		; (. substr :search meta) -> matches
		(if (str? meta) (setq meta (. this :compile meta)))
		(defq out (list) sstr (first meta))
		(when (and sstr (>= (defq l (length text)) (length sstr)))
			(defq j 0)
			(while (and (defq i (find (first sstr) text j))
						(<= (setq j (+ i (length sstr))) l))
				(if (eql (slice text i j) sstr)
					(push out (list (list i j)))
					(setq j (inc i)))))
		out)

	(defmethod :match? (text meta)
		; (. substr :match? text meta) -> :t | :nil
		(if (str? meta) (setq meta (. this :compile meta)))
		(defq out :nil sstr (first meta))
		(when (and sstr (>= (defq l (length text)) (length sstr)))
			(defq j 0)
			(while (and (defq i (find (first sstr) text j))
						(<= (setq j (+ i (length sstr))) l))
				(if (eql (slice text i j) sstr)
					(setq j -1 out :t)
					(setq j (inc i)))))
		out)
	)
