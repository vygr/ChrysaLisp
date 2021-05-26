;Knuth-Morris-Pratt substr search algorithm

(defun substr-lps (pattern)
	(defq lps (cap (length pattern) (list 0)) i 0 j 1)
	(times (dec (length pattern)) (push lps -1))
	(while (< j (length lps))
		(cond 
			((eql (elem i pattern) (elem j pattern))
				(elem-set j lps (inc i))
				(setq i (inc i) j (inc j)))
			(t	(cond
					((= (elem i lps) 0)
						(elem-set j lps 0)
						(setq j (inc j)))
					((/= (elem i lps) 0)
						(elem-set i lps (elem (dec i) lps))
						(setq j (inc j))))))) lps)

(defun substr (pattern text &optional lps)
	(defq out (list) lps (opt lps (substr-lps pattern)) j 0 k 0)
	(while (< k (length text))
		(if (defq match (eql (elem j pattern) (elem k text)))
			(setq j (inc j) k (inc k)))
		(cond 
			((= j (length pattern))
				(push out (- k j))
				(setq j 0))
			((and (not match) (< k (length text)))
				(cond
					((/= j 0)
						(setq j (elem (dec j) lps)))
					(t	(setq k (inc k))))))) out)
