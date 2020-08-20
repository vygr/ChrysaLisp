(import 'sys/lisp.inc)
(import 'class/lisp.inc)


;Knuth-Morris-Pratt substr search algorithm
(defun-bind substr (pattern text)
	(defq return_list (list) lps (create-lps-table pattern) j 0 k 0)
	(while (< k (length text))
		(if (defq match (eql (elem j pattern) (elem k text)))
				(setq j (inc j) k (inc k)))
		(cond 
			((= j (length pattern))
				(push return_list (- k j)) (setq j 0 k (inc k)))
			((and (not match) (< k (length text))) 
				(cond
					((/= j 0) (setq j (elem (dec j) lps)))
					(t (setq k (inc k)))))))
	return_list)

(defun-bind create-lps-table (pattern)
	(defq lps (list 0) i 0 j 1)
	(each (lambda (_) (push lps -1)) (range 1 (length pattern)))
	(while (< j (length lps))
		(cond 
			((defq str_comp (eql (elem i pattern) (elem j pattern)))
				(elem-set j lps (inc i))
				(setq i (inc i) j (inc j)))
			((not str_comp)
				(cond
					((= (elem i lps) 0) (elem-set j lps 0) (setq j (inc j)))
					((/= (elem i lps) 0) (elem-set i lps (elem (dec i) lps)) (setq j (inc j)))))))
	lps)
