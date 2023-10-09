;;;;;;;;;;;;;;;;;;;
; character classes
;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun char-class-intern (s)
	(unless (defq c (const (Fset 3)) e (. c :find s))
		(. c :insert (setq e s))) e)

(defun char-class (k)
	; (char-class key) -> str
	;create char class
	;these are sorted interend char strings
	;can be searched with (bfind)
	(memoize k (char-class-intern
		(apply (const cat) (sort (const cmp) (apply (const cat)
			(map (# (if (and (= (length %0) 3) (eql (second %0) "-"))
						(map (const char) (range (code (first %0)) (inc (code (last %0)))))
						(map (const progn) %0)))
				(partition 3 k)))))) 7))

;module
(export-symbols char-class)
(env-pop)
