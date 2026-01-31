;;;;;;;;;;;;;;;;;;;
; character classes
;;;;;;;;;;;;;;;;;;;

(defun escape (string)
	; (escape str) -> str
	(when (>= (length string) 1)
		(setq string (apply (const cat) (reduce
			(# (if (defq i (find %1 "\t\n\v\f\r"))
				(push %0 "\\" (elem-get "tnvfr" i))
				(push %0 %1))) string (list)))))
	string)

(defun escape-regexp (string)
	; (escape-regexp str) -> str
	(when (>= (length string) 1)
		(setq string (apply (const cat) (reduce
			(# (if (find %1 "^$!.*+?|[]()\\")
				(push %0 {\})) (push %0 %1)) string (list)))))
	string)

(defun char-class (k)
	; (char-class key) -> str
	;create char class
	;these are sorted interend unescaped char strings
	;can be searched with (bfind)
	(memoize k (. (const (Fset 5)) :intern (apply (const cat)
		(usort (apply (const cat)
			(map (# (if (and (= (length %0) 3) (eql (second %0) "-"))
					(map (const char)
						(range (code (first %0)) (inc (code (last %0)))))
					(map (const identity) %0)))
				(partition (unescape k) 3)))))) 7))

;some standard char classes

(defq +char_class_space (char-class " \t")
	+char_class_white_space (char-class " \r\f\v\n\t")
	+char_class_digit (char-class "0-9")
	+char_class_digit_base (char-class "A-Za-z0-9.")
	+char_class_lower (char-class "a-z")
	+char_class_upper (char-class "A-Z")
	+char_class_alpha (char-class "A-Za-z")
	+char_class_alpha_num (char-class "A-Za-z0-9")
	+char_class_word (char-class "A-Za-z0-9_")
	+char_class_path (char-class "A-Za-z0-9_/")
	+char_class_hex (char-class "A-Fa-f0-9")
	+char_class_printable (char-class " -~")
	+char_class_not_whole_word (char-class " .,;'`()[]/\r\f\v\n\t\q{}")
	+char_class_lf (char-class "\n"))
