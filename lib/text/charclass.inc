;;;;;;;;;;;;;;;;;;;
; character classes
;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun escape (string)
	; (escape str) -> str
	(when (>= (length string) 1)
		(setq string (apply (const cat) (reduce
			(# (if (find %1 "^$!.*+?\|[]()")
				(push %0 "\")) (push %0 %1)) string (list)))))
	string)

(defun unescape (string)
	; (unescape str) -> str
	(when (>= (length string) 2)
		(defq out (cap (length string) (list)) state :nil)
		(each (# (case state
				(:\ (push out (if (defq i (find %0 "rfvnt"))
						(elem-get i (static-q
							((ascii-char 13) (ascii-char 12)
							(ascii-char 11) (ascii-char 10)
							(ascii-char 9)))) %0))
					(setq state :nil))
				(:t (if (eql %0 "\") (setq state :\) (push out %0)))))
			string)
		(setq string (apply (const cat) out)))
	string)

(defun char-class-intern (s)
	(. (const (Fset 5)) :intern s))

(defun char-class (k)
	; (char-class key) -> str
	;create char class
	;these are sorted interend unescaped char strings
	;can be searched with (bfind)
	(memoize k (char-class-intern (apply (const cat)
		(reduce (# (if (eql (last %0) %1) %0 (push %0 %1)))
			(sort (const cmp) (apply (const cat)
				(map (# (if (and (= (length %0) 3) (eql (second %0) "-"))
						(map (const char)
							(range (code (first %0)) (inc (code (last %0)))))
						(map (const identity) %0)))
					(partition (unescape k) 3)))) (list)))) 7))

;module
(export-symbols '(char-class escape unescape))
(env-pop)

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
	+char_class_not_whole_word (char-class (cat { .,;'`()[]/\r\f\v\n\t\"} "{}")))
