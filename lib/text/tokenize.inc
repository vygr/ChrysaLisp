;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a line of source as its tokens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;What the (Syntax) highlighter reads a line with, by itself: what only
;wants the tokens, a check of the source, needs no highlighter made, nor
;its inks, nor the words of the boot environment gone over for it.

(import "././consts/chars.inc")

;module
(env-push)

(defq +char_class_not_symbol (char-class " \r\f\v\n\t()")
	+char_class_not_text (char-class "\q:;{"))

(defmacro is-digit-char? (%0) (static-qq (bfind ,%0 +char_class_digit)))
(defmacro is-digit-base-char? (%0) (static-qq (bfind ,%0 +char_class_digit_base)))
(defmacro is-symbol-char? (%0) (static-qq (not (bfind ,%0 +char_class_not_symbol))))

(defun is-text-char? (%0)
	(cond
		((defq i (bfind %0 +char_class_not_text))
			(elem-get '(:string1 :keysym :comment :string2) i))
		((is-digit-char? %0) :number)
		((is-symbol-char? %0) :symbol)
		(:text)))

(defun tokenize-line (line &optional state)
	; (tokenize-line line [state]) -> (token_list state_list state)
	;a line as its tokens, and what each of them is: :text :symbol :keysym
	;:number :comment :string1 :string2. state is what the line before
	;ended in, :text if it is not given, and the state that comes back is
	;what this one ends in, to give with the next: it is not :text when a
	;string is still open
	(setd state :text)
	;a string that began on a line before can end with the first
	;character of this one, one that begins here not with its first
	(defq j 0 l (length line) i 0
		token_list (list) state_list (cap l (list))
		from (if (find state '(:string1 :string2)) 0 1))
	(while (< j l)
		(case state
			(:text
				;spaces and brackets are all that is text, past them in one go
				(if (< (setq j (bskip +char_class_not_symbol line j)) l)
					(setq state (is-text-char? (elem-get line j))))
				(when (/= i j)
					(push state_list :text)
					(push token_list (slice line i (setq i j)))))
			(:symbol
				(push state_list :symbol)
				(push token_list (slice line i (setq j
					(bskipn +char_class_not_symbol line (inc j)) i j)))
				(setq state :text))
			(:keysym
				(push state_list :keysym)
				(push token_list (slice line i (setq j
					(bskipn +char_class_not_symbol line (inc j)) i j)))
				(setq state :text))
			(:number
				(push state_list :number)
				(push token_list (slice line i (setq j
					(bskip +char_class_digit_base line (inc j)) i j)))
				(setq state :text))
			(:comment
				(push state_list :comment)
				(push token_list (slice line i (setq state :text j l))))
			(:string1
				(push state_list :string1)
				(if (setq j (find "\q" line (+ i from)))
					(push token_list (slice line i (setq state :text j (inc j) i j)))
					(push token_list (slice line i (setq j l))))
				(setq from 1))
			(:string2
				(push state_list :string2)
				(push token_list (if (setq j (find "}" line (+ i from)))
					(slice line i (setq state :text j (inc j) i j))
					(slice line i (setq j l))))
				(setq from 1))))
	(list token_list state_list state))

;module
(export-symbols '(tokenize-line))
(env-pop)
