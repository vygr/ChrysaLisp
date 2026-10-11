;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; source as a tree
;
; A file of Lisp or VP read as the (Syntax) highlighter reads it, a token at
; a time, and the tokens nested by their brackets. Unlike (read) nothing is
; thrown away: a comment is where it fell, among the things of the form it
; is in, and so is an empty line.
;
; A node is a list, its kind, the line it is on, from 1, and:
;
;	(:form line nodes end_line)	a bracketed form, what is in it, and
;					the line its bracket closes on
;	(:symbol line text)		and :keysym, :number, :string1, :string2
;	(:comment line text)		from its ; to the end of the line
;	(:blank line)			an empty line
;
; A string that goes over lines is a node for each line of it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./syntax.inc")

;module
(env-push)

(defq +char_class_bracket (char-class "()"))

(defun tree-read (stream)
	; (tree-read stream) -> (nodes open)
	;the nodes of a file, and how many brackets were not closed, 0 for one
	;that is whole. A bracket that closes nothing is left out
	(defq syntax (Syntax) line_no 0 top (list) stack (list) into top)
	(while (defq line (read-line stream))
		(++ line_no)
		(bind '(toks states) (. syntax :tokenize (trim-end line "\r")))
		(if (empty? toks) (push into (list :blank line_no)))
		(each (lambda (tok state)
			(if (eql state :text)
				(progn
					;from bracket to bracket, the spaces between passed over
					(defq j 0 n (length tok))
					(while (< (setq j (bskipn +char_class_bracket tok j)) n)
						(cond
							((eql (elem-get tok j) "(")
								(push stack into)
								(push into (list :form line_no (setq into (list)) line_no)))
							((nempty? stack)
								(setq into (pop stack))
								(elem-set (last into) 3 line_no)))
						(++ j)))
				(push into (list state line_no tok))))
			toks states))
	(list top (length stack)))

(defun tree-form? (node &optional head)
	; (tree-form? node [head]) -> :nil | nodes
	;what is in a form, if it is one, and, given a head, if the first
	;thing in it is that symbol, "defun" say
	(and (eql (first node) :form)
		(defq nodes (third node))
		(or (not head)
			(and (nempty? nodes) (eql (first (first nodes)) :symbol) (eql (third (first nodes)) head)))
		nodes))

(defun tree-comments (nodes)
	; (tree-comments nodes) -> (str ...)
	;the comments that are the first things of what is in a form after
	;where its code begins: those said of it. Each without its ; and the
	;space after
	(defq out (list) i (some (# (if (eql (first %0) :comment) (!))) nodes))
	(when i
		(while (and (< i (length nodes)) (eql (first (defq node (elem-get nodes i))) :comment))
			(push out (trim-start (trim-start (third node) ";") " "))
			(++ i)))
	out)

(defun tree-text (node)
	; (tree-text node) -> str
	;a node as it would be written on one line, comments and empty lines
	;left out. A list of what is left to write, it does not call itself
	(defq out (list) todo (list node))
	(while (defq node (pop todo))
		(cond
			((str? node) (push out node))
			((eql (first node) :form)
				(push todo ")")
				(each! (# (push todo %0)) (list (third node)) -1 0)
				(push todo "("))
			((find (first node) '(:comment :blank)))
			(:t (push out (third node)))))
	;a space between two things, not after a ( or before a )
	(defq text (list) was "(")
	(each (lambda (word)
		(unless (or (eql was "(") (eql word ")")) (push text " "))
		(push text (setq was word))) out)
	(apply (const cat) (push text "")))

;module
(export-symbols '(tree-read tree-form? tree-comments tree-text))
(env-pop)
