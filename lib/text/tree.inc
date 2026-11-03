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

(import "./tokenize.inc")

;module
(env-push)

(defq +char_class_bracket (char-class "()") +char_class_opens (char-class "\q;{")
	+char_class_head_end (char-class " \r\f\v\n\t()"))

(defun tree-read (stream &optional end heads)
	; (tree-read stream [end heads]) -> (nodes open stray)
	;the nodes of a file, how many brackets were not closed, 0 for one
	;that is whole, and how many closed nothing: those are left out. Only
	;its first end lines, if given. Given heads, ("import" "defun") say,
	;only the forms at the top whose first thing is one of them: a form at
	;the top opens at the start of a line, as the source is laid out, and
	;the lines between those wanted are read only for a string they leave
	;open, nothing of them is kept
	(defq tok_state :text top (list) stack (list) into top stray 0
		bracket (lambda (ch)
			;a bracket, a form opened or closed. One at the top not of heads,
			;on a line with one that is, is let go of as it closes
			(cond
				((eql ch "(")
					(push stack into)
					(push into (list :form line_no (setq into (list)) line_no)))
				((nempty? stack)
					(setq into (pop stack))
					(elem-set (last into) 3 line_no)
					(and heads (empty? stack)
						(defq head (first (third (last into))))
						(not (and (eql (first head) :symbol) (find (third head) heads)))
						(pop into)))
				((++ stray)))))
	(lines! (lambda (line)
		(cond
			((and heads (empty? stack)
					(not (and (eql tok_state :text) (eql (first line) "(")
						(find (slice line 1 (bskipn +char_class_head_end line 1)) heads))))
				;a line between those wanted, only a string it opens or closes,
				;a " or a { before any ;
				(and (or (nql tok_state :text)
						(and (< (defq c (bskipn +char_class_opens line 0)) (length line))
							(nql (elem-get line c) ";")))
					(setq tok_state (third (tokenize-line line tok_state)))))
			((empty? (setq line (trim-end line "\r")))
				(push into (list :blank (inc (!)))))
			(:t (defq line_no (inc (!)))
				(bind '(toks states tok_next) (tokenize-line line tok_state))
				(setq tok_state tok_next)
				(each (lambda (tok state)
					(if (eql state :text)
						(progn
							;from bracket to bracket, the spaces between passed over
							(defq k 0 m (length tok))
							(while (< (setq k (bskipn +char_class_bracket tok k)) m)
								(bracket (elem-get tok k))
								(++ k)))
						(if (or (not heads) (nempty? stack)) (push into (list state line_no tok)))))
					toks states)))
		:nil) stream 0 (ifn end +max_long))
	(list top (length stack) stray))

(defun tree-form? (node &optional head)
	; (tree-form? node [head]) -> :nil | nodes
	;what is in a form, if it is one, and, given a head, if the first
	;thing in it is that symbol, "defun" say
	(and (eql (first node) :form)
		(defq nodes (third node))
		(or (not head)
			(and (nempty? nodes) (eql (first (first nodes)) :symbol) (eql (third (first nodes)) head)))
		nodes))

(defun tree-said (node nodes at &optional most)
	; (tree-said node nodes at [most]) -> (comment_node ...)
	;what is said of a form, node, which is nodes at at: the comments
	;on the lines after its first. Of one that is all on a line, those on
	;the lines after it, the next things in nodes. Of one that goes on,
	;the first comments in it, if no more than most things are before
	;them, default 3, its head, a name and what it takes, and they are
	;not on its first line, a note on the end of that is not of it
	(setd most 3)
	(defq out (list) kids (third node) line (elem-get node 3))
	(cond
		((= (second node) line)
			;past a note on the end of its line
			(and (< (inc at) (length nodes)) (eql (first (defq next (elem-get nodes (inc at)))) :comment)
					(= (second next) line)
				(++ at))
			(while (and (< (++ at) (length nodes))
					(eql (first (defq next (elem-get nodes at))) :comment)
					(= (second next) (++ line)))
				(push out next)))
		((and (defq i (some (# (if (eql (first %0) :comment) (!))) kids))
				(<= i most) (> (second (elem-get kids i)) (second node)))
			(while (and (< i (length kids)) (eql (first (defq next (elem-get kids i))) :comment))
				(push out next)
				(++ i))))
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
(export-symbols '(tree-read tree-form? tree-said tree-text))
(env-pop)
