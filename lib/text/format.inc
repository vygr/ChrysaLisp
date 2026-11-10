;;;;;;;;;;;;;;;;;;
; source formatter
;;;;;;;;;;;;;;;;;;

; (format-lisp data [limit]) -> str
;
; Lays out ChrysaLisp source text in the house style. Only the white space
; between tokens is ever changed, so the Lisp reader sees the same forms
; before and after. The layout is made from the forms and the comments
; alone: however the source was laid out, the same forms give the same text.
; See 'fmt -h' for the rules in full.

(import "././consts/chars.inc")

;module
(env-push)

(defq +tab_width 4 +cls_space (char-class " \t\r") +cls_sym_end (char-class " \t\r()") +cls_dq (char-class "\q")
	+cls_rcb (char-class "}") +quote_codes `'(,+char_quote ,+char_tick ,+char_comma ,+char_tilde))

;;;;;;;;;;;;
; form rules
;;;;;;;;;;;;

;What a form wants is all here, in the rules. The code below knows nothing
;of any form, it only applies them.
;
;A rule is weights, then flags. There is a weight for the gap before each
;argument of the form, the first, the second and so on, and the last weight
;is for every argument that is left. A weight is how willing that gap is to
;be where a line is broken.
;	- never.
;	0 always, however short the form is.
;	1 to 8 where what comes after it does not fit on the line. Once a form
;		is over more than one line, every gap of its lowest weight breaks,
;		so each body form has a line of its own.
;	9 a last resort, only if all that is left of the form then fits on a
;		line, or what is after the gap does and there is more after that.
;The flags change that.
;	:fill, no, it breaks only where a line runs out, as bindings do.
;	:pairs, the last two weights take turns, for a value then a name.
;	:one, with two or more of the last arguments, the form is always broken.
;	:all, and :one counts every argument.
;	:half, the form is one line only if it is no wider than half the limit,
;		however much room the line has: a block, a test and a body, whose
;		body is to show at the left edge.
;	:two3, and two thirds of it.
;	:three4, and three quarters.
;	:clauses, an argument that always breaks is a clause, with that rule.
;	:flat, what is in it is filled, as a list is, whatever the forms in it
;		are.
;
;A form that fits on the line is on the line, but one that must be broken,
;and one with a test or a body, a block, only if it is no wider than its
;rule has it, :half :two3 :three4, however much room the line has: the
;left edge is to show the blocks of the code, and a line of 120 columns
;does not if all that fits is put on it. The widths are the tree's own:
;of those that would fit in 120 columns, a (when) wider than 60 is one
;line 18 times in 100, an (if) is 58 at 81 to 90 and 42 past it, a (cat)
;or a (list) 70 and more right up to the limit.
;
;One within three eighths of the limit is never broken. A name and its
;value, or a keyword and its value in a call, are not parted. A form that
;must be over more than one line starts a line of its own. One that is
;only too long, in a form that is being filled, hangs on the line it is on
;if it has half the line and its first argument fits there, but if another
;argument of the form will be over more than one line too, each starts a
;line. A lambda hangs, two of a form each start a line. What comes after
;a form over more than one line starts a line. The limit is pressure: it
;is never less than half of it past the indent, so a line far in is not
;made a narrow column.
;
;A definition is one line only among others, and if it fits: where the
;form before it or after it is a definition too, with no blank line
;between, comments aside, a table of one line definitions. One that
;stands alone has its body on lines of its own.
;
;The docs take the comment on the line after a definition for what is said
;of it, if the definition is all on a line, (tree-said). So a definition
;with a comment on the line after it stays one line, or more, as it was:
;the one thing here that is by how the source was laid out.
;
;A table is a quoted list of atoms alone or of lists, an (enum) or a (bit),
;or a form of eight or more atoms alone. It is laid out in rows of its
;cells: a name and its value in a table of bindings or properties, a form
;in a table of lists, else an atom. A row is a power of two of cells, the
;most that fit, the same in each, worked out for each part of the table
;between its comments and blank lines, and a part that fits in one row is
;one row, but a table of lists takes as many as fit. What heads a table is
;the name it is a call of, an (enum) or a (list), not a constant, a keyword
;or a number that is only the first of what is in it, and what its rule
;has before the names and values, (scatter (Fmap) ...). Its rows start on
;a line of their own if more cells then fit in each, or if it is a column.
;
;A map of keys and the functions they are for, an event map or a key map, a
;(scatter) into a map it makes where each value is a name, is a key and
;its action to a line: every one of the tree's 42 is written so.
;
;The source scanners, (tree-read) of heads, take a ( at the very start of a
;line to open a form at the top. Each form at the top starts its line, and
;every line of what is in one is at least a tab in, so they find what they
;did. A declaration they read is never broken, nor is the first line of a
;definition, and a key map is a key and its action to a line, as the doc
;builder reads it.
;
;The includes command reads a line at a time, and takes one that starts
;with (call, (entry or the like to say the class it then names is used.
;Such a form, in another, always starts a line, and no line starts with
;such a word that is not one.

(defun words (%0)
	(split %0 " "))

(defq +rules (Fmap 101) +blocks (Fmap 31))

(enums +r 0
	(enum weights low one fill pairs clauses flat all own))

(defun rule (ops ws &rest flags)
	;a rule is kept as (weights low one fill pairs clauses flat all own), all
	;worked out here, the once, so that applying a rule is only a matter of
	;looking. own is how many twelfths of the limit the form may be wide and
	;still be one line
	(defq nums (filter (# (num? %0)) ws)
		made (list ws (if (nempty? nums) (reduce (# (min %0 %1)) nums)) (if (find :one flags) :t)
			(if (find :fill flags) :t) (if (find :pairs flags) :t) (if (find :clauses flags) :t)
			(if (find :flat flags) :t) (if (find :all flags) :t)
			(cond
				((find :half flags) 6)
				((find :two3 flags) 8)
				((find :three4 flags) 9))))
	(each (# (. +rules :insert %0 made)) (words ops)))

;definitions, the name and arguments stay on the opening line. A short one,
;with the one body form, is a line
(rule "redefun defun redefmacro defmacro defabstractmethod defmethod" '(- - 1) :one)
(rule "structure enums bits def-class" '(- - 0))
(rule "ui-root ui-stack defclass" '(- - - 0))
(rule "union validatecase" '(0))
;conditionals
(rule "cond condn" '(0) :clauses)
(rule "case" '(- 0) :clauses)
(rule "pcase" '(- - 0) :clauses)
(rule "=clause" '(1) :one)
;the clause of :t, its first form is beside it, a tab in as the rest are
(rule "=clause-t" '(- 1) :one :all)
(rule "if ifn" '(- 2 2) :one :three4)
(rule "when while until times" '(- 1) :one :half)
(rule "unless let let*" '(- 1) :one :two3)
;forms with a body
(rule "progn" '(1))
;a list of forms alone with comments, a body passed to a macro, a form to
;a line
(rule "=body" '(1))
(rule "lambda macro" '(- 1) :one :two3)
(rule "#" '(- 1) :one :two3)
(rule "and or" '(- 1) :three4)
(rule "catch .-> setf-> memoize time-it undoable" '(- 1))
;an (and) of nothing but (defq)s, each makes what the next may use: each on
;a line, under the (and)
(rule "=and-defq" '(0))
(rule "within-compile-env with-lock with-read-lock with-write-lock" '(- 1))
(rule "ui-window ui-flow ui-grid ui-backdrop ui-view ui-md ui-button" '(- - 1))
(rule "ui-label ui-tool-bar ui-sheet" '(- - 1))
;iterators, a function then sequences, the sequences stay together
;a loop is a block, what gives a value is as a call
(rule "each each! each-mergeable reach lines!" '(- 3 8) :three4)
(rule "map map! rmap filter filter!" '(- 3 8))
(rule "reduce reduce! rreduce some some! rsome every notany notevery" '(- 3 8))
;bindings, a break before a name, never between a name and its value: a
;value that is too long is broken inside, not set apart from its name
(rule "defq setq setd pmap" '(- - 2) :pairs :fill)
(rule "def set scatter ui-props" '(- - - 2) :pairs :fill)
(rule "=props" '(- 2) :pairs :fill)
;declarations, never broken
(rule "=line import include ffi signature dec-method def-method def-func" '(-))
(rule "def-func-end gen-vtable gen-create gen-type host-os-call gen-host-call" '(-))
(rule "host-gui-call host-audio-call deffimethod defgetmethod" '(-))
(rule "defsetmethod defproxymethod" '(-))
;pairs of a form and what it gives, a pair to a line, the forms as written
(rule "test-cases" '(0 8 0) :pairs :flat)
(rule "=flat" '(4) :fill :flat)
;a table, laid out in rows, and a key map, a key and its action to a line
(rule "=rows" '(4))
(rule "=keymap" '(- 0 -) :pairs)
;a method call, its method is no keyword of a keyword and its value
(rule ". .? .super" '(- 7) :fill)
;any other call, and a list of data
(rule "=call" '(9 7) :fill)
(rule "=list" '(4) :fill)
;a quoted list is data, and wraps easily
(rule "=data" '(4) :fill)

;VP block forms. The lines after an :open form, up to its :close form, are
;indented. A :mid form sits one back from the lines around it. A (switch) is
;flat, its lines sit where it does, till a case: a case sits there too, and
;the lines after it are indented
(each (lambda ((tag ops)) (each (# (. +blocks :insert %0 tag)) (words ops)))
	'((:open "def-method def-func vpif vpifnot loop-start loop-while switch")
		(:open "loop-whilenot loop-flist loop-list-forward loop-list-backward")
		(:close "def-func-end endif loop-end loop-until loop-untilnot endswitch")
		(:mid "else elseif elseifnot vpcase vpcasenot default vp-label") (:mid "validatecase")))

;forms that are tables, as a quoted list of atoms is
(defq +rows_ops `',(words "enum bit"))

;the :mid forms that are the cases of a (switch)
(defq +cases `',(words "vpcase vpcasenot default else elseif elseifnot"))

;forms that define a name, whose opening line, the operator and its heads,
;is never broken, the forms that hang on the line they are given on,
;however many lines they take, the words the includes command takes, at
;the start of a line, to be a form that names a class, and the forms whose
;first argument is a test
(defq +tests `',(words "when unless while until if ifn") +hangs `',(words "lambda macro #")
	+fn_defs `',(words "redefun defun redefmacro defmacro defabstractmethod defmethod"))
(defq +defs `',(words (cat "redefun defun redefmacro defmacro defabstractmethod defmethod "
			"structure enums bits defclass def-class lambda macro"))
	+line_ops `',(words (cat "include gen-type call entry exit def-method gen-vtable gen-create "
			"to-array jump f-bind save-fields load-fields assign-fields "
			"save-net-id load-net-id assign-net-id host-os-call gen-host-call "
			"host-gui-call host-audio-call signature")))

(defun gap-weight (rule j)
	; (gap-weight rule j) -> :nil | num
	;the weight of the gap before argument j of a form, :nil for never
	(defq ws (first rule) n (length ws) i (dec j)
		w (elem-get ws
			(cond
				((< i n) i)
				((elem-get rule +r_pairs) (if (even? (- i n)) (- n 2) (dec n)))
				(:t (dec n)))))
	(if (num? w) w))

;;;;;;;;;
; scanner
;;;;;;;;;

(defun scan-line (body instr)
	; (scan-line body instr) -> (tokens instr)
	;the tokens of a line as (kind start end), as the Lisp reader would see
	;them. instr is the class of the closing char if in a string.
	(defq tokens (list) i 0 n (length body))
	(when instr
		(if (< (setq i (bskipn instr body 0)) n) (setq i (inc i) instr :nil)))
	(while (< i n)
		(defq c (code body 1 i))
		(cond
			((<= c +char_space) (++ i))
			((= c +char_lrb) (push tokens (list :lp i (setq i (inc i)))))
			((= c +char_rrb) (push tokens (list :rp i (setq i (inc i)))))
			((= c +char_semi)
				(push tokens (list :cmt i n))
				(setq i n))
			((or (= c +char_double_quote) (= c +char_lcb))
				(defq cls (if (= c +char_lcb) +cls_rcb +cls_dq) j (bskipn cls body (inc i)))
				(cond
					((< j n) (push tokens (list :str i (setq i (inc j)))))
					(:t (push tokens (list :str i n))
						(setq i n instr cls))))
			((find c +quote_codes) (push tokens (list :q i (setq i (inc i)))))
			(:t (push tokens (list :atom i (setq i (bskipn +cls_sym_end body i)))))))
	(list tokens instr))

;;;;;;;;;;;;;;;
; layout engine
;;;;;;;;;;;;;;;

;The source is read into a tree, its forms and its comments, and laid out
;afresh from that alone, by the rules above. Of how it was written only the
;comments are kept, and where a blank line was. Strings, the help text of a
;command and what a ;check off covers are kept as they are.

;a node of the tree: what it is, the quotes before it, its text or its
;forms, the comment after it on its line, a blank line before it, a comment
;at the margin, kept as it is, where it is in the source, its rule, how wide
;it is on one line, whether it must be over more than one, the comment
;right after its open bracket
(enums +n 0
	(enum kind pre text kids cmt blank margin frozen from to rule flat must open alone))

(defq +inf 1000000 +key_maps `',(words "*key_map* *key_map_shift* *key_map_control*"))

(defun src-node (kind pre text)
	; (src-node kind pre text) -> node
	(list kind pre text (list) :nil :nil :nil :nil :nil :nil :nil 0 :nil :nil :nil))

(defun src-text (lines from to)
	; (src-text lines from to) -> str
	;the text of the source from (line col) to (line col)
	(bind '(l0 c0) from)
	(bind '(l1 c1) to)
	(if (= l0 l1)
		(slice (elem-get lines l0) c0 c1)
		(join (cat (list (slice (elem-get lines l0) c0 -1)) (slice lines (inc l0) l1)
				(list (slice (elem-get lines l1) 0 c1)))
			"\n")))

(defun read-source (lines)
	; (read-source lines) -> (node ...)
	;the tree of the source: the forms, each with the comment after it on
	;its line, the comments on lines of their own, and where a blank line was
	(defq top (list) stack (list) instr :nil pre "" blank :nil off :nil open_str :nil at -1 opened :nil)
	(defun src-add (n)
		(push (if (nempty? stack) (elem-get (last stack) +n_kids) top) n)
		(if blank (elem-set n +n_blank :t))
		(if off (elem-set n +n_frozen :t))
		(setq blank :nil off :nil))
	(while (< (++ at) (length lines))
		(defq line (elem-get lines at) start_instr instr body (if instr line (trim-start line +cls_space))
			lead (- (length line) (length body)) last_node :nil pend 0 opened :nil)
		(bind '(tokens instr) (scan-line body instr))
		(cond
			(start_instr
				;the rest of a string, to its end on this line or all of it
				(defq e (bskipn start_instr body 0) piece (if (< e (length body)) (slice body 0 (inc e)) body))
				(elem-set open_str +n_text (cat (elem-get open_str +n_text) "\n" piece))
				(setq last_node open_str pend (length piece)))
			((empty? tokens) (setq blank :t)))
		(each (lambda ((kind s e))
				(defq text (slice body s e))
				(unless (find kind '(:lp :cmt)) (setq opened :nil))
				(case kind
					(:q (setq pre (cat pre text)))
					(:lp
						(src-add (defq n (src-node :list pre "")))
						(elem-set n +n_from (list at (+ lead s)))
						(push stack n)
						;a comment right after it, on its line, stays there
						(setq pre "" last_node :nil opened n))
					(:rp
						(when (nempty? stack)
							(elem-set (setq last_node (pop stack)) +n_to (list at (+ lead e)))))
					((:atom :str)
						(src-add (setq last_node (src-node kind pre text)))
						(setq pre "")
						(and instr (= e (length body)) (setq open_str last_node)))
					(:cmt
						(setq text (trim-end text +cls_space))
						(cond
							(opened
								(elem-set opened +n_open text)
								(setq opened :nil))
							(last_node
								(elem-set last_node +n_cmt (list (slice body pend s) text))
								(if (starts-with ";check off" text) (elem-set last_node +n_frozen :t)))
							(:t (src-add (defq n (src-node :note "" text)))
								(elem-set n +n_margin (or (= lead 0) (starts-with ";;" text)))
								(if (starts-with ";check off" text) (setq off :t))))))
				(setq pend e))
			tokens))
	;the help text of a command is kept as it is
	(each (lambda (n)
			(when (and (eql (elem-get n +n_kind) :list)
					(>= (length (defq kids (elem-get n +n_kids))) 2)
					(eql (elem-get (first kids) +n_text) "defq")
					(eql (elem-get (second kids) +n_text) "usage")
					(elem-get n +n_to))
				(elem-set n +n_kind :raw)
				(elem-set n +n_text (src-text lines (elem-get n +n_from) (elem-get n +n_to)))))
		top)
	top)

(defun src-forms (n)
	; (src-forms n) -> (node ...)
	;the forms of a list, not its comments
	(filter (# (nql (elem-get %0 +n_kind) :note)) (elem-get n +n_kids)))

(defun src-op (n)
	; (src-op n) -> :nil | str
	;the operator of a list, its first form if that is a word
	(and (eql (elem-get n +n_kind) :list)
		(nempty? (defq kids (src-forms n)))
		(eql (elem-get (first kids) +n_kind) :atom)
		(elem-get (first kids) +n_text)))

(defun src-key? (n)
	; (src-key? n) -> :t | :nil
	;is the node a keyword, but :t or :nil
	(and (eql (elem-get n +n_kind) :atom)
		(starts-with ":" (defq text (elem-get n +n_text)))
		(not (find text '(":t" ":nil")))
		:t))

(defun src-word? (n)
	; (src-word? n) -> :t | :nil
	;is the node a name, of a function say: no constant, keyword, number or
	;string, and nothing quoted
	(and (eql (elem-get n +n_kind) :atom)
		(eql (elem-get n +n_pre) "")
		(defq text (elem-get n +n_text))
		(not (find (first text) ":.0123456789"))
		(or (= (length text) 1)
			(not (or (starts-with "+" text)
					(and (starts-with "-" text) (find (second text) ".0123456789")))))
		:t))

(defun src-call? (n)
	; (src-call? n) -> :t | :nil
	;is the first form of the list a name it may be a call of
	(and (src-op n) (src-word? (first (src-forms n)))))

(defun src-actions? (op forms)
	; (src-actions? op forms) -> :t | :nil
	;is the list a map made of keys and the functions they are for, an event
	;map: a (scatter) into a map it makes, each value a name, the keys not
	;keywords alone
	(and (eql op "scatter")
		(> (length forms) 3)
		(even? (length forms))
		(eql (elem-get (second forms) +n_kind) :list)
		(not (some! (# (and (odd? (!)) (not (src-word? %0)))) (list forms) :nil 2 -1))
		(some! (# (and (even? (!)) (not (src-key? %0)))) (list forms) :nil 2 -1)
		:t))

(defun keep-form? (n)
	; (keep-form? n) -> :t | :nil
	;is the node a form the includes command looks for at the start of a
	;line, one of +line_ops with what it names after it. It looks past a '
	(and (defq op (src-op n))
		(find op +line_ops)
		(> (length (src-forms n)) 1)
		(= (bskip "'" (defq pre (elem-get n +n_pre)) 0) (length pre))
		:t))

(defun keep-head? (n)
	; (keep-head? n) -> :t | :nil
	;would the includes command, reading a line that starts with the node,
	;a list, take it for a form it looks for. A word alone names nothing
	(defq res :nil)
	(if (nql (elem-get n +n_kind) :list) (setq n :nil))
	(while (and n
			(not (find "`" (elem-get n +n_pre)))
			(not (find "," (elem-get n +n_pre))))
		(case (elem-get n +n_kind)
			(:atom (setq res (find (elem-get n +n_text) +line_ops) n :nil))
			;a comment right after the open bracket is what the line starts with
			(:list (setq n (if (elem-get n +n_open) :nil (first (src-forms n)))))
			(:t (setq n :nil))))
	(and res :t))

(defun src-repl? (n)
	; (src-repl? n) -> :t | :nil
	;is :repl_error anywhere in the form
	(defq todo (list n) res :nil)
	(while (and (not res) (defq n (pop todo)))
		(case (elem-get n +n_kind)
			(:atom (setq res (eql (elem-get n +n_text) ":repl_error")))
			(:list (each (# (push todo %0)) (elem-get n +n_kids)))))
	res)

(defun src-table? (n op forms)
	; (src-table? n op forms) -> :t | :nil
	;a table: a quoted list of atoms alone, or of lists, one of +rows_ops,
	;or a form of eight or more atoms alone, after what heads its names and
	;values if it is bindings, (scatter (Fmap) ...)
	(defq own (and op (. +rules :find op)) quoted (ends-with "'" (elem-get n +n_pre))
		head (if (and own (elem-get own +r_pairs) (not quoted)) (- (length (first own)) 2) 0)
		sub (and (< head (length forms))
			(some! (# (eql (elem-get %0 +n_kind) :list)) (list forms) :nil head -1)))
	(and (nempty? forms)
		(or (and op (find op +rows_ops))
			(and (ends-with "'" (elem-get n +n_pre))
				(or (not sub) (eql (elem-get (first forms) +n_kind) :list)))
			(and (not sub) (> (length forms) 8)))
		:t))

(defun src-rule (n prule argi op forms)
	; (src-rule n prule argi op forms) -> rule
	;the rule for a list, given the rule of the form it is in and which of
	;its arguments it is. A quoted list with a word first is code, made to
	;be run, and is laid out as code is
	(cond
		((and prule (elem-get prule +r_flat)) (. +rules :find "=flat"))
		;a map of keys and their actions is a key and its action to a line
		((src-actions? op forms) (. +rules :find "=keymap"))
		((and prule (elem-get prule +r_clauses) (eql (gap-weight prule argi) 0))
			(. +rules :find (if (and (nempty? forms)
						(eql (elem-get (first forms) +n_text) ":t")
						(eql (elem-get (first forms) +n_pre) ""))
					"=clause-t"
					"=clause")))
		((src-table? n op forms) (. +rules :find "=rows"))
		((and (ends-with "'" (elem-get n +n_pre)) (not op)) (. +rules :find "=data"))
		((not op)
			;a list of forms alone, that the author comments as statements
			(. +rules :find (if (and (nempty? forms)
						(every (# (eql (elem-get %0 +n_kind) :list)) forms)
						(or (elem-get n +n_open)
							(some (# (eql (elem-get %0 +n_kind) :note)) (elem-get n +n_kids))))
					"=body"
					"=list")))
		;a line that holds the usage of a function is never broken
		((and (find op '("jump" "call")) (src-repl? n)) (. +rules :find "=line"))
		((and (eql op "and")
				(> (length forms) 2)
				(not (some! (# (nql (src-op %0) "defq")) (list forms) :nil 1 -1)))
			(. +rules :find "=and-defq"))
		((. +rules :find op))
		;a list that starts with a keyword, and has one before each value, is
		;properties, names and values. Not a method and what it is given,
		;(:fpoly 0.0 0.0 mode paths)
		((and (starts-with ":" op)
				(not (find op '(":t" ":nil")))
				(even? (length forms))
				(not (some (# (and (even? (!)) (not (src-key? %0)))) forms)))
			(. +rules :find "=props"))
		((. +rules :find "=call"))))

(defun src-def? (n)
	; (src-def? n) -> :t | :nil
	;is the node a definition of a function, a macro or a method
	(and (find (src-op n) +fn_defs) (eql (elem-get n +n_pre) "") :t))

(defun src-must (n rule forms)
	; (src-must n rule forms) -> :t | :nil
	;must the list be over more than one line, a gap that always breaks,
	;more than the one of its last arguments for a rule with :one, or a
	;definition with a body that stands alone. A definition among others is
	;a line if it fits, whatever is in it
	(or (some (lambda (_) (and (> (!) 0) (eql (gap-weight rule (!)) 0))) forms)
		(and (elem-get n +n_alone) (> (length forms) (length (first rule))))
		(and (elem-get rule +r_one)
			(not (and (src-def? n) (not (elem-get n +n_alone))))
			(>= (- (length forms) (if (elem-get rule +r_all) 1 (length (first rule)))) 2))))

(defun mark-alone (kids)
	; (mark-alone kids)
	;the definitions among the kids that stand alone: neither the form before
	;nor the one after is a definition too, with no blank line between. The
	;comments between them are looked past
	(each (lambda (k)
			(when (src-def? k)
				(defq at (!) before :nil after :nil gap (elem-get k +n_blank))
				(while (and (not gap) (>= (-- at) 0))
					(if (eql (elem-get (defq b (elem-get kids at)) +n_kind) :note)
						(setq gap (elem-get b +n_blank))
						(setq before b gap :t)))
				(setq at (!) gap :nil)
				(while (and (not gap) (< (++ at) (length kids)))
					(cond
						((elem-get (defq b (elem-get kids at)) +n_blank) (setq gap :t))
						((nql (elem-get b +n_kind) :note) (setq after b gap :t))))
				(unless (or (and before (src-def? before))
						(and after (src-def? after)))
					(elem-set k +n_alone :t))
				;one with a comment on the line after it: the docs take that for
				;what is said of it if it is all on a line, (tree-said), and for
				;nothing of it if not. So it stays a line, or stays more than one,
				;as it was
				(and (< (inc (!)) (length kids))
					(eql (elem-get (defq b (elem-get kids (inc (!)))) +n_kind) :note)
					(not (elem-get b +n_blank))
					(defq from (elem-get k +n_from) to (elem-get k +n_to))
					(if (= (first from) (first to)) (elem-set k +n_frozen :t) (elem-set k +n_alone :t)))))
		kids))

(defun classify (nodes)
	; (classify nodes)
	;what each node of the tree is, worked out the once, from the tree
	;alone: the rule of each list, from the top down, then how wide each is
	;on one line, and whether it must be over more than one, from the bottom
	;up. A list of what is left to do, it does not call itself
	(defq todo (map (# (list %0 :nil 0)) nodes) order (list))
	(mark-alone nodes)
	(while (defq job (pop todo))
		(bind '(n prule argi) job)
		(push order n)
		(when (eql (elem-get n +n_kind) :list)
			(mark-alone (elem-get n +n_kids))
			(defq forms (src-forms n) op (src-op n)
				rule (if (eql prule :keymap) (. +rules :find "=keymap") (src-rule n prule argi op forms)))
			(elem-set n +n_rule rule)
			(each (lambda (k)
					(push todo (list k
							;the value of a key map is a key and its action to a line
							(if (and (> (!) 0)
									(elem-get rule +r_pairs)
									(eql (elem-get k +n_kind) :list)
									(find (elem-get (elem-get forms (dec (!))) +n_text) +key_maps))
								:keymap
								rule)
							(!))))
				forms)))
	(reach (lambda (n)
			(case (elem-get n +n_kind)
				((:atom :str)
					(defq text (elem-get n +n_text))
					(elem-set n +n_flat (if (find "\n" text) +inf (+ (length (elem-get n +n_pre)) (length text)))))
				(:list
					(defq forms (src-forms n) rule (elem-get n +n_rule)
						flat (if (elem-get n +n_open) +inf (+ (length (elem-get n +n_pre)) 1 (length forms))))
					(defq a -1)
					(each (lambda (k)
							(if (nql (elem-get k +n_kind) :note) (++ a))
							(setq flat
								;a form in it that must be over more than one line,
								;or that starts a line where it may
								(if (or (eql (elem-get k +n_kind) :note)
										(elem-get k +n_blank)
										(elem-get k +n_cmt)
										(elem-get k +n_must)
										(and (> a 0) (gap-weight rule a) (keep-form? k)))
									+inf
									(min +inf (+ flat (elem-get k +n_flat))))))
						(elem-get n +n_kids))
					(elem-set n +n_flat flat)
					;one too wide for a line of its kind, a block
					(elem-set n +n_must
						(or (src-must n rule forms)
							(and (defq own (elem-get rule +r_own)) (< flat +inf) (> flat (/ (* lim own) 12))))))
				(:t (elem-set n +n_flat +inf))))
		order))

;;;;;;;;
; layout
;;;;;;;;

;The line being made is cur, at col, indented ind. serial counts the lines,
;brk is a break that must come next, after a comment, here how many forms
;opened on this line are still open. Each form being laid out is a frame
;on a stack, what is still to do of it

(enums +l 0
	(enum node kids i rule cont base line mode forms a blocks kinds fill low pairs kw
		defs multi at entries cnt begun per head lists note pers bigs))

(defun new-frame (n kids rule cont mode)
	; (new-frame n kids rule cont mode) -> frame
	(list n kids -1 rule cont ind serial mode (if n (src-forms n) (list)) -1 0 (list) :nil :nil :nil :nil :nil :nil 0
		:nil 0 :nil 1 0 :nil :nil (list) 0))

(defun put (s)
	; (put s)
	(setq cur (cat cur s) col (+ col (length s))))

(defun put-text (s)
	; (put-text s)
	;text that may run over lines, as a string can, its lines as they are
	(while (defq i (find "\n" s))
		(push out (cat cur (slice s 0 i)))
		(setq cur "" col 0 s (slice s (inc i) -1) serial (inc serial) here 0))
	(put s))

(defun put-cmt (n)
	; (put-cmt n)
	;the comment after the node on its line, and so the line ends
	(when (defq c (elem-get n +n_cmt))
		(put (cat (first c) (second c)))
		(setq brk :t)))

(defun new-line (i &optional gap)
	; (new-line i [gap])
	;a new line indented i, after a blank line if gap
	(push out cur)
	(if gap (push out ""))
	(setq cur (pad "" i "\t") col (* i +tab_width) ind i serial (inc serial) brk :nil here 0))

(defun room (w at)
	; (room w at) -> :t | :nil
	;does w more fit on the line, now at col at. A line far in still has
	;half the limit past its indent
	(<= (+ at w) (max lim (+ (* ind +tab_width) (/ lim 2)))))

(defun room-fresh (w i)
	; (room-fresh w i) -> :t | :nil
	;does w fit on a fresh line indented i
	(<= (+ (* i +tab_width) w) (max lim (+ (* i +tab_width) (/ lim 2)))))

(defun big? (k i)
	; (big? k i) -> :t | :nil
	;will the form be over more than one line, even on a fresh line indented i
	(and (eql (elem-get k +n_kind) :list)
		(or (elem-get k +n_must) (not (room-fresh (elem-get k +n_flat) i)))
		:t))

(defun head-width (k)
	; (head-width k) -> num
	;how wide the form is up to the end of its first argument
	(defq forms (if (eql (elem-get k +n_kind) :list) (src-forms k) (list)))
	(if (< (length forms) 2)
		(elem-get k +n_flat)
		(+ (length (elem-get k +n_pre)) 2 (elem-get (first forms) +n_flat) (elem-get (second forms) +n_flat))))

(defun kid-indent (f k)
	; (kid-indent f k) -> num
	;the indent of a line that starts with k, in the form of frame f, by the
	;VP blocks open in it. A tab less for each switch not yet at a case, but
	;for the switch that k is a case or the end of
	(defq kinds (elem-get f +l_kinds) op (src-op k) tag (and op (. +blocks :find op))
		flats (reduce (# (if (and (eql (first %1) "switch") (not (second %1))) (inc %0) %0)) kinds 0))
	(and (nempty? kinds)
		(eql (first (last kinds)) "switch")
		(not (second (last kinds)))
		(or (eql tag :close) (and (eql tag :mid) (find op +cases)))
		(-- flats))
	(defq i (max 0 (- (+ (elem-get f +l_cont) (elem-get f +l_blocks)) flats)))
	(if (or (eql tag :mid) (eql tag :close)) (max 0 (dec i)) i))

(defun kid-done (f k)
	; (kid-done f k)
	;after k is laid out in the form of frame f: was it over more than one
	;line, and the VP blocks open after it
	(elem-set f +l_multi (/= (elem-get f +l_at) serial))
	(if (eql (elem-get f +l_mode) :top)
		(elem-set f +l_note (or (nql (elem-get k +n_kind) :note) (elem-get f +l_note))))
	(when (and
			(defq op (src-op k))
			(defq tag (. +blocks :find op)))
		(defq kinds (elem-get f +l_kinds))
		(case tag
			(:open
				(push kinds (list op :nil))
				(elem-set f +l_blocks (inc (elem-get f +l_blocks))))
			(:close
				(pop kinds)
				(elem-set f +l_blocks (max 0 (dec (elem-get f +l_blocks)))))
			(:mid
				(and (find op +cases)
					(nempty? kinds)
					(eql (first (last kinds)) "switch")
					(elem-set (last kinds) 1 :t))))))

(defun cell-width (c)
	; (cell-width c) -> num
	(reduce (# (+ %0 (elem-get %1 +n_flat))) c (dec (length c))))

(defun table-per (g at row)
	; (table-per g at row) -> num
	;the most cells of g to a row, a power of two, that fit in every row,
	;the first starting at at, the others at row, or all of them in the one
	(defq per 1 try 2 n (length g))
	(while (<= try n)
		(defq w 0 cnt 0 fits :t pos at)
		(each (lambda (cl)
				(when (= cnt try) (setq w 0 cnt 0 pos row))
				(setq w (+ w (if (> cnt 0) 1 0) (cell-width cl)) cnt (inc cnt))
				(unless (room w pos) (setq fits :nil))
				(if (elem-get (last cl) +n_cmt) (setq w 0 cnt 0 pos row)))
			g)
		(if fits
			(setq per try try (if (and (< try n) (> (* try 2) n)) n (* try 2)))
			(setq try (inc n))))
	per)

(defun table-plan (f n)
	; (table-plan f n)
	;a table over more than one line: what heads it, then its cells in rows.
	;A cell is a name and its value in a table of bindings or properties, a
	;form in a table of lists, else an atom. A row is a power of two of cells,
	;the most that fit, the same in each, but a table of lists takes as many
	;as fit. A comment, or a blank line, starts a row
	(defq op (src-op n) own (and op (. +rules :find op)) quoted (ends-with "'" (elem-get n +n_pre))
		pair (and (not quoted) own (elem-get own +r_pairs) :t)
		head (cond
			(quoted 0)
			(pair (- (length (first own)) 2))
			((src-call? n) 1)
			(0))
		all (src-forms n)
		of_lists (and (< head (length all))
			(some! (# (eql (elem-get %0 +n_kind) :list)) (list all) :nil head -1))
		cells (list) cell :nil a -1 kinds (list) hw -1)
	(and (not quoted) op (starts-with ":" op) (setq pair :t head 0))
	;what each kid is: :head, :note, :cell the first of a cell, :more
	(each (lambda (k)
			(cond
				((eql (elem-get k +n_kind) :note)
					(push cells k)
					(push kinds :note)
					(setq cell :nil))
				((< (++ a) head)
					(push kinds :head)
					(setq hw (+ hw 1 (elem-get k +n_flat))))
				((and cell pair (= (length cell) 1))
					(push cell k)
					(push kinds :more)
					(if (elem-get k +n_cmt) (setq cell :nil)))
				(:t (push cells (setq cell (list k)))
					(push kinds :cell)
					(if (or (not pair) (elem-get k +n_cmt)) (setq cell :nil)))))
		(elem-get n +n_kids))
	;each part of the table, between its comments and blank lines, has the
	;most cells to a row, a power of two, that fit in every row of it. The
	;first row starts on a line of its own if more fit so
	(defq row (* (elem-get f +l_cont) +tab_width) start (+ col (max 0 hw) (if (> head 0) 1 0)) groups (list)
		group (list) pers (list))
	(each (lambda (cl)
			(cond
				((eql (elem-get cl +n_kind) :note)
					(if (nempty? group) (push groups group))
					(setq group (list)))
				(:t (when (and (nempty? group) (elem-get (first cl) +n_blank))
						(push groups group)
						(setq group (list)))
					(push group cl))))
		cells)
	(if (nempty? group) (push groups group))
	(each (lambda (g)
			(cond
				(of_lists (push pers 1))
				((= (!) 0)
					(defq here_per (table-per g start row) fresh_per (table-per g row row))
					(cond
						;on a line of its own if more then fit to a row, and if
						;it is a column, which is all under its head
						((and (> head 0) (or (> fresh_per here_per) (and (= here_per 1) (> (length g) 1))))
							(elem-set f +l_begun :fresh)
							(push pers fresh_per))
						((push pers here_per))))
				((push pers (table-per g row row)))))
		groups)
	(elem-set f +l_entries kinds)
	(elem-set f +l_per (if (nempty? pers) (first pers) 1))
	(elem-set f +l_pers (slice pers 1 -1))
	(elem-set f +l_head head)
	(elem-set f +l_lists of_lists))

(defun begin-list (n whole)
	; (begin-list n whole)
	;open a list, and its frame, how it is to be laid out
	(defq kids (elem-get n +n_kids) rule (elem-get n +n_rule) pre (elem-get n +n_pre) op (src-op n)
		rest (- (elem-get n +n_flat) (length pre) 1) f (new-frame n kids rule (+ ind here 1) :broken)
		;the test of a block that is over more than one line, an (and) or an
		;(or): a condition to a line if it is wider than half the limit
		over (and (find op '("and" "or"))
			(eql (elem-get (defq p (last frames)) +l_mode) :broken)
			(= (elem-get p +l_a) 1)
			(find (src-op (elem-get p +l_node)) +tests)
			(> rest (/ lim 2))))
	(elem-set f +l_fill (elem-get rule +r_fill))
	(elem-set f +l_low (elem-get rule +r_low))
	(elem-set f +l_pairs (elem-get rule +r_pairs))
	(elem-set f +l_defs (and op (find op +defs) :t))
	(if (eql rule (. +rules :find "=call"))
		(elem-set f +l_kw (some (# (and (> (!) 0) (src-key? %0) (!))) (elem-get f +l_forms))))
	(put (cat pre "("))
	(when (defq c (elem-get n +n_open))
		(put c)
		(setq brk :t))
	(++ here)
	(cond
		;on one line: if it must be, it fits, or it is short
		((or (empty? kids)
				whole
				(elem-get n +n_frozen)
				(not (elem-get rule +r_low))
				(and (< rest +inf)
					(not (elem-get n +n_must))
					(not over)
					(or (room rest col) (<= rest (/ (* lim 3) 8)))))
			(elem-set f +l_mode :flat))
		((eql rule (. +rules :find "=rows"))
			(elem-set f +l_mode :table)
			(table-plan f n))
		;how many of its arguments will be over more than one line, and a
		;lambda among others is one of them
		((defq cont (elem-get f +l_cont)
				hangs (reduce (# (if (find (src-op %1) +hangs) (inc %0) %0)) (elem-get f +l_forms) 0))
			(elem-set f +l_bigs (reduce (lambda (cnt k)
						(if (and (> (!) 0)
								(or (big? k cont) (and (> hangs 1) (find (src-op k) +hangs))))
							(inc cnt)
							cnt))
					(elem-get f +l_forms) 0))))
	(push frames f))

(defun place-broken (f k)
	; (place-broken f k) -> whole
	;a list over more than one line, by its rule. A gap of the lowest weight
	;of a rule that does not fill always breaks, another only where what
	;comes next does not fit. One that fills takes what fits, a name with its
	;value, a keyword with its value. A form that will be over more than one
	;line starts a line of its own, but in a form that is only being filled,
	;where it has half the line
	(cond
		((eql (elem-get k +n_kind) :note)
			(new-line (kid-indent f k) (elem-get k +n_blank))
			:nil)
		(:t (defq forms (elem-get f +l_forms) rule (elem-get f +l_rule) cont (elem-get f +l_cont)
				a (inc (elem-get f +l_a)) kw (elem-get f +l_kw) low (elem-get f +l_low) filling (elem-get f +l_fill)
				pairs (elem-get f +l_pairs))
			(elem-set f +l_a a)
			(defq w (cond
					((= a 0) :nil)
					;from its first keyword, a value is not parted from it
					((and kw
							(> a kw)
							(not (even? (- a kw)))
							(src-key? (elem-get forms (dec a)))
							(not (keep-form? k)))
						:nil)
					((gap-weight rule a)))
				next (if (< (inc a) (length forms)) (elem-get forms (inc a)))
				;a name, or a keyword, and its value, are the one cell
				cell (if (and next
						(or (and pairs (eql w low)) (and kw (>= a kw) (even? (- a kw)) (src-key? k))))
					(+ (elem-get k +n_flat) 1 (elem-get next +n_flat))
					(elem-get k +n_flat)))
			(cond
				((= a 0) (if brk (new-line (kid-indent f k))))
				((or brk (elem-get k +n_blank)) (new-line (kid-indent f k) (elem-get k +n_blank)))
				((not w) (put " "))
				;what the includes command reads starts a line, and no line
				;starts with what it would take for such
				((keep-form? k) (new-line (kid-indent f k)))
				((keep-head? k) (put " "))
				((or (= w 0)
						(and (not filling) (eql w low))
						;a name after a value over more than one line
						(and pairs (eql w low) (elem-get f +l_multi)))
					(new-line (kid-indent f k)))
				;what comes after a form over more than one line starts a line
				((and (elem-get f +l_multi) (eql (elem-get (elem-get forms (dec a)) +n_kind) :list))
					(new-line (kid-indent f k)))
				((and (not (elem-get k +n_must)) (room (inc cell) col)) (put " "))
				;a name whose value does not fit starts a line
				((and pairs (eql w low)) (new-line (kid-indent f k)))
				;a form that will be over more than one line starts a line if
				;another of the form will be too, the one such hangs
				((and (> (elem-get f +l_bigs) 1) (or (big? k cont) (find (src-op k) +hangs)))
					(new-line (kid-indent f k)))
				;a scrap at the end is left on the line
				((and (not next) (<= cell (/ lim 4)) (<= (+ col 1 cell) (+ lim (/ lim 10)))) (put " "))
				;a lambda hangs on the line it is given on, if its first line fits
				((find (src-op k) +hangs) (if (room (inc (head-width k)) col) (put " ") (new-line (kid-indent f k))))
				;a last resort, only if what is left then fits, or it does
				;and there is more to come after it
				;a word or a string stays beside what it is given to
				((= w 9)
					(if (and (not (elem-get k +n_must))
							(eql (elem-get k +n_kind) :list)
							(or (and next (room-fresh cell cont))
								(room-fresh (reduce! (# (+ %0 1 (elem-get %1 +n_flat))) (list forms) 0 a -1) cont)))
						(new-line (kid-indent f k))
						(put " ")))
				((not filling) (new-line (kid-indent f k)))
				;one that must be over more than one line starts a line, one
				;that is only too long has the rest of the line if it is half,
				;and its first argument fits there, or would not on a line of
				;its own either
				((or (room-fresh cell cont)
						(elem-get k +n_must)
						(< (- (max lim (+ (* ind +tab_width) (/ lim 2))) col) (/ lim 2))
						(and (not (room (inc (defq hw (head-width k))) col)) (room-fresh hw cont)))
					(new-line (kid-indent f k)))
				((put " ")))
			(and (elem-get f +l_defs) (> a 0) (not w)))))

(defun next-per (f)
	; (next-per f)
	;the count of cells to a row of the next part of the table, if the part
	;before had any
	(when (and (elem-get f +l_note)
			(nempty? (defq pers (elem-get f +l_pers))))
		(elem-set f +l_per (first pers))
		(elem-set f +l_pers (slice pers 1 -1)))
	(elem-set f +l_note :nil))

(defun place-table (f k i)
	; (place-table f k i)
	(defq cont (elem-get f +l_cont) cnt (elem-get f +l_cnt))
	(case (elem-get (elem-get f +l_entries) i)
		(:head (if (> i 0) (put " ")))
		(:more (put " "))
		(:note
			(new-line cont (elem-get k +n_blank))
			(next-per f)
			(setq cnt 0))
		(:cell
			(if (elem-get k +n_blank) (next-per f))
			(elem-set f +l_note :t)
			(cond
				((eql (elem-get f +l_begun) :fresh)
					(new-line cont)
					(setq cnt 0))
				(brk
					(new-line cont (elem-get k +n_blank))
					(setq cnt 0))
				((not (elem-get f +l_begun)) (if (> (elem-get f +l_head) 0) (put " ")))
				((or (elem-get k +n_blank)
						(if (elem-get f +l_lists)
							(not (room (inc (elem-get k +n_flat)) col))
							(= cnt (elem-get f +l_per))))
					(new-line cont (elem-get k +n_blank))
					(setq cnt 0))
				((put " ")))
			(elem-set f +l_begun :t)
			(setq cnt (inc cnt))))
	(elem-set f +l_cnt cnt))

(defun place-top (f k i)
	; (place-top f k i)
	;the top level forms and comments, each on its own line, a comment at the
	;margin there still, a note right under a top level form a tab in, and
	;the lines of VP function bodies by their blocks
	(if (elem-get k +n_blank) (elem-set f +l_note :nil))
	(defq at (kid-indent f k))
	(if (eql (elem-get k +n_kind) :note)
		(setq at (cond
				((elem-get k +n_margin) 0)
				((and (elem-get f +l_note) (= (elem-get f +l_blocks) 0)) 1)
				(at))))
	(if (> i 0)
		(new-line at (elem-get k +n_blank))
		(setq cur (pad "" at "\t") col (* at +tab_width) ind at)))

(defun lay-all (nodes)
	; (lay-all nodes)
	;lay out the tree. A stack of the forms being laid out, it does not call
	;itself
	(defq frames (list (new-frame :nil nodes :nil 0 :top)))
	(while (nempty? frames)
		(defq f (last frames) kids (elem-get f +l_kids) i (inc (elem-get f +l_i)))
		(cond
			((< i (length kids))
				(elem-set f +l_i i)
				(defq k (elem-get kids i)
					whole (case (elem-get f +l_mode)
						(:top
							(place-top f k i)
							:nil)
						(:flat
							(cond
								((eql (elem-get k +n_kind) :note) (new-line (elem-get f +l_cont)))
								((= i 0) (if brk (new-line (elem-get f +l_cont))))
								(brk (new-line (elem-get f +l_cont)))
								((put " ")))
							:t)
						(:table
							(place-table f k i)
							:nil)
						(:broken (place-broken f k))))
				;the line it starts on, after any break before it, but a value
				;in a binding from where its name starts
				(unless (and (eql (elem-get f +l_mode) :broken)
						(elem-get f +l_pairs)
						(> (defq a (- (elem-get f +l_a) (length (first (elem-get f +l_rule))) -2)) 0)
						(not (even? a)))
					(elem-set f +l_at serial))
				(case (elem-get k +n_kind)
					(:list (begin-list k whole))
					((:atom :str)
						(put-text (cat (elem-get k +n_pre) (elem-get k +n_text)))
						(put-cmt k)
						(kid-done f k))
					(:raw
						(put-text (elem-get k +n_text))
						(put-cmt k)
						(kid-done f k))
					(:note
						(put (elem-get k +n_text))
						(setq brk :t)
						(kid-done f k))))
			(:t (pop frames)
				(unless (eql (elem-get f +l_mode) :top)
					(if brk (new-line (elem-get f +l_base)))
					(put ")")
					(if (= (elem-get f +l_line) serial) (-- here))
					(put-cmt (elem-get f +l_node))
					(kid-done (last frames) (elem-get f +l_node)))))))

(defun format-lisp (data &optional limit)
	; (format-lisp data [limit]) -> str
	;the source laid out afresh, in the house style, from its forms and its
	;comments alone. limit is the line length that brings pressure to break,
	;default 120
	(setd limit 120)
	(defq lines (list))
	(lines! (# (push lines %0) :nil) (string-stream data))
	(defq nodes (read-source lines) out (list) cur "" col 0 ind 0 serial 0 brk :nil here 0 lim limit)
	(classify nodes)
	(lay-all nodes)
	(push out cur)
	(while (and (nempty? out) (eql (last out) "")) (pop out))
	(if (empty? out) "" (cat (join out "\n") "\n")))

;module
(export-symbols '(format-lisp))
(env-pop)
