;;;;;;;;;;;;;;;;;;
; source formatter
;;;;;;;;;;;;;;;;;;

; (format-lisp data [limit wide]) -> str
;
; Formats ChrysaLisp source text. Only the white space between tokens is ever
; changed, so the Lisp reader sees the same forms before and after.
;
; The line breaks in the source are kept. A line is indented from the
; structure alone, one tab in from the line its enclosing form opened on. A
; line longer than the limit is broken. See 'fmt -h' for the rules in full.

(import "lib/consts/chars.inc")

;module
(env-push)

(defq +tab_width 4 +cls_space (char-class " \t\r")
	+cls_sym_end (char-class " \t\r()")
	+cls_dq (char-class (ascii-char +char_double_quote))
	+cls_rcb (char-class "}")
	+quote_codes `'(,+char_quote ,+char_tick ,+char_comma ,+char_tilde))

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
;	1 to 5 when the line is over the limit. In a form the lowest goes first.
;	6 to 9 only when the line is half as long again, and 9 only if what
;		is then left fits on a line.
;Across forms the outermost is broken first. Once a form is over more than
;one line, every gap of its lowest weight is broken, so each body form has a
;line of its own. The flags change that.
;	:fill, no, it breaks only where a line runs out, as bindings do.
;	:pairs, the last two weights take turns, for a value then a name.
;	:one, with two or more of the last arguments, the form is always broken.
;	:clauses, an argument that always breaks is a clause, with that rule.
;
;The source scanners read a line at a time, and take a line that starts with
;defun, or the like, to be one. So no line here must start with such a word.

(defun words (%0)
	(split %0 " "))

(defq +rules (Fmap 101) +blocks (Fmap 31))

(defun rule (ops &rest rule)
	(each (# (. +rules :insert %0 rule)) (words ops)))

;definitions, the name and arguments stay on the opening line
(rule "redefun defun redefmacro defmacro defabstractmethod defmethod" '(- - 0))
(rule "structure enums bits def-class" '(- - 0))
(rule "ui-root ui-stack defclass" '(- - - 0))
(rule "def-vars union errorcase validatecase noterrorcase" '(0))
;conditionals
(rule "cond condn" '(0) :clauses)
(rule "case" '(- 0) :clauses)
(rule "pcase" '(- - 0) :clauses)
(rule "clause" '(1) :one)
(rule "if ifn" '(- 5 2) :one)
(rule "when unless while until times let let*" '(- 1) :one)
;forms with a body
(rule "progn" '(1))
(rule "lambda macro and or catch .-> setf-> memoize time-it undoable" '(- 1))
(rule "within-compile-env with-lock with-read-lock with-write-lock" '(- 1))
(rule "ui-window ui-flow ui-grid ui-backdrop ui-view ui-md ui-button" '(- - 1))
(rule "ui-label" '(- - 1))
;iterators, a function then sequences, the sequences stay together
(rule "each each! each-mergeable reach map map! rmap filter filter!" '(- 3 8))
(rule "reduce reduce! rreduce some some! rsome every notany notevery" '(- 3 8))
(rule "lines!" '(- 3 8))
;bindings, a break before a name, and before a value only if it is a
;form that will be over several lines
(rule "defq setq setd pmap" '(- 8 2) :pairs :fill)
(rule "def set scatter ui-props" '(- - 8 2) :pairs :fill)
(rule "props" '(8 2) :pairs :fill)
;never broken, a source scanner reads the form as one line
(rule "line import include ffi signature dec-method def-method def-func" '(-))
(rule "def-func-end gen-vtable gen-create gen-type host-os-call" '(-))
(rule "host-gui-call host-audio-call deffimethod defgetmethod" '(-))
(rule "defsetmethod defproxymethod" '(-))
;any other call, and a list of data
(rule "call" '(9 7) :fill)
(rule "list" '(4) :fill)
;a quoted list is data, and wraps easily
(rule "data" '(4) :fill)

;VP block forms. The lines after an :open form, up to its :close form, are
;indented. A :mid form sits one back from the lines around it.
(each (lambda ((tag ops)) (each (# (. +blocks :insert %0 tag)) (words ops)))
	'((:open "def-method def-func vpif vpifnot loop-start loop-while switch")
		(:open "loop-whilenot loop-flist loop-list-forward loop-list-backward") (:close
																						"def-func-end endif loop-end loop-until loop-untilnot endswitch")
		(:mid "else elseif elseifnot vpcase vpcasenot default vp-label")
		(:mid "errorcase validatecase noterrorcase")))

;forms that add no indent of their own, what is in them sits as if they
;were not there
(defq +transparent `',(words "errorcase validatecase noterrorcase"))

;forms that define a name, or that a source scanner looks for at the start
;of a line. The opening line of a definition, the operator and its heads, is
;never broken, and a line is never broken just before one of these, as a
;scanner would then see a definition where it saw none before.
(defq +defs `',(words (cat
						"redefun defun redefmacro defmacro defabstractmethod defmethod "
						"structure enums bits defclass def-class"))
	+scanned `',(words (cat
						"redefun defun redefmacro defmacro defabstractmethod defmethod "
						"structure enums bits defclass def-class ffi import include "
						"signature dec-method def-method def-func gen-vtable gen-create "
						"def-struct gen-type deffimethod defgetmethod defsetmethod "
						"def-enum defproxymethod *key_map* *key_map_shift* "
						"def-bit *key_map_control*")))

;what the source scanners split a line with, to get its first word
(defq +cls_docs (char-class (cat " ()'\t\r" (ascii-char 34)))
	+cls_info (char-class (cat " ()'\t\r{@}<>" (ascii-char 34))) +keep (Fmap 61))
(each (# (. +keep :insert %0 :t)) +scanned)
(. +blocks :each (# (. +keep :insert %0 %1)))

;VP forms, other than the vp-xxx instructions, that get the wider limit
(defq +vp_forms `',(words (cat
							"assign entry exit call jump f-call s-call v-call d-call r-call f-jump "
							"s-jump v-jump d-jump r-jump f-bind s-bind v-bind d-bind fn-call "
							"fn-jump fn-bind fn-string load-fields save-fields assign-fields goto "
							"gotoif gotoifnot break breakif breakifnot continue continueif "
							"continueifnot nextcaseif nextcaseifnot repeatif repeatifnot exitif "
							"exitifnot errorif errorifnot errorif-lisp-args-sig "
							"errorif-lisp-args-len errorif-lisp-args-match errorif-lisp-args-type "
							"vpif vpifnot elseif elseifnot loop-while loop-whilenot loop-until "
							"loop-untilnot vpcase vpcasenot")))
(each (# (. +keep :insert %0 :t)) +vp_forms)
(each (# (. +keep :insert %0 :t)) (words (cat
											"to-array save-net-id load-net-id assign-net-id host-os-call "
											"host-gui-call host-audio-call")))

;an open form, as it sits on the stack
(enums +f 0
	(enum op col argc blocks here split rule))

(defun keep-word? (text pos)
	; (keep-word? text pos) -> :t | :nil
	;would a source scanner, reading a line that starts at pos, take it
	;for a form it looks for ? Such a line must start where it did in the
	;source, no more and no less.
	(defq i (bskip +cls_docs text pos) j (bskip +cls_info text pos)
		w1 (slice text i (bskipn +cls_docs text i))
		w2 (slice text j (bskipn +cls_info text j)))
	(or (. +keep :find w1) (. +keep :find w2)
		;every VP instruction is a line of its own
		(starts-with "vp-" w1) (starts-with "vp-" w2)))

(defun elem-end (tokens i n)
	; (elem-end tokens i n) -> pos
	;where the element that starts at token i ends
	(defq d 0 e :nil)
	(while (and (not e) (< i n))
		(case (first (defq tok (elem-get tokens i)))
			(:lp (++ d))
			(:rp (if (<= (-- d) 0) (setq e (third tok))))
			((:atom :str) (if (= d 0) (setq e (third tok)))))
		(++ i))
	(ifn e (third (elem-get tokens (dec n)))))

(defun repl-error? (tokens body i n)
	; (repl-error? tokens body i n) -> :t | :nil
	;is the form that opens at token i a jump or call to :repl_error, such
	;a line holds the usage of a function, and is never broken
	(defq d 0 res :nil)
	(while (and (not res) (>= d 0) (< (++ i) n))
		(case (first (defq tok (elem-get tokens i)))
			(:lp (++ d))
			(:rp (-- d))
			(:atom
				(if (eql (slice body (second tok) (third tok)) ":repl_error") (setq res :t)))))
	res)

(defun form-elems (tokens i n)
	; (form-elems tokens i n) -> num
	;the count of elements in the form that opens at token i, operator and
	;all, or 1000 if it does not close on this line
	(defq d 0 cnt 0 open :t)
	(while (and open (< (++ i) n))
		(case (first (elem-get tokens i))
			(:lp
				(if (= d 0) (++ cnt))
				(++ d))
			(:rp (if (= d 0) (setq open :nil) (-- d)))
			((:atom :str) (if (= d 0) (++ cnt)))))
	(if open 1000 cnt))

(defun gap-weight (rule j)
	; (gap-weight rule j) -> :nil | num
	;the weight of the gap before argument j of a form, :nil for never
	(defq ws (first rule) n (length ws) i (dec j)
		w
			(elem-get ws
				(cond
					((< i n) i)
					((find :pairs rule) (if (even? (- i n)) (- n 2) (dec n)))
					(:t (dec n)))))
	(if (num? w) w))

(defun pairs-value? (rule j)
	; (pairs-value? rule j) -> :t | :nil
	;is argument j of a form, with this rule, the value of a binding
	(defq ws (first rule) n (length ws))
	(and (find :pairs rule)
		(> n 1)
		(defq w (gap-weight rule j))
		(eql w (elem-get ws (- n 2)))
		:t))

(defun low-weight (rule)
	; (low-weight rule) -> :nil | num
	;the lowest weight of a rule, :nil if it never breaks
	(defq ws (filter (# (num? %0)) (first rule)))
	(if (nempty? ws) (reduce (# (min %0 %1)) ws)))

(defun form-rule (op par_rule par_argc tokens body i n)
	; (form-rule op par_rule par_argc tokens body i n) -> rule
	;the rule for the form that opens at token i, given the rule of the
	;form around it, and the index of that form's last element
	(cond
		((and par_rule
			(find :clauses par_rule)
			(eql (gap-weight par_rule (inc par_argc)) 0))
			(. +rules :find "clause"))
		((and (> i 0)
			(eql (first (defq tok (elem-get tokens (dec i)))) :q)
			(eql (slice body (second tok) (third tok)) "'"))
			(. +rules :find "data"))
		((not op) (. +rules :find "list"))
		;a line that holds the usage of a function is never broken
		((and (find op '("jump" "call")) (repl-error? tokens body i n))
			(. +rules :find "line"))
		((. +rules :find op))
		;a list that starts with a keyword is properties, names and values
		((and (starts-with ":" op) (not (find op '(":t" ":nil"))))
			(. +rules :find "props"))
		((. +rules :find "call"))))

(defun must-break? (rule elems)
	; (must-break? rule elems) -> :t | :nil
	;is the form broken however short, as it has more than the one of its
	;last arguments
	(and (find :one rule) (>= (- elems (length (first rule))) 2) :t))

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
				(defq cls (if (= c +char_lcb) +cls_rcb +cls_dq)
					j (bskipn cls body (inc i)))
				(cond
					((< j n) (push tokens (list :str i (setq i (inc j)))))
					(:t
						(push tokens (list :str i n))
						(setq i n instr cls))))
			((find c +quote_codes) (push tokens (list :q i (setq i (inc i)))))
			(:t
				(push tokens
					(list :atom i (setq i (bskipn +cls_sym_end body i)))))))
	(list tokens instr))

(defun form-op (tokens body i)
	; (form-op tokens body i) -> :nil | str
	;the operator of the form that opens at token i
	(and (< (++ i) (length tokens))
		(eql (first (defq tok (elem-get tokens i))) :atom)
		(slice body (second tok) (third tok))))

(defun line-op (tokens body)
	; (line-op tokens body) -> :nil | str
	;the operator of the form the line starts with
	(defq i 0 n (length tokens))
	(while (and (< i n) (eql (first (elem-get tokens i)) :q)) (++ i))
	(and (< i n) (eql (first (elem-get tokens i)) :lp) (form-op tokens body i)))

(defun next-op (tokens body i n)
	; (next-op tokens body i n) -> :nil | str
	;the word a line would start with, were it to start at token i
	(while (and (< i n) (find (first (elem-get tokens i)) '(:q :lp))) (++ i))
	(and (< i n)
		(eql (first (defq tok (elem-get tokens i))) :atom)
		(slice body (second tok) (third tok))))

(defun solid-frame ()
	; (solid-frame) -> :nil | frame
	;the innermost open form that is not transparent
	(defq k (length stack) f :nil)
	(while (and (not f) (>= (-- k) 0))
		(unless (find (elem-get (elem-get stack k) +f_op) +transparent)
			(setq f (elem-get stack k))))
	f)

(defun apply-tokens (tokens body ind end)
	; (apply-tokens tokens body ind end)
	;move the stack of open forms over the tokens up to end
	(defq i -1 n (length tokens))
	(while (< (++ i) end)
		(case (first (defq tok (elem-get tokens i)))
			(:lp
				(defq op (form-op tokens body i)
					par (if (nempty? stack) (last stack))
					rule
						(form-rule op
							(if par
								(elem-get par +f_rule))
							(if par (elem-get par +f_argc) 0) tokens body i n))
				(if par (elem-set par +f_argc (inc (elem-get par +f_argc))))
				;the column the form starts at is kept, a line that carries
				;it on is indented to just past there
				(push stack
					(list op (+ (* ind +tab_width) (second tok)) -1 0 :t
						(and relay (must-break? rule (form-elems tokens i n))) rule)))
			(:rp
				(when (nempty? stack)
					(defq f (pop stack) op (elem-get f +f_op)
						tag (and op (. +blocks :find op)))
					(when (or (eql tag :open) (eql tag :close))
						(defq step (if (eql tag :open) 1 -1) tgt (solid-frame))
						(if tgt
							(elem-set tgt +f_blocks (max 0 (+ (elem-get tgt +f_blocks) step)))
							(setq top_blocks (max 0 (+ top_blocks step)))))))
			((:atom :str)
				(when (nempty? stack)
					(elem-set (defq f (last stack)) +f_argc (inc (elem-get f +f_argc))))))))

(defun line-indent (tokens body lead)
	; (line-indent tokens body lead) -> indent
	;the indent of a line, from the open forms and what it starts with
	(defq f (solid-frame) op (line-op tokens body)
		tag (and op (. +blocks :find op))
		ind (if f (+ (/ (elem-get f +f_col) +tab_width) 1 (elem-get f +f_blocks)
					;a value on a line of its own sits in from its name
					(if (pairs-value? (elem-get f +f_rule) (inc (elem-get f +f_argc))) 1 0))
				top_blocks))
	(cond
		((or (eql tag :mid) (eql tag :close)) (max 0 (dec ind)))
		;a close bracket sits under the line its form opened on
		((eql (first (first tokens)) :rp)
			(if (nempty? stack) (/ (elem-get (last stack) +f_col) +tab_width) 0))
		((and (not f) (eql (first (first tokens)) :cmt))
			;a comment outside of any form
			(cond
				;at the margin it stays at the margin, as a banner does
				((or (empty? lead) (starts-with ";;" body)) 0)
				;notes under a top level form keep their indent
				((and note (= top_blocks 0) (nempty? lead)) 1)
				(:t ind)))
		(:t ind)))

;;;;;;;;;;
; wrapping
;;;;;;;;;;

(defun ahead (chain cands tokens)
	; (ahead chain cands tokens) -> :nil | token index
	;look ahead. A break has been found inside the forms that open on this
	;line at the tokens in chain, outermost first. Those forms are going to
	;be over several lines. Better that such a form starts a line of its
	;own, near the left, than hangs off the end of this one. So give the
	;gap before the outermost of them, if there is one and it is more than
	;a last resort.
	(some (lambda (start)
			(while (and (> start 0)
					(eql (first (elem-get tokens (dec start))) :q))
				(-- start))
			(when (and (defq c (some (# (if (= (second %0) start) %0)) cands))
					(<= (elem-get c 4) 8))
				;if the form is the value of a binding, the break goes before
				;its name, the name is not to be left at the end of a line
				(defq rule (elem-get c 6)
					name
						(and (find :pairs rule)
							(/= (elem-get c 4) (low-weight rule))
							(some (# (if (and (= (first %0) (first c))
											(= (second %0) (dec (second c)))) %0))
								cands)))
				(second (ifn name c))))
		chain))

(defun find-break (tokens body ind limit)
	; (find-break tokens body ind limit) -> :nil | token index
	;where to break a line, by the rules of the forms on it
	(defq n (length tokens) n (if (eql (first (last tokens)) :cmt) (dec n) n) res :nil)
	(when (> n 1)
		(defq basei (dec (length stack))
			bf (if (>= basei 0) (elem-get stack basei))
			base_argc (if bf (elem-get bf +f_argc) 0)
			code_end (third (elem-get tokens (dec n)))
			width (+ (* ind +tab_width) code_end) loc (list) gid 0 cands (list)
			i -1 prev :nil seen :nil)
		(defq cut :t)
		(while (progn (while (and (not res) (< (++ i) n)
								;on a line that is too long, there is no need to look
								;past half as long again, once there is a place to break
								(or (not cut)
									(<= width limit)
									(empty? cands)
									(<= (+ (* ind +tab_width) (second (elem-get tokens i))) (/ (* limit 3) 2))))
						(defq kind (first (elem-get tokens i)))
						;is the gap before this token a place to break ? Not if a scanner
						;would take the new line for a form it looks for, nor in a form
						;that never breaks, nor in the opening line of a definition.
						(when (and seen
								(not (eql kind :rp))
								(not (find prev '(:q :lp)))
								(not (keep-word? body (second (elem-get tokens i))))
								(notany (# (or (not (low-weight (second %0)))
											(and (not (eql %0 (last loc)))
												(find (first %0) +defs)
												(not (gap-weight (second %0) (third %0))))))
									loc))
							;the form the gap is in, one opened on this line, else the
							;one the line started in
							(defq g
									(cond
										((nempty? loc) (last loc))
										(bf
											(list (elem-get bf +f_op)
												(elem-get bf +f_rule) base_argc
												(neg (inc basei))
												(elem-get bf +f_split)))))
							(cond
								;one top level form to a line
								((not g) (setq res i))
								((defq w (gap-weight (second g) (inc (third g))))
									(bind '(gop grule gargc
											ggid gbroke &ignore) g)
									(if (or (= w 0)
											;a form over more than one line breaks at
											;every gap of its lowest weight
											(and gbroke
												(not (find :fill grule))
												(= w (low-weight grule))))
										(setq res (or (ahead (map (# (elem-get %0 5)) loc) cands tokens) i))
										;how wide the line is up to here, and the wider
										;of that and what would be left on the next
										(push cands
											(list (length loc) i
												(defq w1 (+ (* ind +tab_width) (third (elem-get tokens (dec i))))) ggid
												w
												(max w1
													(+ (* (inc ind) +tab_width)
														(- code_end (second (elem-get tokens i))))) grule
												;where each form opened on this line, that the
												;gap is in, starts
												(map (# (elem-get %0 5)) loc)))))))
						;move over the token
						(case kind
							(:lp
								(defq op (form-op tokens body i)
									par (if (nempty? loc) (last loc))
									rule
										(form-rule op
											(if par (second par)
												(if bf
													(elem-get bf +f_rule))) (if par (third par) base_argc) tokens body
											i n))
								(if par (elem-set par 2 (inc (third par))) (++ base_argc))
								(push loc
									(list op rule -1 (++ gid)
										(must-break? rule (form-elems tokens i n)) i)))
							(:rp
								(if (nempty? loc) (pop loc)
									(setq basei (dec basei)
										bf
											(if (>= basei 0)
												(elem-get stack basei))
										base_argc
											(if bf (elem-get bf +f_argc) 0))))
							((:atom :str)
								(if (nempty? loc)
									(elem-set (defq g (last loc)) 2 (inc (third g)))
									(++ base_argc))))
						(setq prev kind seen (or seen (not (eql kind :rp)))))
				(when (and (not res) (> width limit) (nempty? cands))
					;the limit is pressure to break, not an order to. First a
					;place, a gap of weight 5 or under, that leaves the line
					;within the limit. Only if the line is half as long again,
					;then any gap but a last resort, that leaves the line within
					;that. Then a last resort, weight 9, if what is left then
					;fits. Then any gap but a last resort.
					(defq hard (/ (* limit 3) 2) far (> width hard) all cands
						t1
							(filter (# (and (<= (elem-get %0 4) 5)
										(<= (third %0) limit)))
								cands)
						t2
							(filter (# (and (<= (elem-get %0 4) 8)
										(<= (third %0) hard)))
								cands)
						t3
							(filter (# (and (= (elem-get %0 4) 9)
										(<= (third %0) limit)
										(<= (elem-get %0 5) limit)))
								cands)
						pool
							(cond
								((nempty? t1) t1)
								((not far) t1)
								((nempty? t2) t2)
								((nempty? t3) t3)
								(:t (filter (# (<= (elem-get %0 4) 8)) cands)))
						fit (or (nempty? t1) (nempty? t2) (nempty? t3)) best :nil)
					(when (nempty? pool)
						;the outermost form, then its lowest weight
						(defq depth
								(reduce (# (min %0 (first %1))) pool +max_int)
							pool (filter (# (= (first %0) depth)) pool)
							low
								(reduce (# (min %0 (elem-get %1 4)))
									pool +max_int)
							pool (filter (# (= (elem-get %0 4) low)) pool)
							rule (elem-get (first pool) 6)
							even (filter (# (<= (elem-get %0 5) limit)) pool))
						(setq best
								(cond
									((not fit) (first pool))
									;every such gap is going to break, so the first
									((not (find :fill rule)) (first pool))
									;bindings are filled, as many to a line as fit
									((or (find :pairs rule) (empty? even)) (last pool))
									;else where the two lines come out most even
									(:t
										(reduce (# (if (<= (elem-get %1 5) (elem-get %0 5)) %1 %0)) even)))))
					;look ahead, the break may be better made further out
					(if best
						(setq best
								(list 0
									(or (ahead (elem-get best 7) all tokens) (second best)))))
					;a break must be worth it. If all it would do is put a
					;scrap on a line of its own, the line is left as it is,
					;until it is half as long again.
					(if (and best
							(or far
								(>= (- code_end (second (elem-get tokens (second best)))) (/ limit 4))))
						(setq res (second best))))
				;if nothing was chosen, and the look along the line stopped short,
				;then look at the rest of it
				(and (not res) cut (< i n) (setq cut :nil i (dec i))))))
	res)

;;;;;;;;;;;
; formatter
;;;;;;;;;;;

(defun squeeze (body tokens)
	; (squeeze body tokens) -> str
	;the line with one space between its tokens, and none inside a bracket.
	;The gap before a comment at the end is kept, it may line comments up.
	(defq out (list) prev :nil pend 0)
	(each (lambda ((kind s e))
			(cond
				((not prev))
				((eql kind :cmt) (push out (slice body pend s)))
				((or (eql kind :rp) (find prev '(:lp :q))))
				(:t (push out " ")))
			(push out (slice body s e))
			(setq prev kind pend e))
		tokens)
	(apply (const cat) out))

(defun unbreak (data)
	; (unbreak data) -> (lines frozen)
	;take out the line breaks inside forms, so the layout is then made
	;from nothing but the code. A line stays a line of its own if it is
	;blank, a comment, follows a comment, is in a string or the help text
	;of a command, or starts with a form the source scanners look for.
	;A frozen line is one that must not then be broken. The doc builder
	;reads the comments under a definition, and the lines of a key map.
	(defq out (list) frozen (list) cur :nil cur_end :nil depth 0 instr :nil
		raw :nil keys :nil joined :nil stream (string-stream data))
	(defun flush (frz)
		(when cur
			(push out cur)
			(push frozen frz)
			(setq cur :nil)))
	(while (defq line (read-line stream))
		(defq start_instr instr
			body (if instr line (trim-start line +cls_space))
			lead (slice line 0 (- (length line) (length body))))
		(bind '(tokens instr) (scan-line body instr))
		(defq kind (if (nempty? tokens) (first (first tokens)))
			usage
				(and (= depth 0)
					(not start_instr)
					(starts-with "(defq usage" body))
			delta
				(reduce (# (case (first %1)
							(:lp (inc %0))
							(:rp (dec %0))
							(:t %0)))
					tokens 0))
		(cond
			((or start_instr raw usage (empty? tokens))
				;kept as it is
				(flush :nil)
				(push out (if instr line (trim-end line +cls_space)))
				(push frozen :t)
				(if usage (setq raw :t))
				(if (empty? tokens) (setq keys :nil)))
			(:t
				(defq body
						(squeeze (if instr body
									(trim-end body +cls_space)) tokens))
				(cond
					(keys
						;a line of a key map
						(flush :t)
						(setq joined :nil cur (cat lead body)))
					((and cur
						(> depth 0)
						(not cur_end)
						(not (eql kind :cmt))
						(not (keep-word? body 0)))
						(setq joined :t
							cur
								(cat cur
									(if (or (eql kind :rp)
											(find (last cur) "('`,~")) ""
										" ") body)))
					(:t ;comments right under a line the scanners look for are
						;its docs, and must stay right under it
						(flush (and cur
								(not joined)
								(eql kind :cmt)
								(keep-word? cur 0)))
						(setq joined :nil cur (cat lead body))))
				(if (starts-with "*key_map" body) (setq keys :t))))
		(setq depth (max 0 (+ depth delta))
			cur_end
				(or instr
					(and (nempty? tokens) (eql (first (last tokens)) :cmt))))
		(if (= depth 0) (setq raw :nil))
		(if instr (flush :t)))
	(flush keys)
	(list out frozen))

(defun format-lisp (data &optional limit wide)
	; (format-lisp data [limit wide]) -> str
	;format the source text. limit is the line length to break at, default
	;80, VP assembler lines get half as much again. wide if it is all VP.
	;A limit of 0 keeps the line breaks of the source, and only indents and
	;tidies.
	(setd limit 80)
	(defq relay (> limit 0) lines (list) frozen (list))
	(if relay (bind '(lines frozen) (unbreak data))
		(lines! (# (push lines %0) (push frozen :nil) :nil)
			(string-stream data)))
	(defq out (list) stack (list) top_blocks 0 instr :nil raw :nil note :nil
		joinable :nil line_idx -1)
	(while (< (++ line_idx) (length lines))
		(defq line (elem-get lines line_idx) start_instr instr
			body (if instr line (trim-start line +cls_space))
			lead (slice line 0 (- (length line) (length body))))
		(bind '(tokens instr) (scan-line body instr))
		;trailing white space goes, unless it is in a string
		(unless instr
			(when (/= (length (defq trimmed (trim-end body +cls_space))) (length body))
				(setq body trimmed)
				(if (and (nempty? tokens) (eql (first (last tokens)) :cmt))
					(elem-set (last tokens) 2 (length body)))))
		;close brackets at the start of a line join the line above
		(defq moved :nil)
		(when (and joinable
				(not start_instr)
				(not raw)
				(nempty? tokens)
				(eql (first (first tokens)) :rp))
			(defq k 0)
			(while (and (< k (length tokens))
					(eql (first (elem-get tokens k)) :rp))
				(++ k))
			(elem-set out -2 (cat (last out) (pad "" k ")")))
			(apply-tokens tokens body 0 k)
			(setq moved :t note (empty? stack)
				body
					(if (< k (length tokens))
						(slice body (second (elem-get tokens k)) -1)
						""))
			(bind '(tokens instr) (scan-line body :nil)))
		(cond
			((or start_instr raw)
				;starts in a string, or is in a raw form, so is kept as is
				(push out (cat lead body))
				(apply-tokens tokens body 0 (length tokens))
				(each (# (elem-set %0 +f_here :nil)) stack)
				(if (empty? stack) (setq raw :nil))
				(setq joinable :nil note :nil))
			((empty? tokens)
				;blank line, only ever one
				(unless (or moved (empty? out) (eql (last out) "")) (push out ""))
				(unless moved (setq joinable :nil note :nil)))
			((and (empty? stack) (starts-with "(defq usage" body))
				;the help text of a command, kept as is
				(push out body)
				(apply-tokens tokens body 0 (length tokens))
				(each (# (elem-set %0 +f_here :nil)) stack)
				(setq raw (nempty? stack) joinable :nil note :nil))
			(:t ;a line of code, or a comment. It may be broken into several,
				;each part is then looked at again, as it may still be too long
				(defq parts (list body))
				(while (defq body (pop parts))
					(bind '(tokens instr) (scan-line body :nil))
					(defq ind (line-indent tokens body lead)
						op (line-op tokens body)
						brk
							(and relay
								(not instr)
								(not (elem-get frozen line_idx))
								(find-break tokens body ind
									(if (or wide
											(> top_blocks 0)
											(and op
												(or (starts-with "vp-" op)
													(find op +vp_forms)))) (/ (* limit 3) 2)
										limit))))
					(cond
						(brk
							(defq pos (second (elem-get tokens brk)))
							(push parts (slice body pos -1) (trim-end (slice body 0 pos) +cls_space)))
						(:t
							(push out (cat (pad "" ind "\t") body))
							(apply-tokens tokens body ind (length tokens))
							(when (nempty? parts)
								;the forms left open by this part are now broken
								(if (nempty? stack)
									(elem-set (last stack) +f_split :t))
								(each (# (if (elem-get %0 +f_here)
											(elem-set %0 +f_split :t)))
									stack))
							(each (# (elem-set %0 +f_here :nil)) stack)
							(defq is_cmt (eql (first (first tokens)) :cmt))
							(setq lead ""
								joinable
									(and (not instr)
										(not is_cmt)
										(not (eql (first (last tokens)) :cmt)))
								note
									(and (empty? stack)
										(or (not is_cmt) note)))))))))
	;one newline at the end
	(while (and (nempty? out) (eql (last out) "")) (pop out))
	(if (empty? out) ""
		(cat (join out (ascii-char +char_lf)) (ascii-char +char_lf))))

;module
(export-symbols '(format-lisp))
(env-pop)
