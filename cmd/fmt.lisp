(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/files/files.inc")
(import "lib/text/syntax.inc")
(import "service/lock/app.inc")
(import "lib/streams/diff.inc")

(defq usage `(
(("-h" "--help")
"Usage: fmt [options] [path] ...

    options:
        -h --help: this help info.
        -j --jobs num: max jobs per batch, default 8.
        -w --write: overwrite files in-place, default :nil.
        -c --check: check if files need formatting without writing.

    Auto-formats ChrysaLisp source code according to tab indentation,
    declarative form templates, and pair-packing rules.

    Restricts targets to unique .lisp, .inc, and .vp files. If no paths
    are specified on the command line, paths are read from stdin.

    Template roles and numeric options:
        :head
            header parameter; stays on the same line as the opening operator.
        :body
            body statement; breaks to a new line indented by 1 tab.
        :flow
            flows elements horizontally on the same line separated by spaces.
        (:pairs [max_pairs_per_line])
            packs key-value pairs up to max_pairs_per_line per line (default 4).
            never breaks between a key and its value.
        (:clauses [max_per_line] [max_body_actions])
            controls clause structure and layout (default: 1 1).
            max_per_line: maximum number of clauses allowed on a single line.
            max_body_actions: maximum body expressions allowed for single-line flow.
            clauses with more body expressions break each action onto its own line.
        (:data [max_items_per_line])
            formats quoted data lists, packing max_items_per_line per line (default 5).
        (:choice short_tmpl multiline_tmpl)
            dynamically uses short_tmpl if the form is compact and fits on one line,
            otherwise falls back to multiline_tmpl.")
(("-j" "--jobs") ,(opt-num 'opt_j))
(("-w" "--write") ,(opt-flag 'opt_w))
(("-c" "--check") ,(opt-flag 'opt_c))
))

(defq +file_types ''(".lisp" ".inc" ".vp") +tab_width 4)

;;;;;;;;;;;;;;;;;;;;;;;
; form template rules
;;;;;;;;;;;;;;;;;;;;;;;

(defq +templates (scatter (Fmap 64)
	; definitions & bindings: (:pairs max_pairs_per_line)
	"defq" '((:pairs 4)) "setq" '((:pairs 4)) "def" '(:head (:pairs 3)) "set" '(:head (:pairs 3))
	"scatter" '(:head (:pairs 4)) "pmap" '((:pairs 4)) "defun" '(:head :head :body) "redefun" '(:head :head :body)
	"defmacro" '(:head :head :body) "redefmacro" '(:head :head :body) "defmethod" '(:head :head :body) "defabstractmethod" '(:head :head :body)
	"defclass" '(:head :head :head :body) "def-class" '(:head :head :body) "def-method" '(:head :head :body) "def-func" '(:head :body)
	"lambda" '(:head :body) "macro" '(:head :body) "let" '(:head :body) "let*" '(:head :body)
	"structure" '(:head :head :body) "enums" '(:head :head :body) "bits" '(:head :head :body) "def-vars" '(:body)

	; single-line declarations
	"deffimethod" '(:flow) "defgetmethod" '(:flow) "defsetmethod" '(:flow) "defproxymethod" '(:flow)
	"dec-method" '(:flow) "#" '(:flow)

	; conditionals & branching: (:clauses max_per_line max_body_actions)
	"cond" '((:clauses 1 1)) "condn" '((:clauses 1 1)) "case" '(:head (:clauses 1 1)) "pcase" '(:head :head (:clauses 1 1))
	"switch" '(:head (:clauses 1 1)) "if" '(:choice (:flow) (:head :body)) "ifn" '(:choice (:flow) (:head :body)) "when" '(:choice (:flow) (:head :body))
	"unless" '(:choice (:flow) (:head :body))

	; loops & iteration
	"while" '(:head :body) "until" '(:head :body) "for" '(:head :head :body) "times" '(:head :body)

	; higher-order sequence functions
	"each" '(:flow) "each!" '(:flow) "reach" '(:flow) "map" '(:flow)
	"map!" '(:flow) "rmap" '(:flow) "filter" '(:flow) "filter!" '(:flow)
	"reduce" '(:flow) "reduce!" '(:flow) "rreduce" '(:flow) "some" '(:flow)
	"some!" '(:flow) "rsome" '(:flow) "every" '(:flow) "notany" '(:flow)
	"notevery" '(:flow) "lines!" '(:flow)

	; logic, blocks, exception handling
	"and" '(:choice (:flow) (:head :body)) "or" '(:choice (:flow) (:head :body)) "throw" '(:flow) "catch" '(:flow)
	"progn" '(:body) "errorcase" '(:body) "validatecase" '(:body) "noterrorcase" '(:body)
	"time-it" '(:head :body) "undoable" '(:head :body) "within-compile-env" '(:head :body)))

(defq +major_definitions ''("defun" "redefun" "defmacro" "redefmacro" "defclass" "def-class"
	"def-method" "def-func" "structure" "enums" "bits"))

(defun role-type (role)
	(if (list? role) (first role) role))

(defun role-clauses? (role)
	(eql (role-type role) :clauses))

(defun clause-max-per-line (role)
	(if (and (list? role) (> (length role) 1))
		(second role)
		1))

(defun clause-max-body-elems (role)
	(if (and (list? role) (> (length role) 2))
		(third role)
		1))

(defun template-role (tmpl arg_count)
	(cond
		((empty? tmpl) :body)
		((<= arg_count 1) (first tmpl))
		((defq idx (dec arg_count))
			(if (< idx (length tmpl))
				(elem-get tmpl idx)
				(last tmpl)))))

(defun parent-role (form_stack)
	(if (empty? form_stack)
		:body
		(bind '(p_tmpl & p_argc &ignore) (last form_stack))
		(template-role p_tmpl p_argc)))

(defun skip-ws-nl (tokens idx)
	(defq len (length tokens) i idx)
	(while (and (< i len)
			(find (first (elem-get tokens i)) '(:ws :nl)))
		(++ i))
	i)

(defun skip-ws (tokens idx)
	; skip whitespace tokens only
	(defq len (length tokens) i idx)
	(while (and (< i len)
			(eql (first (elem-get tokens i)) :ws))
		(++ i))
	i)

(defun next-significant-idx (tokens idx)
	(defq len (length tokens) i idx res :nil)
	(while (and (< i len) (not res))
		(if (find (first (elem-get tokens i)) '(:ws :nl))
			(++ i)
			(setq res i)))
	res)

(defun skip-element (tokens idx)
	; return index after the element starting at idx
	(defq len (length tokens) i idx)
	(cond
		((>= i len) len)
		((eql (first (elem-get tokens i)) :quote)
			(skip-element tokens (inc i)))
		((eql (first (elem-get tokens i)) :lparen)
			(defq depth 1)
			(++ i)
			(while (and (< i len) (> depth 0))
				(defq t_type (first (elem-get tokens i)))
				(cond
					((eql t_type :lparen) (++ depth))
					((eql t_type :rparen) (-- depth)))
				(++ i))
			i)
		(:t (inc i))))

(defun clause-elem-count (tokens lparen_idx)
	; count top-level expressions within clause (...)
	(defq len (length tokens) i (inc lparen_idx) depth 1 count 0)
	(while (and (< i len) (> depth 0))
		(defq tok (elem-get tokens i) tok_type (first tok))
		(cond
			((or (eql tok_type :ws)
				(eql tok_type :nl)
				(eql tok_type :comment))
				(++ i))
			((eql tok_type :quote)
				(if (= depth 1) (++ count))
				(setq i (skip-element tokens (inc i))))
			((eql tok_type :lparen)
				(if (= depth 1) (++ count))
				(setq i (skip-element tokens i)))
			((eql tok_type :rparen)
				(-- depth)
				(++ i))
			(:t
				(if (= depth 1) (++ count))
				(++ i))))
	count)

(defun form-short? (tokens lparen_idx)
	; check if a form is structurally short, shallow, and fits on a single line
	(defq len (length tokens) i (inc lparen_idx) depth 1 count 0
		sub_lists 0 est_len 2 is_short :t)
	(while (and (< i len) (> depth 0) is_short)
		(defq tok (elem-get tokens i) tok_type (first tok))
		(cond
			((or (eql tok_type :ws) (eql tok_type :nl)))
			((or (eql tok_type :comment) (eql tok_type :raw))
				(setq is_short :nil))
			((eql tok_type :lparen)
				(++ depth)
				(++ sub_lists)
				(++ est_len)
				(if (or (> depth 3) (> sub_lists 3))
					(setq is_short :nil))
				(++ count))
			((eql tok_type :rparen)
				(-- depth)
				(++ est_len))
			(:t
				(defq val (second tok))
				(if (and (find tok_type '(:string :cscript)) (find "\n" val))
					(setq is_short :nil))
				(++ count)
				(setq est_len (+ est_len (length val) 1))
				(if (or (> count 12) (> est_len 65))
					(setq is_short :nil))))
		(++ i))
	(and is_short (= depth 0)))

(defun clause-actions-short? (tokens lparen_idx)
	; check if the action expression in a clause is itself short
	(defq next_i (skip-ws-nl tokens (inc lparen_idx)))
	(when (< next_i (length tokens))
		(defq act_i (skip-element tokens next_i))
		(if (or (not act_i) (>= act_i (length tokens)))
			:t
			(progn
				(setq act_i (skip-ws-nl tokens act_i))
				(cond
					((>= act_i (length tokens)) :t)
					((eql (first (elem-get tokens act_i)) :rparen)
						:t)
					((eql (first (elem-get tokens act_i)) :lparen)
						(form-short? tokens act_i))
					(:t :t))))))

(defun resolve-template (tmpl tokens lparen_idx)
	; resolve template choices based on form shortness
	(if (and (list? tmpl) (eql (first tmpl) :choice))
		(if (form-short? tokens lparen_idx) (second tmpl) (third tmpl))
		tmpl))

(defun resolve-clause-template (tokens lparen_idx clause_role)
	(defq max_body (clause-max-body-elems clause_role) elems (clause-elem-count tokens lparen_idx))
	(cond
		; single short-form body allowed on one line
		((and (<= elems (inc max_body))
			(form-short? tokens lparen_idx)
			(clause-actions-short? tokens lparen_idx))
			'(:flow))
		; multiple body expressions or multiline actions break onto new lines
		(:t '(:body))))

(defun lookup-form-template (tokens lparen_idx parent_raw_role after_quote)
	(defq next_i (skip-ws-nl tokens (inc lparen_idx)) tok (if (< next_i (length tokens)) (elem-get tokens next_i)) tok_atom (if (and tok (eql (first tok) :atom))
		(second tok)) known_tmpl (if (and tok_atom (not after_quote))
			(. +templates :find tok_atom)))
	(cond
		; 1. Quoted data list wraps items every 5 elements
		(after_quote '((:data 5)))
		; 2. Registered operator (defq, if, setq, while, etc.) always keeps its own template!
		(known_tmpl (resolve-template known_tmpl tokens lparen_idx))
		; 3. Unregistered form in a clause context is an anonymous clause
		((role-clauses? parent_raw_role)
			(resolve-clause-template tokens lparen_idx parent_raw_role))
		; 4. Default to flow
		(:t '(:flow))))

(defun wants-section-break? (tokens idx consec_nl prev_was_comment)
	; determine if a top-level form or comment warrants a blank line
	(defq next_i (next-significant-idx tokens idx))
	(cond
		((not next_i) :nil)
		((>= consec_nl 2) :t)
		; do not inject extra blank lines between consecutive comment lines
		(prev_was_comment :nil)
		(:t
			(defq next_tok (elem-get tokens next_i) t_type (first next_tok))
			(cond
				((eql t_type :comment)
					; comment banners starting with multiple semicolons
					(starts-with ";;" (second next_tok)))
				((eql t_type :lparen)
					(defq op_i (next-significant-idx tokens (inc next_i)))
					(and op_i
						(eql (first (elem-get tokens op_i)) :atom)
						(find (second (elem-get tokens op_i)) +major_definitions)))
				(:nil)))))

(defun check-template-break (form_stack at_line_start pending_nl after_quote)
	; check if current template requires a newline before this argument
	(when (and (nempty? form_stack) (not after_quote))
		(bind '(tmpl & arg_count &ignore) (last form_stack))
		(when (> arg_count 0)
			(defq raw_role (template-role tmpl arg_count) role (role-type raw_role))
			(cond
				((eql role :body) (unless (or at_line_start pending_nl) :t))
				((eql role :clauses)
					(defq max_per_line (clause-max-per-line raw_role))
					(and (>= arg_count 1)
						(not at_line_start)
						(not pending_nl)
						(= max_per_line 1)))
				((eql role :pairs)
					(defq is_def (eql (first tmpl) :head) pair_arg (if is_def (dec arg_count) arg_count) is_key (odd? pair_arg) line_pairs (elem-get (last form_stack) 5)
						max_pairs (if (and (list? raw_role) (> (length raw_role) 1))
							(second raw_role)
							4))
					(and is_key
						(> pair_arg 1)
						(not at_line_start)
						(not pending_nl)
						(>= line_pairs max_pairs)))
				((eql role :data)
					(defq max_items (if (and (list? raw_role) (> (length raw_role) 1))
						(second raw_role)
						5))
					(and (> arg_count 1)
						(not at_line_start)
						(not pending_nl)
						(= (% (dec arg_count) max_items) 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;
; fast syntax tokenizer
;;;;;;;;;;;;;;;;;;;;;;;

(defun tokenize-lisp (stream)
	(defq ends_nl :t)
	(when (/= (stream-seek stream -1 2) -1)
		(setq ends_nl (= (read-char stream) +char_lf))
		(stream-seek stream 0 0))
	(defq tokens (list) lines (list) syntax (Syntax) line_idx 0)
	(while (defq raw (read-line stream))
		(push lines raw))
	(defq num_lines (length lines))
	(while (< line_idx num_lines)
		(defq raw_line (trim-end (elem-get lines line_idx) "\r") prev_state (. syntax :get_state))
		(cond
			((and (eql prev_state :text)
				(starts-with "(defq usage" (trim-start raw_line)))
				(defq u_lines (list) depth 0 in_str :nil done :nil)
				(while (and (< line_idx num_lines) (not done))
					(defq u_line (elem-get lines line_idx) u_len (length u_line) ui 0)
					(push u_lines u_line)
					(while (< ui u_len)
						(defq uc (elem-get u_line ui))
						(cond
							(in_str
								(cond
									((eql uc "\\") (++ ui))
									((eql uc "\q") (setq in_str :nil))))
							((eql uc ";") (setq ui u_len))
							((eql uc "\q") (setq in_str :t))
							((eql uc "(") (++ depth))
							((eql uc ")")
								(-- depth)
								(if (= depth 0) (setq done :t))))
						(++ ui))
					(++ line_idx))
				(push tokens (list :raw (cat (join u_lines "\n") "\n"))))
			(:t
				(bind '(toks states) (. syntax :tokenize raw_line))
				(if (and (empty? toks) (find prev_state '(:string1 :string2)))
					(elem-set (last tokens) 1 (cat (second (last tokens)) "\n"))
					(each (lambda (val tok_state)
						(cond
							((find tok_state '(:string1 :string2))
								(defq kind (if (eql tok_state :string1) :string :cscript))
								(if (and (eql tok_state prev_state)
										(nempty? tokens)
										(eql (first (last tokens)) kind))
									(elem-set (last tokens) 1 (cat (second (last tokens)) "\n" val))
									(push tokens (list kind val))))
							((eql tok_state :comment) (push tokens (list :comment val)))
							((find tok_state '(:number :keysym))
								(push tokens (list :atom val)))
							((eql tok_state :symbol)
								(defq s val)
								(while (and (nempty? s) (find (first s) "'`~,"))
									(push tokens (list :quote (slice s 0 1)))
									(setq s (slice s 1 -1)))
								(if (nempty? s) (push tokens (list :atom s))))
							((eql tok_state :text)
								(defq tlen (length val) ti 0)
								(while (< ti tlen)
									(defq ch (elem-get val ti))
									(cond
										((or (eql ch " ") (eql ch "\t"))
											(defq ws_start ti)
											(while (and (< ti tlen)
													(or (eql (defq c (elem-get val ti)) " ")
														(eql c "\t")))
												(++ ti))
											(push tokens (list :ws (slice val ws_start ti))))
										((eql ch "(")
											(push tokens (list :lparen "("))
											(++ ti))
										((eql ch ")")
											(push tokens (list :rparen ")"))
											(++ ti))
										((find ch "'`~,")
											(push tokens (list :quote (str ch)))
											(++ ti))
										(:t
											(defq atom_start ti)
											(while (and (< ti tlen)
													(not (find (elem-get val ti) " \t()'`~,")))
												(++ ti))
											(push tokens (list :atom (slice val atom_start ti))))))))) toks states))
				; preserve line endings and trailing newline if present in source
				(when (or (< (inc line_idx) num_lines) ends_nl)
					(unless (find (. syntax :get_state) '(:string1 :string2))
						(push tokens (list :nl "\n"))))
				(++ line_idx))))
	tokens)

(defun inc-arg-count (form_stack)
	(when (nempty? form_stack)
		(defq frame (last form_stack) tmpl (first frame) arg_c (inc (third frame)))
		(elem-set frame 2 arg_c)
		; increment pair count when a value in :pairs completes
		(when (eql (role-type (template-role tmpl arg_c)) :pairs)
			(defq is_def (eql (first tmpl) :head) pair_arg (if is_def (dec arg_c) arg_c))
			(when (even? pair_arg)
				(elem-set frame 5 (inc (elem-get frame 5)))))))

(defun make-indent (level)
	(pad "" level "\t"))

(defun last-line-len (s)
	(if (defq pos (rfind "\n" s))
		(- (length s) (inc pos))
		(length s)))

(defun current-target-indent (form_stack)
	(if (nempty? form_stack) (elem-get (last form_stack) 3) 0))

;;;;;;;;;;;;;;;;;;;;;;;
; code formatter
;;;;;;;;;;;;;;;;;;;;;;;

(defun format-lisp (stream_or_src)
	(defq stream (if (str? stream_or_src)
		(string-stream stream_or_src)
		stream_or_src) tokens (tokenize-lisp stream) len (length tokens) out (memory-stream) cur_line_indent 0
		current_col 0 at_line_start :t after_lparen :nil after_quote :nil
		pending_nl :nil consec_nl 0 just_saw_comment :nil prev_was_comment :nil
		form_stack (list) idx 0 tok :nil tok_type :nil
		val "")

	(while (< idx len)
		(setq tok (elem-get tokens idx) tok_type (first tok) val (second tok))
		(cond
			((eql tok_type :ws) (++ idx))
			((eql tok_type :nl)
				(if just_saw_comment
					; consume newline immediately closing comment without inflating consec_nl
					(setq just_saw_comment :nil)
					(if (< consec_nl 2) (++ consec_nl)))
				(cond
					((empty? form_stack) (setq pending_nl :t))
					((defq next_sig_i (next-significant-idx tokens (inc idx)))
						(when (find (first (elem-get tokens next_sig_i)) '(:comment :raw))
							(setq pending_nl :t)))
					(:t
						; inside forms, ignore user newlines
						:nil))
				(++ idx))
			((eql tok_type :raw)
				(when pending_nl
					(times (if (>= consec_nl 2) 2 1)
						(write-blk out "\n")))
				(write-blk out val)
				(setq at_line_start :t pending_nl :nil just_saw_comment :nil prev_was_comment :nil
					consec_nl 0 current_col 0 after_lparen :nil after_quote :nil)
				(++ idx))
			((eql tok_type :comment)
				(if (or at_line_start pending_nl)
					(progn
						(when pending_nl
							(times (if (empty? form_stack)
									(if (wants-section-break? tokens idx consec_nl prev_was_comment)
										2
										1)
									(if (>= consec_nl 2) 2 1))
								(write-blk out "\n")))
						(defq ind (current-target-indent form_stack))
						(write-blk out (make-indent ind))
						(setq cur_line_indent ind current_col (* ind +tab_width)))
					(write-blk out " "))
				(write-blk out val)
				; comments always terminate the line
				; subsequent code must be on a new line
				(setq at_line_start :nil pending_nl :t consec_nl 1 just_saw_comment :t
					prev_was_comment :t current_col 0 after_lparen :nil after_quote :nil)
				(++ idx))
			((eql tok_type :rparen)
				(defq closed_frame (if (nempty? form_stack) (pop form_stack) :nil))
				(when (and pending_nl
						(eql (first (elem-get tokens (dec idx))) :comment))
					(times (if (>= consec_nl 2) 2 1)
						(write-blk out "\n"))
					(defq r_ind (if closed_frame
						(second closed_frame)
						(current-target-indent form_stack)))
					(write-blk out (make-indent r_ind))
					(setq at_line_start :t cur_line_indent r_ind current_col (* r_ind +tab_width)))
				(setq pending_nl :nil consec_nl 0 just_saw_comment :nil prev_was_comment :nil)
				(write-blk out ")")
				(setq at_line_start :nil after_lparen :nil after_quote :nil current_col (+ current_col 1))
				(if (empty? form_stack)
					; top-level form closed
					; next form starts on a new line
					(setq pending_nl :t consec_nl 0)
					(inc-arg-count form_stack))
				(++ idx))
			(:t
				; check if current template requires a newline before this argument
				(when (check-template-break form_stack at_line_start pending_nl after_quote)
					(setq pending_nl :t consec_nl (if (>= consec_nl 2) 2 1)))

				; flush deferred newlines and apply stack-directed indentation
				(when pending_nl
					(defq nl_count (if (empty? form_stack)
						(if (wants-section-break? tokens idx consec_nl prev_was_comment)
							2
							1)
						(if (>= consec_nl 2) 2 1)))
					(times nl_count
						(write-blk out "\n"))
					(defq ind (current-target-indent form_stack))
					(write-blk out (make-indent ind))
					(setq at_line_start :t pending_nl :nil just_saw_comment :nil prev_was_comment :nil
						cur_line_indent ind consec_nl 0 current_col (* ind +tab_width))
					; reset pair counter on current line
					(each (lambda (f)
						(elem-set f 5 0)) form_stack))

				(cond
					((find tok_type '(:cscript :string))
						(unless (or at_line_start after_lparen after_quote)
							(write-blk out " ")
							(++ current_col))
						(write-blk out val)
						(setq at_line_start :nil after_lparen :nil after_quote :nil consec_nl 0
							current_col (if (find "\n" val)
								(last-line-len val)
								(+ current_col (length val))))
						(inc-arg-count form_stack)
						(++ idx))
					((eql tok_type :lparen)
						(unless (or at_line_start after_lparen after_quote)
							(write-blk out " ")
							(++ current_col))
						(write-blk out "(")
						(defq p_role (parent-role form_stack) is_clause (role-clauses? p_role) tmpl (lookup-form-template tokens idx p_role after_quote) is_head (eql (role-type p_role) :head)
							parent_has_body (and (nempty? form_stack)
								(find :body (first (last form_stack)))) f_base (cond
									(at_line_start cur_line_indent)
									; clauses and headers preceding a body indent deeper to prevent collisions
									((or is_clause (and is_head parent_has_body))
										(current-target-indent form_stack))
									(:t cur_line_indent)) child_ind (+ f_base 1))
						; stack frame: (tmpl f_base arg_count child_ind is_clause pairs_on_line)
						(push form_stack (list tmpl f_base 0 child_ind is_clause 0))
						(setq at_line_start :nil after_lparen :t after_quote :nil consec_nl 0
							current_col (+ current_col 1))
						(++ idx))
					((eql tok_type :quote)
						(unless (or at_line_start after_lparen after_quote)
							(write-blk out " ")
							(++ current_col))
						(write-blk out val)
						(setq at_line_start :nil after_lparen :nil after_quote :t consec_nl 0
							current_col (+ current_col (length val)))
						(++ idx))
					(:t
						(unless (or at_line_start after_lparen after_quote)
							(write-blk out " ")
							(++ current_col))
						(write-blk out val)
						(setq at_line_start :nil after_lparen :nil after_quote :nil consec_nl 0
							current_col (+ current_col (length val)))
						(inc-arg-count form_stack)
						(++ idx))))))
	(when pending_nl (write-blk out "\n"))
	(stream-flush out)
	(stream-seek out 0 0)
	out)

;;;;;;;;;;;;;;;;;;;;;;;
; file worker
;;;;;;;;;;;;;;;;;;;;;;;

(defun work (file opt_w opt_c)
	; read stream under read lock
	(defq in :nil formatted :nil)
	(lock-claim-rpc file +lock_mode_read)
	(catch (when (setq in (file-stream file))
		(setq formatted (format-lisp in))) :nil)
	(lock-release-rpc file)
	(when formatted
		; check differences using stream-diff directly on streams
		(defq diff_out (string-stream (cat "")))
		(stream-diff (file-stream file) formatted diff_out)
		(defq differs (nempty? (str diff_out)))
		(stream-seek formatted 0 0)
		(cond
			(opt_w
				; only acquire write lock if file actually changed
				(when differs
					(lock-claim-rpc file +lock_mode_write)
					(catch (when (defq out (file-stream file +file_open_write))
						(while (defq blk (read-blk formatted 4096))
							(write-blk out blk))
						(stream-flush out)) :nil)
					(lock-release-rpc file)
					(print "Formatted: " file)))
			(opt_c (when differs (print "Needs formatting: " file)))
			(:t
				(defq stdout (io-stream 'stdout))
				(while (defq blk (read-blk formatted 4096))
					(write-blk stdout blk))
				(stream-flush stdout)))))

(defun main ()
	(when (and (defq stdio (create-stdio))
			(defq opt_j 8 opt_w :nil opt_c :nil args (options stdio usage)))
		(defq files (rest args))
		(if (empty? files)
			(lines! (# (push files %0) :nil) (io-stream 'stdin)))
		(setq files (usort (filter (lambda (file)
			(some (# (ends-with %0 file)) +file_types)) files)))
		(if (<= (length files) opt_j)
			(each (# (work %0 opt_w opt_c)) files)
			(each (lambda ((job result))
				(prin result)) (pipe-farm (map (# (str (first args) " -j " opt_j (if opt_w " -w" "") (if opt_c " -c" "") " " (slice (str %0) 1 -2))) (partition files opt_j)))))))
