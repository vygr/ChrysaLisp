(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/files/files.inc")
(import "lib/text/syntax.inc")
(import "service/lock/app.inc")

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
        (:pairs [max_pairs_per_line] [max_col])
            packs key-value pairs up to max_pairs_per_line per line (default 4)
            and wraps when line length reaches max_col (default 70).
            never breaks between a key and its value.
        (:clauses [max_per_line] [max_body_actions] [max_len])
            controls clause structure and layout (default: 1 1 70).
            max_per_line: maximum number of clauses allowed on a single line.
            max_body_actions: maximum body expressions allowed for single-line flow.
            max_len: character length threshold for inline clause flow.
            clauses with more body expressions break each action onto its own line.
        (:data [max_items_per_line])
            formats quoted data lists, packing max_items_per_line per line (default 5).
        (:choice [max_len] short_tmpl multiline_tmpl)
            dynamically uses short_tmpl if the form fits within max_len (default 72),
            otherwise falls back to multiline_tmpl.")
(("-j" "--jobs") ,(opt-num 'opt_j))
(("-w" "--write") ,(opt-flag 'opt_w))
(("-c" "--check") ,(opt-flag 'opt_c))
))

(defq +file_types ''(".lisp" ".inc" ".vp") +tab_width 4
	+default_short_len 72)

;;;;;;;;;;;;;;;;;;;;;;;
; form template rules
;;;;;;;;;;;;;;;;;;;;;;;

(defq +templates (scatter (Fmap 256)
	; definitions & bindings: (:pairs max_pairs_per_line max_col)
	"defq" '((:pairs 4 70)) "setq" '((:pairs 4 70))
	"setd" '((:pairs 4 70)) "def" '(:head (:pairs 3 70))
	"set" '(:head (:pairs 3 70)) "scatter" '(:head (:pairs 3 70))
	"pmap" '((:pairs 4 70)) "defun" '(:head :head :body)
	"redefun" '(:head :head :body) "defmacro" '(:head :head :body)
	"redefmacro" '(:head :head :body) "defmethod" '(:head :head :body)
	"defabstractmethod" '(:head :head :body)
	"defclass" '(:head :head :head :body)
	"def-class" '(:head :head :body) "def-method" '(:head :head :body)
	"def-func" '(:head :body)
	"lambda" '(:choice 75 (:flow) (:head :body))
	"macro" '(:choice 75 (:flow) (:head :body)) "let" '(:head :body)
	"let*" '(:head :body) "structure" '(:head :head :body)
	"enums" '(:head :head :body) "bits" '(:head :head :body)
	"def-vars" '(:body)

	; single-line declarations & evaluators
	"deffimethod" '(:flow) "defgetmethod" '(:flow)
	"defsetmethod" '(:flow) "defproxymethod" '(:flow)
	"dec-method" '(:flow) "#" '(:flow) "ffi" '(:flow)
	"bind" '(:flow) "const" '(:flow) "exec" '(:flow)
	"macrobind" '(:flow) "static-q" '(:flow) "static-qq" '(:flow)
	"static-qqp" '(:flow) "callback" '(:flow) "export" '(:flow)
	"export-symbols" '(:flow) "export-classes" '(:flow)
	"debug-brk" '(:flow) "profile-report" '(:flow) "inc" '(:flow)
	"dec" '(:flow) "++" '(:flow) "--" '(:flow)
	"not" '(:flow)

	; collections & data helpers (lib/collections, lib/class/struct)
	"gather" '(:flow) "transfer" '(:flow) "tsort" '(:flow)
	"memoize" '(:choice 75 (:flow) (:head :body)) "getf" '(:flow)
	"setf" '(:flow) "getf->" '(:flow)
	"setf->" '(:choice 75 (:flow) (:head :body)) "bits?" '(:flow)
	"bit-mask" '(:flow) "load-fields" '(:flow) "save-fields" '(:flow)
	"assign-fields" '(:choice 75 (:flow) (:head :body))

	; conditionals & branching: (:clauses max_per_line max_body_actions max_len)
	"cond" '((:clauses 1 1 70)) "condn" '((:clauses 1 1 70))
	"case" '(:head (:clauses 1 1 70))
	"pcase" '(:head :head (:clauses 1 1 70))
	"switch" '(:block_open :head (:clauses 1 1 70))
	"if" '(:choice 75 (:flow) (:head :body))
	"ifn" '(:choice 75 (:flow) (:head :body))
	"when" '(:choice 75 (:flow) (:head :body))
	"unless" '(:choice 75 (:flow) (:head :body))

	; loops & iteration
	"while" '(:head :body) "until" '(:head :body)
	"for" '(:head :head :body) "times" '(:head :body)

	; higher-order sequence & collection functions
	"each" '(:choice 75 (:flow) (:head :body :flow))
	"each!" '(:choice 75 (:flow) (:head :body :flow))
	"each-mergeable" '(:choice 75 (:flow) (:head :body :flow))
	"reach" '(:choice 75 (:flow) (:head :body :flow))
	"map" '(:choice 75 (:flow) (:head :body :flow))
	"map!" '(:choice 75 (:flow) (:head :body :flow))
	"rmap" '(:choice 75 (:flow) (:head :body :flow))
	"filter" '(:choice 75 (:flow) (:head :body :flow))
	"filter!" '(:choice 75 (:flow) (:head :body :flow))
	"reduce" '(:choice 75 (:flow) (:head :body :flow))
	"reduce!" '(:choice 75 (:flow) (:head :body :flow))
	"rreduce" '(:choice 75 (:flow) (:head :body :flow))
	"some" '(:choice 75 (:flow) (:head :body :flow))
	"some!" '(:choice 75 (:flow) (:head :body :flow))
	"rsome" '(:choice 75 (:flow) (:head :body :flow))
	"every" '(:choice 75 (:flow) (:head :body :flow))
	"notany" '(:choice 75 (:flow) (:head :body :flow))
	"notevery" '(:choice 75 (:flow) (:head :body :flow))
	"lines!" '(:choice 75 (:flow) (:head :body :flow)) "sort" '(:flow)
	"usort" '(:flow) "shuffle" '(:flow)

	; logic, blocks, exception handling
	"and" '(:choice 75 (:flow) (:head :body))
	"or" '(:choice 75 (:flow) (:head :body)) "throw" '(:flow)
	"catch" '(:flow) "progn" '(:body) "errorcase" '(:body)
	"validatecase" '(:body) "noterrorcase" '(:body)
	"time-it" '(:head :body) "undoable" '(:head :body)
	"within-compile-env" '(:head :body)

	; fluent method chaining & view properties
	".->" '(:choice 75 (:flow) (:head :body)) ".?" '(:flow)
	".super" '(:flow) "raise" '(:flow) "lower" '(:flow)
	"ui-props" '(:head (:pairs 3))

	; ui builder containers and elements
	"ui-root" '(:head :head :head :body)
	"ui-window" '(:choice 75 (:flow) (:head :head :body))
	"ui-flow" '(:choice 75 (:flow) (:head :head :body))
	"ui-grid" '(:choice 75 (:flow) (:head :head :body))
	"ui-stack" '(:head :head :head :body)
	"ui-backdrop" '(:choice 75 (:flow) (:head :head :body))
	"ui-view" '(:choice 75 (:flow) (:head :head :body))
	"ui-md" '(:choice 75 (:flow) (:head :head :body))
	"ui-button" '(:choice 75 (:flow) (:head :head :body))
	"ui-label" '(:choice 75 (:flow) (:head :head :body))

	; vp structured coding (lib/asm/code.inc)
	"vpif" '(:block_open :choice 75 (:flow) (:head :body))
	"vpifnot" '(:block_open :choice 75 (:flow) (:head :body))
	"elseif" '(:block_mid :choice 75 (:flow) (:head :body))
	"elseifnot" '(:block_mid :choice 75 (:flow) (:head :body))
	"else" '(:block_mid :flow) "endif" '(:block_close :flow)
	"loop-start" '(:block_open :head :body)
	"loop-while" '(:block_open :head :body)
	"loop-whilenot" '(:block_open :head :body)
	"loop-until" '(:block_close :flow)
	"loop-untilnot" '(:block_close :flow)
	"loop-end" '(:block_close :flow)
	"switch" '(:block_open :head (:clauses 1 1 70))
	"vpcase" '(:block_mid :choice 75 (:flow) (:head :body))
	"vpcasenot" '(:block_mid :choice 75 (:flow) (:head :body))
	"default" '(:block_mid :flow) "endswitch" '(:block_close :flow)
	"goto" '(:flow) "gotoif" '(:flow) "gotoifnot" '(:flow)
	"break" '(:flow) "breakif" '(:flow) "breakifnot" '(:flow)
	"continue" '(:flow) "continueif" '(:flow) "continueifnot" '(:flow)
	"nextcaseif" '(:flow) "nextcaseifnot" '(:flow) "repeatif" '(:flow)
	"repeatifnot" '(:flow) "exitif" '(:flow) "exitifnot" '(:flow)
	"errorif" '(:flow) "errorifnot" '(:flow)
	"errorif-lisp-args-sig" '(:flow) "errorif-lisp-args-len" '(:flow)
	"errorif-lisp-args-match" '(:flow)
	"errorif-lisp-args-type" '(:flow)
	"assert" '(:choice 75 (:flow) (:head :body))

	; vp register, invocation & memory primitives (lib/asm)
	"vp-rdef" '(:flow) "vp-fdef" '(:flow) "vp-simd" '(:head :flow)
	"assign" '(:flow) "entry" '(:flow) "exit" '(:flow)
	"signature" '(:flow) "f-call" '(:flow) "s-call" '(:flow)
	"v-call" '(:flow) "d-call" '(:flow) "r-call" '(:flow)
	"f-jump" '(:flow) "s-jump" '(:flow) "v-jump" '(:flow)
	"d-jump" '(:flow) "r-jump" '(:flow) "f-bind" '(:flow)
	"s-bind" '(:flow) "v-bind" '(:flow) "d-bind" '(:flow)
	"fn-call" '(:flow) "fn-jump" '(:flow) "fn-bind" '(:flow)
	"fn-string" '(:flow) "return" '(:flow)

	; vp linked list traversal loops
	"loop-flist" '(:head :head :head :head :body)
	"loop-list-forward" '(:head :head :head :head :body)
	"loop-list-backward" '(:head :head :head :head :body)))

(defq +major_definitions ''("defun" "redefun" "defmacro" "redefmacro" "defclass" "def-class"
	"def-method" "def-func" "structure" "enums" "bits"))

(defun role-type (role)
	(if (list? role) (first role) role))

(defun role-clauses? (role)
	(eql (role-type role) :clauses))

(defun clause-max-per-line (role)
	(if (and (list? role) (> (length role) 1)) (second role) 1))

(defun clause-max-body-elems (role)
	(if (and (list? role) (> (length role) 2)) (third role) 1))

(defun clause-max-len (role)
	(if (and (list? role) (> (length role) 3))
		(elem-get role 3)
		+default_short_len))

(defun clean-template (tmpl)
	; strip block delimiter tags for argument role resolution
	(if (list? tmpl)
		(filter (# (not (find %0 '(:block_open :block_mid :block_close)))) tmpl)
		tmpl))

(defun template-role (tmpl arg_count)
	(cond
		((empty? tmpl) :body)
		((<= arg_count 1) (first tmpl))
		((defq idx (dec arg_count))
			(if (< idx (length tmpl)) (elem-get tmpl idx) (last tmpl)))))

(defun parent-role (form_stack)
	(if (empty? form_stack)
		:body
		(bind '(p_tmpl & p_argc &ignore) (last form_stack))
		(template-role p_tmpl p_argc)))

(defun skip-ws-nl (tokens idx)
	(defq len (length tokens) i idx)
	(while (and (< i len) (find (first (elem-get tokens i)) '(:ws :nl)))
		(++ i))
	i)

(defun skip-ws (tokens idx)
	; skip whitespace tokens only
	(defq len (length tokens) i idx)
	(while (and (< i len) (eql (first (elem-get tokens i)) :ws))
		(++ i))
	i)

(defun next-significant-idx (tokens idx)
	(defq len (length tokens) i idx res :nil)
	(while (and (< i len) (not res))
		(if (find (first (elem-get tokens i)) '(:ws :nl)) (++ i) (setq res i)))
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
			((or (eql tok_type :ws) (eql tok_type :nl) (eql tok_type :comment))
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

(defun form-fits-inline? (tokens lparen_idx max_len)
	; returns :t if form fits within max_len with no comments, raw blocks, or multiline strings
	(defq len (length tokens) i (inc lparen_idx) depth 1 est_len 2
		fits :t)
	(while (and (< i len) (> depth 0) fits)
		(defq tok (elem-get tokens i) tok_type (first tok))
		(cond
			((or (eql tok_type :ws) (eql tok_type :nl)))
			((or (eql tok_type :comment) (eql tok_type :raw)) (setq fits :nil))
			((eql tok_type :lparen)
				(++ depth)
				(++ est_len))
			((eql tok_type :rparen)
				(-- depth)
				(++ est_len))
			(:t
				(defq val (second tok))
				(if (and (find tok_type '(:string :cscript)) (find "\n" val))
					(setq fits :nil)
					(setq est_len (+ est_len (length val) 1)))
				(if (> est_len max_len) (setq fits :nil))))
		(++ i))
	(and fits (= depth 0)))

(defun upcoming-pair-fits? (tokens key_idx rem_len)
	; check if upcoming key-value pair fits within rem_len
	(cond
		((<= rem_len 0) :nil)
		((defq key_tok (elem-get tokens key_idx)
			key_len (length (second key_tok)))
			(if (>= key_len rem_len)
				:nil
				(progn
					(defq val_i (skip-ws-nl tokens (inc key_idx)))
					(if (>= val_i (length tokens))
						:t
						(defq val_tok (elem-get tokens val_i)
							val_type (first val_tok)
							avail (- rem_len (+ key_len 1)))
						(cond
							((<= avail 0) :nil)
							((eql val_type :lparen) (form-fits-inline? tokens val_i avail))
							((eql val_type :quote)
								(defq q_elem_i (skip-ws-nl tokens (inc val_i)))
								(if (>= q_elem_i (length tokens))
									:t
									(defq q_tok (elem-get tokens q_elem_i))
									(if (eql (first q_tok) :lparen)
										(form-fits-inline? tokens q_elem_i (dec avail))
										(<= (length (second q_tok)) (dec avail)))))
							(:t (<= (length (second val_tok)) avail)))))))))

(defun clause-actions-short? (tokens lparen_idx max_len)
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
					((eql (first (elem-get tokens act_i)) :rparen) :t)
					((eql (first (elem-get tokens act_i)) :lparen)
						(form-fits-inline? tokens act_i max_len))
					(:t :t))))))

(defun resolve-template (tmpl tokens lparen_idx)
	; (:choice [max_len] short_tmpl multiline_tmpl)
	(if (and (list? tmpl) (eql (first tmpl) :choice))
		(progn
			(defq has_custom_len (num? (second tmpl))
				max_len (if has_custom_len (second tmpl) +default_short_len)
				short_tmpl (if has_custom_len (third tmpl) (second tmpl))
				multi_tmpl (if has_custom_len (elem-get tmpl 3) (third tmpl)))
			(if (form-fits-inline? tokens lparen_idx max_len) short_tmpl multi_tmpl))
		tmpl))

(defun resolve-clause-template (tokens lparen_idx clause_role)
	(defq max_body (clause-max-body-elems clause_role)
		max_len (clause-max-len clause_role)
		elems (clause-elem-count tokens lparen_idx))
	(cond
		; single-line clause only if actions <= max_body, and entire clause fits within max_len
		((and (<= elems (inc max_body))
			(form-fits-inline? tokens lparen_idx max_len)
			(clause-actions-short? tokens lparen_idx max_len))
			'(:flow))
		; multiple body expressions or multiline actions break onto new lines
		(:t '(:body))))

(defun lookup-form-template (tokens lparen_idx parent_raw_role after_quote)
	(defq next_i (skip-ws-nl tokens (inc lparen_idx))
		tok (if (< next_i (length tokens)) (elem-get tokens next_i))
		tok_atom (if (and tok (eql (first tok) :atom)) (second tok))
		known_tmpl (if (and tok_atom (not after_quote)) (. +templates :find tok_atom)))
	(cond
		; 1. quoted data list wraps items every 5 elements
		(after_quote '((:data 5)))
		; 2. registered operator always keeps its own template
		(known_tmpl
			(resolve-template (clean-template known_tmpl) tokens lparen_idx))
		; 3. unregistered form in a clause context is an anonymous clause
		((role-clauses? parent_raw_role)
			(resolve-clause-template tokens lparen_idx parent_raw_role))
		; 4. default to flow
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
			(defq next_tok (elem-get tokens next_i)
				t_type (first next_tok))
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

(defun check-template-break (form_stack at_line_start pending_nl after_quote current_col tokens idx)
	; check if current template requires a newline before this argument
	(when (and (nempty? form_stack) (not after_quote))
		(bind '(tmpl & arg_count &ignore) (last form_stack))
		(when (> arg_count 0)
			(defq raw_role (template-role tmpl arg_count)
				role (role-type raw_role))
			(cond
				((eql role :body) (unless (or at_line_start pending_nl) :t))
				((eql role :clauses)
					(defq max_per_line (clause-max-per-line raw_role))
					(and (>= arg_count 1)
						(not at_line_start)
						(not pending_nl)
						(= max_per_line 1)))
				((eql role :pairs)
					(defq is_def (eql (first tmpl) :head)
						pair_arg (if is_def (dec arg_count) arg_count)
						is_key (odd? pair_arg)
						line_pairs (elem-get (last form_stack) 5)
						val_multiline (elem-get (last form_stack) 8)
						max_pairs (if (and (list? raw_role) (> (length raw_role) 1)) (second raw_role) 4)
						max_col (if (and (list? raw_role) (> (length raw_role) 2)) (third raw_role) 70))
					(and is_key
						(> pair_arg 1)
						(not at_line_start)
						(not pending_nl)
						(or (>= line_pairs max_pairs)
							(and (> line_pairs 0) (>= current_col max_col))
							val_multiline
							(not (upcoming-pair-fits? tokens idx (- max_col current_col))))))
				((eql role :data)
					(defq max_items (if (and (list? raw_role) (> (length raw_role) 1)) (second raw_role) 5))
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
		(defq raw_line (trim-end (elem-get lines line_idx) "\r")
			prev_state (. syntax :get_state))
		(cond
			((and (eql prev_state :text)
				(starts-with "(defq usage" (trim-start raw_line)))
				(defq u_lines (list) depth 0 in_str :nil done :nil)
				(while (and (< line_idx num_lines) (not done))
					(defq u_line (elem-get lines line_idx)
						u_len (length u_line) ui 0)
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
								((find tok_state '(:number :keysym)) (push tokens (list :atom val)))
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
												(while (and (< ti tlen) (or (eql (defq c (elem-get val ti)) " ") (eql c "\t")))
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
												(while (and (< ti tlen) (not (find (elem-get val ti) " \t()'`~,")))
													(++ ti))
												(push tokens (list :atom (slice val atom_start ti)))))))))
						toks states))
				; preserve line endings and trailing newline if present in source
				(when (or (< (inc line_idx) num_lines) ends_nl)
					(unless (find (. syntax :get_state) '(:string1 :string2))
						(push tokens (list :nl "\n"))))
				(++ line_idx))))
	tokens)

(defun inc-arg-count (form_stack)
	(when (nempty? form_stack)
		(defq frame (last form_stack) tmpl (first frame)
			arg_c (inc (third frame)))
		(elem-set frame 2 arg_c)
		; increment pair count when a value in :pairs completes
		(when (eql (role-type (template-role tmpl arg_c)) :pairs)
			(defq is_def (eql (first tmpl) :head)
				pair_arg (if is_def (dec arg_c) arg_c))
			(when (even? pair_arg)
				(elem-set frame 5 (inc (elem-get frame 5)))
				(elem-set frame 8 :nil)))))

(defun make-indent (level)
	(pad "" level "\t"))

(defun last-line-len (s)
	(if (defq pos (rfind "\n" s)) (- (length s) (inc pos)) (length s)))

(defun current-target-indent (form_stack)
	(if (nempty? form_stack) (elem-get (last form_stack) 3) 0))

;;;;;;;;;;;;;;;;;;;;;;;
; code formatter
;;;;;;;;;;;;;;;;;;;;;;;

(defun format-lisp (stream_or_src)
	(defq stream (if (str? stream_or_src) (string-stream stream_or_src) stream_or_src)
		tokens (tokenize-lisp stream) len (length tokens)
		out (string-stream (str-alloc len)) cur_line_indent 0
		current_col 0 at_line_start :t after_lparen :nil
		after_quote :nil pending_nl :nil consec_nl 0
		just_saw_comment :nil prev_was_comment :nil top_block_indent 0
		form_stack (list) idx 0 tok :nil tok_type :nil
		val "")

	(while (< idx len)
		(setq tok (elem-get tokens idx) tok_type (first tok)
			val (second tok))
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
				(when pending_nl (times (if (>= consec_nl 2) 2 1)
									(write-blk out "\n")))
				(write-blk out val)
				(setq at_line_start :t pending_nl :nil
					just_saw_comment :nil prev_was_comment :nil
					consec_nl 0 current_col 0 after_lparen :nil
					after_quote :nil)
				(++ idx))
			((eql tok_type :comment)
				(if (or at_line_start pending_nl)
					(progn
						(when pending_nl
							(times (if (empty? form_stack)
									(if (wants-section-break? tokens idx consec_nl prev_was_comment) 2 1)
									(if (>= consec_nl 2) 2 1))
								(write-blk out "\n")))
						(defq cur_blk_ind (if (nempty? form_stack) (elem-get (last form_stack) 7) top_block_indent)
							ind (+ (current-target-indent form_stack) cur_blk_ind))
						(write-blk out (make-indent ind))
						(setq cur_line_indent ind
							current_col (* ind +tab_width)))
					(write-blk out " "))
				(write-blk out val)
				; comments always terminate the line; subsequent code must be on a new line
				(setq at_line_start :nil pending_nl :t consec_nl 1
					just_saw_comment :t prev_was_comment :t
					current_col 0 after_lparen :nil after_quote :nil)
				(++ idx))
			((eql tok_type :rparen)
				(defq closed_frame (if (nempty? form_stack) (pop form_stack) :nil))
				(when (and pending_nl (eql (first (elem-get tokens (dec idx))) :comment))
					(times (if (>= consec_nl 2) 2 1)
						(write-blk out "\n"))
					(defq r_ind (if closed_frame (second closed_frame) (current-target-indent form_stack)))
					(write-blk out (make-indent r_ind))
					(setq at_line_start :t cur_line_indent r_ind
						current_col (* r_ind +tab_width)))
				(setq pending_nl :nil consec_nl 0 just_saw_comment :nil
					prev_was_comment :nil)
				(write-blk out ")")
				(setq at_line_start :nil after_lparen :nil
					after_quote :nil current_col (+ current_col 1))
				(when closed_frame
					(defq btag (elem-get closed_frame 6))
					(cond
						((eql btag :open)
							(if (nempty? form_stack)
								(elem-set (last form_stack) 7 (inc (elem-get (last form_stack) 7)))
								(++ top_block_indent)))
						((eql btag :close)
							(if (nempty? form_stack)
								(elem-set (last form_stack) 7 (max 0 (dec (elem-get (last form_stack) 7))))
								(setq top_block_indent (max 0 (dec top_block_indent)))))))
				(if (empty? form_stack)
					; top-level form closed; next form starts on a new line
					(setq pending_nl :t consec_nl 0 top_block_indent 0)
					(inc-arg-count form_stack))
				(++ idx))
			(:t
				; check if current template requires a newline before this argument
				(when (check-template-break form_stack at_line_start pending_nl after_quote current_col tokens idx)
					(setq pending_nl :t
						consec_nl (if (>= consec_nl 2) 2 1)))

				; flush deferred newlines and apply stack-directed indentation
				(when pending_nl
					(defq next_op_i (if (eql tok_type :lparen) (next-significant-idx tokens (inc idx)))
						next_op (if next_op_i (elem-get tokens next_op_i))
						next_op_str (if (and next_op (eql (first next_op) :atom)) (second next_op))
						next_raw_tmpl (if next_op_str (. +templates :find next_op_str))
						is_unindent (and next_raw_tmpl
										(or (find :block_close next_raw_tmpl) (find :block_mid next_raw_tmpl)))
						cur_blk_ind (if (nempty? form_stack) (elem-get (last form_stack) 7) top_block_indent)
						effective_block_indent (if is_unindent (max 0 (dec cur_blk_ind)) cur_blk_ind)
						ind (+ (current-target-indent form_stack) effective_block_indent)
						nl_count (if (empty? form_stack)
									(if (wants-section-break? tokens idx consec_nl prev_was_comment) 2 1)
									(if (>= consec_nl 2) 2 1)))
					(times nl_count
						(write-blk out "\n"))
					(write-blk out (make-indent ind))
					(setq at_line_start :t pending_nl :nil
						just_saw_comment :nil prev_was_comment :nil
						cur_line_indent ind consec_nl 0
						current_col (* ind +tab_width))
					(each (lambda (f) (elem-set f 5 0) (elem-set f 8 :t)) form_stack))

				(cond
					((find tok_type '(:cscript :string))
						(unless (or at_line_start after_lparen after_quote)
							(write-blk out " ")
							(++ current_col))
						(write-blk out val)
						(setq at_line_start :nil after_lparen :nil
							after_quote :nil consec_nl 0
							current_col (if (find "\n" val) (last-line-len val) (+ current_col (length val))))
						(inc-arg-count form_stack)
						(++ idx))
					((eql tok_type :lparen)
						(unless (or at_line_start after_lparen after_quote)
							(write-blk out " ")
							(++ current_col))
						(defq start_col current_col)
						(write-blk out "(")
						(defq p_role (parent-role form_stack)
							is_clause (role-clauses? p_role)
							tmpl (lookup-form-template tokens idx p_role after_quote)
							next_op_i (next-significant-idx tokens (inc idx))
							next_op (if next_op_i (elem-get tokens next_op_i))
							next_op_str (if (and next_op (eql (first next_op) :atom)) (second next_op))
							raw_tmpl (if next_op_str (. +templates :find next_op_str))
							block_tag (cond
								((and raw_tmpl (find :block_open raw_tmpl)) :open)
								((and raw_tmpl (find :block_close raw_tmpl)) :close)
								((and raw_tmpl (find :block_mid raw_tmpl)) :mid))
							is_head (eql (role-type p_role) :head)
							parent_has_body (and (nempty? form_stack)
												(find :body (clean-template (first (last form_stack)))))
							start_tab (/ start_col +tab_width)
							child_ind (cond
								(at_line_start (inc cur_line_indent))
								((and is_head parent_has_body)
									(max (inc (current-target-indent form_stack)) (inc start_tab)))
								((find (role-type (first tmpl)) '(:pairs :data))
									(inc cur_line_indent))
								((find :body (clean-template tmpl))
									(max (inc cur_line_indent) (inc start_tab)))
								(:t (inc cur_line_indent)))
							f_base (dec child_ind))
						; stack frame: (tmpl f_base arg_count child_ind is_clause pairs_on_line block_tag block_indent val_multiline)
						(push form_stack (list tmpl f_base 0 child_ind is_clause 0 block_tag 0 :nil))
						(setq at_line_start :nil after_lparen :t
							after_quote :nil consec_nl 0
							current_col (+ current_col 1))
						(++ idx))
					((eql tok_type :quote)
						(unless (or at_line_start after_lparen after_quote)
							(write-blk out " ")
							(++ current_col))
						(write-blk out val)
						(setq at_line_start :nil after_lparen :nil
							after_quote :t consec_nl 0
							current_col (+ current_col (length val)))
						(++ idx))
					(:t
						(unless (or at_line_start after_lparen after_quote)
							(write-blk out " ")
							(++ current_col))
						(write-blk out val)
						(setq at_line_start :nil after_lparen :nil
							after_quote :nil consec_nl 0
							current_col (+ current_col (length val)))
						(inc-arg-count form_stack)
						(++ idx))))))
	(when pending_nl (write-blk out "\n"))
	(str out))

;;;;;;;;;;;;;;;;;;;;;;;
; file worker
;;;;;;;;;;;;;;;;;;;;;;;

(defun work (file opt_w opt_c)
	; read stream under read lock
	(defq in :nil formatted :nil)
	(with-read-lock file
		(catch (when (setq in (file-stream file)) (setq formatted (format-lisp in))) :nil))
	(when formatted
		(defq orig (load file) differs (nql orig formatted))
		(cond
			(opt_w
				; only acquire write lock if file actually changed
				(when differs
					(with-write-lock file
						(catch (when (defq out (file-stream file +file_open_write))
								(write-blk out formatted)
								(stream-flush out)
								(setq out :nil)) :nil))
					(print "Formatted: " file)))
			(opt_c (when differs (print "Needs formatting: " file)))
			(:t (prin formatted)))))

(defun main ()
	(when (and (defq stdio (create-stdio))
			(defq opt_j 8 opt_w :nil opt_c :nil
				args (options stdio usage)))
		(defq files (rest args))
		(if (empty? files) (lines! (# (push files %0) :nil) (io-stream 'stdin)))
		(setq files (usort (filter (lambda (file) (some (# (ends-with %0 file)) +file_types)) files)))
		(if (<= (length files) opt_j)
			(each (# (work %0 opt_w opt_c)) files)
			(each (lambda ((job result)) (prin result))
				(pipe-farm (map (# (str (first args) " -j " opt_j (if opt_w " -w" "") (if opt_c " -c" "") " " (slice (str %0) 1 -2)))
							(partition files opt_j)))))))
