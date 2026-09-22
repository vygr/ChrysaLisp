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
    are specified on the command line, paths are read from stdin.")
(("-j" "--jobs") ,(opt-num 'opt_j))
(("-w" "--write") ,(opt-flag 'opt_w))
(("-c" "--check") ,(opt-flag 'opt_c))
))

(defq +file_types ''(".lisp" ".inc" ".vp") +tab_width 4)

;;;;;;;;;;;;;;;;;;;;;;;
; form template rules
;;;;;;;;;;;;;;;;;;;;;;;

(defq +templates (scatter (Fmap 64)
	; definitions & bindings
	"defq" '(:pairs)
	"setq" '(:pairs)
	"def" '(:head :pairs)
	"set" '(:head :pairs)
	"defun" '(:head :head :body)
	"redefun" '(:head :head :body)
	"defmacro" '(:head :head :body)
	"redefmacro" '(:head :head :body)
	"defmethod" '(:head :head :body)
	"defabstractmethod" '(:head :head :body)
	"defclass" '(:head :head :head :body)
	"def-class" '(:head :head :body)
	"def-method" '(:head :head :body)
	"def-func" '(:head :body)
	"lambda" '(:head :body)
	"macro" '(:head :body)
	"let" '(:head :body)
	"let*" '(:head :body)
	"structure" '(:head :head :body)
	"enums" '(:head :head :body)
	"bits" '(:head :head :body)
	"def-vars" '(:body)

	; single-line declarations
	"deffimethod" '(:flow)
	"defgetmethod" '(:flow)
	"defsetmethod" '(:flow)
	"defproxymethod" '(:flow)
	"dec-method" '(:flow)
	"#" '(:flow)

	; conditionals & branching
	"cond" '(:clauses)
	"condn" '(:clauses)
	"case" '(:head :clauses)
	"pcase" '(:head :head :clauses)
	"switch" '(:head :clauses)
	"if" '(:choice (:flow) (:head :body))
	"ifn" '(:choice (:flow) (:head :body))
	"when" '(:choice (:flow) (:head :body))
	"unless" '(:choice (:flow) (:head :body))

	; loops & iteration
	"while" '(:head :body)
	"until" '(:head :body)
	"for" '(:head :head :body)
	"times" '(:head :body)

	; higher-order sequence functions
	"each" '(:flow)
	"each!" '(:flow)
	"reach" '(:flow)
	"map" '(:flow)
	"map!" '(:flow)
	"rmap" '(:flow)
	"filter" '(:flow)
	"filter!" '(:flow)
	"reduce" '(:flow)
	"reduce!" '(:flow)
	"rreduce" '(:flow)
	"some" '(:flow)
	"some!" '(:flow)
	"rsome" '(:flow)
	"every" '(:flow)
	"notany" '(:flow)
	"notevery" '(:flow)
	"lines!" '(:flow)

	; logic, blocks, exception handling
	"and" '(:choice (:flow) (:head :clauses))
	"or" '(:choice (:flow) (:head :clauses))
	"throw" '(:flow)
	"catch" '(:flow)
	"progn" '(:body)
	"errorcase" '(:body)
	"validatecase" '(:body)
	"noterrorcase" '(:body)
	"time-it" '(:head :body)
	"undoable" '(:head :body)
	"within-compile-env" '(:head :body)))

(defq +major_definitions ''(
	"defun" "redefun" "defmacro" "redefmacro" "defclass"
	"def-class" "def-method" "def-func" "structure" "enums" "bits"))

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

(defun form-short? (tokens lparen_idx)
	; check if a form is structurally short, shallow, and fits on a single line
	(defq len (length tokens) i (inc lparen_idx)
		depth 1 count 0 sub_lists 0 est_len 2 is_short :t)
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

(defun resolve-template (tmpl tokens lparen_idx)
	; resolve template choices based on form shortness
	(if (and (list? tmpl) (eql (first tmpl) :choice))
		(if (form-short? tokens lparen_idx)
			(second tmpl)
			(third tmpl))
		tmpl))

(defun lookup-form-template (tokens lparen_idx parent_role)
	(cond
		((eql parent_role :clauses)
			(resolve-template '(:choice (:head :flow) (:head :body)) tokens lparen_idx))
		(:t
			(defq next_i (skip-ws-nl tokens (inc lparen_idx)))
			(if (and (< next_i (length tokens))
					(eql (first (defq tok (elem-get tokens next_i))) :atom))
				(resolve-template
					(or (. +templates :find (second tok)) '(:flow))
					tokens lparen_idx)
				'(:flow)))))

(defun wants-section-break? (tokens idx consec_nl)
	; determine if a top-level form or comment warrants a blank line
	(defq next_i (next-significant-idx tokens idx))
	(cond
		((not next_i) :nil)
		((>= consec_nl 2) :t)
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

(defun check-template-break (form_stack at_line_start pending_nl after_quote current_col)
	; check if current template requires a newline before this argument
	(when (and (nempty? form_stack) (not after_quote))
		(bind '(tmpl & arg_count &ignore) (last form_stack))
		(when (> arg_count 0)
			(defq role (template-role tmpl arg_count))
			(cond
				((or (eql role :body) (eql role :clauses))
					(unless (or at_line_start pending_nl) :t))
				((eql role :pairs)
					(defq is_def (eql (first tmpl) :head)
						pair_arg (if is_def (dec arg_count) arg_count)
						is_key (odd? pair_arg))
					(and is_key (> pair_arg 1) (> current_col 75)
						(not at_line_start) (not pending_nl)))))))

;;;;;;;;;;;;;;;;;;;;;;;
; fast syntax tokenizer
;;;;;;;;;;;;;;;;;;;;;;;

(defun tokenize-lisp (stream)
	(defq ends_nl :t)
	(when (/= (stream-seek stream -1 2) -1)
		(setq ends_nl (= (read-char stream) +char_lf))
		(stream-seek stream 0 0))
	(defq tokens (list) lines (list)
		syntax (Syntax) line_idx 0)
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
							((eql uc ";")
								(setq ui u_len))
							((eql uc "\q")
								(setq in_str :t))
							((eql uc "(")
								(++ depth))
							((eql uc ")")
								(-- depth)
								(if (= depth 0)
									(setq done :t))))
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
							((eql tok_state :comment)
								(push tokens (list :comment val)))
							((find tok_state '(:number :keysym))
								(push tokens (list :atom val)))
							((eql tok_state :symbol)
								(defq s val)
								(while (and (nempty? s) (find (first s) "'`~,"))
									(push tokens (list :quote (slice s 0 1)))
									(setq s (slice s 1 -1)))
								(if (nempty? s)
									(push tokens (list :atom s))))
							((eql tok_state :text)
								(defq tlen (length val) ti 0)
								(while (< ti tlen)
									(defq ch (elem-get val ti))
									(cond
										((or (eql ch " ") (eql ch "\t"))
											(defq ws_start ti)
											(while (and (< ti tlen)
													(or (eql (defq c (elem-get val ti)) " ") (eql c "\t")))
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
		(defq frame (last form_stack))
		(elem-set frame 2 (inc (third frame)))))

(defun make-indent (level)
	(pad "" level "\t"))

(defun last-line-len (s)
	(if (defq pos (rfind "\n" s))
		(- (length s) (inc pos))
		(length s)))

(defun current-target-indent (form_stack)
	(if (nempty? form_stack)
		(elem-get (last form_stack) 3)
		0))

;;;;;;;;;;;;;;;;;;;;;;;
; code formatter
;;;;;;;;;;;;;;;;;;;;;;;

(defun format-lisp (stream_or_src)
	(defq stream (if (str? stream_or_src) (string-stream stream_or_src) stream_or_src)
		tokens (tokenize-lisp stream) len (length tokens)
		out (string-stream (str-alloc len))
		cur_line_indent 0 current_col 0
		at_line_start :t after_lparen :nil after_quote :nil
		pending_nl :nil consec_nl 0 just_saw_comment :nil
		form_stack (list) idx 0 tok :nil tok_type :nil val "")

	(while (< idx len)
		(setq tok (elem-get tokens idx) tok_type (first tok) val (second tok))
		(cond
			((eql tok_type :ws)
				(++ idx))
			((eql tok_type :nl)
				(if just_saw_comment
					; consume newline immediately closing comment without inflating consec_nl
					(setq just_saw_comment :nil)
					(if (< consec_nl 2)
						(++ consec_nl)))
				(cond
					((empty? form_stack)
						(setq pending_nl :t))
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
				(setq at_line_start :t pending_nl :nil just_saw_comment :nil
					consec_nl 0 current_col 0 after_lparen :nil after_quote :nil)
				(++ idx))
			((eql tok_type :comment)
				(if (or at_line_start pending_nl)
					(progn
						(when pending_nl
							(times (if (empty? form_stack)
								(if (wants-section-break? tokens idx consec_nl) 2 1)
								(if (>= consec_nl 2) 2 1))
								(write-blk out "\n")))
						(defq ind (current-target-indent form_stack))
						(write-blk out (make-indent ind))
						(setq cur_line_indent ind current_col (* ind +tab_width)))
					(write-blk out " "))
				(write-blk out val)
				; comments always terminate the line; subsequent code must be on a new line
				(setq at_line_start :nil pending_nl :t consec_nl 1 just_saw_comment :t
					current_col 0 after_lparen :nil after_quote :nil)
				(++ idx))
			((eql tok_type :rparen)
				(defq closed_frame (if (nempty? form_stack)
					(pop form_stack)
					:nil))
				(when (and pending_nl (eql (first (elem-get tokens (dec idx))) :comment))
					(times (if (>= consec_nl 2) 2 1)
						(write-blk out "\n"))
					(defq r_ind (if closed_frame
						(second closed_frame)
						(current-target-indent form_stack)))
					(write-blk out (make-indent r_ind))
					(setq at_line_start :t cur_line_indent r_ind
						current_col (* r_ind +tab_width)))
				(setq pending_nl :nil consec_nl 0 just_saw_comment :nil)
				(write-blk out ")")
				(setq at_line_start :nil after_lparen :nil after_quote :nil
					current_col (+ current_col 1))
				(if (empty? form_stack)
					; top-level form closed; next form starts on a new line (0 consec_nl until next :nl)
					(setq pending_nl :t consec_nl 0)
					(inc-arg-count form_stack))
				(++ idx))
			(:t
				; check if current template requires a newline before this argument
				(when (check-template-break form_stack at_line_start pending_nl after_quote current_col)
					(setq pending_nl :t consec_nl (if (>= consec_nl 2) 2 1)))

				; flush deferred newlines and apply stack-directed indentation
				(when pending_nl
					(defq nl_count (if (empty? form_stack)
						(if (wants-section-break? tokens idx consec_nl) 2 1)
						(if (>= consec_nl 2) 2 1)))
					(times nl_count
						(write-blk out "\n"))
					(defq ind (current-target-indent form_stack))
					(write-blk out (make-indent ind))
					(setq at_line_start :t pending_nl :nil just_saw_comment :nil
						cur_line_indent ind
						consec_nl 0 current_col (* ind +tab_width)))

				(cond
					((find tok_type '(:cscript :string))
						(unless (or at_line_start after_lparen after_quote)
							(write-blk out " ")
							(++ current_col))
						(write-blk out val)
						(setq at_line_start :nil after_lparen :nil after_quote :nil
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
						(defq p_role (parent-role form_stack)
							is_clause (eql p_role :clauses)
							tmpl (lookup-form-template tokens idx p_role)
							f_base (cond
								(at_line_start cur_line_indent)
								((or is_clause (and (eql p_role :head) (find :clauses tmpl)))
									(current-target-indent form_stack))
								(:t cur_line_indent))
							child_ind (+ f_base 1))
						(push form_stack (list tmpl f_base 0 child_ind is_clause))
						(setq at_line_start :nil after_lparen :t after_quote :nil
							current_col (+ current_col 1))
						(++ idx))
					((eql tok_type :quote)
						(unless (or at_line_start after_lparen after_quote)
							(write-blk out " ")
							(++ current_col))
						(write-blk out val)
						(setq at_line_start :nil after_lparen :nil after_quote :t
							current_col (+ current_col (length val)))
						(++ idx))
					(:t
						(unless (or at_line_start after_lparen after_quote)
							(write-blk out " ")
							(++ current_col))
						(write-blk out val)
						(setq at_line_start :nil after_lparen :nil after_quote :nil
							current_col (+ current_col (length val)))
						(inc-arg-count form_stack)
						(++ idx))))))
	(when pending_nl
		(write-blk out "\n"))
	(str out))

;;;;;;;;;;;;;;;;;;;;;;;
; file worker
;;;;;;;;;;;;;;;;;;;;;;;

(defun work (file opt_w opt_c)
	; read stream under read lock
	(defq in :nil formatted :nil)
	(lock-claim-rpc file +lock_mode_read)
	(catch
		(when (setq in (file-stream file))
			(setq formatted (format-lisp in)))
		:nil)
	(lock-release-rpc file)
	(when formatted
		; check differences using stream-diff
		(defq diff_out (string-stream (cat "")))
		(stream-diff (file-stream file) (string-stream formatted) diff_out)
		(defq differs (nempty? (str diff_out)))
		(cond
			(opt_w
				; only acquire write lock if file actually changed
				(when differs
					(lock-claim-rpc file +lock_mode_write)
					(catch
						(when (defq out (file-stream file +file_open_write))
							(write-blk out formatted)
							(stream-flush out))
						:nil)
					(lock-release-rpc file)
					(print "Formatted: " file)))
			(opt_c
				(when differs
					(print "Needs formatting: " file)))
			(:t
				(prin formatted)))))

(defun main ()
	(when (and
			(defq stdio (create-stdio))
			(defq opt_j 8 opt_w :nil opt_c :nil args (options stdio usage)))
		(defq files (rest args))
		(if (empty? files)
			(lines! (# (push files %0) :nil) (io-stream 'stdin)))
		(setq files (usort (filter (lambda (file)
			(some (# (ends-with %0 file)) +file_types)) files)))
		(if (<= (length files) opt_j)
			(each (# (work %0 opt_w opt_c)) files)
			(each (lambda ((job result))
				(prin result))
				(pipe-farm (map (# (str (first args)
					" -j " opt_j
					(if opt_w " -w" "")
					(if opt_c " -c" "")
					" " (slice (str %0) 1 -2)))
					(partition files opt_j)))))))