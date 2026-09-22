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
	;; definitions & bindings
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
	"deffimethod" '(:head :head :body)
	"defgetmethod" '(:head :body)
	"defsetmethod" '(:head :body)
	"defproxymethod" '(:head :head :head :body)
	"defclass" '(:head :head :head :body)
	"def-class" '(:head :head :body)
	"def-method" '(:head :head :body)
	"def-func" '(:head :body)
	"lambda" '(:head :body)
	"macro" '(:head :body)
	"let" '(:head :body)
	"let*" '(:head :body)
	"#" '(:flow)

	;; conditionals & branching
	"cond" '(:clauses)
	"condn" '(:clauses)
	"case" '(:head :clauses)
	"pcase" '(:head :head :clauses)
	"switch" '(:head :clauses)
	"if" '(:head :body)
	"ifn" '(:head :body)
	"when" '(:head :body)
	"unless" '(:head :body)

	;; loops & iteration
	"while" '(:head :body)
	"until" '(:head :body)
	"for" '(:head :head :body)
	"times" '(:head :body)
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

	;; logic, blocks, exception handling
	"and" '(:flow)
	"or" '(:flow)
	"progn" '(:body)
	"catch" '(:head :body)
	"throw" '(:flow)
	"structure" '(:head :head :body)
	"enums" '(:head :head :body)
	"bits" '(:head :head :body)
	"time-it" '(:head :body)
	"undoable" '(:head :body)
	"within-compile-env" '(:head :body)))

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
	(while (and (< i len) (find (first (elem-get tokens i)) '(:ws :nl)))
		(++ i))
	i)

(defun skip-ws (tokens idx)
	; skip whitespace tokens only
	(defq len (length tokens) i idx)
	(while (and (< i len) (eql (first (elem-get tokens i)) :ws))
		(++ i))
	i)

(defun form-opens-at-eol? (tokens lparen_idx)
	; check if an opening paren is at the end of a line
	(defq len (length tokens)
		op_i (skip-ws tokens (inc lparen_idx)))
	(if (>= op_i len)
		:t
		(defq next_i (skip-ws tokens (inc op_i)))
		(if (>= next_i len)
			:t
			(eql (first (elem-get tokens next_i)) :nl))))

(defun lookup-form-template (tokens idx parent_role)
	(cond
		((eql parent_role :clauses)
			'(:head :flow))
		(:t
			(defq next_i (skip-ws-nl tokens idx))
			(if (and (< next_i (length tokens))
				(eql (first (defq tok (elem-get tokens next_i))) :atom))
				(or (. +templates :find (second tok))
					'(:flow))
				'(:flow)))))

;;;;;;;;;;;;;;;;;;;;;;;
; fast syntax tokenizer
;;;;;;;;;;;;;;;;;;;;;;;

(defun check-template-break (form_stack at_line_start pending_nl after_quote)
	; check if current template requires a newline before this argument
	(when (and (nempty? form_stack) (not after_quote))
		(bind '(tmpl & arg_count &ignore) (last form_stack))
		(when (> arg_count 0)
			(defq role (template-role tmpl arg_count))
			(when (or (eql role :body) (eql role :clauses))
				(unless (or at_line_start pending_nl)
					:t)))))

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
								(defq kind (if (eql tok_state :string1)
									:string
									:cscript))
								(if (and (eql tok_state prev_state) (nempty? tokens) (eql (first (last tokens)) kind))
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
	(defq stream (if (str? stream_or_src)
		(string-stream stream_or_src)
		stream_or_src)
		tokens (tokenize-lisp stream) len (length tokens)
		out (string-stream (str-alloc len))
		cur_line_indent 0 current_col 0
		at_line_start :t after_lparen :nil after_quote :nil
		pending_nl :nil consec_nl 0
		form_stack (list) idx 0 tok :nil tok_type :nil val "")

	(while (< idx len)
		(setq tok (elem-get tokens idx) tok_type (first tok) val (second tok))
		(cond
			((eql tok_type :ws)
				(++ idx))
			((eql tok_type :nl)
				(setq pending_nl :t)
				(if (< consec_nl 3)
					(++ consec_nl))
				(++ idx))
			((eql tok_type :raw)
				(when pending_nl
					(times consec_nl
						(write-blk out "\n")))
				(write-blk out val)
				(setq at_line_start :t pending_nl :nil
					consec_nl 0 current_col 0 after_lparen :nil after_quote :nil)
				(++ idx))
			((eql tok_type :comment)
				(if (or at_line_start pending_nl)
					(progn
						(when pending_nl
							(times consec_nl
								(write-blk out "\n")))
						(defq ind (current-target-indent form_stack))
						(write-blk out (make-indent ind))
						(setq cur_line_indent ind current_col (* ind +tab_width)))
					(write-blk out " "))
				(write-blk out val)
				(setq at_line_start :nil pending_nl :nil
					consec_nl 0 current_col 0 after_lparen :nil after_quote :nil)
				(++ idx))
			((eql tok_type :rparen)
				(defq closed_frame (if (nempty? form_stack)
					(pop form_stack)
					:nil))
				(when pending_nl
					(times consec_nl
						(write-blk out "\n"))
					(defq r_ind (if closed_frame
						(second closed_frame)
						(current-target-indent form_stack)))
					(write-blk out (make-indent r_ind))
					(setq at_line_start :t cur_line_indent r_ind
						current_col (* r_ind +tab_width)))
				(write-blk out ")")
				(setq at_line_start :nil after_lparen :nil after_quote :nil
					pending_nl :nil consec_nl 0
					current_col (+ current_col 1))
				(inc-arg-count form_stack)
				(++ idx))
			(:t
				; check if current template requires a newline before this argument
				(when (check-template-break form_stack at_line_start pending_nl after_quote)
					(setq pending_nl :t consec_nl 1))

				; flush deferred newlines and apply stack-directed indentation
				(when pending_nl
					(times consec_nl
						(write-blk out "\n"))
					(defq ind (current-target-indent form_stack))
					(write-blk out (make-indent ind))
					(setq at_line_start :t pending_nl :nil
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
							tmpl (lookup-form-template tokens (inc idx) p_role)
							eol_open (form-opens-at-eol? tokens idx)
							f_base (cond
								(at_line_start cur_line_indent)
								(eol_open (current-target-indent form_stack))
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
		(times consec_nl
			(write-blk out "\n")))
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
					(if opt_w
						" -w"
						"")
					(if opt_c
						" -c"
						"")
					" " (slice (str %0) 1 -2)))
					(partition files opt_j)))))))