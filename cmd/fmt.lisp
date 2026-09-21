(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/files/files.inc")
(import "lib/text/syntax.inc")

(defq usage `(
(("-h" "--help")
"Usage: fmt [options] [path] ...

    options:
        -h --help: this help info.
        -j --jobs num: max jobs per batch, default 8.
        -w --write: overwrite files in-place, default :nil.
        -c --check: check if files need formatting without writing.

    Auto-formats ChrysaLisp source code according to tab indentation
    and sensible pair-packing rules for defq, setq, def, and set.

    Restricts targets to unique .lisp, .inc, and .vp files. If no paths
    are specified on the command line, paths are read from stdin.")
(("-j" "--jobs") ,(opt-num 'opt_j))
(("-w" "--write") ,(opt-flag 'opt_w))
(("-c" "--check") ,(opt-flag 'opt_c))
))

(defq +file_types ''(".lisp" ".inc" ".vp") +max_line_len 90 +tab_width 4)

;;;;;;;;;;;;;;;;;;;;;;;
; Fast Syntax Tokenizer
;;;;;;;;;;;;;;;;;;;;;;;

(defun tokenize-lisp (source)
	(defq tokens (list) stream (string-stream source) lines (list)
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
				(when (< (inc line_idx) num_lines)
					(unless (find (. syntax :get_state) '(:string1 :string2))
						(push tokens (list :nl "\n"))))
				(++ line_idx))))
	tokens)

;;;;;;;;;;;;;;;;;;;;;;;
; Token Measurement
;;;;;;;;;;;;;;;;;;;;;;;

(defun skip-ws-nl (tokens idx)
	(defq len (length tokens) i idx)
	(while (and (< i len) (find (first (elem-get tokens i)) '(:ws :nl)))
		(++ i))
	i)

(defun measure-form (tokens idx)
	(defq len (length tokens) i (skip-ws-nl tokens idx))
	(if (>= i len)
		(list len 0)
		(defq tok (elem-get tokens i) tok_type (first tok))
		(cond
			((eql tok_type :quote)
				(bind '(next_i next_len) (measure-form tokens (inc i)))
				(list next_i (inc next_len)))
			((eql tok_type :lparen)
				(defq depth 1 cur_i (inc i) total_len 1)
				(while (and (< cur_i len) (> depth 0))
					(defq t_type (first (elem-get tokens cur_i))
						t_val (second (elem-get tokens cur_i)))
					(cond
						((eql t_type :lparen) (++ depth))
						((eql t_type :rparen) (-- depth)))
					(setq total_len (+ total_len (length t_val)))
					(++ cur_i))
				(list cur_i total_len))
			(:t (list (inc i) (length (second tok)))))))

(defun measure-pair (tokens idx)
	(bind '(val_start var_len) (measure-form tokens idx))
	(bind '(pair_end val_len) (measure-form tokens val_start))
	(list pair_end (+ var_len 1 val_len)))

(defun form-kind (tokens idx)
	(defq next_i (skip-ws-nl tokens idx))
	(if (and (< next_i (length tokens))
		(eql (first (defq tok (elem-get tokens next_i))) :atom))
		(case (second tok)
			(("defq" "setq") :defq)
			(("def" "set") :def)
			(:t :normal))
		:normal))

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

;;;;;;;;;;;;;;;;;;;;;;;
; Code Formatter
;;;;;;;;;;;;;;;;;;;;;;;

(defun current-target-indent (form_stack)
	(if (nempty? form_stack)
		(last (last form_stack))
		0))

(defun format-lisp (source)
	(defq tokens (tokenize-lisp source) len (length tokens)
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
				(if (< consec_nl 3) (++ consec_nl))
				(++ idx))
			((eql tok_type :raw)
				(when pending_nl
					(times consec_nl (write-blk out "\n")))
				(write-blk out val)
				(setq at_line_start :t pending_nl :nil
					consec_nl 0 current_col 0 after_lparen :nil after_quote :nil)
				(++ idx))
			((eql tok_type :comment)
				(if (or at_line_start pending_nl)
					(progn
						(when pending_nl
							(times consec_nl (write-blk out "\n")))
						(defq ind (current-target-indent form_stack))
						(write-blk out (make-indent ind))
						(setq cur_line_indent ind current_col (* ind +tab_width)))
					(write-blk out " "))
				(write-blk out val)
				(setq at_line_start :nil pending_nl :nil
					consec_nl 0 current_col 0 after_lparen :nil after_quote :nil)
				(++ idx))
			((eql tok_type :rparen)
				(defq closed_frame (if (nempty? form_stack) (pop form_stack) :nil))
				(when pending_nl
					(times consec_nl (write-blk out "\n"))
					(defq r_ind (if closed_frame (second closed_frame) (current-target-indent form_stack)))
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
				; Flush deferred newlines and apply line-relative indentation
				(when pending_nl
					(times consec_nl (write-blk out "\n"))
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
							current_col (if (find "\n" val) (last-line-len val) (+ current_col (length val))))
						(inc-arg-count form_stack)
						(++ idx))
					((eql tok_type :lparen)
						(unless (or at_line_start after_lparen after_quote)
							(write-blk out " ")
							(++ current_col))
						(write-blk out "(")
						(defq kind (form-kind tokens (inc idx))
							child_ind (+ cur_line_indent 1))
						(push form_stack (list kind cur_line_indent 0 child_ind))
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
						; Handle pair wrapping for defq, setq, def, and set
						(when (and (nempty? form_stack) (not at_line_start))
							(bind '(kind base_indent arg_count &ignore) (last form_stack))
							(defq is_pair_start (cond
								((and (eql kind :defq) (>= arg_count 3) (odd? arg_count)) :t)
								((and (eql kind :def) (>= arg_count 2) (even? arg_count)) :t)
								(:nil)))
							(when is_pair_start
								(bind '(& pair_len) (measure-pair tokens idx))
								(when (> (+ current_col 1 pair_len) +max_line_len)
									(write-blk out "\n")
									(defq p_ind (+ base_indent 1))
									(write-blk out (make-indent p_ind))
									(setq at_line_start :t
										cur_line_indent p_ind
										current_col (* p_ind +tab_width)))))

						(unless (or at_line_start after_lparen after_quote)
							(write-blk out " ")
							(++ current_col))
						(write-blk out val)
						(setq at_line_start :nil after_lparen :nil after_quote :nil
							current_col (+ current_col (length val)))
						(inc-arg-count form_stack)
						(++ idx))))))
	(when pending_nl
		(times consec_nl (write-blk out "\n")))
	(str out))

;;;;;;;;;;;;;;;;;;;;;;;
; File Worker
;;;;;;;;;;;;;;;;;;;;;;;

(defun work (file opt_w opt_c)
	(when (defq raw (load file))
		(defq formatted (format-lisp raw))
		(cond
			(opt_w
				(when (nql raw formatted)
					(save formatted file)
					(print "Formatted: " file)))
			(opt_c
				(when (nql raw formatted)
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
			(each (lambda ((job result)) (prin result))
				(pipe-farm (map (# (str (first args)
					" -j " opt_j
					(if opt_w " -w" "")
					(if opt_c " -c" "")
					" " (slice (str %0) 1 -2)))
					(partition files opt_j)))))))