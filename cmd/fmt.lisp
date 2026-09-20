(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/files/files.inc")

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
; Lossless Tokenizer
;;;;;;;;;;;;;;;;;;;;;;;

(defun tokenize-lisp (source)
	(defq tokens (list) len (length source) idx 0 start 0 depth 0 ch "")
	(while (< idx len)
		(setq ch (elem-get source idx))
		(cond
			((or (eql ch " ") (eql ch "\t"))
				(setq start idx)
				(while (and (< idx len)
						(or (eql (defq c (elem-get source idx)) " ") (eql c "\t")))
					(++ idx))
				(push tokens (list :ws (slice source start idx))))
			((or (eql ch "\n") (eql ch "\r"))
				(if (and (eql ch "\r") (< (inc idx) len) (eql (elem-get source (inc idx)) "\n"))
					(++ idx))
				(++ idx)
				(push tokens (list :nl "\n")))
			((eql ch ";")
				(setq start idx)
				(while (and (< idx len)
						(nql (defq c (elem-get source idx)) "\n") (nql c "\r"))
					(++ idx))
				(push tokens (list :comment (slice source start idx))))
			((eql ch "\q")
				(setq start idx)
				(++ idx)
				(while (and (< idx len) (nql (elem-get source idx) "\q"))
					(if (and (eql (elem-get source idx) "\\") (< (inc idx) len))
						(++ idx 2)
						(++ idx)))
				(if (< idx len) (++ idx))
				(push tokens (list :string (slice source start idx))))
			((eql ch "{")
				(setq start idx depth 1)
				(++ idx)
				(while (and (< idx len) (> depth 0))
					(defq c (elem-get source idx))
					(cond
						((eql c "{") (++ depth))
						((eql c "}") (-- depth)))
					(++ idx))
				(push tokens (list :cscript (slice source start idx))))
			((eql ch "(")
				(push tokens (list :lparen "("))
				(++ idx))
			((eql ch ")")
				(push tokens (list :rparen ")"))
				(++ idx))
			((find ch "'`~,")
				(push tokens (list :quote (str ch)))
				(++ idx))
			(:t
				(setq start idx)
				(while (and (< idx len)
						(not (find (elem-get source idx) " \t\r\n();{}\q'`~,")))
					(++ idx))
				(push tokens (list :atom (slice source start idx))))))
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
		(defq tok (elem-get tokens i) type (first tok))
		(cond
			((eql type :quote)
				(bind '(next_i next_len) (measure-form tokens (inc i)))
				(list next_i (inc next_len)))
			((eql type :lparen)
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
	(if (< next_i (length tokens))
		(defq tok (elem-get tokens next_i))
		(if (eql (first tok) :atom)
			(case (second tok)
				(("defq" "setq") :defq)
				(("def" "set") :def)
				(:t :normal))
			:normal)
		:normal))

(defun inc-arg-count (form_stack)
	(when (nempty? form_stack)
		(defq frame (last form_stack))
		(elem-set frame 2 (inc (third frame)))))

(defun make-indent (level)
	(pad "" level "\t"))

;;;;;;;;;;;;;;;;;;;;;;;
; Code Formatter
;;;;;;;;;;;;;;;;;;;;;;;

(defun format-lisp (source)
	(defq tokens (tokenize-lisp source) len (length tokens)
		out (string-stream (str-alloc len)) indent_level 0 current_col 0
		at_line_start :t after_lparen :nil after_quote :nil
		pending_nl :nil pending_comment :nil consec_nl 0
		form_stack (list) idx 0 tok :nil type :nil val "")

	(while (< idx len)
		(setq tok (elem-get tokens idx) type (first tok) val (second tok))
		(cond
			((eql type :ws)
				(++ idx))
			((eql type :nl)
				(setq pending_nl :t)
				(if (< consec_nl 2) (++ consec_nl))
				(++ idx))
			((eql type :comment)
				(if at_line_start
					(progn
						(if pending_nl
							(times (min consec_nl 2) (write-blk out "\n")))
						(write-blk out (make-indent indent_level)))
					(write-blk out " "))
				(write-blk out val)
				(setq at_line_start :t pending_nl :t pending_comment :t
					consec_nl 1 current_col 0 after_lparen :nil after_quote :nil)
				(++ idx))
			((eql type :rparen)
				(setq indent_level (max 0 (dec indent_level)))
				(when at_line_start
					(if pending_comment
						(progn
							(write-blk out "\n")
							(write-blk out (make-indent indent_level)))
						(write-blk out (make-indent indent_level))))
				(write-blk out ")")
				(setq at_line_start :nil after_lparen :nil after_quote :nil
					pending_nl :nil pending_comment :nil consec_nl 0
					current_col (+ current_col 1))
				(when (nempty? form_stack)
					(pop form_stack)
					(inc-arg-count form_stack))
				(++ idx))
			(:t
				; Flush deferred newlines and indent before normal token content
				(when pending_nl
					(times (min consec_nl 2) (write-blk out "\n"))
					(write-blk out (make-indent indent_level))
					(setq at_line_start :t pending_nl :nil pending_comment :nil
						consec_nl 0 current_col (* indent_level +tab_width)))

				(cond
					((eql type :cscript)
						(unless (or at_line_start after_lparen after_quote)
							(write-blk out " ")
							(++ current_col))
						(write-blk out val)
						(setq at_line_start :nil after_lparen :nil after_quote :nil
							current_col (+ current_col (length val)))
						(inc-arg-count form_stack)
						(++ idx))
					((eql type :lparen)
						(unless (or at_line_start after_lparen after_quote)
							(write-blk out " ")
							(++ current_col))
						(write-blk out "(")
						(push form_stack (list (form-kind tokens (inc idx)) indent_level 0))
						(++ indent_level)
						(setq at_line_start :nil after_lparen :t after_quote :nil
							current_col (+ current_col 1))
						(++ idx))
					((eql type :quote)
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
							(bind '(kind base_indent arg_count) (last form_stack))
							(defq is_pair_start (cond
								((and (eql kind :defq) (>= arg_count 3) (odd? arg_count)) :t)
								((and (eql kind :def) (>= arg_count 2) (even? arg_count)) :t)
								(:nil)))
							(when is_pair_start
								(bind '(& pair_len) (measure-pair tokens idx))
								(when (> (+ current_col 1 pair_len) +max_line_len)
									(write-blk out "\n")
									(write-blk out (make-indent (+ base_indent 1)))
									(setq at_line_start :t
										current_col (* (+ base_indent 1) +tab_width)))))

						(unless (or at_line_start after_lparen after_quote)
							(write-blk out " ")
							(++ current_col))
						(write-blk out val)
						(setq at_line_start :nil after_lparen :nil after_quote :nil
							current_col (+ current_col (length val)))
						(inc-arg-count form_stack)
						(++ idx))))))
	(str out))

;;;;;;;;;;;;;;;;;;;;;;;
; File Worker
;;;;;;;;;;;;;;;;;;;;;;;

(defun work (file opt_w opt_c)
	(when (defq raw (load file))
		(defq formatted (format-lisp raw))
		(when (nql raw formatted)
			(cond
				(opt_w
					(save formatted file)
					(print "Formatted: " file))
				(opt_c
					(print "Needs formatting: " file))
				(:t
					(print "--- " file " ---")
					(print formatted))))))

(defun main ()
	(when (and
			(defq stdio (create-stdio))
			(defq opt_j 8 opt_w :nil opt_c :nil args (options stdio usage)))
		(defq files (rest args))
		(if (empty? files)
			(lines! (# (push files %0) :nil) (io-stream 'stdin)))
		(setq files (usort (filter (lambda (file)
			(some (# (ends-with %0 file)) +file_types)) files)))
		(cond
			((empty? files))
			((<= (length files) opt_j)
				(each (# (work %0 opt_w opt_c)) files))
			(:t
				(each (lambda ((job result)) (prin result))
					(pipe-farm (map (# (str (first args)
							" -j " opt_j
							(if opt_w " -w" "")
							(if opt_c " -c" "")
							" " (slice (str %0) 1 -2)))
						(partition files opt_j))))))))
