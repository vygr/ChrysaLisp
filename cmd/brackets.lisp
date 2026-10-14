(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/files/files.inc")
(import "lib/text/tokenize.inc")
(import "service/lock/app.inc")

(defun opt-verbosity (opt_var)
	(static-qq (lambda (args arg)
		(if (and (nempty? args)
				(not (starts-with "-" (first args)))
				(defq n (catch (str-as-num (first args)) :nil)))
			(progn (setq ,opt_var n) (rest args))
			(setq ,opt_var (if (num? ,opt_var) (max 1 (inc ,opt_var)) 1)) args))))

(defq usage `(
(("-h" "--help")
"Usage: brackets [options] [path] ...

    options:
        -h --help: this help info.
        -j --jobs num: max jobs per batch, default 8.
        -v --verbosity [level]: verbosity level 0..3 (default 0, bare -v is 1).
            0: standard (file: OK or error).
            1: summary (total bracket count, max nesting depth).
            2: type breakdown (parens, square, braces, depth, top forms).
            3: deep diagnostic with source line metrics.
        -q --quiet: quiet mode, only report errors.

    Scan source files for bracket matching (parentheses,
    square brackets, and braces) using syntax-aware scanning.
    Comments and string literals are safely ignored.

    What is in a {} string is code being made, and its
    brackets must pair too, but by themselves. A {}
    string ends at its first }, so a name can be put
    into such code, {(def-func "} name {")}: what one
    leaves open, a quote too, goes on in the next, and
    must be closed by the end of the form they are in.

    If no paths given on command line
    then paths are read from stdin.")
(("-j" "--jobs") ,(opt-num 'opt_j))
(("-v" "--verbosity") ,(opt-verbosity 'opt_v))
(("-q" "--quiet") ,(opt-flag 'opt_q))
))

(defq +file_types ''(".vp" ".inc" ".lisp"))

(defun work (file opt_v opt_q)
	(with-read-lock file
		(when (defq in (file-stream file))
			(defq tok_state :text line_no 0 stack (list)
				errs (list) num_paren 0 num_square 0 num_brace 0
				max_depth 0 top_forms 0 in_quote :nil
				;what is in a {} string is code being made, and its brackets are
				;to pair too, on a stack of their own. A string is ended by its
				;first }, so code that has a name put into it, {(def-func "}
				;name {")}, goes on in the next: what is open in one is open in
				;the next, a quote too, till the form they are in is closed
				text_stack (list) in_brace :nil)
			(defun text-left ()
				;what a {} string of a form left open, said when the form is closed
				(each (lambda ((ch open_line open_col))
						(push errs (cat file " (" (str open_line) ":" (str open_col) "): unclosed bracket '" ch "' in a {} string")))
					text_stack)
				(setq text_stack (list) in_quote :nil))
			(lines! (lambda (raw_line)
				(task-slice)
				(++ line_no)
				(defq line (trim-end raw_line "\r"))
				(bind '(toks states tok_next) (tokenize-line line tok_state))
				(setq tok_state tok_next)
				(defq col 1)
				(each (lambda (tok state)
					(defq ti 0 tlen (length tok))
					(case state
						((:comment :string1))
						(:string2
							(while (< ti tlen)
								(defq ch (elem-get tok ti) ch_col (+ col ti))
								(cond
									;the { that begins it, and the } that ends it, the last of its token
									((and (= ti 0) (not in_brace) (eql ch "{"))
										(++ num_brace)
										(if (empty? stack) (++ top_forms))
										(setq in_brace :t max_depth (max max_depth (inc (length stack)))))
									((and (= ti (dec tlen)) in_brace (eql ch "}"))
										(++ num_brace)
										(setq in_brace :nil))
									;a quoted string in it, {this, "missing )"}
									((eql ch "\q") (setq in_quote (not in_quote)))
									(in_quote)
									((or (eql ch "(") (eql ch "["))
										(push text_stack (list ch line_no ch_col)))
									((or (eql ch ")") (eql ch "]"))
										(cond
											((empty? text_stack)
												(push errs (cat file " (" (str line_no) ":" (str ch_col) "): unexpected closing bracket '" ch "' in a {} string")))
											((nql (if (eql ch ")") "(" "[") (first (defq top (pop text_stack))))
												(push errs (cat file " (" (str line_no) ":" (str ch_col) "): mismatched closing '" ch
													"' in a {} string, for '" (first top) "' opened at (" (str (second top)) ":" (str (third top)) ")"))))))
								(++ ti)))
						(:t
							(while (< ti tlen)
								(defq ch (elem-get tok ti) ch_col (+ col ti))
								(cond
									((or (eql ch "(") (eql ch "["))
										(if (eql ch "(") (++ num_paren) (++ num_square))
										(if (empty? stack) (++ top_forms))
										(push stack (list ch line_no ch_col))
										(setq max_depth (max max_depth (length stack))))
									((or (eql ch ")") (eql ch "]"))
										(if (eql ch ")") (++ num_paren) (++ num_square))
										(cond
											((empty? stack)
												(push errs (cat file " (" (str line_no) ":" (str ch_col) "): unexpected closing bracket '" ch "'")))
											(:t (defq top (pop stack) open_ch (first top) expected (if (eql open_ch "(") ")" "]"))
												(unless (eql ch expected)
													(push errs (cat file " (" (str line_no) ":" (str ch_col) "): mismatched closing '" ch "', expected '" expected "' for '" open_ch "' opened at (" (str (second top)) ":" (str (third top)) ")")))
												(if (empty? stack) (text-left))))))
								(++ ti))))
					(setq col (+ col tlen)))
					toks states)
				:nil) in)
			(text-left)
			(while (nempty? stack)
				(defq top (pop stack) open_ch (first top) open_line (second top) open_col (third top))
				(push errs (cat file " (" (str open_line) ":" (str open_col) "): unclosed bracket '" open_ch "'")))
			(when (eql tok_state :string1)
				(push errs (cat file ": unclosed string literal at EOF")))
			(when (eql tok_state :string2)
				(push errs (cat file ": unclosed brace '{' at EOF")))
			(cond
				((nempty? errs)
					(each (const print) errs))
				((= opt_v 1)
					(print file ": OK (" (str (+ num_paren num_square num_brace)) " brackets, max depth " (str max_depth) ")"))
				((= opt_v 2)
					(print file ": OK (" (str (+ num_paren num_square num_brace)) " brackets: "
						(str num_paren) " parens, "
						(str num_square) " square, "
						(str num_brace) " braces | max depth " (str max_depth)
						", " (str top_forms) " top forms)"))
				((>= opt_v 3)
					(print file ": OK (" (str (+ num_paren num_square num_brace)) " brackets: "
						(str num_paren) " parens, "
						(str num_square) " square, "
						(str num_brace) " braces | max depth " (str max_depth)
						" | " (str top_forms) " top forms | " (str line_no) " lines)"))
				((not opt_q)
					(print file ": OK"))))))

(defun main ()
	(when (and
			(defq stdio (create-stdio))
			(defq opt_j 8 opt_v 0 opt_q :nil args (options stdio usage)))
		(defq jobs (rest args))
		(if (empty? jobs)
			(lines! (# (push jobs %0) :nil) (io-stream 'stdin)))
		(setq jobs (filter (lambda (f) (some (# (ends-with %0 f)) +file_types)) jobs))
		(if (<= (length jobs) opt_j)
			(each (# (work %0 opt_v opt_q)) jobs)
			(each (lambda ((job result)) (prin result))
				(pipe-farm (map (# (str (first args)
						" -j " opt_j
						(if (/= opt_v 0) (cat " -v " (str opt_v)) "")
						(if opt_q " -q" "")
						" " (slice (str %0) 1 -2)))
					(partition jobs opt_j)))))))
