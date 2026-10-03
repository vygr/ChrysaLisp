(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/files/files.inc")
(import "lib/text/syntax.inc")
(import "service/lock/app.inc")

(defun opt-verbosity (opt_var)
	(static-qq (lambda (args arg)
		(if (and (nempty? args)
				(not (starts-with "-" (first args)))
				(defq n (catch (str-as-num (first args)) :nil)))
			(progn (setq ,opt_var n) (rest args))
			(progn (setq ,opt_var (if (num? ,opt_var) (max 1 (inc ,opt_var)) 1)) args)))))

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
			(defq syntax (Syntax) line_no 0 stack (list)
				errs (list) num_paren 0 num_square 0 num_brace 0
				max_depth 0 top_forms 0 in_quote :nil)
			(while (defq raw_line (read-line in))
				(task-slice)
				(++ line_no)
				(defq line (trim-end raw_line "\r"))
				(bind '(toks states) (. syntax :tokenize line))
				(defq col 1)
				(each (lambda (tok state)
					(unless (find state '(:comment :string1))
						(defq ti 0 tlen (length tok))
						(while (< ti tlen)
							(defq ch (elem-get tok ti) ch_col (+ col ti))
							(cond
								;a quoted string inside a {} block, {this, "missing )"}, is not
								;seen by the tokenizer, so skip over it here, it may span lines
								((and (eql ch "\q") (some (# (eql (first %0) "{")) stack))
									(setq in_quote (not in_quote)))
								((and in_quote (nql ch "}")))
								((or (eql ch "(") (eql ch "[") (eql ch "{"))
									(cond
										((eql ch "(") (++ num_paren))
										((eql ch "[") (++ num_square))
										((eql ch "{") (++ num_brace)))
									(if (empty? stack) (++ top_forms))
									(push stack (list ch line_no ch_col))
									(setq max_depth (max max_depth (length stack))))
								((or (eql ch ")") (eql ch "]") (eql ch "}"))
									(setq in_quote :nil)
									(cond
										((eql ch ")") (++ num_paren))
										((eql ch "]") (++ num_square))
										((eql ch "}") (++ num_brace)))
									(if (empty? stack)
										(push errs (cat file " (" (str line_no) ":" (str ch_col) "): unexpected closing bracket '" ch "'"))
										(defq top (pop stack) open_ch (first top) open_line (second top) open_col (third top))
										(defq expected (case open_ch ("(" ")") ("[" "]") ("{" "}")))
										(unless (eql ch expected)
											(push errs (cat file " (" (str line_no) ":" (str ch_col) "): mismatched closing '" ch "', expected '" expected "' for '" open_ch "' opened at (" (str open_line) ":" (str open_col) ")"))))))
							(++ ti)))
					(setq col (+ col (length tok))))
					toks states))
			(while (nempty? stack)
				(defq top (pop stack) open_ch (first top) open_line (second top) open_col (third top))
				(push errs (cat file " (" (str open_line) ":" (str open_col) "): unclosed bracket '" open_ch "'")))
			(when (eql (. syntax :get_state) :string1)
				(push errs (cat file ": unclosed string literal at EOF")))
			(when (eql (. syntax :get_state) :string2)
				(push errs (cat file ": unclosed brace '{' or CScript block at EOF")))
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
