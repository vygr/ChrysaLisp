(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/files/files.inc")
(import "service/lock/app.inc")
(import "lib/text/syntax.inc")

(defq usage `(
(("-h" "--help")
"Usage: forward [options] [path] ...

    options:
        -h --help: this help info.
        -j --jobs num: max jobs per batch, default 1.

    Scan source files for a function or macro that is
    used above where it is defined, and for a function
    that calls itself.

    Such a use is not bound to the function as the code
    is read, the name is looked up each time it is run.
    In a module the name is not there to find. And a
    function that calls itself can run out of stack, a
    list is the stack to use, as (flatten) does.

    What is in a comment or a string is not looked at.

    If no paths given on command line
    then will test files from stdin.")
(("-j" "--jobs") ,(opt-num 'opt_j))
))

;do the work on a file
(defun work (file)
	;The file is read as the highlighter reads it, a token at a time, so
	;what is in a comment or a string is not looked at, and it is known
	;which definition each line is in. A function calls itself if its
	;name is anywhere in its own definition, as the head of a form or
	;handed to another function, (some my-self list)
	(with-read-lock file
		(when (defq in (file-stream file))
			(defq syntax (Syntax) line_no 0 depth 0 defs_map (Fmap 11) uses (list)
				;the definitions that are open, the innermost last, each
				;(name depth kind), and what the next symbol is
				open (list) after_open :nil want :nil)
			(while (defq raw_line (read-line in))
				(task-slice)
				(++ line_no)
				(bind '(toks states) (. syntax :tokenize (trim-end raw_line "\r")))
				(each (lambda (tok state)
					(case state
						(:text (each (lambda (ch)
							(cond
								((eql ch "(") (++ depth) (setq after_open :t))
								((eql ch ")") (-- depth) (setq after_open :nil)
									;the end of a definition
									(if (and (nempty? open) (<= depth (second (last open)))) (pop open)))
								((find ch " \t"))
								((setq after_open :nil)))) tok))
						(:symbol
							(cond
								(want ;the name of a definition, the first of it is the one
									(unless (. defs_map :find tok) (. defs_map :insert tok line_no))
									(push open (list tok (dec depth) want))
									(setq want :nil))
								((and after_open (or (eql tok "defun") (eql tok "defmacro")))
									(setq want tok))
								((push uses (list tok line_no (if (nempty? open) (last open)) after_open))))
							(setq after_open :nil))
						(:t (setq after_open :nil))))
					toks states))
			(each (lambda ((name at inside head))
				(when (defq n (. defs_map :find name))
					(cond
						;used above where it is defined, as the head of a form.
						;Anywhere else it may be a name of something else, an
						;enum called main in a file that has a main
						((and head (< at n)) (print file " (" at ") " name))
						;a function used in its own definition, it calls itself
						((and inside (eql (first inside) name) (eql (third inside) "defun"))
							(print file " (" at ") " name " calls itself")))))
				uses))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_j 1 args (options stdio usage)))
		;from args ?
		(if (empty? (defq jobs (rest args)))
			;no, so from stdin
			(lines! (# (push jobs %0) :nil) (io-stream 'stdin)))
		;only source files
		(setq jobs (filter (lambda (f) (some (# (ends-with %0 f)) '(".vp" ".inc" ".lisp"))) jobs))
		(if (<= (length jobs) opt_j)
			;do the work when batch size ok !
			(each (const work) jobs)
			;do them all out there, by calling myself !
			(each (lambda ((job result)) (prin result))
				(pipe-farm (map (# (str (first args)
						" -j " opt_j
						" " (slice (str %0) 1 -2)))
					(partition jobs opt_j)))))))
