(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/files/files.inc")
(import "service/lock/app.inc")
(import "lib/text/tree.inc")

(defq usage `(
(("-h" "--help")
"Usage: forward [options] [path] ...

    options:
        -h --help: this help info.
        -j --jobs num: max jobs per batch, default 1.

    Scan source files for a function or macro that is
    used above where it is defined, as the head of a form
    or named in (const), and for a function that calls
    itself.

    Such a use is not bound to the function as the code
    is read, the name is looked up each time it is run.
    In a module the name is not there to find. And a
    function that calls itself can run out of stack, a
    list is the stack to use, as (flatten) does.

    What is in a comment or a string is not looked at.
    A test of the very thing has a comment that begins
    ;check off on its line, or on the line before, and
    nothing is said of it.

    If no paths given on command line
    then will test files from stdin.")
(("-j" "--jobs") ,(opt-num 'opt_j))
))

;do the work on a file
(defun work (file)
	;The file is read as the highlighter reads it, (tree-read), so what is
	;in a comment or a string is not looked at, and it is known which
	;definition each thing is in. A function calls itself if its name is
	;anywhere in its own definition, as the head of a form or handed to
	;another function, (some my-self list)
	(with-read-lock file
		(when (defq in (file-stream file))
			(defq defs_map (Fmap 11) uses (list) off (list)
				;what is left to look at, each (node inside head): the
				;definition it is in, (name kind), and if it is the head of
				;its form, a list of them, it does not call itself
				stack (rmap (# (list %0 :nil :nil)) (first (tree-read in))))
			(setq in :nil)
			(while (defq item (pop stack))
				(bind '(node inside head) item)
				(case (first node)
					(:form
						(task-slice)
						(defq kids (filter (# (nql (first %0) :blank)) (third node))
							word (if (eql (first (first kids)) :symbol) (third (first kids)))
							from 0 named :nil)
						(cond
							((find word '("defun" "defmacro"))
								;the name of a definition is on the line its defun is
								;on, the first of it is the one. A defun with none
								;after it is a word of something else, a key of a case
								(setq from 1)
								(when (and (defq name (second kids)) (eql (first name) :symbol)
										(= (second name) (second (first kids))))
									(unless (. defs_map :find (third name)) (. defs_map :insert (third name) (second name)))
									(setq inside (list (third name) word) from 2)))
							((eql word "const")
								;a function named in (const), its value is taken as the
								;code is read, as a head of a form is bound then
								(setq from 1)
								(and (defq name (second kids)) (eql (first name) :symbol)
									(setq from 2 named name))))
						(each! (lambda (kid) (push stack (list kid inside (and (= (!) 0) (= from 0)))))
							(list kids) -1 from)
						(if named (push stack (list named inside :t))))
					(:symbol
						(push uses (list (third node) (second node) inside head)))
					(:comment
						(if (nempty? (substr (third node) ";check off")) (push off (second node))))))
			(each (lambda ((name at inside head))
					(and (defq n (. defs_map :find name)) (not (find at off)) (not (find (dec at) off))
						(cond
							;used above where it is defined, as the head of a form.
							;Anywhere else it may be a name of something else, an
							;enum called main in a file that has a main
							((and head (< at n)) (print file " (" at ") " name))
							;a function used in its own definition, it calls itself
							((and inside (eql (first inside) name) (eql (second inside) "defun"))
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
