(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/text/tree.inc")
(import "service/lock/app.inc")

(defq usage `(
(("-h" "--help")
"Usage: progns [options] [path] ...

    options:
        -h --help: this help info.
        -j --jobs num: max jobs per batch, default 1.

    Scan source files for a (progn) where there is one
    already: round the else forms of an (if) or an (ifn),
    or as the whole body of a (when), (unless), (while)
    or (until). Each of those takes as many forms as are
    given, the (progn) says nothing and costs a call.

    What is in a comment or a string is not looked at.

    If no paths given on command line
    then will test files from stdin.")
(("-j" "--jobs") ,(opt-num 'opt_j))
))

;do the work on a file
(defun work (file)
	;The file is read as a tree, lib/text/tree.inc, and every form of it
	;looked at, those inside others too. A list of the lists of nodes
	;still to look at, no function here calls itself
	(with-read-lock file
		(when (defq in (file-stream file))
			(defq todo (list (first (tree-read in))))
			(while (defq nodes (pop todo))
				(task-slice)
				(each (lambda (node)
					(when (defq kids (tree-form? node))
						(push todo kids)
						;what is in it that is not a comment or an empty line. A
						;quote before a form, ' ` , or ~, is a thing of its own in the
						;tree, and is no form: it is left out, and the form after
						;it is not looked at as a (progn), it is a list or code that
						;is being made, `(progn ~body)
						(defq forms (list) quoted :nil)
						(each (lambda (kid)
							(cond
								((find (first kid) '(:comment :blank)))
								((and (eql (first kid) :symbol) (= (bskip (const (char-class "'`,~")) (third kid) 0) (length (third kid))))
									(setq quoted :t))
								(quoted (push forms (list :quoted (second kid))) (setq quoted :nil))
								((push forms kid))))
							kids)
						(defq head (if (and (nempty? forms) (eql (first (first forms)) :symbol)) (third (first forms))))
						(cond
							;(if tst form else ...), the first of the else forms
							((and (find head '("if" "ifn")) (> (length forms) 3)
									(defq found (some (# (if (tree-form? %0 "progn") %0)) (slice forms 3 -1))))
								(print file " (" (second found) ") a (progn) round the else of an (" head ")"))
							;(when tst body ...), a body that is one (progn)
							((and (find head '("when" "unless" "while" "until")) (= (length forms) 3)
									(tree-form? (third forms) "progn"))
								(print file " (" (second (third forms)) ") a (progn) is all the body of a (" head ")")))))
					nodes)))))

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
