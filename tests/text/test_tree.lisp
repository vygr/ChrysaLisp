(import "lib/text/tree.inc")

(report-header "Tree: source nested by its brackets, comments kept")

(defun tt-read (text) (tree-read (string-stream text)))

(bind '(tt_nodes tt_open) (tt-read (cat
	";a file" (ascii-char 10)
	(ascii-char 10)
	"(defun add (a b)" (ascii-char 10)
	"	; (add a b) -> num" (ascii-char 10)
	"	;both of them" (ascii-char 10)
	"	(+ a b)) ;after" (ascii-char 10)
	"(defq s \q(not ; a form)\q k :key n 12)" (ascii-char 10))))
(assert-eq "nothing left open" 0 tt_open)
(assert-eq "a comment, an empty line, a form, a comment after it, a form" "(:comment :blank :form :comment :form)" (str (map (const first) tt_nodes)))
(assert-eq "the lines a form is on" "(3 6)" (str (list (second (third tt_nodes)) (elem-get (third tt_nodes) 3))))
(defq tt_fn (tree-form? (third tt_nodes) "defun"))
(assert-true "it is a defun" tt_fn)
(assert-true "and not a defq" (not (tree-form? (third tt_nodes) "defq")))
(assert-eq "its name" "add" (third (second tt_fn)))
(assert-eq "its parameters" "(a b)" (tree-text (third tt_fn)))
(assert-eq "what is said of it" "(\q; (add a b) -> num\q \q;both of them\q)" (str (map (const third) (tree-said (third tt_nodes) tt_nodes 2))))
(assert-eq "and the lines that is on" "(4 5)" (str (map (const second) (tree-said (third tt_nodes) tt_nodes 2))))
(bind '(tt_one &ignore) (tt-read (cat "(ffi path name) ;a note" (ascii-char 10) ";of it" (ascii-char 10) ";more" (ascii-char 10) (ascii-char 10) ";not of it")))
(assert-eq "of a form on one line, the lines after it" "(\q;of it\q \q;more\q)" (str (map (const third) (tree-said (first tt_one) tt_one 0))))
(bind '(tt_one &ignore) (tt-read (cat "(defun f (a) ;a note" (ascii-char 10) "	(g a)" (ascii-char 10) "	;later" (ascii-char 10) "	a)")))
(assert-eq "a note on its first line, and a comment after its code, are not of it" "()" (str (tree-said (first tt_one) tt_one 0)))
(assert-eq "it on a line" "(defun add (a b) (+ a b))" (tree-text (third tt_nodes)))
(assert-eq "brackets and a ; in a string are of the string" "(:symbol :symbol :string1 :symbol :keysym :symbol :number)"
	(str (map (const first) (third (last tt_nodes)))))
(assert-eq "one not closed" 2 (second (tt-read "(a (b c")))
(assert-eq "one closed that was not open is left out" "(0 1)" (str (list (second (defq tt_r (tt-read "a) (b)"))) (length (filter (# (eql (first %0) :form)) (first tt_r))))))
;a string in braces that goes over lines and ends with the first character of one
(bind '(tt_nodes tt_open) (tt-read (cat "(cat {one (" (ascii-char 10) "} x {two" (ascii-char 10) "})" (ascii-char 10) "(b)")))
(assert-eq "it ends there, and what is after is code" "(0 2)" (str (list tt_open (length tt_nodes))))
;every source file of the system is whole
(import "lib/files/files.inc")
(defq tt_bad (list))
(each (lambda (file)
	(when (defq s (file-stream file))
		(if (/= 0 (second (tree-read s))) (push tt_bad file)))
	(task-slice))
	(files-all "./lib" '(".inc" ".vp" ".lisp")))
(assert-eq "no file of lib has a bracket left open" "()" (str tt_bad))

;the progns command, cmd/progns.lisp, reads the tree: a (progn) where there is one already
(defq tt_file "tmp_progns_test.lisp")
(save (cat "(if a b (progn c d))" (ascii-char 10) "(ifn a b" (ascii-char 10) "	;note" (ascii-char 10) "	(progn c))" (ascii-char 10)
	"(when a (progn b c))" (ascii-char 10) "(if a (progn b) c)" (ascii-char 10) "(while x (progn y) z)" (ascii-char 10)
	"{(if a b (progn c))}" (ascii-char 10) ";(if a b (progn c))" (ascii-char 10)) tt_file)
(defq tt_said (list))
(pipe-run (cat "progns " tt_file) (# (push tt_said %0)))
(assert-eq "round the else of an if and of an ifn, and all of the body of a when; not the first form of an if, nor one in a string or a comment"
	(cat tt_file " (1) a (progn) round the else of an (if)" (ascii-char 10)
		tt_file " (4) a (progn) round the else of an (ifn)" (ascii-char 10)
		tt_file " (5) a (progn) is all the body of a (when)" (ascii-char 10))
	(join tt_said ""))
;a quote before a form is not a form, and what it quotes is code being
;made, not a (progn) that is run: (if tst `(progn ~body)) has no else at all
(save (cat "(if a `(progn ~e))" (ascii-char 10) "(if a `(progn b) `(progn c))" (ascii-char 10)
	"(if a b 'x (progn c))" (ascii-char 10) "(when a `(progn ,b))" (ascii-char 10)) tt_file)
(setq tt_said (list))
(pipe-run (cat "progns " tt_file) (# (push tt_said %0)))
(assert-eq "a quoted (progn) is not said, one after a quoted thing is"
	(cat tt_file " (3) a (progn) round the else of an (if)" (ascii-char 10))
	(join tt_said ""))
(pipe-run (cat "rm " tt_file) (lambda (&) :nil))
