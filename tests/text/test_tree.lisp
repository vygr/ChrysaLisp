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

;the check command, cmd/check.lisp, reads the tree: what can be seen to be wrong from the source
(defq tt_file "tmp_check_test.lisp" tt_lf (ascii-char 10))
(defun tt-check (checks &rest lines)
	;what check says of a file of these lines, each line it says less the name of the file
	(save (apply (const cat) (map (# (cat %0 tt_lf)) lines)) tt_file)
	(defq said (list))
	(pipe-run (cat "check -c " checks " " tt_file) (# (push said %0)))
	(map (# (slice %0 (inc (length tt_file)) -1)) (filter (const nempty?) (split (join said "") tt_lf))))
(assert-list-eq "progn: round the else of an if and of an ifn, and all of the body of a when; not the first form of an if, nor one in a string or a comment"
	'("(1) progn: a (progn) round the else of an (if)" "(4) progn: a (progn) round the else of an (ifn)"
		"(5) progn: a (progn) is all the body of a (when)")
	(tt-check "progn" "(if a b (progn c d))" "(ifn a b" "	;note" "	(progn c))" "(when a (progn b c))" "(if a (progn b) c)"
		"(while x (progn y) z)" "{(if a b (progn c))}" ";(if a b (progn c))"))
;a quote before a form is not a form, and what it quotes is code being
;made, not a (progn) that is run: (if tst `(progn ~body)) has no else at all
(assert-list-eq "progn: a quoted one is not said, one after a quoted thing is, and nothing inside a quoted form"
	'("(3) progn: a (progn) round the else of an (if)")
	(tt-check "progn" "(if a `(progn ~e))" "(if a `(progn b) `(progn c))" "(if a b 'x (progn c))" "(when a `(progn ,b))"
		"(str `(if a b (progn c)))"))
(assert-list-eq "class: a string of characters not in order, given where a class is looked up; one in order, one of one, and a (char-class) are not"
	(list (cat "(2) class: " (ascii-char 34) "ba" (ascii-char 34) " given to (bskip) is not in order, (char-class " (ascii-char 34) "ba" (ascii-char 34) ")")
		(cat "(5) class: " (ascii-char 34) " ;" (ascii-char 92) "t" (ascii-char 34) " given to (trim) is not in order, (char-class " (ascii-char 34) " ;" (ascii-char 92) "t" (ascii-char 34) ")"))
	(tt-check "class" (cat "(bskip " (ascii-char 34) "ab" (ascii-char 34) " s 0)") (cat "(bskip " (ascii-char 34) "ba" (ascii-char 34) " s 0)")
		(cat "(bfind c " (ascii-char 34) "x" (ascii-char 34) ")") (cat "(split s (char-class " (ascii-char 34) "ba" (ascii-char 34) "))")
		(cat "(trim s " (ascii-char 34) " ;" (ascii-char 92) "t" (ascii-char 34) ")")))
(assert-list-eq "zero: a number with a point that is 0 as a fixed, and is not written 0"
	'("(1) zero: 0.000001 is too small for a fixed, it is 0" "(3) zero: -0.00001 is too small for a fixed, it is 0")
	(tt-check "zero" "(n2r 0.000001)" "(+ 0.0 0.5 0.00002 100 0)" "(* x -0.00001)"))
(assert-list-eq "shadow: a name of the system as a test of an and, an or, or a clause of a cond; not where it is given to a function"
	'("(1) shadow: first as a test of (and) is taken as the function of the system, not the variable"
		"(2) shadow: list as a test of (or) is taken as the function of the system, not the variable"
		"(3) shadow: cat as a test of (cond) is taken as the function of the system, not the variable")
	(tt-check "shadow" "(and first (> x 0))" "(or x list)" "(cond (cat 1) ((cat) 2) (mine 3))" "(if first 1 2)" "(and (first x) mine)"))
(assert-list-eq "const: a + name given a list of strings or of numbers quoted once; not one quoted twice, nor one that begins with a name"
	'("(1) const: +a is a list quoted once, ''(...)" "(4) const: +n is a list quoted once, ''(...)")
	(tt-check "const" (cat "(defq +a '(" (ascii-char 34) ".md" (ascii-char 34) ") b '(1 2)")
		(cat "	+c ''(" (ascii-char 34) ".md" (ascii-char 34) ")") "	+d '(lambda))" "(defq +n '(1 2 3))"))
(assert-list-eq "style: an if of a progn with no else, an if of a not with an else and with none, a cond of one clause"
	'("(1) style: (if tst (progn ...)) with no else is a (when)" "(2) style: (if (not tst) form else ...) is an (ifn tst form else ...)"
		"(3) style: a (cond) of one clause" "(7) style: (if (not tst) form) with no else is a (unless tst form)")
	(tt-check "style" "(if a (progn b c))" "(if (not a) b c)" "(cond ((a) b))" "(if a (progn b) c)" "(cond ((a) b) (:t c))" "(cond)"
		"(if (not a) b)"))
(assert-list-eq "style: and of an ifn and a condn, the other way about"
	'("(1) style: (ifn tst (progn ...)) with no else is a (unless)" "(2) style: (ifn (not tst) form else ...) is an (if tst form else ...)"
		"(3) style: a (condn) of one clause" "(5) style: (ifn (not tst) form) with no else is a (when tst form)")
	(tt-check "style" "(ifn a (progn b c))" "(ifn (not a) b c)" "(condn ((a) b))" "(condn ((a) b) (:t c))" "(ifn (not a) b)" "(ifn a b c)"))
(assert-list-eq "style: a when, or an if with no else, of an and and one form; not with two forms, nor an if with an else"
	'("(1) style: (when (and ...) form) is an (and ... form)" "(2) style: (if (and ...) form) is an (and ... form)")
	(tt-check "style" "(when (and a b) c)" "(if (and a b) c)" "(when (and a b) c d)" "(if (and a b) c d)" "(when (or a b) c)" "(when a (and b c))"))
(assert-list-eq "and they are the same thing: what each gives, with every test so, with one not, and with the form :nil"
	'((3 3 3) (:nil :nil :nil) (:nil :nil :nil) (:nil :nil :nil))
	;check off, the forms themselves are what is tried
	(map (lambda ((a b x)) (list (when (and a b) x) (if (and a b) x) (and a b x))) '((1 2 3) (:nil 2 3) (1 :nil 3) (1 2 :nil))))
;why the two are told apart: an (ifn) with no else is not an (if) of a (not) with none
(assert-list-eq "an (ifn) with an else is the (if) of a (not); with none it gives its test, the (if) gave :nil, as an (unless) does"
	'(1 2 1 2 7 :nil :nil)
	;check off, the forms themselves are what is tried
	(list (ifn :nil 1 2) (ifn 7 1 2) (if (not :nil) 1 2) (if (not 7) 1 2) (ifn 7 1) (if (not 7) 1) (unless 7 1)))
(assert-list-eq "doc: a comment of another name, or of more or fewer things than it takes; a list or a choice is one thing, a rest may be named"
	'("(2) doc: says (h ...), this is (g ...)" "(5) doc: (k) is said to take 1, it takes 2" "(8) doc: (m) is said to take 3, it takes 2")
	(tt-check "doc" "(defun g (a b &optional c)" "	; (h a b [c]) -> x" "	1)" "(defun k (a b)" "	; (k a) -> x" "	1)"
		"(defun m (a b)" "	; (m a b c) -> x" "	1)" "(defun n (a &rest b)" "	; (n a b c) -> x" "	1)"
		"(defun o (a)" "	; (o (x y) | z) -> x" "	1)" "(defmacro p (a &optional b)" "	; (p a [b]) -> x" "	1)"))
(assert-list-eq "enum: a field named size"
	'("(2) enum: a field named size is the (enums)'s own count of its fields")
	(tt-check "enum" "(enums +x 0" "	(enum a size b))" "(enums +y 0 (enum a sizes b))"))
(assert-list-eq "capture: a function given to another of the file that uses a name the other has as a parameter, called or not; not one of its own, nor one the other has not"
	'("(5) capture: visit in the function given to (walk) is a parameter of (walk), it is that one it has"
		"(9) capture: name in the function given to (walk) is a parameter of (walk), it is that one it has")
	(tt-check "capture" "(defun walk (name visit) (visit name))" "(defun fold (name step)" "	(walk name (lambda (x) (step x))))"
		"(defun fold2 (name visit)" "	(walk name (lambda (x) (visit x))))" "(defun fold3 (thing step)"
		"	(walk thing (lambda (name) (defq visit 1) (list name visit step))))" "(defun fold4 (thing)"
		"	(walk thing (# (cat %0 name))))" "(defun fold5 (thing)" "	(walk thing (lambda (x) (list x))))"))
(assert-list-eq "held: a stdio made and not kept in a name"
	'("(1) held: a (create-stdio) that is not kept in a name is let go of at once, (defq stdio (create-stdio))"
		"(4) held: a (create-stdio) that is not kept in a name is let go of at once, (defq stdio (create-stdio))")
	(tt-check "held" "(when (create-stdio) (print 1))" "(when (defq stdio (create-stdio)) (print 1))"
		"(when (and (defq stdio (create-stdio)) (defq args 1)) 1)" "(if (and x (create-stdio)) 1)"))
(assert-list-eq "stray: a name at the top of a file that is in no form; not a quoted one, nor a form, a number or a str"
	'("(2) stray: a name by itself at the top of the file, from, was a form closed before its end ?"
		"(3) stray: a name by itself at the top of the file, to, was a form closed before its end ?")
	(tt-check "stray" "(defq has (map f has))" "	from (max 0 a)" "	to (min b c))" "(defq x 1)" "'quoted" "42" "{a str}"))
(assert-list-eq "nothing is said of a line with a comment that begins ;check off, nor of the line after one"
	'("(3) style: a (cond) of one clause")
	(tt-check "style" "(cond ((a) b)) ;check off, tried on purpose" "(cond ((a) b))" "(cond ((a) b))"))
(assert-list-eq "all the checks are run when none are named"
	'("(1) progn: a (progn) round the else of an (if)" "(2) zero: 0.000001 is too small for a fixed, it is 0")
	(progn (save (cat "(if a b (progn c))" tt_lf "(n2r 0.000001)" tt_lf) tt_file)
		(defq tt_said (list))
		(pipe-run (cat "check " tt_file) (# (push tt_said %0)))
		(map (# (slice %0 (inc (length tt_file)) -1)) (filter (const nempty?) (split (join tt_said "") tt_lf)))))
(pipe-run (cat "rm " tt_file) (lambda (&) :nil))
