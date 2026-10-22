(import "lib/text/tree.inc")

(report-header "Tree: source nested by its brackets, comments kept")

(defun tt-read (text) (tree-read (string-stream text)))

(bind '(tt_nodes tt_open &ignore) (tt-read (cat
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
(bind '(tt_nodes tt_open &ignore) (tt-read (cat "(cat {one (" (ascii-char 10) "} x {two" (ascii-char 10) "})" (ascii-char 10) "(b)")))
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

;and the whole tree is clean under it. A fault it knows is found the day
;it is written, not the day it is run into: a (# ) with no %0 given to an
;(each) was written three times in a day, and found three times by running
;it, when check had known of it all along. Not what tests leave in
;tests/scratch, some of that is wrong on purpose
(defq tt_said (list))
(pipe-run "files | check -j 40" (# (push tt_said %0)))
(assert-list-eq "check finds nothing wrong in any source file of the tree" '()
	;nor the file this test makes further down, wrong on purpose, which a
	;run that was stopped part way leaves behind
	(filter (# (and (nempty? %0) (not (starts-with "tests/scratch/" %0)) (not (starts-with "./tests/scratch/" %0))
			(not (starts-with "tmp_check_test.lisp" %0))))
		(split (join tt_said "") (ascii-char 10))))

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
	'("(5) capture: visit in the function given to (walk) is a name of (walk), it is that one it has"
		"(9) capture: name in the function given to (walk) is a name of (walk), it is that one it has")
	(tt-check "capture" "(defun walk (name visit) (visit name))" "(defun fold (name step)" "	(walk name (lambda (x) (step x))))"
		"(defun fold2 (name visit)" "	(walk name (lambda (x) (visit x))))" "(defun fold3 (thing step)"
		"	(walk thing (lambda (name) (defq visit 1) (list name visit step))))" "(defun fold4 (thing)"
		"	(walk thing (# (cat %0 name))))" "(defun fold5 (thing)" "	(walk thing (lambda (x) (list x))))"))
(assert-list-eq "capture: and a name the other makes with a (defq) of its own; not one made in a function inside it"
	'("(4) capture: said in the function given to (runner) is a name of (runner), it is that one it has")
	(tt-check "capture" "(defun runner (name say)" "	(defq said (list)) (each (lambda (x) (defq inner 1) x) said) (say 1))"
		"(defun user1 ()" "	(runner 1 (lambda (line) (push said line))))" "(defun user2 ()" "	(runner 1 (lambda (line) (list inner line))))"))
(assert-list-eq "held: a stdio made and not kept in a name"
	'("(1) held: a (create-stdio) that is not kept in a name is let go of at once, (defq stdio (create-stdio))"
		"(4) held: a (create-stdio) that is not kept in a name is let go of at once, (defq stdio (create-stdio))")
	(tt-check "held" "(when (create-stdio) (print 1))" "(when (defq stdio (create-stdio)) (print 1))"
		"(when (and (defq stdio (create-stdio)) (defq args 1)) 1)" "(if (and x (create-stdio)) 1)"))
(assert-list-eq "cut: a (str ) cut to a number written there; not to a length worked out, nor from other than 0"
	'("(1) cut: a (str ) cut to 200 is an error if it is shorter, (min 200 (length s))")
	(tt-check "cut" "(prin (slice (str _) 0 200))" "(slice (defq t (str _)) 0 (min 200 (length t)))"
		"(slice (str x) 2 5)" "(slice s 0 8)"))
(assert-list-eq "width: (char) of a width written there of more than 8, or 0; not one of 1 to 8, nor a name"
	'("(1) width: (char x 32) makes no more than 8 bytes, of 1 to 8" "(2) width: (char x 0) makes no more than 8 bytes, of 1 to 8")
	(tt-check "width" "(defq z (char 0 32) a (char 0 8))" "(char 1 0)" "(char 0 +long_size)" "(char c)"))
(assert-list-eq "made: a + name given a list made as it runs; not a quoted one, nor a number, nor a name of *stars*"
	'("(1) made: +scenes is a list made by (map), put in as a form where it is used, make it a *name*")
	(tt-check "made" "(defq +scenes (map (const first) (scene-list)) +size 4)" "(defq +ok ''(1 2) *scenes* (map f l) +quoted (list quote l) +name (cat a b))"))
(assert-list-eq "inner: (const f) of a function made inside another; not of one at the top"
	'("(3) inner: pass is made inside a function, it is not there as the file is read, (const) has nothing to take")
	(tt-check "inner" "(defun outer ()" "	(defun pass (x) x)" "	(each (const pass) l))" "(defun top (x) x)" "(each (const top) l)"))
(assert-list-eq "eqlist: eql or nql of a quoted list with a str or a list in it; not one of numbers or symbols, an empty one, nor a symbol"
	'("(1) eqlist: (eql) of a quoted list with a str or a list in it, those are never the same objects as another's, compare (str ) of each"
		"(2) eqlist: (nql) of a quoted list with a str or a list in it, those are never the same objects as another's, compare (str ) of each")
	(tt-check "eqlist" "(eql got '(({monday} 2)))" "(nql got '({a}))" "(eql got '(2 0))" "(eql got '(a :b))" "(eql got '())" "(eql got 'a)"))
(assert-list-eq "index: (elem-get x (!)) in an iterator's function; not of one it makes, nor one whose length it looks at, nor (inc (!)), and of a nested one its own"
	'("(1) index: (elem-get bs (!)) in the function of (each), give bs to it beside the others, (each (lambda (... e) ...) ... bs)"
		"(5) index: (elem-get bs (!)) in the function of (each), give bs to it beside the others, (each (lambda (... e) ...) ... bs)")
	(tt-check "index" "(each (lambda (a) (print (elem-get bs (!)))) as)" "(each (lambda (a) (defq ev (g a)) (elem-get ev (!))) as)"
		"(each (# (and (< (!) (length bs)) (elem-get bs (!)))) as)" "(map (# (+ %0 (elem-get bs (inc (!))))) as)"
		"(each (# (each (# (elem-get bs (!))) cs)) as)"))
(assert-list-eq "collect: (defq out (list)) then an (each) to (push out x); not to a list with things in it, nor a push of two"
	'("(3) collect: (defq out (list)) then (each) to (push out x), it is (defq out (map ... as))")
	(tt-check "collect" "(defun f (as)" "	(defq out (list))" "	(each (# (push out (g %0))) as)" "	(defq todo (list 1))"
		"	(each (# (push todo %0)) as)" "	(defq o2 (list))" "	(each (# (push o2 %0 %0)) as))"))
;a .vp file of its own, the names of the others' (defq)s are what it is checked against
(defq tt_vp "tmp_check_test.vp" tt_said (list))
(save (cat "(defq +ae_ctr 0 +tt_own 8)" tt_lf) tt_vp)
(pipe-run (cat "check -c vpname " tt_vp) (# (push tt_said %0)))
(pii-remove tt_vp)
(assert-list-eq "vpname: a + name of a .vp file's (defq) another .vp file's (defq) gives too; not one of its own alone"
	'("(1) vpname: +ae_ctr is given by a (defq) in lib/crypto/lisp.vp as well: read by the one assembler task the second is of the first's value, give it a prefix of this file's own")
	(map (# (slice %0 (inc (length tt_vp)) -1)) (filter (const nempty?) (split (join tt_said "") tt_lf))))
(assert-list-eq "early: a + constant that is a quoted list used in the (defq) that makes it, in a function there too; not in a later (defq), nor a number"
	'("(1) early: +tt is used in the (defq) that makes it, where it is not yet a constant, use it in a later (defq)"
		"(2) early: +tt is used in the (defq) that makes it, where it is not yet a constant, use it in a later (defq)")
	(tt-check "early" "(defq +tt ''((1) (2) (3)) m (map (lambda (x) :nil) +tt)" "	f (lambda () (length +tt)))"
		"(defq +ok ''((1) (2)) n 5)" "(defq m2 (length +ok) +num 5 k (* +num 2))"))
(assert-list-eq "args: a (# ) with no %0 given to what calls it with something, an inner (# )'s %0 not its; not one with a %0, nor a lambda, nor one with a part put in, nor (each!) of no sequences; the :each of a map too"
	'("(1) args: a (# ) with no %0 in it takes nothing, and (map) calls it with something, (lambda (x) ...)"
		"(2) args: a (# ) with no %0 in it takes nothing, and (. :each) calls it with something, (lambda (x) ...)"
		"(3) args: a (# ) with no %0 in it takes nothing, and (each!) calls it with something, (lambda (x) ...)"
		"(4) args: a (# ) with no %0 in it takes nothing, and (sort) calls it with something, (lambda (x) ...)"
		"(9) args: a (# ) with no %0 in it takes nothing, and (filter) calls it with something, (lambda (x) ...)")
	(tt-check "args" "(map (# (<< 1 (!))) slots)" "(. m :each (# (print 1)))" "(each! (# (f)) (list a b))" "(sort l (# 0))"
		"(map (# (inc %0)) slots)" "(map (lambda (x) 1) slots)" "(defmacro m (p) `(each (# (f ,p)) s))"
		"(each! (# (f)) '() 0 9)" "(filter (# (g (# %0))) l)" "(f (# (g)))"))
(assert-list-eq "args: a (# ) of so many %n given to what calls it with another number: (reduce) two, (map) one a sequence, (each!) one a sequence of its list, %1 alone one; not when it matches, nor an inner (# )'s %n, nor an :each, a map's two and a set's one, nor (filter!)"
	'("(1) args: a (# ) of 1 %n takes 1, and (reduce) calls it with 2, (lambda (x y) ...)"
		"(2) args: a (# ) of 2 %n takes 2, and (map) calls it with 1, (lambda (x) ...)"
		"(3) args: a (# ) of 1 %n takes 1, and (each!) calls it with 2, (lambda (x y) ...)"
		"(10) args: a (# ) of 1 %n takes 1, and (reduce) calls it with 2, (lambda (x y) ...)")
	(tt-check "args" "(reduce (# (push %0 1)) l (list))" "(map (# (+ %0 %1)) a)" "(each! (# (g %0)) (list a b) 0 3)"
		"(reduce (# (push %0 %1)) l (list))" "(map (# (+ %0 %1)) a b)" "(sort l (# (- %0 %1)))"
		"(map (# (reduce (# (+ %0 %1)) %0 0)) l)" "(. s :each (# (h %0)))" "(filter! (# (> %0 2)) (list 1 2 3 4))"
		"(reduce (# (push out %1)) l (list))" "(map (# (f %1)) a)"))
;&ignore ends a list of names, in a list inside one too; a list of
;parameters whose first name is a macro is a call of it; () given to a
;function is run, and calls nothing. Not () given to a macro, nor a macro's
;name later in a list, nor (lambda () ...)
(assert-list-eq "ignore, param and empty: in a list in the parameters too, a macro first, () to a function"
	'("(1) ignore: names after &ignore are never bound, it ends the list; & takes one thing and binds nothing"
		"(2) param: bits first in a list of parameters is the name of a macro of the system, the list is taken as a call of it"
		"(3) empty: () given to (list) is run, and calls nothing: (list) or '()")
	(tt-check "ignore,param,empty" "(each (lambda (k (a &ignore b)) b) x)" "(defun f (bits) bits)" "(list () x)"
		"(lambda (x (y z when)) z)" "(ui-window w ())" "(lambda () 1)" "(list (list) '())"))
;a call of what nothing names: not of a function of the file, made later
;in it, nor a parameter that is a function, nor a name a macro makes, nor
;a clause of a cond or a binding of a let, which are no calls
(assert-list-eq "unbound: a call of nothing, in a cond and a let too; not what the file, a parameter or a macro names"
	'("(1) unbound: no-such is called, and is no function or macro of the system, of this file, or of what it imports"
		"(3) unbound: nor-this is called, and is no function or macro of the system, of this file, or of what it imports"
		"(4) unbound: nor-that is called, and is no function or macro of the system, of this file, or of what it imports")
	(tt-check "unbound" "(defun f (x fn) (no-such x) (later x) (fn x))" "(defun later (x) x)"
		"(defun g (x) (cond (x (nor-this x)) ((> x 1) 2)))" "(defun h (x) (let ((a 1)) (nor-that a)))"
		"(defmacro gen (n) `(defun ,(sym (cat \qmade-\q n)) () 1))" "(defun k () (made-one))"))
(assert-list-eq "past: an inc of an rfind, as it is and in an ifn; not an inc of a find"
	'("(1) past: (inc) of an (rfind), which gives the place after what it found already"
		"(2) past: (inc) of an (rfind), which gives the place after what it found already")
	(tt-check "past" "(slice p 0 (inc (rfind \q/\q p)))" "(slice p 0 (inc (ifn (rfind \q/\q p) -1)))" "(inc (find x s))"))
(assert-list-eq "shift: a logical shift of a difference; not a signed one, nor of a sum"
	'("(1) shift: (>>) of a difference, a logical shift: one less than 0 is made large, (>>>) shifts it signed")
	(tt-check "shift" "(>> (- psd m) 5)" "(>>> (- psd m) 5)" "(>> (+ a b) 1)"))
;(relies), where a function goes by more than is declared of what it calls:
;a function made up, as an assembled one is left. It calls :sys_mem :alloc,
;which takes :r0, gives :r0 and :r1 and is declared to trash :r0 to :r2,
;and then reads :r1, which it was given, :r2, which it was not, and :r3,
;which is not trashed
(defq tt_probe "obj/vp/debug/tests/scratch_relies_probe")
(save (cat "(progn" tt_lf "(emit-label '_0)" tt_lf "(emit-long -1)" tt_lf "(emit-short _9 _1 _6 _8 4096)" tt_lf
	"(emit-label '_1)" tt_lf "(emit-cpy-cr 8 :r0)" tt_lf "(emit-cpy-cr 5 :r2)" tt_lf "(emit-cpy-cr 6 :r3)" tt_lf
	"(emit-call-p _7)" tt_lf "(emit-add-rr :r1 :r0)" tt_lf "(emit-add-rr :r2 :r0)" tt_lf "(emit-add-rr :r3 :r0)" tt_lf
	"(emit-xor-rr :r4 :r4)" tt_lf "(emit-ret)" tt_lf
	"(emit-label '_6)" tt_lf "(emit-label '_7)" tt_lf "(emit-long (- _8 *pc*))" tt_lf
	"(emit-label '_8)" tt_lf "(emit-string \qsys/mem/alloc\q)" tt_lf "(emit-byte 0)" tt_lf "(emit-label '_9))" tt_lf) tt_probe)
(defq tt_said (list))
(pipe-run "relies -v tests/scratch_relies_probe" (# (push tt_said %0)))
;if it is not said, what was said is what the failure shows: it failed
;the once on the Pi, in a run of every test, and what it said was not kept
(assert-eq "relies: a register read after a call declared to trash it and not give it is said; not one it gives, one it leaves, nor one made 0 by itself"
	"said"
	(if (or (found? (join tt_said "") "tests/scratch_relies_probe (4): after sys/mem/alloc, :r2")
			;the classes are read under a lock, which another test may hold too long
			(found? (join tt_said "") "The classes could not be read"))
		"said" (cat "not said: " (join tt_said " | "))))
(pii-remove tt_probe)
;(relies -s), the other way round: a register put by round a call that
;leaves it. A function made up that pushes :r1 and :r3 round :sys_mem
;:alloc, which trashes :r0 to :r2 and gives :r0 and :r1: :r3 is put by
;for nothing, :r1 is not
(defq tt_probe "obj/vp/debug/tests/scratch_saves_probe")
(save (cat "(progn" tt_lf "(emit-label '_0)" tt_lf "(emit-long -1)" tt_lf "(emit-short _9 _1 _6 _8 4096)" tt_lf
	"(emit-label '_1)" tt_lf "(emit-cpy-cr 8 :r0)" tt_lf "(emit-push :r1 :r3)" tt_lf
	"(emit-call-p _7)" tt_lf "(emit-pop :r1 :r3)" tt_lf "(emit-add-rr :r3 :r0)" tt_lf "(emit-ret)" tt_lf
	"(emit-label '_6)" tt_lf "(emit-label '_7)" tt_lf "(emit-long (- _8 *pc*))" tt_lf
	"(emit-label '_8)" tt_lf "(emit-string \qsys/mem/alloc\q)" tt_lf "(emit-byte 0)" tt_lf "(emit-label '_9))" tt_lf) tt_probe)
(defq tt_said (list))
(pipe-run "relies -s -q tests/scratch_saves_probe" (# (push tt_said %0)))
(assert-eq "relies -s: a register pushed round a call that leaves it is said, and not one the call gives"
	"said"
	(if (or (and (found? (join tt_said "") "round sys/mem/alloc, :r3, put by") (not (found? (join tt_said "") ":r1")))
			(found? (join tt_said "") "The classes could not be read"))
		"said" (cat "not said: " (join tt_said " | "))))
(pii-remove tt_probe)
(assert-list-eq "bracket: more opened than closed, and more closed than opened; not one in a str or a comment"
	'("(1) bracket: 1 not closed by the end of the file")
	(tt-check "bracket" "(defq a (list 1 2)" "{a ) str} ;a ) comment"))
(assert-list-eq "bracket: one that closes nothing"
	'("(1) bracket: 2 that close nothing")
	(tt-check "bracket" "(defq a 1))" "(defq b 2))"))
(assert-list-eq "bracket: a file that is whole, nothing"
	'()
	(tt-check "bracket" "(defq a (list 1 2))" "(defun f (x)" "	(+ x 1))"))
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

;(locals), what a function's named locals cost: a function made up for it,
;as an assembled one is left in obj/vp/debug/, with a frame of three, a, b and
;a struct s. a is read before the call, b after it, s has its address
;taken, and the last write of a is of nothing that is read. Kept in
;registers no call keeps: the read of a is of what was just written, and
;only b has to be written before the call
(defq tt_probe "obj/vp/debug/tests/scratch_locals_probe")
(save (cat "(progn" tt_lf "(emit-label '_0)" tt_lf "(emit-long -1)" tt_lf "(emit-short _9 _1 _8 _9 4096)" tt_lf
	"(emit-label '_1)" tt_lf "(emit-alloc 32)" tt_lf "(emit-cpy-ri :r0 :rsp 0)" tt_lf "(emit-cpy-ri :r1 :rsp 8)" tt_lf
	"(emit-cpy-ir :rsp 0 :r0)" tt_lf "(emit-call-p _7)" tt_lf "(emit-cpy-ir :rsp 8 :r1)" tt_lf
	"(emit-lea-i :rsp 16 :r2)" tt_lf "(emit-cpy-ri :r1 :rsp 24)" tt_lf "(emit-beq-cr 0 :r1 _2)" tt_lf
	"(emit-cpy-ri :r1 :rsp 0)" tt_lf "(emit-label '_2)" tt_lf "(emit-free 32)" tt_lf "(emit-ret)" tt_lf
	"(emit-label '_8)" tt_lf "(emit-label '_9))" tt_lf
	"(locals (1 32 ((a 0 \qp\q) (b 8 \qp\q) (s 16 \q\q))))" tt_lf) tt_probe)
(defq tt_said (list))
(pipe-run "locals -v -t 0 tests/scratch_locals_probe" (# (push tt_said %0)))
(assert-eq "locals: the frame, its named locals, what is read and written, what could be registers, what is wanted after a call, and what is in memory"
	"tests/scratch_locals_probe: frames 32 bytes, named locals 3, read 2 written 4, could be registers: read 2 written 3, of those wanted after a call: read 1 written 1, 1 are in memory, a struct or their address taken, in registers a call does not keep: 1 reads less, 1 writes before calls, 2 wanted at once, 12 registers it does not use, calls 1, that say what they trash 0"
	(first (split (join tt_said "") tt_lf)))
(defq tt_said (list))
(pipe-run "locals -t 0 -c 3 tests/scratch_locals_probe" (# (push tt_said %0)))
(assert-true "locals -c: what is called and what it costs is said, after the sums"
	(found? (join tt_said "") "What is called, and what it costs those that call it"))
;if the classes can not be read, another has the lock of them too long, it says that and not nothing
(pii-remove tt_probe)
