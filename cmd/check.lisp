(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/text/tree.inc")
(import "service/lock/app.inc")

(defq usage `(
(("-h" "--help")
"Usage: check [options] [path] ...

    options:
        -h --help: this help info.
        -j --jobs num: max jobs per batch, default 1.
        -c --checks names: only these checks, a , between
            them, default all of them.

    Scan source files for what can be seen to be wrong,
    or better said another way, from the source alone.
    Each file is read as a tree, lib/text/tree.inc, so
    what is in a comment or a string is not looked at.
    Each thing found is a line: the file, the line, the
    check and what it found. Nothing is said of a line
    that has, or comes after, a comment that begins
    ;check off

    progn   a (progn) where there is one already, round
            the else forms of an (if) or an (ifn), or as
            the whole body of a (when), (unless), (while)
            or (until).
    class   a class of characters, written as a string,
            that is not in order: (bskip), (bfind), (trim)
            and (split) look one up by halves, and find
            only some of it. (char-class) puts it in order.
    zero    a number written with a point that is too
            small for a fixed to hold, and so is 0.
    shadow  a variable with the name of a function or a
            macro of the system, as a test of an (and), an
            (or) or a clause of a (cond): there it is taken
            as the function, and is always true.
    const   a + constant given a list of strings or numbers
            quoted once: it is run as a form where the
            constant is used, and is no form.
    made    a + constant given a list made as it runs, by
            (map), (filter), (list) or the like: a + name
            is put in where it is used, as the file is read,
            and a list put in is run as a form there. Make
            it a name of *stars*.
    inner   (const f) of a function made inside another: it
            is not there yet as the file is read, and the
            (const) has nothing to take.
    eqlist  (eql x '(...)) of a quoted list with a str or a
            list in it: two lists
            are eql if their elements are the same objects in
            the same order. A number or a symbol is, a str or
            a list in a quoted list is its own and is never the
            same object as one made elsewhere. Compare what
            they print as.
    again   a (defun) inside another, that uses a name the
            other made with a (defq) before it, and a (defq)
            of that name again after it, in the other: the
            (defq) is to the one name, in the other's
            environment, and what the inner function had by
            it is gone, from then on.
    unquote ,f, ,(const f), ,(f ...) or ~(f ...) in a
            backquote in a module,
            f a function of the module not exported: it is
            looked for as the form is made, when the
            function runs, and the module's names are gone.
    ignore  a name after &ignore in a (bind) list or a list
            of parameters, or a list in one: &ignore ends
            the list, and the name is never bound. & is the
            place that takes a thing and binds nothing.
    param   a list of parameters, or a list in one, whose
            first name is a macro of the system, (defun f
            (bits) ...): it is expanded as a call of it.
    empty   () given to a call, (list () x): it is run as
            a form, and calls nothing. (list) or '().
    past    (inc) of an (rfind), which gives the place after
            what it found already: one more is past it.
    shift   (>> (- a b) n), a logical shift of a difference:
            one less than 0 is made a large number, the place
            in a table of an AC-3 decoder 15 bits not 0.
            (>>>) shifts it as a signed number.
    unbound a call of what is no function or macro of the
            system, nor named by the file or anything it
            imports, at any depth: a program, or a module.
            What runs it may give it more, the tests their
            suite, the diagrams make's; the assembler, which
            runs in an env of many files, is not looked at.
    early   a + constant that is a quoted list, used in the
            (defq) that makes it: there it is not yet a
            constant, and is the quote and the list, two things.
    copy    (each f (slice a s e) (slice b s e)) or the like,
            every sequence given to (each), (map), (some),
            (every), (notany), (notevery), (filter) or a
            (reduce) with its init, a (slice), a (rest) or a
            (most) of the same part: each is copied to be
            walked once. The ! form walks that part of the
            one it is given, (each! f (list a b) s e), and (!)
            in it is then the index in the whole. A (reverse)
            is the part from the end, -1 0, and (reach),
            (rmap), (rsome), (rreduce) and (rfilter) walk from
            the end. A (rest) or a
            (most) of an empty one is (), where the ! form
            from 1, or to -2, is an error: if it can be empty
            it is not the same.
    reverse (reverse (slice x s e)), two copies, is (slice x e s),
            one. Of a (rest) or a (most), (slice x -1 1) or
            (slice x -2 0), which of an empty one is an error.
    width   (char x n) of a number n written there that is
            not 1 to 8: (char) makes no more than 8 bytes,
            (char 0 32) is 8 of them, not 32.
    cut     (slice (str x) 0 n), a string of something cut to
            a number written there: one that is shorter than
            that is an error, (min n (length s)).
    style   (if tst (progn ...)) with no else, a (when);
            (if (not tst) form else ...), an (ifn), and
            with no else an (unless); the same of an (ifn),
            the other way about; a (cond) or a (condn) of
            one clause; and (when (and ...) form), or an
            (if) of one with no else, the (and) with the
            form as the last of it.
    doc     a ; (name args) -> result comment under a
            (defun) or (defmacro) that is of another name,
            or of more or fewer things than it takes.
    enum    a field of an (enums) named size: the name it
            makes is the one the (enums) makes for how many
            fields there are.
    bracket brackets that do not pair, more opened than
            closed or more closed than opened. The file does
            not load.
    stray   a name by itself at the top of a file, in no form.
            A bracket closed a form before its end, and the
            rest of it is run by itself.
    args    a (# ...) with no %0 in it, which takes nothing,
            given to (map), (each), (filter), (some), (reduce),
            (sort), (lines!) or the like, or the :each of a
            map or a set, which call it with an element.
    global  a *name* made at the top of a module, not
            exported, used in a function of it: it is looked
            for when the function runs, and is gone by then.
    self    in the method :m of a class, a (call :c :m '(:r0
            ...)) of the same method of this class or one
            above it, that is not static: a call goes by the
            vtable of the object, and of one of this class
            that is this method again, for ever. s-call for
            the one above, d-call for a class's own.
    held    a (create-stdio) that is not kept in a name, by a
            (defq). It is let go of as soon as it has been
            looked at, its streams say they are stopping, and
            what is written after may not be read.
    struct  in a module, a file with an (env-push), a field
            of a structure, in (getf), (setf) or (setf->),
            that no (structure), (enums) or (defq) gives, of
            this file, of what it imports, and of what those
            import, nor of what root.inc imports. A module that
            works only because what loaded it first had
            imported the structure fails on its own, and on a
            release build its class is not made at all. And
            in any file, a (structure +x) where root.inc's
            imports have a +x already.
    capture a function written where it is given to another
            of this file, or of a file it imports, that uses
            a name the other has as a parameter, or makes
            with a (defq). A name is found where a function
            is called from, so it is the other's that is had.

    A path that is a folder is the source files in it, at
    any depth. If no paths given on command line then will
    test files from stdin.")
(("-j" "--jobs") ,(opt-num 'opt_j))
(("-c" "--checks") ,(opt-str 'opt_c))
))

(defq +check_names ''("global" "progn" "class" "zero" "shadow" "const" "style" "doc" "enum" "capture" "held" "stray" "bracket" "args" "early" "cut" "made" "inner" "eqlist" "again" "unquote" "ignore" "self" "copy" "reverse" "width" "struct" "param" "empty" "past" "shift" "unbound")
	+char_class_quote (char-class "'`,~") +char_class_nonzero (char-class "123456789"))

;where a class of characters is, of what a function is given, from 1
(defq *class_at* (scatter (Fmap 11) "bskip" 1 "bskipn" 1 "rbskip" 1 "rbskipn" 1
	"bfind" 2 "split" 2 "trim" 2 "trim-start" 2 "trim-end" 2))

(defun found (file node name &rest text)
	;kept till the file has been gone through, what is not to be said of
	;a line may be told after the thing on it is found
	(push *found* (list (second node) (cat file " (" (str (second node)) ") " name ": " (apply (const cat) text)))))

(defun forms-of (kids)
	; (forms-of kids) -> forms
	;what is in a form that is not a comment or an empty line. A quote
	;before a thing, ' ` , or ~, is a thing of its own in the tree: it is
	;left out, and the thing after it is (:quoted line node), not a form
	;that is run
	(defq forms (list) quoted :nil)
	(each (lambda (kid)
		(cond
			((find (first kid) '(:comment :blank)))
			((and (eql (first kid) :symbol) (= (bskip +char_class_quote (third kid) 0) (length (third kid))))
				(setq quoted :t))
			(quoted (push forms (list :quoted (second kid) kid)) (setq quoted :nil))
			((push forms kid))))
		kids)
	forms)

(defun check-progn (file forms head)
	(cond
		;(if tst form else ...), the first of the else forms
		((and (find head '("if" "ifn")) (> (length forms) 3)
				(defq at (some! (# (if (tree-form? %0 "progn") %0)) (list forms) :nil 3 -1)))
			(found file at "progn" "a (progn) round the else of an (" head ")"))
		;(when tst body ...), a body that is one (progn)
		((and (find head '("when" "unless" "while" "until")) (= (length forms) 3)
				(tree-form? (third forms) "progn"))
			(found file (third forms) "progn" "a (progn) is all the body of a (" head ")"))))

(defun check-class (file forms head)
	(when (and (defq at (. *class_at* :find head)) (> (length forms) at)
			(eql (first (defq arg (elem-get forms at))) :string1))
		;the string as it is read, a \t in it a tab
		(defq cls (catch (first (read (string-stream (third arg)))) :t))
		(and (str? cls) (> (length cls) 1) (nql cls (char-class cls))
			(found file arg "class" (third arg) " given to (" head ") is not in order, (char-class " (third arg) ")"))))

(defun check-zero (file node)
	;a number with a sign before it is read as a symbol by the tree
	(and (find (first node) '(:number :symbol)) (nempty? (defq text (third node)))
			(defq digits (if (find (first text) "+-") (rest text) text))
			(nempty? digits) (find (first digits) "0123456789.")
			(find "." digits) (not (find "x" digits)) (not (find "e" digits))
			(/= (bskipn +char_class_nonzero digits 0) (length digits))
			(num? (defq val (catch (str-to-num digits) :t))) (= val 0)
		(found file node "zero" text " is too small for a fixed, it is 0")))

(defun check-shadow (file forms head)
	;(and a b) is (condn (a) (b)), and a clause of a (cond) is a list
	;with its test first: a list whose first thing is the name of a
	;function is taken as a call of it, the name is bound to the function
	;as the code is read, and the variable is never looked at
	(each (lambda (node)
		(and (eql (first node) :symbol) (. *root_names* :find (third node))
			(found file node "shadow" (third node) " as a test of (" head ") is taken as the function of the system, not the variable")))
		(cond
			((find head '("and" "or")) (rest forms))
			((find head '("cond" "condn"))
				(map (# (first (forms-of (third %0)))) (filter! (# (and (eql (first %0) :form) (nempty? (forms-of (third %0))))) forms 1 -1)))
			((list)))))

(defun check-const (file kids)
	;in a (defq), a + name, a ' and nothing but, and a list
	(defq kids (filter (# (not (find (first %0) '(:comment :blank)))) kids))
	(each (lambda (name quote lst)
		(and (eql (first name) :symbol) (starts-with "+" (third name))
				(eql (first quote) :symbol) (eql (third quote) "'") (eql (first lst) :form)
				;one that begins with a string or a number is no form to run,
				;one that begins with a name may be meant as one
				(nempty? (defq in (forms-of (third lst))))
				(find (first (first in)) '(:string1 :string2 :number))
			(found file name "const" (third name) " is a list quoted once, ''(...)")))
		kids (rest kids) (rest (rest kids))))

(defun check-early (file kids)
	;in a (defq), a + name, a quote or two, and a list, then that name in
	;what is after it in the same (defq). The name is made a constant by
	;the (defq), so till that is done it is looked for as a variable is,
	;and what is found is (quote (...))
	(defq kids (filter (# (not (find (first %0) '(:comment :blank)))) kids))
	(each (lambda (name)
		(defq at (inc (!)))
		(when (and (eql (first name) :symbol) (starts-with "+" (third name)) (< at (length kids))
				(eql (first (defq quote (elem-get kids at))) :symbol)
				(= (bskip +char_class_quote (third quote) 0) (length (third quote))))
			;past the quotes, to the list
			(while (and (< at (length kids)) (eql (first (defq lst (elem-get kids at))) :symbol)
					(= (bskip +char_class_quote (third lst) 0) (length (third lst))))
				(++ at))
			(when (and (< at (length kids)) (eql (first (elem-get kids at)) :form))
				(defq todo (list (slice kids (inc at) -1)))
				(while (defq nodes (pop todo))
					(each (lambda (node)
						(cond
							((eql (first node) :form) (push todo (third node)))
							((and (eql (first node) :symbol) (eql (third node) (third name)))
								(found file node "early" (third name) " is used in the (defq) that makes it, where it is not yet a constant, use it in a later (defq)"))))
						nodes)))))
		kids))

(defun check-eqlist (file forms head)
	;(eql x '(("a" 1))): the elements of lists are compared as objects, and
	;a str or a list in a quoted list is one of its own. Chris: "list
	;compares equal if all the members of the list are the same objects in
	;the same order", "That's because it just runs :array :eql !"
	(and (find head '("eql" "nql")) (= (length forms) 3)
		(some! (lambda (arg)
				(and (eql (first arg) :quoted) (defq inner (tree-form? (third arg)))
					(some (# (find (first %0) '(:form :string1 :string2))) (forms-of inner)))) (list forms) :nil 1 -1)
		(found file (second forms) "eqlist" "(" head ") of a quoted list with a str or a list in it, those are never the same objects as another's, compare (str ) of each")))

(defun check-ignore (file forms head)
	;(bind '(a &ignore b) x), (lambda (a &ignore b)): &ignore ends the list,
	;b is never bound. & is the place that takes a thing and binds nothing
	(defq names (cond
		((and (eql head "bind") (> (length forms) 1) (eql (first (second forms)) :quoted))
			(tree-form? (third (second forms))))
		((and (find head '("defun" "defmacro")) (> (length forms) 2)) (tree-form? (third forms)))
		((and (eql head "lambda") (> (length forms) 1)) (tree-form? (second forms)))))
	;and each list in it, (lambda (k (a &ignore b)) ...)
	(defq todo (if names (list names) (list)))
	(while (defq names (pop todo))
		(setq names (forms-of names))
		(each (# (if (defq inner (tree-form? %0)) (push todo inner))) names)
		(and (defq at (some (# (and (eql (first %0) :symbol) (eql (third %0) "&ignore") (!))) names))
			(< (inc at) (length names))
			(found file (elem-get names at) "ignore" "names after &ignore are never bound, it ends the list; & takes one thing and binds nothing"))))

(defun check-param (file forms head)
	;(defun f (bits) ...): a list of parameters is expanded as any list is,
	;and one whose first name is a macro of the system is a call of it
	(defq names (cond
		((and (find head '("defun" "defmacro")) (> (length forms) 2)) (third forms))
		((and (eql head "lambda") (> (length forms) 1)) (second forms))))
	(defq todo (if (and names (tree-form? names)) (list (tree-form? names)) (list)))
	(while (defq kids (pop todo))
		(defq inner (forms-of kids))
		(each (# (if (defq more (tree-form? %0)) (push todo more))) inner)
		(and (nempty? inner) (eql (first (first inner)) :symbol)
			(. *root_macros* :find (third (first inner)))
			(found file (first inner) "param" (third (first inner))
				" first in a list of parameters is the name of a macro of the system, the list is taken as a call of it"))))

(defun check-past (file forms head)
	;(inc (rfind ...)): (rfind) gives the place after what it found, one
	;more is past it, (slice path 0 (inc (rfind "/" path))) cuts a letter
	;into the folder. And so (inc (ifn (rfind ...) n))
	(when (and (eql head "inc") (= (length forms) 2) (defq inner (tree-form? (second forms))))
		(defq in (forms-of inner))
		(and (nempty? in) (eql (first (first in)) :symbol)
			(or (eql (third (first in)) "rfind")
				(and (eql (third (first in)) "ifn") (> (length in) 1) (tree-form? (second in) "rfind")))
			(found file (second forms) "past" "(inc) of an (rfind), which gives the place after what it found already"))))

(defun check-shift (file forms head)
	;(>> (- a b) n): >> is a logical shift, a difference that is less than
	;0 is made a large number. >>> shifts it as a signed one. Not in a .vp,
	;where (vpx (>> ...)) is the VP's shift of what it is, pointers apart
	(and (eql head ">>") (not (ends-with ".vp" file)) (> (length forms) 1) (defq inner (tree-form? (second forms)))
		(nempty? (defq in (forms-of inner))) (eql (first (first in)) :symbol) (eql (third (first in)) "-")
		(found file (second forms) "shift" "(>>) of a difference, a logical shift: one less than 0 is made large, (>>>) shifts it signed")))

(defun check-empty (file forms head)
	;(f a () b): an empty list as a thing given to a call is run as a form,
	;and there is nothing to call. (list) is an empty list, '() too. Only a
	;function of the system: a macro, or a form, may take one as it is
	(and (. *root_names* :find head) (not (. *root_macros* :find head))
		(not (find head '("lambda" "#" "defun" "defmacro" "defmethod" "macro" "bind" "let" "let*"
			"cond" "condn" "case" "catch" "defq" "setq" "quote" "quasi-quote" "while" "if" "when" "unless")))
		(each! (lambda (arg)
				(and (defq inner (tree-form? arg)) (empty? (forms-of inner))
					(found file arg "empty" "() given to (" head ") is run, and calls nothing: (list) or '()")))
			(list forms) 1 -1)))

(defun check-made (file kids)
	;in a (defq), a + name and then a form that makes a list
	(defq kids (filter (# (not (find (first %0) '(:comment :blank)))) kids))
	(each (lambda (name val)
		(and (eql (first name) :symbol) (starts-with "+" (third name))
			(defq inner (tree-form? val))
			(nempty? (defq inner (forms-of inner))) (eql (first (first inner)) :symbol)
			(find (third (first inner)) '("map" "filter" "list" "split" "range" "partition"))
			;(list quote x) is the quote of a list, which is the way to have one
			(not (and (> (length inner) 1) (eql (first (second inner)) :symbol) (eql (third (second inner)) "quote")))
			(found file name "made" (third name) " is a list made by (" (third (first inner)) "), put in as a form where it is used, make it a *name*")))
		kids (rest kids)))

(defun check-inner (file forms head)
	;(const f), f a function this file makes inside another
	(and (eql head "const") (= (length forms) 2) (eql (first (second forms)) :symbol)
		(. *inner_defuns* :find (third (second forms)))
		(found file (second forms) "inner" (third (second forms))
			" is made inside a function, it is not there as the file is read, (const) has nothing to take")))

(defun params-of (node)
	; (params-of node) -> (name ...)
	;the names a list of parameters gives, those in lists in it too
	(defq out (list) todo (list node))
	(while (defq node (pop todo))
		(cond
			((eql (first node) :symbol)
				(unless (or (starts-with "&" (third node)) (eql (third node) "_")) (push out (third node))))
			((eql (first node) :form) (each (# (push todo %0)) (third node)))))
	out)

(defun defqs-of (forms)
	; (defqs-of forms) -> (name ...)
	;the names a (defq) makes in these forms, and in the forms in them, but
	;not in a (lambda) or a (#) there, whose names are its own
	(defq out (list) todo (list forms))
	(while (defq nodes (pop todo))
		(each (lambda (node)
			(when (defq kids (tree-form? node))
				(defq inner (forms-of kids)
					head (and (nempty? inner) (eql (first (first inner)) :symbol) (third (first inner))))
				(cond
					((find head '("lambda" "#" "defun" "defmacro" "macro")))
					((eql head "defq")
						(each! (# (and (= (logand (!) 1) 1) (eql (first %0) :symbol) (push out (third %0)))) (list inner) 1 -1)
						(push todo (rest inner)))
					((push todo inner)))))
			nodes))
	out)

(defun uses-of (body)
	; (uses-of nodes) -> (used made)
	;the names used in some forms, not as the first thing of a form,
	;which is a call, and the names made in them: the parameters of a
	;(lambda) there, and what a (defq) or a (bind) names
	(defq used (list) made (list) todo (list body))
	(while (defq nodes (pop todo))
		(defq forms (forms-of nodes)
			head (and (nempty? forms) (eql (first (first forms)) :symbol) (third (first forms))))
		(cond
			((find head '("lambda" "macro"))
				(if (> (length forms) 1) (each (# (push made %0)) (params-of (second forms)))))
			((eql head "defq")
				(each! (# (and (= (logand (!) 1) 1) (eql (first %0) :symbol) (push made (third %0)))) (list forms) 1 -1))
			((and (eql head "bind") (> (length forms) 1) (eql (first (second forms)) :quoted))
				(each (# (push made %0)) (params-of (third (second forms))))))
		(each (lambda (form)
			(cond
				((eql (first form) :form) (push todo (third form)))
				;the first thing of a form is a call. A name of the system
				;there is bound to its function as the code is read, any
				;other is looked for when it is called, as a variable is
				((and (eql (first form) :symbol) (or (> (!) 0) (not head) (not (. *root_names* :find (third form)))))
					(push used (third form)))))
			forms))
	(list used made))

(defun check-again (file forms head)
	;(defun main () (defq kept ...) (defun take () ... kept ...) ... (defq
	;kept ...)): the second is to the name take has, not a new one
	(when (and (eql head "defun") (> (length forms) 3))
		(defq body (slice forms 3 -1))
		(each (lambda (node)
			(when (and (defq kids (tree-form? node "defun")) (> (length (defq inner (forms-of kids))) 3))
				(bind '(used made) (uses-of (slice inner 3 -1)))
				(defq own (cat made (params-of (third inner)))
					before (defqs-of (slice body 0 (!))) after (defqs-of (slice body (inc (!)) -1)) said (list))
				(each (# (and (find %0 before) (find %0 after) (not (find %0 own)) (not (find %0 said))
						(push said %0)
						(found file node "again" "(" (third (second inner)) ") uses " %0
							", which a (defq) after it makes again, it is that one it has from then on")))
					used)))
			body)))

(defun check-global (file region exported)
	;a *name* made by a (defq) at the top of a module and not exported,
	;used in a function of it: a name that is not a function is looked for
	;when the function runs, and the module's names are gone by then
	(defq own (list))
	(each (lambda (k)
			(defq f (forms-of k))
			(and (nempty? f) (eql (first (first f)) :symbol) (eql (third (first f)) "defq")
				(each (lambda (n)
						(and (odd? (!)) (eql (first n) :symbol) (> (length (third n)) 2)
							(starts-with "*" (third n)) (ends-with "*" (third n))
							(not (. exported :find (third n)))
							(push own (third n))))
					f)))
		region)
	(when (nempty? own)
		(each (lambda (k)
				(defq f (forms-of k))
				(when (and (nempty? f) (eql (first (first f)) :symbol) (find (third (first f)) '("defun" "defmacro" "defmethod")))
					(defq todo (list (rest k)))
					(while (defq nodes (pop todo))
						(each (lambda (n)
								(and (eql (first n) :symbol) (find (third n) own)
									(found file n "global" (third n) " of the module, not exported, is looked for when the function runs, and is gone by then"))
								(if (defq sub (tree-form? n)) (push todo sub)))
							nodes))))
			region)))

(defun check-unquote (file top)
	;(env-push) (defun f () ...) (defun g () `(... ,f ...)) (export-symbols
	;'(g)) (env-pop): f is not there when g runs. Of each module of a file,
	;its functions that are not exported, and a , before one of them, or
	;before (const one), anywhere in it
	(defq forms (forms-of top) in :nil region (list) exported (Fset 31) quoted_all :t)
	(each (lambda (node)
		(when (defq kids (tree-form? node))
			(defq inner (forms-of kids)
				head (and (nempty? inner) (eql (first (first inner)) :symbol) (third (first inner))))
			(cond
				((eql head "env-push") (setq in :t region (list) exported (Fset 31) quoted_all :t))
				((and in (eql head "env-pop"))
					(and quoted_all (find "global" checks) (check-global file region exported))
					(when quoted_all
						(defq own (Fset 31))
						(each (lambda (k)
								(defq f (forms-of k))
								(and (> (length f) 1) (eql (first (first f)) :symbol) (eql (third (first f)) "defun")
									(eql (first (second f)) :symbol) (not (. exported :find (third (second f))))
									(. own :insert (third (second f)))))
							region)
						(each (lambda (k)
								(defq todo (list k))
								(while (defq nodes (pop todo))
									(each (lambda (n)
											(defq next (if (< (inc (!)) (length nodes)) (elem-get nodes (inc (!)))))
											;,f is one symbol, , (const f) a , and a form, and so are
											;,(f x) and ~(f x), a call of it as the form is made
											(defq name (cond
												((not (eql (first n) :symbol)) :nil)
												((and (> (length (third n)) 1) (starts-with "," (third n))) (slice (third n) 1 -1))
												((not (and next (find (third n) '("," "~")))) :nil)
												((and (eql (third n) ",") (defq nk (tree-form? next "const")))
													(defq nf (forms-of nk))
													(and (> (length nf) 1) (eql (first (second nf)) :symbol) (third (second nf))))
												((defq nk (tree-form? next))
													(defq nf (forms-of nk))
													(and (nempty? nf) (eql (first (first nf)) :symbol) (third (first nf))))))
											(and name (. own :find name) (find "unquote" checks)
												(found file n "unquote" "," name " in a backquote of the module, not exported, is looked for when the form is made, and is gone by then"))
											;a (static-qq) is made as it is read, while the module is there
											(and (defq sub (tree-form? n)) (not (tree-form? n "static-qq")) (not (tree-form? n "static-qqp"))
												(push todo sub)))
										nodes)))
							region))
					(setq in :nil))
				((and in (find head '("export-symbols" "export-classes")))
					(if (and (> (length inner) 1) (eql (first (second inner)) :quoted) (defq list_kids (tree-form? (third (second inner)))))
						(each (# (if (eql (first %0) :symbol) (. exported :insert (third %0)))) (forms-of list_kids))
						(setq quoted_all :nil)))
				(in (push region kids)))))
		forms))

(defun check-cut (file forms head)
	;(slice (str x) 0 200): how long the string of a thing is, is not known
	;here, and one shorter than the end given is not_valid_index
	(and (eql head "slice") (= (length forms) 4)
		(defq inner (tree-form? (second forms) "str"))
		(eql (first (third forms)) :number) (eql (third (third forms)) "0")
		(eql (first (elem-get forms 3)) :number) (> (str-as-num (third (elem-get forms 3))) 0)
		(found file (second forms) "cut" "a (str ) cut to " (third (elem-get forms 3))
			" is an error if it is shorter, (min " (third (elem-get forms 3)) " (length s))")))

;the ! form of an iterator: before it, its name, and after the sequences,
;what comes before the start and end
(defq *bang_of* (scatter (Fmap 11) "each" '("" "each!" "") "map" '("" "map!" "")
	"some" '("" "some!" " :nil") "every" '("" "some!" " :t")
	"notany" '("not (" "some!" " :nil") "notevery" '("not (" "some!" " :t")
	"filter" '("" "filter!" "") "reduce" '("" "reduce!" "")))

;an iterator of its own, from the end
(defq *back_of* (scatter (Fmap 11) "each" "reach" "map" "rmap" "some" "rsome"
	"reduce" "rreduce" "filter" "rfilter"))

(defun copy-part (node)
	; (copy-part node) -> :nil | (seq "start end" empty_ok)
	;a (slice x s e), a (rest x) or a (most x), what is cut and the part
	;of it, and whether an empty one was not an error, as the ! form is
	(cond
		((and (defq kids (tree-form? node "slice")) (= (length (defq forms (forms-of kids))) 4))
			(list (second forms) (cat (tree-text (third forms)) " " (tree-text (elem-get forms 3))) :nil))
		((and (defq kids (tree-form? node "rest")) (= (length (defq forms (forms-of kids))) 2))
			(list (second forms) "1 -1" :t))
		((and (defq kids (tree-form? node "most")) (= (length (defq forms (forms-of kids))) 2))
			(list (second forms) "0 -2" :t))
		((and (defq kids (tree-form? node "reverse")) (= (length (defq forms (forms-of kids))) 2))
			(list (second forms) "-1 0" :nil))))

(defun check-reverse (file forms head)
	;the reverse of a part is the part taken from its end: one copy, not two
	(when (and (eql head "reverse") (= (length forms) 2)
			(defq part (copy-part (second forms)))
			(not (tree-form? (second forms) "reverse")))
		(bind '(seq range empty) part)
		(defq ends (split range " "))
		(found file (second forms) "reverse" "(reverse) of a part is one copy more, (slice "
			(tree-text seq) " " (second ends) " " (first ends) ")"
			(if empty ", and if it can be empty it is an error" ""))))

(defun check-width (file forms head)
	;(char) of a width written there that it does not make
	(and (eql head "char") (= (length forms) 3)
		(nempty? (defq text (tree-text (third forms))))
		(every (# (find %0 "0123456789")) text)
		(not (<= 1 (str-as-num text) 8))
		(found file (third forms) "width" "(char x " text
			") makes no more than 8 bytes, of 1 to 8")))

(defun check-copy (file forms head)
	;an iterator given only copies of the one part of its sequences
	(when (and (defq bang (. *bang_of* :find head))
			(defq seqs (cond
				((eql head "filter") (if (= (length forms) 3) (list (third forms))))
				((eql head "reduce") (if (= (length forms) 4) (list (third forms))))
				;a list of parameters (map) is no call of it
				((> (length forms) 2) (slice forms 2 -1))))
			(nempty? seqs)
			(defq parts (map (const copy-part) seqs))
			(every (const identity) parts)
			(every (# (eql (second %0) (second (first parts)))) parts))
		(bind '(before name after) bang)
		(if (eql head "reduce") (setq after (cat " " (tree-text (elem-get forms 3)))))
		(defq range (second (first parts)) names (join (map (# (tree-text (first %0))) parts) " "))
		(found file (first seqs) "copy" "(" head ") of copies of the part " range ", "
			(if (and (eql range "-1 0") (defq back (. *back_of* :find head)))
				(cat "(" back " f " names (if (eql head "reduce") (cat " " (tree-text (elem-get forms 3))) "") ")")
				(cat "(" before name " f " (if (eql head "filter") names (cat "(list " names ")")) after " " range
					(if (eql before "") ")" "))")))
			", (!) is then the index in the whole"
			(if (some (# (third %0)) parts) ", and if it can be empty it is an error" ""))))

(defun check-style (file node forms head)
	;an (ifn) has the test of an (if) the other way about, and a (condn)
	;that of a (cond): what is said of the one is said of the other, with
	;the form that it is turned to the other of the pair too
	(defq other (if (eql head "if") "ifn" "if")
		;with no else: what runs a body when the test is so, and when it is not
		runs (if (eql head "if") "when" "unless") runs_not (if (eql head "if") "unless" "when"))
	(cond
		((and (find head '("if" "ifn")) (= (length forms) 3) (tree-form? (third forms) "progn"))
			(found file node "style" "(" head " tst (progn ...)) with no else is a (" runs ")"))
		;with an else it is the other of the pair. With none it is not: an
		;(ifn) with no else gives its test when that is not :nil, where an
		;(if) gives :nil, so it is the (when) or the (unless)
		((and (find head '("if" "ifn")) (> (length forms) 3) (tree-form? (second forms) "not"))
			(found file node "style" "(" head " (not tst) form else ...) is an (" other " tst form else ...)"))
		((and (find head '("if" "ifn")) (= (length forms) 3) (tree-form? (second forms) "not"))
			(found file node "style" "(" head " (not tst) form) with no else is a (" runs_not " tst form)"))
		;(when (and a b) form) is (and a b form), and so is an (if) of one
		;with no else: the (and) gives the last of its tests if every one
		;before is so, and :nil if not, as the (when) does of its one form
		((and (find head '("when" "if")) (= (length forms) 3) (tree-form? (second forms) "and"))
			(found file node "style" "(" head " (and ...) form) is an (and ... form)"))
		((and (find head '("cond" "condn")) (= (length forms) 2) (eql (first (second forms)) :form))
			(found file node "style" "a (" head ") of one clause"))))

(defun doc-words (text)
	; (doc-words text) -> (word ...)
	;the things of a call as a comment writes it, (name a (b c) d | e [f]):
	;the name, then each thing it is given. A list in it is one thing, and
	;so is a choice of two, a | b
	(defq words (list) word (list) depth 0)
	(each (lambda (c)
		(cond
			((eql c "(") (if (> (++ depth) 1) (push word c)))
			((eql c ")") (if (> (-- depth) 0) (push word c)))
			((and (eql c " ") (<= depth 1))
				(when (nempty? word) (push words (apply (const cat) word)) (setq word (list))))
			((push word c))))
		text)
	(if (nempty? word) (push words (apply (const cat) word)))
	;a | and what is after it are of the thing before
	(defq out (list) skip :nil)
	(each (lambda (w)
		(cond
			((eql w "|") (setq skip :t))
			(skip (setq skip :nil))
			((push out w))))
		words)
	out)

(defun check-doc (file node forms head nodes at)
	(when (and (find head '("defun" "defmacro")) (> (length forms) 2)
			(eql (first (second forms)) :symbol) (eql (first (third forms)) :form)
			(nempty? (defq said (tree-said node nodes at)))
			(starts-with "(" (defq line (trim-start (third (first said)) " ;")))
			(nempty? (defq to (substr line " -> "))))
		;what it says it is called with, up to the -> and inside its brackets
		(defq words (doc-words (slice line 0 (first (first (first to)))))
			name (third (second forms)))
		(when (nempty? words)
			(defq takes (filter (# (not (eql (first %0) :quoted))) (forms-of (third (third forms))))
				;what must be given: what is before an & of the definition,
				;and before a [ or a ... of the comment
				need (ifn (some (# (and (eql (first %0) :symbol) (starts-with "&" (third %0)) (!))) takes)
					(length takes))
				says (ifn (some! (# (if (or (starts-with "[" %0) (starts-with "..." %0) (starts-with "{" %0)) (dec (!)))) (list words) :nil 1 -1)
					(dec (length words))))
			(cond
				((nql (first words) name)
					(found file (first said) "doc" "says (" (first words) " ...), this is (" name " ...)"))
				;fewer than it must be given. Or more, of one that takes no more:
				;what a &rest or an &optional takes may be named one by one
				((or (< says need) (and (> says need) (= need (length takes))))
					(found file (first said) "doc" "(" name ") is said to take " (str says) ", it takes " (str need)))))))

(defun check-enum (file node forms)
	;(enums +name base (enum a b size)): +name_size is how many there are
	(each! (lambda (line)
		(when (defq fields (tree-form? line "enum"))
			(each! (lambda (field)
				(and (eql (first field) :symbol) (eql (third field) "size")
					(found file field "enum" "a field named size is the (enums)'s own count of its fields")))
				(list (forms-of fields)) 1 -1)))
		(list forms) 1 -1))

(defun has-symbol? (nodes name)
	; (has-symbol? nodes name) -> :nil | :t
	;is that name anywhere in these, in the forms in them too
	(defq todo (list nodes) yes :nil)
	(while (and (not yes) (defq nodes (pop todo)))
		(each (lambda (node)
			(cond
				((and (eql (first node) :symbol) (eql (third node) name)) (setq yes :t))
				((defq kids (tree-form? node)) (push todo kids))))
			nodes))
	yes)

(defun defuns-of (top)
	; (defuns-of top)
	;the functions made in a tree, into *defuns*: what each takes, and the
	;names it makes with a (defq) of its own, not in a function it has
	;inside it. Either is what a function it is given finds, if it uses
	;that name
	(defq scan (list top))
	(while (defq nodes (pop scan))
		(each (lambda (node)
			(when (defq kids (tree-form? node))
				(push scan kids)
				(defq forms (forms-of kids))
				(and (> (length forms) 2) (eql (first (first forms)) :symbol) (eql (third (first forms)) "defun")
					(eql (first (second forms)) :symbol) (eql (first (third forms)) :form)
					;one that calls what it is given by (callback f env ...) runs it
					;in that env, the caller's, and its own names are not had
					(not (has-symbol? kids "callback"))
					(. *defuns* :insert (third (second forms))
						(cat (params-of (third forms)) (defqs-of (slice forms 3 -1)))))))
			nodes)))

(defun import-defuns (path)
	; (import-defuns path) -> fmap
	;the functions of a file another imports, read the once by this task:
	;a file is imported by many, and was read again for each
	(unless (defq got (. *import_defuns* :find path))
		(defq keep *defuns*)
		(setq *defuns* (Fmap 31))
		(if (defq in (file-stream path)) (defuns-of (first (tree-read in))))
		(. *import_defuns* :insert path (setq got *defuns*))
		(setq *defuns* keep))
	got)

(defun imports-of (file top)
	; (imports-of file top) -> (path ...)
	;the files a file imports, at its top, those that are there
	(defq out (list))
	(each (lambda (node)
		(when (defq kids (tree-form? node "import"))
			(defq forms (forms-of kids))
			(and (> (length forms) 1) (eql (first (second forms)) :string1)
				(str? (defq path (catch (first (read (string-stream (third (second forms))))) :nil)))
				(str? (setq path (catch (path-to-absolute path file) :nil)))
				(/= 0 (age path)) (push out path))))
		(forms-of top))
	out)

(defun check-capture (file forms head)
	;(fn ... (lambda (a) (... x ...)) ...) where fn, of this file or one it
	;imports, has a parameter x: the x the lambda has when fn calls it is fn's
	(when (defq theirs (. *defuns* :find head))
		(each! (lambda (arg)
			(when (and (defq kids (tree-form? arg)) (nempty? (defq inner (forms-of kids)))
					(eql (first (first inner)) :symbol) (find (third (first inner)) '("lambda" "#")))
				(bind '(used made) (uses-of kids))
				(defq taken (list))
				(each (# (and (find %0 theirs) (not (find %0 made)) (not (find %0 taken)) (push taken %0))) used)
				(each (# (found file arg "capture" %0 " in the function given to (" head ") is a name of (" head "), it is that one it has")) taken)))
			(list forms) 1 -1)))

(defun takes-none? (node)
	; (takes-none? node) -> :nil | :t
	;is it a (# ...) with no %0, %1 and so on anywhere in it: a function
	;of no arguments, to call it with one is an error. Not if a part of
	;it is put in as it is made, ,x or ~x in a macro: what that part has
	;in it is not here to be seen, and it may be the %0
	(when (and (defq kids (tree-form? node)) (nempty? (defq inner (forms-of kids)))
			(eql (first (first inner)) :symbol) (eql (third (first inner)) "#"))
		(defq todo (list kids) none :t)
		(while (and none (defq nodes (pop todo)))
			(each (lambda (form)
				(cond
					((eql (first form) :symbol)
						(if (or (and (starts-with "%" (third form)) (> (length (third form)) 1))
								(find (first (third form)) ",~"))
							(setq none :nil)))
					((eql (first form) :form) (push todo (third form)))))
				nodes))
		none))

(defun check-args (file forms head)
	;(map (# (f)) seq): a (# ) with no %0 in it takes nothing, and what
	;it is given to is known to call it with something. Only where that
	;is known: each of these calls its function with an element, or two.
	;Chris, who had asked of this rule "how do you know what will call
	;it ?": "if there are cases we can spot and we see they are wrong,
	;then by all means let's spot them"
	(defq at (cond
		((find head '("each" "map" "filter" "some" "every" "notany" "notevery" "reach" "rmap" "reduce" "rreduce" "lines!"))
			(if (> (length forms) 2) 1))
		;one with a ! is given its sequences in a list, which may have none in it
		((find head '("each!" "map!" "some!" "filter!"))
			(and (> (length forms) 2) (defq kids (tree-form? (third forms)))
				(> (length (defq seqs (forms-of kids))) 1) (eql (first (first seqs)) :symbol) (eql (third (first seqs)) "list") 1))
		((eql head "sort") (if (> (length forms) 2) 2))
		;the :each of a map or a set, every one of them calls it with what it holds
		((and (eql head ".") (> (length forms) 3) (eql (first (third forms)) :keysym) (eql (third (third forms)) ":each"))
			(setq head ". :each") 3)))
	(and at (takes-none? (elem-get forms at))
		(found file (elem-get forms at) "args" "a (# ) with no %0 in it takes nothing, and (" head
			") calls it with something, (lambda (x) ...)")))

(defun check-held (file forms head)
	;(when (create-stdio) ...): the stdio is the value of the test and no more
	(unless (find head '("defq" "setq" "bind"))
		(each (lambda (form)
			(and (defq inner (tree-form? form "create-stdio")) (= (length (forms-of inner)) 1)
				(found file form "held" "a (create-stdio) that is not kept in a name is let go of at once, (defq stdio (create-stdio))")))
			forms)))

;the classes, their parents and the kinds of their methods, as
;lib/files/info.inc keeps them, the first time a .vp is looked at. As it
;was saved, not made again if out of date: it is made again by reading
;every .vp, which here goes wrong, and what a class comes from seldom
;changes. With none, nothing is said
(defq *class_db* :nil +check_class_db "lib/files/class_db.tre")

(defun class-above? (above cls)
	;:t if above is cls or a class it comes from
	(defq at cls)
	(while (and at (nql at above) (defq entry (. *class_db* :find at)))
		(setq at (. entry :find :parent))
		(if (eql at :nil) (setq at :nil)))
	(eql at above))

(defun method-kind (cls meth)
	;the kind of a method, of the class or the first above it that has it
	(defq at cls kind :nil)
	(while (and at (not kind) (defq entry (. *class_db* :find at)))
		(if (defq m (.-> entry (:find :methods) (:find meth)))
			(setq kind (. m :find :type))
			(setq at (. entry :find :parent))))
	kind)

(defun check-self (file top)
	(unless *class_db*
		(setq *class_db* (ifn (with-read-lock +check_class_db
			(if (defq check_db_stream (file-stream +check_class_db)) (tree-load check_db_stream))) (Emap))))
	(defq cls :nil meth :nil)
	(each (lambda (node)
		(when (defq kids (tree-form? node))
			(defq forms (forms-of kids)
				head (and (nempty? forms) (eql (first (first forms)) :symbol) (third (first forms))))
			(cond
				((and (eql head "def-method") (> (length forms) 2))
					(setq cls (sym (third (second forms))) meth (sym (third (third forms)))))
				((eql head "def-func-end") (setq cls :nil))
				(cls ;every form in it, those inside others too
					(defq todo (list kids))
					(while (defq nodes (pop todo))
						(defq fs (forms-of nodes)
							h (and (nempty? fs) (eql (first (first fs)) :symbol) (third (first fs))))
						(and (eql h "call") (> (length fs) 3)
							(eql (first (second fs)) :keysym) (eql (first (third fs)) :keysym)
							(eql (sym (third (third fs))) meth)
							(eql (first (elem-get fs 3)) :quoted)
							(defq args (tree-form? (third (elem-get fs 3))))
							(nempty? (defq afs (forms-of args))) (eql (third (first afs)) ":r0")
							(class-above? (defq above (sym (third (second fs)))) cls)
							(defq kind (method-kind above meth)) (nql kind :static)
							(found file (elem-get nodes 0) "self" "(call " (str above) " " (str meth)
								") in " (str cls) " " (str meth) " goes by the vtable of :r0, to this method again: s-call or d-call"))
						(each (# (if (defq k (tree-form? %0)) (push todo k))) nodes))))))
		top))

;what each file gives as a name, (structure), (enums) and (defq) of a
;+name at its top, and what it imports, read the once by this task
(defq *provides* (Fmap 31) *root_provides* :nil)

(defun provides-of (path)
	; (provides-of path) -> (prefixes names imports)
	(unless (defq got (. *provides* :find path))
		(defq prefixes (list) names (list) top :nil)
		(if (defq in (file-stream path)) (setq top (first (tree-read in))))
		(when top
			(each (lambda (node)
				(when (defq kids (tree-form? node))
					(defq forms (forms-of kids)
						head (and (nempty? forms) (eql (first (first forms)) :symbol) (third (first forms))))
					(cond
						((and (find head '("structure" "enums")) (> (length forms) 1)
								(eql (first (second forms)) :symbol) (starts-with "+" (third (second forms))))
							(push prefixes (third (second forms))))
						((eql head "defq")
							;the names of a (defq) are 1, 3, 5 and on
							(each! (# (and (odd? (!)) (eql (first %0) :symbol) (starts-with "+" (third %0)) (push names (third %0))))
								(list forms) 1)))))
				(forms-of top)))
		(. *provides* :insert path (setq got (list prefixes names (if top (imports-of path top) (list))))))
	got)

(defun provides-from (paths)
	; (provides-from paths) -> (prefixes names files)
	;what those files give, and every file they import, at any depth, and
	;those files
	(defq prefixes (list) names (list) seen (Fset 31) todo (cat paths))
	(while (defq path (pop todo))
		(unless (. seen :find path)
			(. seen :insert path)
			(bind '(p n i) (provides-of path))
			(each (# (push prefixes %0)) p)
			(each (# (push names %0)) n)
			(each (# (push todo %0)) i)))
	(list prefixes names seen))

(defun root-provides ()
	; (root-provides) -> (prefixes names files)
	;what every task has from root.inc, worked out the once
	(unless *root_provides* (setq *root_provides* (provides-from (list "class/lisp/root.inc"))))
	*root_provides*)

;what each file names, a function, a macro, a class, an ffi, or a name it
;binds, anywhere in it, and what it imports, read the once by this task:
;what a call there may be of, (check-unbound)
(defq *names_of* (Fmap 31) +unbound_data ''("enums" "structure" "bits" "static-q" "static-qq"
		"static-qqp" "macrobind" "lists" "raise" "import" "import-cmd" "import-from" "ffi"
		"export-symbols" "export-classes" "quote" "quasi-quote")
	+unbound_defs ''("defun" "defmacro" "redefun" "redefmacro")
	;a file run by another that gives it names it does not import: the
	;tests by the tests command, the diagrams by make docs, the parts of
	;the assembler in the env it makes. And what is not to be looked at
	+unbound_context ''(("tests/" "tests/suite.inc" "tests/bench.inc")
		("docs/diagrams/" "lib/cwb/diagram.inc" "cmd/make.lisp")
		("service/gui/" "sys/lisp.inc" "class/lisp.inc" "gui/lisp.inc" "service/gui/app_impl.lisp")
		("cmd/make.lisp" "lib/asm/class.inc")
		("apps/desktop/docs/handlers/" "apps/desktop/docs/app.lisp"))
	;the assembler and its translators run in the env it makes of many
	;files, not by import
	+unbound_skip ''("tests/scratch/" "lib/asm/" "lib/trans/")
	+unbound_methods ''("defmethod" "defabstractmethod" "deffimethod" "defgetmethod" "defsetmethod" "defproxymethod"))

(defun head-of (inner)
	(and (nempty? inner) (eql (first (first inner)) :symbol) (third (first inner))))

(defun names-of (path)
	; (names-of path) -> (names imports)
	(unless (defq got (. *names_of* :find path))
		(defq names (list) more (list) top (if (defq in (file-stream path)) (first (tree-read in))) todo (if top (list top) (list)))
		(while (defq nodes (pop todo))
			(each (lambda (node)
				(when (defq kids (tree-form? node))
					(push todo kids)
					(defq inner (forms-of kids) head (head-of inner) n (length inner))
					(cond
						((not head))
						;a name made by a macro, (sym (cat "edit-" n)): what it begins
						;with, any name that does may be one
						((and (eql head "sym") (> n 1) (defq made (tree-form? (second inner)))
								(defq made (forms-of made)) (> (length made) 1)
								(find (head-of made) '("cat" "str")) (eql (first (second made)) :string1))
							(defq prefix (catch (first (read (string-stream (third (second made))))) :nil))
							(and (str? prefix) (nempty? prefix) (push names (cat "prefix " prefix))))
						((and (find head +unbound_defs) (> n 2))
							(if (eql (first (second inner)) :symbol) (push names (third (second inner))))
							(each (# (push names %0)) (params-of (third inner))))
						((and (eql head "defclass") (> n 2))
							(when (eql (first (second inner)) :symbol)
								(push names (third (second inner)))
								(push names (cat (third (second inner)) "?")))
							(each (# (push names %0)) (params-of (third inner))))
						((and (find head +unbound_methods) (> n 2)) (each (# (push names %0)) (params-of (third inner))))
						((and (eql head "lambda") (> n 1)) (each (# (push names %0)) (params-of (second inner))))
						((and (eql head "ffi") (> n 2) (eql (first (third inner)) :symbol)) (push names (third (third inner))))
						((find head '("defq" "setq" "setd"))
							(each! (# (and (odd? (!)) (eql (first %0) :symbol) (push names (third %0)))) (list inner) 1))
						((and (eql head "bind") (> n 1) (eql (first (second inner)) :quoted))
							(each (# (push names %0)) (params-of (third (second inner)))))
						((and (find head '("let" "let*")) (> n 1) (defq binds (tree-form? (second inner))))
							(each (# (if (defq b (tree-form? %0)) (each (# (push names %0)) (params-of (first (forms-of b))))))
								(forms-of binds)))
						;an import anywhere, in a function or a catch, what it names may
						;be called after it
						((and (eql head "import") (> n 1) (eql (first (second inner)) :string1)
								(str? (defq to (catch (first (read (string-stream (third (second inner))))) :nil)))
								(str? (setq to (catch (path-to-absolute to path) :nil))) (/= 0 (age to)))
							(push more to))
						((and (eql head "raise") (> n 2) (defq binds (tree-form? (third inner))))
							(each! (# (and (even? (!)) (eql (first %0) :symbol) (push names (third %0)))) (list (forms-of binds)))))))
				nodes))
		(. *names_of* :insert path (setq got (list names more))))
	got)

(defun names-from (paths)
	; (names-from paths) -> fset
	;what those files name, and every file they import, at any depth
	(defq out (Fset 101) seen (Fset 31) todo (cat paths))
	(while (defq path (pop todo))
		(unless (. seen :find path)
			(. seen :insert path)
			(bind '(n i) (names-of path))
			(each (# (. out :insert %0)) n)
			(each (# (push todo %0)) i)))
	out)

(defun check-unbound (file top)
	;(f ...) where f is no function, macro or name of the system, nor named
	;by this file or anything it imports at any depth: a call of nothing,
	;the app would not start. Only for a file that stands by itself
	(defq context (some (# (if (starts-with (first %0) file) (rest %0))) +unbound_context)
		known (names-from (cat (list file "class/lisp/root.inc") (ifn context (list)))) todo (list top)
		prefixes (list))
	(. known :each (# (if (starts-with "prefix " %0) (push prefixes (slice %0 7 -1)))))
	(while (defq nodes (pop todo))
		(each (lambda (node)
			(when (defq kids (tree-form? node))
				(defq inner (forms-of kids) head (head-of inner) n (length inner))
				(and head (not (. *root_all* :find head)) (not (. known :find head))
					(not (find (first head) ":+%&.,~'`0123456789"))
					(notany (# (starts-with %0 head)) prefixes)
					(found file (first inner) "unbound" head " is called, and is no function or macro of the system, of this file, or of what it imports"))
				(cond
					((not head) (push todo inner))
					((find head +unbound_data))
					((find head +unbound_defs) (push todo (slice inner (min n 3) -1)))
					((find head +unbound_methods) (push todo (slice inner (min n 3) -1)))
					((eql head "defclass") (push todo (slice inner (min n 4) -1)))
					((eql head "lambda") (push todo (slice inner (min n 2) -1)))
					((find head '("let" "let*"))
						(and (> n 1) (defq binds (tree-form? (second inner)))
							(each (# (if (defq b (tree-form? %0)) (push todo (rest (forms-of b))))) (forms-of binds)))
						(push todo (slice inner (min n 2) -1)))
					((find head '("cond" "condn"))
						(each! (# (if (defq c (tree-form? %0)) (push todo (forms-of c)))) (list inner) 1))
					((find head '("case" "pcase"))
						(if (> n 1) (push todo (list (second inner))))
						(each! (# (if (defq c (tree-form? %0)) (push todo (rest (forms-of c))))) (list inner) (min n 2)))
					((push todo (rest inner))))))
			nodes)))

(defun check-root-struct (file forms)
	;(structure +x ...) where root.inc's imports have a +x already: a task
	;that has both can not tell them apart, and one of fields in other
	;places is a field redefined
	(bind '(prefixes & files) (root-provides))
	(and (> (length forms) 1) (eql (first (second forms)) :symbol)
		(starts-with "+" (defq name (third (second forms))))
		(find name prefixes) (not (. files :find file))
		(found file (second forms) "struct" "(structure " name ") is one of root.inc's imports too, use that one or name this for its app")))

(defun provided? (field prov)
	; (provided? field prov) -> :t | :nil
	(bind '(prefixes names) prov)
	(or (find field names) (some (# (starts-with (cat %0 "_") field)) prefixes)))

(defun check-struct (file forms head prov)
	;(getf msg +x_field) (setf msg +x_field v) (setf-> msg (+x_field v) ...)
	;where nothing this file can see gives +x_field
	(defq fields (cond
		((find head '("getf" "setf")) (if (> (length forms) 2) (list (third forms))))
		((eql head "setf->") (map! (# (if (defq k (tree-form? %0)) (first (forms-of k)))) (list forms) 2 -1))))
	(each (lambda (field)
			(and field (eql (first field) :symbol) (starts-with "+" (defq name (third field)))
				(find "_" name) (not (provided? name prov))
				(found file field "struct" name " is given by no (structure) or (enums) this file imports, import where it is made")))
		(ifn fields (list))))

;do the work on a file
(defun work (file)
	;The file is read as a tree, and every form of it looked at, those
	;inside others too. A list of the lists of nodes still to look at, no
	;function here calls itself
	(with-read-lock file
		(when (defq in (file-stream file))
			(defq whole (tree-read in) top (first whole) todo (list top) *found* (list) off (Fset 11)
				;the functions of this file, and what each takes
				*defuns* (Fmap 31) *inner_defuns* (Fset 11) inner_scan (list top))
			;the functions made inside another, (defun) not at the top of the file
			(while (defq nodes (pop inner_scan))
				(each (lambda (node)
					(when (defq kids (tree-form? node))
						(push inner_scan kids)
						(defq forms (forms-of kids))
						(and (not (eql nodes top)) (> (length forms) 1) (eql (first (first forms)) :symbol)
							(eql (third (first forms)) "defun") (eql (first (second forms)) :symbol)
							(. *inner_defuns* :insert (third (second forms))))))
					nodes))
			(if (or (find "unquote" checks) (find "global" checks)) (check-unquote file top))
			;a file that stands by itself: a program, or a module
			(and (find "unbound" checks) (not (ends-with ".vp" file)) (notany (# (starts-with %0 file)) +unbound_skip)
				(or (ends-with ".lisp" file) (some (# (tree-form? %0 "env-push")) (forms-of top)))
				(check-unbound file top))
			(and (find "self" checks) (ends-with ".vp" file) (check-self file top))
			;brackets that do not pair: the file does not load at all, and
			;if it is one a task is started from, nothing says so
			(when (find "bracket" checks)
				(if (> (second whole) 0)
					(push *found* (list 0 (cat file " (1) bracket: " (str (second whole)) " not closed by the end of the file"))))
				(if (> (third whole) 0)
					(push *found* (list 0 (cat file " (1) bracket: " (str (third whole)) " that close nothing")))))
			;what the file can see of structures and constants, its own, what it
			;imports at any depth, and what every task has from root.inc
			(defq prov (when (and (find "struct" checks) (not (ends-with ".vp" file))
					;a module stands by itself, a part of a file, as an app's
					;actions.inc, has what the file it is put in has
					(some (# (tree-form? %0 "env-push")) (forms-of top)))
				(bind '(p n &ignore) (provides-from (list file)))
				(list (cat p (first (root-provides))) (cat n (second (root-provides))))))
			(when (find "capture" checks)
				;the functions of the files it imports, and its own after, which
				;are the ones a name is had of if both have it
				(each (lambda (path) (. (import-defuns path) :each (# (. *defuns* :insert %0 %1))))
					(imports-of file top))
				(defuns-of top))
			(while (defq nodes (pop todo))
				(task-slice)
				(defq quoted :nil)
				(each (lambda (node)
					(defq at (!) was quoted)
					;a thing with a quote before it is a list, or code that is
					;being made: it is not looked at, nor what is in it
					(setq quoted (and (eql (first node) :symbol)
						(= (bskip +char_class_quote (third node) 0) (length (third node)))))
					(if was (setq node (list :quoted (second node))))
					;a comment that begins ;check off, nothing is said of its
					;line or of the next: a test of the very thing, say
					(when (and (eql (first node) :comment) (starts-with ";check off" (third node)))
						(. off :insert (second node))
						(. off :insert (inc (second node))))
					;a name by itself, not in any form, at the top of a file: a
					;bracket closed a form before its end, and what was to be
					;the rest of it is now run by itself
					(and (find "stray" checks) (eql nodes top) (eql (first node) :symbol) (not quoted) (not was)
						(= 0 (bskip +char_class_quote (third node) 0))
						(found file node "stray" "a name by itself at the top of the file, " (third node)
							", was a form closed before its end ?"))
					(if (find "zero" checks) (check-zero file node))
					(when (defq kids (tree-form? node))
						(push todo kids)
						(defq forms (forms-of kids)
							head (and (nempty? forms) (eql (first (first forms)) :symbol) (third (first forms))))
						(when head
							(if (find "progn" checks) (check-progn file forms head))
							(if (find "class" checks) (check-class file forms head))
							(if (find "shadow" checks) (check-shadow file forms head))
							(and (find "const" checks) (eql head "defq") (check-const file kids))
							(and (find "early" checks) (eql head "defq") (check-early file kids))
							(and (find "made" checks) (eql head "defq") (check-made file kids))
							(if (find "inner" checks) (check-inner file forms head))
							(if (find "eqlist" checks) (check-eqlist file forms head))
							(if (find "ignore" checks) (check-ignore file forms head))
							(if (find "param" checks) (check-param file forms head))
							(if (find "empty" checks) (check-empty file forms head))
							(if (find "past" checks) (check-past file forms head))
							(if (find "shift" checks) (check-shift file forms head))
							(if (find "again" checks) (check-again file forms head))
							(if (find "cut" checks) (check-cut file forms head))
							(if (find "copy" checks) (check-copy file forms head))
							(if (find "reverse" checks) (check-reverse file forms head))
							(if (find "width" checks) (check-width file forms head))
							(if (find "style" checks) (check-style file node forms head))
							(if (find "doc" checks) (check-doc file node forms head nodes at))
							(and (find "enum" checks) (eql head "enums") (check-enum file node forms))
							(if (find "capture" checks) (check-capture file forms head))
							(if (find "held" checks) (check-held file forms head))
							(if (find "args" checks) (check-args file forms head))
							(and prov (find head '("getf" "setf" "setf->")) (check-struct file forms head prov))
							(and (find "struct" checks) (eql head "structure") (check-root-struct file forms)))))
					nodes))
			;in the order of the lines they are on
			(each (lambda ((line text)) (unless (. off :find line) (print text)))
				(sort *found* (# (- (first %0) (first %1))))))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_j 1 opt_c :nil args (options stdio usage)))
		(defq checks (if opt_c (filter (# (find %0 +check_names)) (split opt_c ",")) +check_names)
			;the names of the system, what a function, a macro or a form is
			;bound to in the environment every task starts with
			*root_names* (Fset 101) *root_macros* (Fset 31) *root_all* (Fset 101)
			;the functions of each file imported, as read, (import-defuns)
			*import_defuns* (Fmap 31))
		(each (lambda ((name val))
			;a macro is a list that begins with macro
			(defq is_macro (and (list? val) (nempty? val) (eql (first val) macro)))
			(. *root_all* :insert (str name))
			(if (or (func? val) (lambda? val) is_macro) (. *root_names* :insert (str name)))
			(if is_macro (. *root_macros* :insert (str name))))
			(tolist *root_env*))
		;from args ?
		(if (empty? (defq jobs (rest args)))
			;no, so from stdin
			(lines! (# (push jobs %0) :nil) (io-stream 'stdin)))
		;only source files, a folder the source files in it at any depth
		(setq jobs (reduce (lambda (out f)
				(if (some (# (ends-with %0 f)) '(".vp" ".inc" ".lisp"))
					(push out f)
					(reduce (# (push %0 %1)) (files-all f '(".vp" ".inc" ".lisp")) out)))
			jobs (list)))
		(if (<= (length jobs) opt_j)
			;do the work when batch size ok !
			(each (const work) jobs)
			;do them all out there, by calling myself !
			(each (lambda ((job result)) (prin result))
				(pipe-farm (map (# (str (first args)
						" -j " opt_j
						(if opt_c (cat " -c " opt_c) "")
						" " (slice (str %0) 1 -2)))
					(partition jobs opt_j)))))))
