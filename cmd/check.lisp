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
    style   (if tst (progn ...)) with no else, a (when);
            (if (not tst) ...), an (ifn); and a (cond) of
            one clause.
    doc     a ; (name args) -> result comment under a
            (defun) or (defmacro) that is of another name,
            or of more or fewer things than it takes.

    If no paths given on command line
    then will test files from stdin.")
(("-j" "--jobs") ,(opt-num 'opt_j))
(("-c" "--checks") ,(opt-str 'opt_c))
))

(defq +check_names ''("progn" "class" "zero" "shadow" "const" "style" "doc")
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
				(defq at (some (# (if (tree-form? %0 "progn") %0)) (slice forms 3 -1))))
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
		(if (and (str? cls) (> (length cls) 1) (nql cls (char-class cls)))
			(found file arg "class" (third arg) " given to (" head ") is not in order, (char-class " (third arg) ")"))))

(defun check-zero (file node)
	;a number with a sign before it is read as a symbol by the tree
	(when (and (find (first node) '(:number :symbol)) (nempty? (defq text (third node)))
			(defq digits (if (find (first text) "+-") (rest text) text))
			(nempty? digits) (find (first digits) "0123456789.")
			(find "." digits) (not (find "x" digits)) (not (find "e" digits))
			(/= (bskipn +char_class_nonzero digits 0) (length digits))
			(num? (defq val (catch (str-to-num digits) :t))) (= val 0))
		(found file node "zero" text " is too small for a fixed, it is 0")))

(defun check-shadow (file forms head)
	;(and a b) is (condn (a) (b)), and a clause of a (cond) is a list
	;with its test first: a list whose first thing is the name of a
	;function is taken as a call of it, the name is bound to the function
	;as the code is read, and the variable is never looked at
	(each (lambda (node)
		(if (and (eql (first node) :symbol) (. *root_names* :find (third node)))
			(found file node "shadow" (third node) " as a test of (" head ") is taken as the function of the system, not the variable")))
		(cond
			((find head '("and" "or")) (rest forms))
			((find head '("cond" "condn"))
				(map (# (first (forms-of (third %0)))) (filter (# (and (eql (first %0) :form) (nempty? (forms-of (third %0))))) (rest forms))))
			((list)))))

(defun check-const (file kids)
	;in a (defq), a + name, a ' and nothing but, and a list
	(defq kids (filter (# (not (find (first %0) '(:comment :blank)))) kids))
	(each (lambda (name quote lst)
		(if (and (eql (first name) :symbol) (starts-with "+" (third name))
				(eql (first quote) :symbol) (eql (third quote) "'") (eql (first lst) :form)
				;one that begins with a string or a number is no form to run,
				;one that begins with a name may be meant as one
				(nempty? (defq in (forms-of (third lst))))
				(find (first (first in)) '(:string1 :string2 :number)))
			(found file name "const" (third name) " is a list quoted once, ''(...)")))
		kids (rest kids) (rest (rest kids))))

(defun check-style (file node forms head)
	(cond
		((and (eql head "if") (= (length forms) 3) (tree-form? (third forms) "progn"))
			(found file node "style" "(if tst (progn ...)) with no else is a (when)"))
		((and (eql head "if") (> (length forms) 2) (tree-form? (second forms) "not"))
			(found file node "style" "(if (not tst) ...) is an (ifn tst ...)"))
		((and (eql head "cond") (= (length forms) 2) (eql (first (second forms)) :form))
			(found file node "style" "a (cond) of one clause"))))

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
				need (ifn (some (# (if (and (eql (first %0) :symbol) (starts-with "&" (third %0))) (!))) takes)
					(length takes))
				says (ifn (some (# (if (or (starts-with "[" %0) (starts-with "..." %0) (starts-with "{" %0)) (!))) (rest words))
					(dec (length words))))
			(cond
				((nql (first words) name)
					(found file (first said) "doc" "says (" (first words) " ...), this is (" name " ...)"))
				;fewer than it must be given. Or more, of one that takes no more:
				;what a &rest or an &optional takes may be named one by one
				((or (< says need) (and (> says need) (= need (length takes))))
					(found file (first said) "doc" "(" name ") is said to take " (str says) ", it takes " (str need)))))))

;do the work on a file
(defun work (file)
	;The file is read as a tree, and every form of it looked at, those
	;inside others too. A list of the lists of nodes still to look at, no
	;function here calls itself
	(with-read-lock file
		(when (defq in (file-stream file))
			(defq todo (list (first (tree-read in))) *found* (list) off (Fset 11))
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
					(if (find "zero" checks) (check-zero file node))
					(when (defq kids (tree-form? node))
						(push todo kids)
						(defq forms (forms-of kids)
							head (if (and (nempty? forms) (eql (first (first forms)) :symbol)) (third (first forms))))
						(when head
							(if (find "progn" checks) (check-progn file forms head))
							(if (find "class" checks) (check-class file forms head))
							(if (find "shadow" checks) (check-shadow file forms head))
							(if (and (find "const" checks) (eql head "defq")) (check-const file kids))
							(if (find "style" checks) (check-style file node forms head))
							(if (find "doc" checks) (check-doc file node forms head nodes at)))))
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
			*root_names* (Fset 101))
		(each (lambda ((name val))
			(if (or (func? val) (lambda? val) (macro? val)) (. *root_names* :insert (str name))))
			(tolist *root_env*))
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
						(if opt_c (cat " -c " opt_c) "")
						" " (slice (str %0) 1 -2)))
					(partition jobs opt_j)))))))
