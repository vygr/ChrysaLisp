(report-header "Format: indent from structure, line breaking, what is never touched")

(import "lib/text/format.inc")

(defq fm_lf (ascii-char 10))

(defun fm-src (&rest lines)
	;source text from its lines
	(cat (join lines fm_lf) fm_lf))

(defun fm-squash (text)
	;the text with all white space taken out
	(apply (const cat) (split text (char-class (cat " \t\r" fm_lf)))))

(defun fm-check (name want text &optional limit)
	;the text must format to want, and want must then be left alone
	(assert-eq name want (format-lisp text limit))
	(assert-eq (cat name ", again") want (format-lisp want limit)))

(defun fm-keep (name want text)
	;as fm-check, but with the line breaks of the text kept
	(fm-check name want text 0))

; --- indent comes from the structure alone ---
(fm-keep "a line is one tab in from where its form opened"
	(fm-src "(defun f (a)" "\t(print a)" "\t(if a" "\t\tb" "\t\tc))")
	(fm-src "(defun f (a)" "(print a)" "    (if a" "  b" "c))"))
(fm-keep "a condition carried over is two tabs in"
	(fm-src "(when (and a" "\t\tb)" "\tc)") (fm-src "(when (and a" "b)" "c)"))
(fm-keep "a form opened inside another on a line is a tab further in, to show whose a line is"
	(fm-src "(each (lambda (x)" "\t\t(print x))" "\tl)") (fm-src "(each (lambda (x)" "(print x))" "l)"))
(fm-keep "and so on for each form opened on the line" (fm-src "(a (b (c d" "\t\t\te)" "\t\tf)" "\tg)")
	(fm-src "(a (b (c d" "e)" "f)" "g)"))
(fm-keep "however far along the line the form starts" (fm-src "(defq a_name (list 1" "\t\t2)" "\tb 3)")
	(fm-src "(defq a_name (list 1" "2)" "b 3)"))
(fm-keep "bindings, and a value carried over" (fm-src "(defq a 1" "\tb (foo" "\t\tc)" "\td 2)")
	(fm-src "(defq a 1" "b (foo" "c)" "d 2)"))
(fm-keep "cond clauses and their bodies" (fm-src "(cond" "\t((= a 1)" "\t\t(print a))" "\t(:t" "\t\t(print b)))")
	(fm-src "(cond" "((= a 1)" "(print a))" "(:t" "(print b)))"))
(fm-keep "spaces become tabs" (fm-src "(a" "\t(b" "\t\tc))") (fm-src "(a" "    (b" "            c))"))

; --- tidying ---
(fm-keep "trailing white space goes" (fm-src "(a" "\tb)") (fm-src "(a   " "b)  \t"))
(fm-keep "close brackets join the line above" (fm-src "(a" "\t(b" "\t\tc))") (fm-src "(a" "(b" "c" ")" ")"))
(fm-keep "close brackets at the start of a line join the line above"
	(fm-src "(a" "\t(b" "\t\tc)" "\td)") (fm-src "(a" "(b" "c" ") d)"))
(fm-keep "but not on to a comment" (fm-src "(a" "\tb ;note" ")") (fm-src "(a" "b ;note" ")"))
(fm-keep "runs of blank lines become one" (fm-src "(a)" "" "(b)") (fm-src "" "(a)" "" "" "" "(b)" "" ""))
(assert-eq "a newline is added at the end" (fm-src "(a)") (format-lisp "(a)"))
(assert-eq "nothing gives nothing" "" (format-lisp ""))
(assert-eq "only blank lines give nothing" "" (format-lisp (fm-src "" "  " "")))

; --- what is never touched ---
(fm-keep "the lines of a string are kept as they are" (fm-src "(print \qone  " {   two  } "  three\q a)" {(b)})
	(fm-src "  (print \qone  " {   two  } "  three\q a)  " {  (b)}))
(fm-keep "brackets in a string are not structure" (fm-src "(a \q(((\q b" "\tc)") (fm-src "(a \q(((\q b" "c)"))
(fm-keep "brackets in a {} string are not structure" (fm-src "(a {)))} b" "\tc)") (fm-src "(a {)))} b" "c)"))
(fm-keep "brackets in a comment are not structure" (fm-src "(a ;(((" "\tb)") (fm-src "(a ;(((" "b)"))
(fm-keep "a comment keeps its text, and is indented as code" (fm-src "(a ;x  y" "\t; z  (" "\tb)")
	(fm-src "(a ;x  y" "       ; z  (" "b)"))
(fm-keep "a semicolon in a string is not a comment" (fm-src "(a \q;\q (b" "\t\tc))") (fm-src "(a \q;\q (b" "c))"))
(fm-keep "notes under a top level form keep their indent" (fm-src "(f)" "\t; note" "\t; more" "(g)")
	(fm-src "(f)" "    ; note" "\t\t; more" "(g)"))
(fm-keep "a comment at the margin stays there" (fm-src "(f)" ";module" "(g)") (fm-src "(f)" ";module" "(g)"))
(fm-keep "the help text of a command is kept as it is"
	(fm-src "(defq usage `(" "((\q-h\q \q--help\q)" "\qUsage: x" "" "    more\q)" "))" "(a" "\tb)")
	(fm-src "(defq usage `(" "((\q-h\q \q--help\q)" "\qUsage: x" "" "    more\q)" "))" "(a" "b)"))

; --- VP assembler blocks ---
(fm-keep "block forms indent the lines between them"
	(fm-src "(def-method :a :b)" "\t;inputs"
		"\t(vpif '(:r0 = 0))" "\t\t(vp-cpy-rr :r0 :r1)" "\t(else)" "\t\t(vp-ret)" "\t(endif)"
		"(errorcase" "(vp-label 'error)" "\t(jump :a :b))" "(def-func-end)")
	(fm-src "(def-method :a :b)" " ;inputs" "(vpif '(:r0 = 0))"
		"(vp-cpy-rr :r0 :r1)" "(else)" "(vp-ret)" "(endif)"
		"(errorcase" "(vp-label 'error)" "(jump :a :b))" "(def-func-end)"))
(fm-keep "loops and a switch"
	(fm-src "(def-func 'a)" "\t(loop-start)" "\t\t(switch)" "\t\t(vpcase '(:r0 = 0))" "\t\t\t(break)" "\t\t(default)"
		"\t\t\t(vp-ret)"
		"\t\t(endswitch)"
		"\t(loop-until '(:r0 = 0))" "(def-func-end)")
	(fm-src "(def-func 'a)" "(loop-start)" "(switch)" "(vpcase '(:r0 = 0))"
		"(break)" "(default)" "(vp-ret)" "(endswitch)"
		"(loop-until '(:r0 = 0))" "(def-func-end)"))
(fm-keep "in a function, a comment at the margin stays, as a banner does"
	(fm-src "(def-func 'a)" "\t;note" ";;;;;;" "; b" ";;;;;;" "\t(vp-ret)"
		"(def-func-end)") (fm-src "(def-func 'a)" "  ;note" ";;;;;;" "; b" "  ;;;;;;" "(vp-ret)"
			"(def-func-end)"))

; --- the layout is made from nothing but the code ---
(fm-check "line breaks in a form are not kept, a short form is one line"
	(fm-src "(defq x 1 y 2 z 3)" "(if (> a b) (print a) (print b))")
	(fm-src "(defq x 1 y 2" "\tz 3)" "(if (> a b) (print a)" "\t(print b))"))
(fm-check "a short definition, with the one body form, is a line"
	(fm-src "(defun f (a) (print a))" "(defmacro m (a) :nil)")
	(fm-src "(defun f (a)" "\t(print a))" "(defmacro m (a) :nil)"))
(fm-check "with more than the one body form, each has a line" (fm-src "(defun f (a)" "\t(print a)" "\t(print a))")
	(fm-src "(defun f (a) (print a) (print a))"))
(fm-check "cond always has a clause to a line, a clause with one form is a line"
	(fm-src "(cond" "\t((= a 1) (print 1))" "\t((= a 2)" "\t\t(print 2)" "\t\t(print 3))" "\t(:t :nil))")
	(fm-src "(cond ((= a 1) (print 1)) ((= a 2) (print 2)" "(print 3)) (:t :nil))"))
(fm-check "when is one line only with one body form"
	(fm-src "(when a (print 1))" "(when a" "\t(print 1)" "\t(print 2))")
	(fm-src "(when a" "\t(print 1))" "(when a (print 1) (print 2))"))
(fm-check "one top level form to a line" (fm-src "(a)" "(b)") (fm-src "(a) (b)"))
(fm-check "spaces between tokens become one, none inside a bracket"
	(fm-src "(a b (c d) 'e)") (fm-src "( a   b\t( c d )  ' e )"))
(fm-check "a comment ends a line, and the gap before it is kept"
	(fm-src "(a b\t\t;note" "\tc d)") (fm-src "(a b\t\t;note" "c" "d)"))
(fm-check "a blank line in a form is kept" (fm-src "(progn (a)" "" "\t(b))") (fm-src "(progn" "(a)" "" "(b))"))

; --- what the source scanners read stays as they read it ---
(fm-check "comments right under a definition stay right under it"
	(fm-src "(defmacro m (a) :nil)" "\t; (m a) -> :nil" "(defun f (a)" "\t; (f a)" "\t(print a))")
	(fm-src "(defmacro m (a) :nil)" "\t; (m a) -> :nil" "(defun f (a)" "\t; (f a)" "\t(print a))"))
(fm-check "a scanned form keeps its line, and is not joined to another"
	(fm-src "(def-class :a :b" "\t(dec-method :c a/b/c :static (:r0) (:r0))" "\t(dec-method :d a/b/d))")
	(fm-src "(def-class :a :b" "(dec-method :c a/b/c :static (:r0) (:r0))"
		"(dec-method :d a/b/d))"))
(fm-check "a VP instruction is a line of its own"
	(fm-src "(defun m ()" "\t(vp-cpy-rr :r0 :r1)" "\t(call :a :b '(:r0))" "\t(vp-ret))" "(def-func 'a)" "(errorcase"
		"\t(call :a :b)"
		"\t(jump :c :d))" "(def-func-end)") (fm-src "(defun m ()" "(vp-cpy-rr :r0 :r1)" "(call :a :b" "'(:r0))"
			"(vp-ret))" "(def-func 'a)" "(errorcase" "(call :a :b)"
			"(jump :c :d))" "(def-func-end)"))
(fm-check "and is not moved to the start of a line if it was not at one"
	(fm-src "(defun m () (vp-cpy-rr :r0 :r1)" "\t(vp-ret))") (fm-src "(defun m () (vp-cpy-rr :r0 :r1)" "(vp-ret))"))
(fm-check "the lines of a key map are kept"
	(fm-src "(defq" "\t*key_map* (scatter (Fmap)" "\t\t(ascii-code \qa\q) action-a" "\t\t(ascii-code \qb\q) action-b)"
		""
		"\tx 1 y 2)")
	(fm-src "(defq" "*key_map* (scatter (Fmap)"
		"(ascii-code \qa\q) action-a" "(ascii-code \qb\q) action-b)" "" "x 1" "y 2)"))

; --- a line that is too long is broken ---
(fm-check "bindings break before a name, as many to a line as fit"
	(fm-src "(defq alpha (list 1 2) beta (list 3 4)" "\tgamma (list 5 6))")
	(fm-src "(defq alpha (list 1 2) beta (list 3 4) gamma (list 5 6))") 40)
(fm-check "a form with a body has each body form on its own line"
	(fm-src "(progn" "\t(first-thing alpha)" "\t(other-thing alpha)" "\t(third-thing))")
	(fm-src "(progn (first-thing alpha) (other-thing alpha) (third-thing))") 40)
(fm-check "an if keeps its then form on the opening line, if it fits there"
	(fm-src "(if (test alpha) (first-thing alpha)" "\t(other-thing alpha))")
	(fm-src "(if (test alpha) (first-thing alpha) (other-thing alpha))") 40)
(fm-check "and if it does not fit, the then form has a line too"
	(fm-src "(if (a-long-test alpha beta gamma)" "\t(first-thing alpha)" "\t(other-thing alpha))")
	(fm-src "(if (a-long-test alpha beta gamma) (first-thing alpha) (other-thing alpha))") 40)
(fm-check "an if with more than one else form has each on a line" (fm-src "(ifn a 0" "\t(b)" "\t(c))" "(if a b c)")
	(fm-src "(ifn a 0 (b) (c))" "(if a b" "c)"))
(fm-check "cond has each clause on its own line"
	(fm-src "(cond" "\t((= a 1) (print 1))" "\t((= a 2) (print 2))" "\t(:t :nil))")
	(fm-src "(cond ((= a 1) (print 1)) ((= a 2) (print 2)) (:t :nil))") 40)
(fm-check "a condition that is broken, then the body"
	(fm-src "(when (and (first-test alpha beta)" "\t\t(second-test alpha)" "\t\t(third-test))" "\t(print 1))")
	(fm-src "(when (and (first-test alpha beta) (second-test alpha) (third-test)) (print 1))") 40)
(fm-check "each, the body then the sequence"
	(fm-src "(each (lambda (x)" "\t\t(print x)" "\t\t(print x x x))" "\t(list 1 2 3))")
	(fm-src "(each (lambda (x) (print x) (print x x x)) (list 1 2 3))") 40)
(fm-check "a break that would only put a scrap on a line is not made"
	(fm-src "(some (lambda (a) (if (test a) a)) moves)") (fm-src "(some (lambda (a) (if (test a) a))" "moves)") 40)
(fm-check "a call a little over the limit is left alone"
	(fm-src "(print (alpha-function one two) (beta-function three) four)")
	(fm-src "(print (alpha-function one two) (beta-function three) four)") 40)
(fm-check "a call well over the limit is filled, the lines as even as can be"
	(fm-src "(print (alpha-function one two) (beta-function three)" "\t(gamma-function four five) six)")
	(fm-src "(print (alpha-function one two) (beta-function three) (gamma-function four five) six)") 40)
(fm-check "the opening line of a definition is never broken"
	(fm-src "(defun a-long-function-name (argument_one argument_two)" "\t(print 1))") (fm-src
		"(defun a-long-function-name (argument_one argument_two) (print 1))") 40)
(fm-check "a form the scanners read as a line is never broken" (fm-src
		"(dec-method :a_method class/name/a_method :static (:r0 :r1) (:r0))") (fm-src
			"(dec-method :a_method class/name/a_method :static (:r0 :r1) (:r0))") 40)
(fm-check "a string is never broken" (fm-src "(print \qa long string that will not fit in the limit at all\q)")
	(fm-src "(print \qa long string that will not fit in the limit at all\q)") 40)
(fm-check "a line is never broken just before a definition"
	(fm-src "(when alpha-beta-gamma-delta-epsilon (defun foo ()" "\t\t(print 1)))")
	(fm-src "(when alpha-beta-gamma-delta-epsilon (defun foo () (print 1)))") 40)
(fm-check "a VP instruction gets half as much again" (fm-src "(vp-simd vp-cpy-ri-i `(,a ,b) `(,c) `(,d ,e) `(,f))")
	(fm-src "(vp-simd vp-cpy-ri-i `(,a ,b) `(,c) `(,d ,e) `(,f))") 40)
(fm-check "properties break before a name"
	(fm-src "(ui-label x (:text \q0\q" "\t\t:color +argb_white" "\t\t:font a_font_name))")
	(fm-src "(ui-label x (:text \q0\q :color +argb_white :font a_font_name))") 40)
(fm-check "a value that is too long has a line of its own, in from its name"
	(fm-src "(setq a_name" "\t\t(a-function-name (another-function argument_one)))")
	(fm-src "(setq a_name (a-function-name (another-function argument_one)))") 40)
(fm-check "a form that will be over several lines starts a line of its own"
	(fm-src "(print a b" "\t(cond" "\t\t((= a 1) (print 1))" "\t\t(:t (print 2))))")
	(fm-src "(print a b (cond ((= a 1) (print 1)) (:t (print 2))))"))
(fm-check "a limit of 0 breaks nothing" (fm-src "(if (test alpha) (first-thing alpha) (other-thing alpha))")
	(fm-src "(if (test alpha) (first-thing alpha) (other-thing alpha))") 0)
(fm-check "a short line is left alone" (fm-src "(if a b c)") (fm-src "(if a b c)") 40)

; --- on real source, only white space changes, and a second pass does nothing ---
(each (lambda (file)
		(defq text (load file) once (format-lisp text :nil (ends-with ".vp" file)))
		(assert-eq (cat file ", same tokens") :t (eql (fm-squash text) (fm-squash once)))
		(assert-eq (cat file ", second pass") :t (eql once (format-lisp once :nil (ends-with ".vp" file)))))
	'("class/lisp/root.inc" "lib/text/buffer.inc" "sys/heap/class.vp"
		"gui/region/class.vp" "apps/tui/tui.lisp" "lib/asm/vp.inc"))

; --- fmt formats itself, and finds nothing to do ---
(each (lambda (file)
		(assert-eq (cat file ", is as fmt would have it") :t (eql (defq text (load file)) (format-lisp text))))
	'("lib/text/format.inc" "cmd/fmt.lisp" "tests/text/test_format.lisp"))
