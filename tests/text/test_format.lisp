(report-header "Format: the source laid out afresh from its forms and comments, in the house style")

(import "lib/text/format.inc")

(defq fm_lf "\n")

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

(defun fm-same (name a b &optional limit)
	;two layouts of the same forms give the same text
	(assert-eq name (format-lisp a limit) (format-lisp b limit)))

; --- the layout is made from the forms alone ---
(fm-same "however the lines were broken, the same forms give the same text"
	(fm-src "(defun f (a)" "(when (> a 0) (print a)" "(print b)) (g a))")
	(fm-src "(defun f" "  (a) (when" "(> a" "0)" "  (print a) (print b))" "      (g a))"))
(fm-check "a form that fits is one line" (fm-src "(defq a 1 b 2)") (fm-src "(defq a" "1" "b 2)"))
(fm-check "a definition that stands alone has its body on a line of its own" (fm-src "(defun f (a)" "\t(print a))")
	(fm-src "(defun f (a) (print a))"))
(fm-check "definitions of one body form, one after another, are each a line"
	(fm-src "(defun f (a) (print a))" "(defun g (a) (print a))" "" "(defun h (a)" "\t(print a))")
	(fm-src "(defun f (a)" "(print a))" "(defun g (a)" "(print a))" "" "(defun h (a) (print a))"))
(fm-check "comments between them are looked past"
	(fm-src "(defun f (a) (print a))" ";of g" ";and more" "(defun g (a) (print a))")
	(fm-src "(defun f (a) (print a))" ";of g" ";and more" "(defun g (a)" "(print a))"))
(fm-check "one among others with more than one body form is a line too, if it fits"
	(fm-src "(defun f (a) (print a))" "(defun g (a) (print a) (print a))")
	(fm-src "(defun f (a) (print a))" "(defun g (a)" "(print a)" "(print a))"))
(fm-check "a line with a comment after it stays a line, the docs take it as said of it"
	(fm-src "(defmacro a-long-macro-name (argument) `(f ,argument))" "; (a-long-macro-name argument) -> argument")
	(fm-src "(defmacro a-long-macro-name (argument) `(f ,argument))" "; (a-long-macro-name argument) -> argument") 40)
(fm-check "and one over lines with a comment after it is not made a line"
	(fm-src "(defun f (a) (print a))" "(defun g (a)" "\t(print a))" ";of what is next" "(h)")
	(fm-src "(defun f (a) (print a))" "(defun g (a)" "(print a))" ";of what is next" "(h)"))
(fm-check "one of them too long for the line is broken"
	(fm-src "(defun f (a) (print a))" "(defun a-long-function-name (argument)" "\t(print argument))")
	(fm-src "(defun f (a) (print a))" "(defun a-long-function-name (argument) (print argument))") 40)
(fm-check "with more than one body form, each has a line" (fm-src "(defun f (a)" "\t(print a)" "\t(print a))")
	(fm-src "(defun f (a) (print a) (print a))"))
(fm-check "a when with more than one body form has each on a line" (fm-src "(when a" "\t(b)" "\t(c))") (fm-src "(when a (b) (c))"))
(fm-check "cond has each clause on a line" (fm-src "(cond" "\t((= a 1) 1)" "\t(:t 2))")
	(fm-src "(cond ((= a 1) 1) (:t 2))"))
(fm-check "an if with the one else form is one line" (fm-src "(if a b c)") (fm-src "(if a" "b" "c)"))
(fm-check "an if with more than one else form has each on a line" (fm-src "(if a" "\tb" "\tc" "\td)") (fm-src "(if a b c d)"))
(fm-check "an if that does not fit has its then and else each on a line"
	(fm-src "(if (a-long-test-name a b)" "\t(a-long-then-name a b)" "\t(a-long-else-name a b))")
	(fm-src "(if (a-long-test-name a b) (a-long-then-name a b) (a-long-else-name a b))") 40)
(fm-check "the clause of :t has its first form beside it, the rest under that"
	(fm-src "(cond" "\t((= a 1)" "\t\t(print a)" "\t\t(print b))" "\t(:t (print c)" "\t\t(print d)))")
	(fm-src "(cond ((= a 1) (print a) (print b)) (:t (print c) (print d)))"))
(fm-check "an and of nothing but defqs has each on a line, under the and"
	(fm-src "(when (and" "\t\t(defq a (b))" "\t\t(defq c (d a)))" "\t(e c))")
	(fm-src "(when (and (defq a (b)) (defq c (d a))) (e c))"))
(fm-check "an and with a defq among other tests is as any and" (fm-src "(when (and (defq a (b)) (> a 0)) (e a))")
	(fm-src "(when (and" "(defq a (b))" "(> a 0))" "(e a))"))
(fm-check "a lambda with more than one body form has each on a line, and hangs"
	(fm-src "(each-thing a_name (lambda (x)" "\t\t(print x)" "\t\t(print x))" "\tb_name)")
	(fm-src "(each-thing a_name (lambda (x) (print x) (print x)) b_name)"))
(fm-check "two lambdas given to a form each start a line"
	(fm-src "(walk-nodes top" "\t(lambda (n)" "\t\t(push said n)" "\t\t:nil)" "\t(lambda (n) (push said n)))")
	(fm-src "(walk-nodes top (lambda (n) (push said n) :nil) (lambda (n) (push said n)))"))
(fm-check "a # of two forms that fits is one line" (fm-src "(each (# (push a %0) :nil) seq)")
	(fm-src "(each (#" "(push a %0)" ":nil) seq)"))
(fm-check "of more it has each on a line" (fm-src "(each (# (push a %0)" "\t\t(push b %0)" "\t\t:nil)" "\tseq)")
	(fm-src "(each (# (push a %0) (push b %0) :nil) seq)"))

; --- blocks, a width of their own, however much room the line has ---
(fm-check "a when wider than half the limit has its body on a line of its own"
	(fm-src "(when (or (empty? stack) (nql (str (last stack)) (str pos)))" "\t(push stack pos))")
	(fm-src "(when (or (empty? stack) (nql (str (last stack)) (str pos))) (push stack pos))"))
(fm-check "and its test, an or wider than half the limit, a condition to a line"
	(fm-src "(when (or (empty? *cursor_stack*)" "\t\t(nql (str (last *cursor_stack*)) (str pos)))" "\t(push *cursor_stack* pos))")
	(fm-src "(when (or (empty? *cursor_stack*) (nql (str (last *cursor_stack*)) (str pos))) (push *cursor_stack* pos))"))
(fm-check "a short one is a line" (fm-src "(when (> a_count 0) (print a_count))")
	(fm-src "(when (> a_count 0)" "(print a_count))"))
(fm-check "an if up to three quarters of the limit is a line"
	(fm-src "(if (or (empty? stack) (nql (str (last stack)) (str pos))) (push stack pos) (print stack))")
	(fm-src "(if (or (empty? stack) (nql (str (last stack)) (str pos)))" "(push stack pos)" "(print stack))"))
(fm-check "a wider one has its then and else each on a line"
	(fm-src "(if (or (empty? a_stack) (nql (str (last a_stack)) (str a_pos)))" "\t(push a_stack a_pos)" "\t(print a_stack a_pos))")
	(fm-src "(if (or (empty? a_stack) (nql (str (last a_stack)) (str a_pos))) (push a_stack a_pos) (print a_stack a_pos))"))
(fm-check "a lambda wider than two thirds of the limit has its body under it"
	(fm-src "(each (lambda (an_item)" "\t\t(print (cat (str an_item) (str (length an_item)) (str (first an_item)))))" "\ta_list)")
	(fm-src "(each (lambda (an_item) (print (cat (str an_item) (str (length an_item)) (str (first an_item))))) a_list)"))
(fm-check "a call or a binding has the whole line"
	(fm-src "(defq a_list (cat (list an_item (length an_item) (first an_item)) (list another (length another) (first another))))")
	(fm-src "(defq a_list (cat (list an_item (length an_item) (first an_item))"
		"(list another (length another) (first another))))"))

; --- indent ---
(fm-check "a line is a tab in for each form opened on the line it carries on"
	(fm-src "(each (lambda (x)" "\t\t(print x)" "\t\t(print x x x))" "\t(list 1 2 3))")
	(fm-src "(each (lambda (x) (print x) (print x x x)) (list 1 2 3))") 40)
(fm-check "VP blocks indent the forms between them, a switch is flat"
	(fm-src "(def-func 'a)" "\t(vpif '(:r0 = 0))" "\t\t(vp-cpy-rr :r0 :r1)" "\t(else)"
		"\t\t(vp-ret)" "\t(endif)" "\t(switch)" "\t(vpcase '(:r0 = 1))"
		"\t\t(break)" "\t(default)" "\t\t(vp-ret)" "\t(endswitch)"
		"(def-func-end)")
	(fm-src "(def-func 'a)" "(vpif '(:r0 = 0))" "(vp-cpy-rr :r0 :r1)" "(else)"
		"(vp-ret)" "(endif)" "(switch)" "(vpcase '(:r0 = 1))"
		"(break)" "(default)" "(vp-ret)" "(endswitch)"
		"(def-func-end)"))

; --- line breaks ---
(fm-check "a value too long is broken inside, beside its name"
	(fm-src "(setq a_name (a-function-name" "\t\t(another-function argument_one)))")
	(fm-src "(setq a_name (a-function-name (another-function argument_one)))") 40)
(fm-check "a name whose value does not fit starts a line"
	(fm-src "(defq a 1" "\tb (if (nempty? things)" "\t\t(apply (const cat) things)" "\t\t(list)))")
	(fm-src "(defq a 1 b (if (nempty? things) (apply (const cat) things) (list)))") 40)
(fm-check "a keyword and its value are not parted"
	(fm-src "(cwb-shape (cwb-d-rect 10 10 60 40)" "\t:fill 0xffff0000 :stroke 0xff000000" "\t:width 4)")
	(fm-src "(cwb-shape (cwb-d-rect 10 10 60 40) :fill 0xffff0000 :stroke 0xff000000 :width 4)") 40)
(fm-check "a short form is never broken" (fm-src "(print-something-long alpha beta" "\t'(b0 b1 b2) (+ c d))")
	(fm-src "(print-something-long alpha beta '(b0 b1 b2) (+ c d))") 40)
(fm-check "a form that will be over several lines starts a line of its own"
	(fm-src "(print a b" "\t(cond" "\t\t((= a 1) (print 1))" "\t\t(:t (print 2))))")
	(fm-src "(print a b (cond ((= a 1) (print 1)) (:t (print 2))))"))
(fm-check "what comes after a form over several lines starts a line"
	(fm-src "(print a" "\t(cond" "\t\t((< a 0) 0)" "\t\t(:t a))" "\tb)")
	(fm-src "(print a (cond ((< a 0) 0) (:t a)) b)"))
(fm-check "two that will be over several lines each start a line, neither hangs"
	(fm-src "(foo" "\t(list a_long_name b_long_name" "\t\tc_long_name)" "\t(list d_long_name e_long_name" "\t\tf_long_name))")
	(fm-src "(foo (list a_long_name b_long_name c_long_name) (list d_long_name e_long_name f_long_name))") 40)
(fm-check "one too long hangs only where its first argument fits"
	(fm-src "(bind '(x y)" "\t(gather (defq entry (push-attributes stack keys)) \qx\q \qy\q))")
	(fm-src "(bind '(x y) (gather (defq entry (push-attributes stack keys)) \qx\q \qy\q))") 60)
(fm-check "the method of a call is not the keyword of a value"
	(fm-src "(. a_long_object_name :find" "\t(str a_long_parent_name))")
	(fm-src "(. a_long_object_name :find (str a_long_parent_name))") 40)
(fm-check "only a keyword keeps what is after it"
	(fm-src "(spv-emit :vec3 145" "\t(apply fnc a_long_name b_long_name)" "\t(last ids))")
	(fm-src "(spv-emit :vec3 145 (apply fnc a_long_name b_long_name) (last ids))") 40)
(fm-check "a string is never broken" (fm-src "(print \qa long string that will not fit in the limit at all\q)")
	(fm-src "(print \qa long string that will not fit in the limit at all\q)") 40)
(fm-check "a string first stays beside what it is given to"
	(fm-src "(assert-true \qa long string that will not fit in the limit\q" "\t(a b))")
	(fm-src "(assert-true \qa long string that will not fit in the limit\q (a b))") 40)
(fm-check "the opening line of a definition is never broken"
	(fm-src "(defun a-long-function-name (argument_one argument_two)" "\t(print 1))")
	(fm-src "(defun a-long-function-name (argument_one argument_two) (print 1))") 40)
(fm-check "a list of forms alone, with a comment in it, is a body, a form to a line"
	(fm-src "(m ((a 1) (b 2)) (;the body" "\t\t(print a)" "\t\t(print b)))")
	(fm-src "(m ((a 1) (b 2)) (;the body" "(print a) (print b)))"))
(fm-check "without one it is data, filled" (fm-src "(m ((a 1) (b 2)) ((print a) (print b)))")
	(fm-src "(m ((a 1) (b 2))" "((print a)" "(print b)))"))
(fm-check "test cases are a form and what it gives to a line" (fm-src "(test-cases" "\t(+ 1 2) 3" "\t(* 2 2) 4)")
	(fm-src "(test-cases (+ 1 2) 3 (* 2 2) 4)"))

; --- tables ---
(fm-check "a table of atoms is rows of a power of two of them, the same in each"
	(fm-src "(defq a (list 1 2 3 4 5 6 7 8" "\t\t9 10 11 12 13 14 15 16" "\t\t17 18 19 20))")
	(fm-src "(defq a (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))") 40)
(fm-check "a table of bindings is rows of names and their values"
	(fm-src "(defq +a 1 +b 2 +c 3 +d 4" "\t+e 5 +f 6 +g 7 +h 8" "\t+i 9 +j 10)")
	(fm-src "(defq +a 1 +b 2 +c 3 +d 4 +e 5 +f 6 +g 7 +h 8 +i 9 +j 10)") 40)
(fm-check "each part of a table, between its blank lines, has its own rows"
	(fm-src "(defq +a 1 +b 2" "" "\t+c 3 +d 4 +e 5 +f 6" "\t+g 7 +h 8 +i 9 +j 10)")
	(fm-src "(defq +a 1 +b 2" "" "+c 3 +d 4 +e 5 +f 6 +g 7 +h 8 +i 9 +j 10)") 40)
(fm-check "a quoted list of lists takes as many as fit"
	(fm-src "(defq a '((a 1) (b 2) (c 3) (d 4) (e 5)" "\t\t(f 6) (g 7) (h 8) (i 9)))")
	(fm-src "(defq a '((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7) (h 8) (i 9)))") 40)
(fm-check "a key map is a key and its action to a line"
	(fm-src "(defq *key_map* (scatter (Fmap)" "\t\t1 action-a" "\t\t2 action-b))")
	(fm-src "(defq *key_map* (scatter (Fmap) 1 action-a 2 action-b))"))
(fm-check "a map of keys and the functions they are for is a key and its action to a line"
	(fm-src "(defq *event_map* (scatter (Fmap)" "\t\t+event_close action-close" "\t\t+event_min action-minimise"
		"\t\t+event_max action-maximise" "\t\t+event_open action-open-a-file))")
	(fm-src "(defq *event_map* (scatter (Fmap) +event_close action-close +event_min action-minimise +event_max action-maximise +event_open action-open-a-file))")
	50)
(fm-check "however much room there is"
	(fm-src "(defq *event_map* (scatter (Fmap)" "\t\t+event_close action-close" "\t\t+event_min action-minimise))")
	(fm-src "(defq *event_map* (scatter (Fmap) +event_close action-close +event_min action-minimise))"))
(fm-check "a scatter of names and other values is a table, rows of a power of two of them"
	(fm-src "(defq m (scatter (Fmap)" "\t\t+a 1 +b 2 +c 3 +d 4" "\t\t+e 5 +f 6 +g 7 +h 8))")
	(fm-src "(defq m (scatter (Fmap) +a 1 +b 2 +c 3 +d 4 +e 5 +f 6 +g 7 +h 8))") 40)

; --- what is kept ---
(fm-check "comments are kept, on their own lines and on the end of one"
	(fm-src "(defun f (a) ;the f" "\t; on its own" "\t(print a) ;tail" "\t(print b))")
	(fm-src "(defun f (a) ;the f" "; on its own" "(print a) ;tail" "(print b))"))
(fm-check "a comment right after an open bracket is in that list" (fm-src "(list ;first" "\ta b)") (fm-src "(list ;first" "a b)"))
(fm-check "a comment right after an open bracket stays on its line"
	(fm-src "(m a" "\t(;first" "\t\t(b))" "\t(;second" "\t\t(c)" "\t\t(d)))")
	(fm-src "(m a (;first" "(b)) (;second" "(c) (d)))"))
(fm-check "a blank line is kept, only ever one" (fm-src "(progn" "\t(a)" "" "\t(b))" "" "(c)")
	(fm-src "(progn (a)" "" "" "(b))" "" "" "" "(c)"))
(fm-check "a comment at the margin stays there, a note under a form is a tab in"
	(fm-src ";;;;" "; banner" ";;;;" "(defq a 1)" "\t; note" "(f)")
	(fm-src ";;;;" "; banner" ";;;;" "(defq a 1)" "  ; note" "(f)"))
(fm-check "the lines of a string are kept as they are" (fm-src "(print \qone  " {   two  } "  three\q a)")
	(fm-src "(print \qone  " {   two  } "  three\q a)"))
(fm-check "the help text of a command is kept as it is"
	(fm-src "(defq usage `(" "((\q-h\q \q--help\q)" "\qUsage: x" "" "    more\q)" "))")
	(fm-src "(defq usage `(" "((\q-h\q \q--help\q)" "\qUsage: x" "" "    more\q)" "))"))
(fm-check "a line with ;check off is kept as it is, check says nothing of it"
	(fm-src "(when a" "\t(cond (:nil 1)) ;check off" "\tb)") (fm-src "(when a" "(cond (:nil 1)) ;check off" "b)"))
(fm-check "and so is the form after one on a line of its own" (fm-src "(when a" "\t;check off" "\t(cond (:t))" "\tb)")
	(fm-src "(when a" ";check off" "(cond (:t)) b)"))

; --- what the source scanners read ---
(fm-check "a declaration is never broken" (fm-src "(dec-method :a_method class/name/a_method :static (:r0 :r1) (:r0))")
	(fm-src "(dec-method :a_method class/name/a_method :static (:r0 :r1) (:r0))") 40)
(fm-check "a VP instruction in a form is laid out as any form is" (fm-src "(when a (vp-ret))") (fm-src "(when a" "(vp-ret))"))
(fm-check "a form the includes command reads a line for starts a line"
	(fm-src "(when a" "\t(call :sys_mem :alloc '(:r0) '(:r0)))")
	(fm-src "(when a (call :sys_mem :alloc '(:r0) '(:r0)))"))
(fm-check "and so does one with a quote before it, it looks past it"
	(fm-src "(mathop :random" "\t'(call :sys_math :i_rand '(:r0) '(:r0)))")
	(fm-src "(mathop :random '(call :sys_math :i_rand '(:r0) '(:r0)))"))
(fm-check "a list that starts with such a word and names nothing is a list"
	(fm-src "(defun show (entry)" "\t(print entry))") (fm-src "(defun show" "(entry)" "(print entry))"))
(fm-check "and no line starts with one" (fm-src "(a-long-function-name a_long_argument (entry)" "\tb)")
	(fm-src "(a-long-function-name a_long_argument (entry) b)") 40)
(fm-check "no line of what is in a form is at the margin" (fm-src "(when a" "\t(import \qb\q)" "\t(import \qc\q))")
	(fm-src "(when a" "(import \qb\q)" "(import \qc\q))"))

; --- tidying ---
(assert-eq "a newline is added at the end" (fm-src "(a)") (format-lisp "(a)"))
(assert-eq "nothing gives nothing" "" (format-lisp ""))
(assert-eq "only blank lines give nothing" "" (format-lisp (fm-src "" "  " "")))

; --- on real source, only white space changes, and a second pass does nothing ---
;a Windows checkout may have CR LF line ends, the formatter works in LF
(defun fm-load (file)
	(apply (const cat) (split (load file) "\r")))
(each (lambda (file)
		(defq text (fm-load file) once (format-lisp text))
		(assert-eq (cat file ", same tokens") :t (eql (fm-squash text) (fm-squash once)))
		(assert-eq (cat file ", second pass") :t (eql once (format-lisp once))))
	'("class/lisp/root.inc" "lib/text/buffer.inc" "sys/heap/class.vp" "gui/region/class.vp"
		"apps/tui/tui.lisp" "lib/asm/vp.inc"))

; --- fmt formats itself, and finds nothing to do ---
(each (lambda (file)
		(assert-eq (cat file ", is as fmt would have it") :t (eql (defq text (fm-load file)) (format-lisp text))))
	'("lib/text/format.inc" "cmd/fmt.lisp" "tests/text/test_format.lisp"))
