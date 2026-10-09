(report-header "Lisp: the traps of docs/ai_digest/lisp_traps.md are as it says")

;each of these is in the doc as a thing that catches people out. If one
;fails the system has changed, and the doc is to be changed with it
(defq +tr_list (list 1 2 3) +tr_quoted ''(1 2 3))
(assert-error "a + constant that is a list is run as a form"
	(progn (defun tr-f1 () (first +tr_list)) (tr-f1)))
(assert-eq "quoted twice it is a list" 1 (progn (defun tr-f2 () (first +tr_quoted)) (tr-f2)))
(assert-eq "and so is a static-q" 1 (progn (defun tr-f3 () (first (static-q (1 2 3)))) (tr-f3)))

(defun tr-case (op) (case op (+ :plus) (- :minus) (:t :other)))
(assert-eq "a case key of + is not the symbol" :other (tr-case '+))
(assert-eq "one of - is" :minus (tr-case '-))

(assert-error "a parameter with the name of a macro"
	(eval (read (string-stream "(progn (defun tr-f4 (bits) bits) (tr-f4 5))"))))

(defun tr-and-fn (&optional num) (and num (> num 0)))
(defun tr-and-ok (&optional cnt) (and cnt (> cnt 0)))
(if (starts-with "obj/vp64" (load-path))
	(test-skip "a local with the name of a function, alone in an and" "needs an error checked build")
	(assert-error "a local with the name of a function, alone in an and, is called" (tr-and-fn)))
(assert-eq "with another name it is looked at" :nil (tr-and-ok))

(assert-error "a # with no %0 in it takes no arguments" (map (# (!)) (list 1 2 3)))
(assert-list-eq "a lambda that ignores what it is given does" '(0 1 2) (map (lambda (&) (!)) (list 1 2 3)))
(assert-list-eq "and one that ignores all it is given, however many" '(0 1 2)
	(map (lambda (&ignore) (!)) (list 1 2 3) (list 4 5 6)))
(assert-list-eq "an & takes a thing and binds nothing, the second of two here" '(4 5 6)
	(map (lambda (& b) b) (list 1 2 3) (list 4 5 6)))
(assert-true "where a _ is a name, and is bound in the function" (first (map (lambda (_) (def? '_ (env))) (list 7))))
(assert-eq "and an & is not" :nil (first (map (lambda (&) (def? '& (env))) (list 7))))

(defun tr-setd (&optional listen) (setd listen :t) listen)
(assert-eq "an optional that defaults to :t can not be passed :nil" :t (tr-setd :nil))

(defun tr-helper () (setq tr_total 99))
(defun tr-caller () (defq tr_total 1) (tr-helper) tr_total)
(assert-eq "a function can setq a local of its caller" 99 (tr-caller))

(env-push)
(defun tr-mod-self (n) (if (> n 0) (tr-mod-self (dec n)) :bottom))
(defun tr-mod-first (n) (tr-mod-second n))
(defun tr-mod-second (n) n)
(defun tr-mod-inner (n) (if (> n 0) (tr-mod-inner (dec n)) :bottom))
(defun tr-mod-outer (n) (tr-mod-inner n))
(export-symbols '(tr-mod-self tr-mod-first tr-mod-outer))
(env-pop)
(assert-error "one that is not exported can not" (tr-mod-outer 3))
(assert-eq "though it can be called, if it does not" :bottom (tr-mod-outer 0))
(assert-eq "an exported function of a module can call itself" :bottom (tr-mod-self 3))
(assert-error "a function of a module can not call one below it" (tr-mod-first 3))

(assert-eq "an empty list is true" :yes (if (list) :yes :no))
(assert-eq "0 is true" :yes (if 0 :yes :no))
(assert-eq "an empty str is true" :yes (if "" :yes :no))

(assert-eq "the first of :nil is its first character" ":" (first :nil))
(assert-eq "so the first of the first of nothing is true" :yes (if (first (first (list))) :yes :no))

(defun tr-quoted () (defq out '()) (push out 1))
(tr-quoted)
(assert-eq "a quoted list is the one list every time" 2 (length (tr-quoted)))
(defun tr-fresh () (defq out (list)) (push out 1))
(tr-fresh)
(assert-eq "a (list) is a new one" 1 (length (tr-fresh)))
(defq tr_a (list (list 1)) tr_b (cat tr_a))
(push (first tr_b) 2)
(assert-eq "a cat of a list has the same lists in it" 2 (length (first tr_a)))
(defq tr_n (nums 1 2) tr_deep (list 1 (list 2 (list 3)) tr_n) tr_c (copy tr_deep))
(push (second (second tr_c)) 4)
(assert-list-eq "a copy has lists of its own, all the way down" '(3) (second (second tr_deep)))
(assert-list-eq "and they are the copy's" '(3 4) (second (second tr_c)))
(assert-true "what is not a list is the same one in both, a nums here" (progn (elem-set (third tr_c) 0 9) (= (first tr_n) 9)))
(assert-true "copy of what is not a list gives it back" (progn (elem-set (copy tr_n) 1 8) (= (second tr_n) 8)))
(assert-eq "copy of a map is a plain list, not a map" :nil (find :hmap (type-of (copy (Fmap 1)))))
;which object a thing is, is what (weak-ref) gives. So every list of a
;copy can be shown to be a new one, and what is not a list the same one
(defun tr-refs (form)
	;the object of each list in a form, itself and all inside it, and of
	;each thing that is not a list
	(defq lists (list) others (list) stack (list form))
	(while (defq it (pop stack))
		(cond
			((list?? it) (push lists (weak-ref it)) (each (# (push stack %0)) it))
			((array? it) (push others (weak-ref it)))))
	(list lists others))
(defq tr_big (list (list 1 (list 2 (list 3 (nums 4)))) (list (list) (list (list (fixeds 5.0)))) (nums 6))
	tr_orig (tr-refs tr_big) tr_copy (tr-refs (copy tr_big)) tr_cat (tr-refs (cat tr_big)))
(assert-list-eq "a form of 8 lists and 3 vectors of numbers" '(8 3) (map (const length) tr_orig))
(assert-list-eq "its copy has as many" '(8 3) (map (const length) tr_copy))
(assert-true "and not one of the copy's lists is a list of the original"
	(notany (# (find %0 (first tr_orig))) (first tr_copy)))
(assert-list-eq "what is not a list is the same objects, every one" (sort (cat (second tr_orig)) (const -)) (sort (cat (second tr_copy)) (const -)))
(assert-eq "a cat has one new list, the top, and the other 7 are the original's" 7
	(length (filter (# (find %0 (first tr_orig))) (first tr_cat))))

(defq tr_mbox (mail-mbox) tr_sent (str-alloc 8))
(mail-send tr_mbox tr_sent)
(set-long (mail-read tr_mbox) 0 5)
(assert-eq "a str mailed on the one node is the str itself" 5 (get-long tr_sent 0))

(assert-error "an integer and a fixed do not add" (+ 1 1.5))
(assert-error "a real and a fixed do not add" (+ (n2r 1.5) 1.5))
(assert-true "an integer is not equal to a fixed of the same worth" (not (= 1 1.0)))
(assert-eq "changed to a fixed it adds" 2.5 (+ (n2f 1) 1.5))
(assert-eq "a fixed is cut as it is read" "0.00099" (str 0.001))
(assert-true "a real is a real, a fixed and a num"
	(and (real? (n2r 1.5)) (fixed? (n2r 1.5)) (num? (n2r 1.5))))
(assert-true "a fixed is not a real" (not (real? 1.5)))
(assert-eq "num? of a num is 0, which is true" 0 (num? 3))
(assert-list-eq "a type is the chain of classes, its own last" '(:num :fixed :real) (type-of (n2r 1.5)))
(assert-true "a predicate with one ? is true of a class built on its own" (and (num? (n2r 1.5)) (fixed? (n2r 1.5))))
(assert-true "so a map is a list" (list? (Fmap 1)))
(assert-eq "a predicate with two asks if it is that class itself" :t (list?? (list)))
(assert-eq "and a map is not" :nil (list?? (Fmap 1)))

(assert-eq "trim with characters not in order trims nothing" 6 (length (trim (cat "  ab " (ascii-char 10)) " \t\r\n")))
(assert-eq "with a class of them it does" "ab" (trim (cat "  ab " (ascii-char 10)) (char-class " \t\r\n")))
(assert-eq "find of a str in a str is of its first character" 1 (find "elx" "hello"))
(assert-list-eq "substr is the one for a str in a str" '(2 4) (first (first (substr "hello" "ll"))))
(assert-error "sort of numbers with nothing said" (sort (list 3 1 2)))
(defq tr_sort (list 3 1 2))
(sort tr_sort (const -))
(assert-list-eq "sort told how, and it is the list given that is sorted" '(1 2 3) tr_sort)

(assert-error "a wrong number of args is an error, in the checked build" ((lambda (a) a) 1 2))
(assert-eq "a handler that gives :nil passes the error on" :outer
	(catch (catch (throw "inner" 1) :nil) :outer))
(assert-eq "one that gives :t ends it" :t (catch (throw "inner" 1) :t))

(undef (env) 'tr_a 'tr_b 'tr_sort 'tr_mbox 'tr_sent)
