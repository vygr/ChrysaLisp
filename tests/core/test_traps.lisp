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

(defun tr-quoted () (defq out '()) (push out 1))
(tr-quoted)
(assert-eq "a quoted list is the one list every time" 2 (length (tr-quoted)))
(defun tr-fresh () (defq out (list)) (push out 1))
(tr-fresh)
(assert-eq "a (list) is a new one" 1 (length (tr-fresh)))
(defq tr_a (list (list 1)) tr_b (cat tr_a))
(push (first tr_b) 2)
(assert-eq "a copy of a list has the same lists in it" 2 (length (first tr_a)))

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
