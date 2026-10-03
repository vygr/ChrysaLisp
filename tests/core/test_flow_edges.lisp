(report-header "Flow & Binding Edges: return values, optional args, scoping")

; --- only :nil is false ---
(test-cases
	(if 0 1 2) 1
	(if "" 1 2) 1
	(if (list) 1 2) 1
	(not :nil) :t
	(not 0) :nil
	(not (list)) :nil)

; --- what a conditional gives when it has nothing to run ---
(test-cases
	(if :t 1) 1
	(if :nil 1) :nil
	;the else is an implicit progn
	(if :nil 1 2 3) 3
	(ifn :nil 1) 1
	;with no else, ifn gives the test value
	(ifn :t 1) :t
	(ifn 5 1) 5
	(ifn :t 1 2 3) 3
	(when :t) :t
	(when :nil 1) :nil
	(when :t 1 2) 2
	(unless :t 1) :nil
	(unless :nil 1 2) 2
	(progn) :nil
	(progn 1 2 3) 3)

; --- cond and case ---
(test-cases
	(cond) :nil
	(cond (:nil 1)) :nil
	;a clause with no body gives its test value
	(cond (:t)) :t
	(cond (5)) 5
	(cond ((= 1 2) 1) ((= 1 1))) :t
	(cond (:nil 1) (:t 2 3)) 3
	(case 3 (1 :a) (2 :b)) :nil
	(case 2 (1 :a) (2 :b)) :b
	(case 3 (1 :a) (:t :z)) :z
	(case 2 ((1 2) :ab) (3 :c)) :ab
	(case :k (:j 1) (:k 2)) 2
	(case "b" ("a" 1) ("b" 2)) 2)

; --- and, or, with no and one argument ---
(test-cases
	(and) :t		(and 1) 1
	(and 1 2 3) 3	(and 1 :nil 3) :nil
	(or) :nil		(or :nil) :nil
	(or :nil 2 3) 2	(or :nil :nil) :nil)

; --- optional, rest and nested lambda arguments ---
(test-cases
	((lambda (&optional a b) (list a b))) '(:nil :nil)
	((lambda (&optional a b) (list a b)) 1) '(1 :nil)
	((lambda (a &optional b &rest c) (list a b c)) 1) '(1 :nil ())
	((lambda (a &optional b &rest c) (list a b c)) 1 2 3 4) '(1 2 (3 4))
	((lambda (&rest r) r)) '()
	((lambda (a &ignore) a) 1 2 3) 1
	((lambda ((a b) c) (list a b c)) (list 1 2) 3) '(1 2 3)
	((lambda ((a &rest b)) (list a b)) (list 1 2 3)) '(1 (2 3))
	((lambda ((a &optional b)) (list a b)) (list 1)) '(1 :nil)
	(apply + (list 1 2 3)) 6
	(apply cat (list "a" "b")) "ab"
	(apply list (list)) '()
	(apply (lambda (&rest r) (length r)) (list 1 2 3)) 3)

; --- bind over any sequence type ---
(defun flow-bind (params seq)
	(bind params seq)
	(map (const eval) (filter (# (not (starts-with "&" %0))) params)))

(test-cases
	(flow-bind '(a b c) "xyz") '("x" "y" "z")
	(flow-bind '(a b) (nums 5 6)) '(5 6)
	;the rest of a string is a string
	(flow-bind '(a &rest b) "xyz") '("x" "yz")
	(flow-bind '(&rest b) (list)) '(())
	(flow-bind '(&most m l) (list 1 2 3)) '((1 2) 3)
	(flow-bind '(&most m l) (list 1)) '(() 1)
	(flow-bind '(a & c) (list 1 2 3)) '(1 3)
	(flow-bind '(&optional a) (list)) '(:nil)
	(flow-bind '() (list)) '()
	(flow-bind '(&ignore) (list 1 2)) '())

; --- defq, setq, setd ---
(defq flow_a 1 flow_b flow_a)
(assert-eq "defq sees earlier pair" 1 flow_b)
(defq flow_c 5)
(setd flow_c 9)
(assert-eq "setd leaves a value" 5 flow_c)
(defq flow_d :nil flow_e 2)
(setd flow_d 1 flow_e 3)
(assert-eq "setd sets :nil" 1 flow_d)
(assert-eq "setd pair leaves a value" 2 flow_e)

; --- dynamic scope: a function sees its caller's variables ---
(defq flow_v 1)
((lambda () (setq flow_v 2)))
(assert-eq "setq in a call updates the caller" 2 flow_v)
((lambda () (defq flow_v 3)))
(assert-eq "defq in a call is local" 2 flow_v)
((lambda (flow_v) (setq flow_v 4)) 5)
(assert-eq "setq of an argument is local" 2 flow_v)

(defun flow-free () flow_dyn)
(defq flow_dyn 7)
(assert-eq "free variable from definer scope" 7 (flow-free))
(assert-eq "free variable from caller scope" 9 ((lambda (flow_dyn) (flow-free)) 9))

(assert-eq "let binds" 3 (let ((a 1) (b 2)) (+ a b)))
(assert-eq "let* sees earlier" 3 (let* ((a 1) (b (+ a 1))) (+ a b)))
(assert-eq "let empty" 5 (let () 5))
(assert-eq "let shadows" 9 (let ((flow_v 9)) flow_v))
(let ((flow_v 9)) (setq flow_v 10))
(assert-eq "let setq is local" 2 flow_v)

; --- environments ---
(defq flow_env (env 1))
(def flow_env 'x 1 'y 2)
(assert-list-eq "env def get" '(1 2 :nil) (list (get 'x flow_env) (get 'y flow_env) (get 'z flow_env)))
(set flow_env 'x 5)
(assert-eq "env set" 5 (get 'x flow_env))
(undef flow_env 'x)
(assert-eq "env undef" :nil (get 'x flow_env))
(def flow_env 'y 3)
(assert-eq "env def again replaces" 1 (length (tolist flow_env)))
(assert-eq "env has no parent" :nil (penv flow_env))
(assert-eq "get unbound" :nil (get 'flow_no_such_symbol))
(assert-eq "def? is this env only" :nil (def? 'first))
(assert-true "def? local" (def? 'flow_env))
(assert-true "env? env" (env? flow_env))
(assert-true "env? list" (not (env? (list))))

;a pushed env reads and sets as plain variables, defq goes into it
(defq flow_outer 7)
(env-push flow_env)
(setq y 4 flow_outer 8)
(defq flow_inner (list y flow_outer))
(defq flow_popped (env-pop))
(assert-true "env-pop gives the pushed env" (eql flow_popped flow_env))
(assert-eq "setq through a pushed env" 4 (get 'y flow_env))
(assert-eq "setq of an outer variable" 8 flow_outer)
(assert-list-eq "defq lands in the pushed env" '(4 8) (get 'flow_inner flow_env))
(assert-eq "defq did not land outside" :nil (def? 'flow_inner))

; --- eval, quote, quasi-quote, macro expansion ---
(defq flow_q 5)
(test-cases
	(eval 5) 5
	(eval '(+ 1 2)) 3
	(eval (list + 1 2)) 3
	(eval (list 'quote 'x)) 'x
	(identity :nil) :nil
	(const (+ 1 2)) 3
	(static-q (a b)) '(a b)
	(static-qq (a ,flow_q ~(list 1 2))) '(a 5 1 2)
	`(a ,(+ 1 2) ~(list 4 5)) '(a 3 4 5)
	`() '()
	`a 'a
	(macroexpand 5) 5
	(exec '(inc 1)) 2
	(lambda-func? (lambda ())) :t
	(lambda-func? first) :nil
	(macro-func? when) :t)

; --- catch, the handler must give non :nil to stop the error ---
(test-cases
	(catch 5 :t) 5
	(catch (throw "x" 1) :t) :t
	(catch (throw "x" 1) (progn 99)) 99
	(catch (throw "x" 1) (found? _ "x")) :t
	;an inner handler that gives :nil passes the error out
	(catch (catch (throw "in" 1) :nil) :t) :t
	(catch (progn (catch (throw "in" 1) :t) 7) :t) 7)
