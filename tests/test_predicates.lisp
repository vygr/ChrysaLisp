;;;;;;;;;;;;;;;;;;;;;;;;;;
; tests/test_predicates.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;
(report-header "Predicates & State")

; Basic Primitives
(assert-true "nil? true"  (nil? :nil))
(assert-true "nil? false" (not (nil? 0)))

; Type Predicates
(assert-true "num? int"   (num? 123))
(assert-true "num? fixed" (num? 1.5))
(assert-true "fixed? true" (fixed? 1.5))
(assert-true "fixed? false" (not (fixed? 123)))
(assert-true "real? true"  (real? (n2r 1.0)))
(assert-true "str? true"   (str? "Hello"))
(assert-true "sym? true"   (sym? 'abc))
(assert-true "list? true"  (list? (list 1 2)))
(assert-true "array? true" (array? (array 1 2)))
(assert-true "env? true"   (env? (env)))

; Vector Types
(assert-true "nums? true"   (nums? (nums 1 2)))
(assert-true "fixeds? true" (fixeds? (fixeds 1.0 2.0)))
(assert-true "reals? true"  (reals? (reals (n2r 1.0))))

; Sequence Predicates
(assert-true "seq? list"   (seq? (list 1)))
(assert-true "seq? array"  (seq? (array 1)))
(assert-true "seq? string" (seq? "abc"))
(assert-true "seq? nums"   (seq? (nums 1)))

(assert-true "empty? list"  (empty? (list)))
(assert-true "empty? str"   (empty? ""))
(assert-true "nempty? list" (nempty? (list 1)))

; Functional Predicates
(assert-true "lambda? symbol" (lambda? 'lambda))
(assert-true "lambda? const"  (lambda? (const lambda)))
(assert-true "macro? symbol"  (macro? 'macro))
(assert-true "macro? const"   (macro? (const macro)))

; Note: lambda and macro as objects are lists, not :func
(assert-true "func? FFI" (func? +))
(assert-true "not func? lambda" (not (func? (lambda (x) x))))

(assert-true "lambda-func? true" (lambda-func? (lambda (x) x)))
(assert-true "macro-func? true"  (macro-func? (macro (x) x)))

; quote / quasi-quote
(assert-true "quote? symbol" (quote? 'quote))
(assert-true "quote? const"  (quote? (const quote)))
(assert-true "quasi-quote? symbol" (quasi-quote? 'quasi-quote))
(assert-true "quasi-quote? const"  (quasi-quote? (const quasi-quote)))

; atom? and msafe?
(assert-true "atom? symbol" (atom? :abc))
(assert-true "atom? number" (atom? 123))
(assert-true "atom? string" (atom? "abc"))
(assert-true "not atom? list" (not (atom? (list 1 2))))

(assert-true "msafe? symbol" (msafe? 'abc))
(assert-true "msafe? atom"   (msafe? 123))

; node predicates
; mail-nodes might be empty in this test environment
(defq nodes (mail-nodes))
(if (nempty? nodes)
    (assert-true "lisp-node? check" (or (lisp-node? (first nodes)) (cpp-node? (first nodes)))))

; eql vs = vs nql
(assert-true "eql strings" (eql "abc" "abc"))
(assert-true "eql nums"    (eql 123 123))
(assert-true "= ints"      (= 10 10))

; environment
(defq ep (env))
(def ep 'test_key 100)
(assert-eq "env get" 100 (get 'test_key ep))
(assert-true "env tolist find" (some (# (and (= (length %0) 2) (eql (first %0) 'test_key) (eql (second %0) 100))) (tolist ep)))

(defq my_env (env))
(def my_env 'test_sym 123)
(assert-eq "def?" 123 (def? 'test_sym my_env))
(undef my_env 'test_sym)
(assert-eq "undef" :nil (def? 'test_sym my_env))

(defq e1 (env))
(def e1 'a 1)
(defq e2 (env-copy e1 10))
(assert-eq "env-copy" 1 (get 'a e2))

(defq old_env (penv))
(env-push)
(defq inner_env (penv))
(assert-true "env-push" (nql old_env inner_env))
(env-pop)
(assert-eq "env-pop" old_env (penv))
