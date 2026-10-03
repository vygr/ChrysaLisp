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
(assert-true "list?? true"  (list?? (list 1 2)))
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

; eql vs = vs nql
(assert-true "eql strings" (eql "abc" "abc"))
(assert-true "eql nums"	(eql 123 123))
(assert-true "= ints"	  (= 10 10))

; environment
(defq ep (env 1))
(def ep 'test_key 100)
(assert-eq "env get" 100 (get 'test_key ep))
(assert-true "env tolist find" (some (# (and (= (length %0) 2) (eql (first %0) 'test_key) (eql (second %0) 100))) (tolist ep)))

(defq my_env (env 1))
(def my_env 'test_sym 123)
(assert-eq "def?" 123 (def? 'test_sym my_env))
(undef my_env 'test_sym)
(assert-eq "undef" :nil (def? 'test_sym my_env))

(defq e1 (env 1))
(def e1 'a 1)
(defq e2 (env-copy e1 10))
(assert-eq "env-copy" 1 (get 'a e2))

; (env-push) pushes a new empty env, (env-pop) returns the popped env
(defq old_env (env) inner_env :nil pushed_env :nil)
(setq pushed_env (env-push) inner_env (env))
(defq inner_val 1)
(defq popped_env (env-pop))
(assert-true "env-push" (nql old_env inner_env))
(assert-true "env-push ret" (eql pushed_env inner_env))
(assert-true "env-pop" (eql old_env (env)))
(assert-true "env-pop ret" (eql popped_env inner_env))
(assert-eq "env-pop parent" :nil (penv popped_env))
(assert-eq "env-push local" 1 (get 'inner_val popped_env))
(assert-eq "env-push no leak" :nil (def? 'inner_val))
(undef (env) 'old_env 'inner_env 'pushed_env 'popped_env)

; (env-push env) pushes the given env as the current scope
(defq old_env (env) e1 (env 1) inner_ok :nil)
(def e1 'pushed_val 42)
(assert-eq "env-push env parent" :nil (penv e1))
(defq pushed_env (env-push e1))
(setq inner_ok (and (eql (env) e1) (eql (penv) old_env) (= pushed_val 42))
	pushed_val 43)
(defq e2 (env-pop))
(assert-true "env-push env" inner_ok)
(assert-true "env-push env ret" (eql (get 'pushed_env e1) e1))
(assert-true "env-pop env" (and (eql (env) old_env) (eql e2 e1)))
(assert-eq "env-pop env parent" :nil (penv e1))
(assert-eq "env-push env setq" 43 (get 'pushed_val e1))
(assert-eq "env-pop env no leak" :nil (def? 'pushed_val))

; popped env can be pushed again, and pushes nest
(defq e3 (env 1) nest_ok :nil)
(def e3 'nested_val 7)
(env-push e1)
(env-push e3)
(setq nest_ok (and (eql (penv) e1) (eql (penv e1) old_env) (= nested_val 7))
	pushed_val (+ pushed_val nested_val))
(env-pop)
(setq nest_ok (and nest_ok (eql (env) e1) (not (penv e3))))
(env-pop)
(assert-true "env-push nested" nest_ok)
(assert-true "env-pop nested" (eql (env) old_env))
(assert-eq "env-push again" 50 (get 'pushed_val e1))
(assert-eq "env-pop nested parent" :nil (penv e1))
(undef e1 'pushed_env)
(undef (env) 'old_env 'e1 'e2 'e3 'inner_ok 'nest_ok)
