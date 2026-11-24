(report-header "GPU: shader language, type checker")

(import "lib/gpu/glsl.inc")
(import "lib/gpu/msl.inc")
(import "lib/gpu/spirv.inc")
(import "lib/gpu/vp.inc")
(import "lib/hash/sums.inc")

(defq sh_lf "\n")

(defun sh-src (&rest lines)
	;a program from the lines of its source
	(shader-compile (shader-read (string-stream (join lines sh_lf)))))

(defun sh-main (&rest lines)
	;a program that is a main with this body
	(sh-src (cat "(defun main :vec4 ((frag :vec2)) " (join lines " ") ")")))

(defun sh-pixel (program &optional vals x y)
	;the vec4 the native code gives for one pixel, as fixeds
	(setd x 0 y 0)
	(defq native (shader-vp program))
	(map (const n2q) (first (shader-vp-pixels native
				(shader-vp-frame program native vals) x y (inc x) (inc y)))))

(defun sh-near? (a b)
	(and (= (length a) (length b))
		(every (# (< (abs (- %0 %1)) 0.0015)) a b)))

(defmacro assert-pixel (name expected program &rest args)
	; (assert-pixel name expected program [vals x y])
	;the native code must give the pixel
	(defq res (gensym) exp (gensym))
	`(progn
		(defq ,exp ,expected ,res (sh-pixel ,program ~args))
		(if (sh-near? ,exp ,res)
			(test-pass ,name)
			(test-fail ,name ,exp ,res))))

;the tree for a small program
(defq prog (sh-src
		"(definput k :float 1.5 0.0 4.0)"
		"(definput n :int 3)"
		"(definput size :vec2)"
		"(defconst two 2.0)"
		"(defglobal g (* k two))"
		"(defun half :float ((a :float)) (return (* a 0.5)))"
		"(defun main :vec4 ((frag :vec2)) (return (vec4 (half g) frag 1.0)))"))
(bind '(inputs consts globals funcs &ignore) prog)
(assert-list-eq "inputs" '((k :float 1.5 0.0 4.0) (n :int 3 :nil :nil) (size :vec2 0 :nil :nil)) inputs)
(assert-list-eq "const" '((two :float (:float :lit "2.0000"))) consts)
(assert-eq "global type" :float (second (first globals)))
(assert-list-eq "function names" '(half main) (map (const first) funcs))
(assert-list-eq "function types" '(:float :vec4) (map (const second) funcs))
(assert-list-eq "return statement" '(:return (:float :op * ((:float :var a :param) (:float :lit "0.5000"))))
	(first (last (first funcs))))

;float literals are decimals, to 4 places from the reader, or as given in a str
(defq prog (sh-src "(defconst a 0.001)" "(defconst b -43758.5453)" {(defconst c "3.1415926535898")} {(defconst d "7")}
		"(defun main :vec4 ((frag :vec2)) (return (vec4 a b c d)))"))
(assert-list-eq "decimal literals" '("0.0010" "-43758.5453" "3.1415926535898" "7.0")
	(map (# (third (third %0))) (second prog)))

;types of the ops
(defun sh-local-type (program)
	;the type of the first local of main
	(first (third (first (last (first (elem-get program 3)))))))

(test-cases
	(sh-local-type (sh-main "(defq a (* (vec3 1.0) 2.0))" "(return (vec4 a 1.0))")) :vec3
	(sh-local-type (sh-main "(defq a (- 1.0 (vec2 1.0)))" "(return (vec4 a a))")) :vec2
	(sh-local-type (sh-main "(defq a (:xy (vec4 1.0)))" "(return (vec4 a a))")) :vec2
	(sh-local-type (sh-main "(defq a (:rgb (vec4 1.0)))" "(return (vec4 a 1.0))")) :vec3
	(sh-local-type (sh-main "(defq a (dot frag frag))" "(return (vec4 a))")) :float
	(sh-local-type (sh-main "(defq a (< 1 2))" "(return (vec4 1.0))")) :bool
	(sh-local-type (sh-main "(defq a (+ 1 2))" "(return (vec4 1.0))")) :int
	(sh-local-type (sh-main "(defq a (vec3 1 frag))" "(return (vec4 a 1.0))")) :vec3)

;what the checker must refuse
(assert-error "unknown name" (sh-main "(return (vec4 nope))") {Shader, unknown name !})
(assert-error "unknown function" (sh-main "(return (nope 1.0))") {Shader, unknown function !})
(assert-error "unknown statement" (sh-main "(print 1.0)" "(return (vec4 1.0))") {Shader, unknown statement !})
(assert-error "int with float" (sh-main "(return (vec4 (+ 1 2.0)))") {Shader, wrong types !})
(assert-error "vec2 with vec3" (sh-main "(return (vec4 (+ (vec2 1.0) (vec3 1.0)) 1.0))") {Shader, wrong types !})
(assert-error "test is not a bool" (sh-main "(if 1.0 (return (vec4 1.0)))" "(return (vec4 0.0))") {Shader, test is not a bool !})
(assert-error "wrong return type" (sh-main "(return 1.0)") {Shader, wrong return type !})
(assert-error "no return" (sh-main "(defq a 1.0)") {Shader, function does not return !})
(assert-error "no return on one path" (sh-main "(if (> (:x frag) 1.0) (return (vec4 1.0)))") {Shader, function does not return !})
(assert-error "local hides a parameter" (sh-main "(defq frag 1.0)" "(return (vec4 1.0))") {Shader, name is already in use !})
(assert-error "local hides a local" (sh-main "(defq a 1.0)" "(when (> a 0.0) (defq a 2.0))" "(return (vec4 a))") {Shader, name is already in use !})
(assert-error "local out of scope" (sh-main "(when (> (:x frag) 0.0) (defq a 2.0))" "(return (vec4 a))") {Shader, unknown name !})
(assert-error "local is a reserved word" (sh-main "(defq sin 1.0)" "(return (vec4 1.0))") {Shader, name is a reserved word !})
(assert-error "setq changes the type" (sh-main "(defq a 1.0)" "(setq a 1)" "(return (vec4 a))") {Shader, wrong types !})
(assert-error "setq of a loop counter" (sh-main "(for (i 0 4) (setq i 2))" "(return (vec4 1.0))") {Shader, can only set a local, a parameter or a varying !})
(assert-error "setq of a const" (sh-src "(defconst c 1.0)" "(defun main :vec4 ((frag :vec2)) (setq c 2.0) (return (vec4 c)))") {Shader, can only set a local, a parameter or a varying !})
(assert-error "setq of an input" (sh-src "(definput k :float)" "(defun main :vec4 ((frag :vec2)) (setq k 2.0) (return (vec4 k)))") {Shader, can only set a local, a parameter or a varying !})
(assert-error "component out of range" (sh-main "(return (vec4 (:z frag)))") {Shader, component out of range !})
(assert-error "component set twice" (sh-main "(defq a (vec2 1.0))" "(setq (:xx a) (vec2 1.0))" "(return (vec4 a a))") {Shader, wrong types !})
(assert-error "break outside a loop" (sh-main "(break)" "(return (vec4 1.0))") {Shader, break outside a loop !})
(assert-error "loop bound is not a constant" (sh-src "(definput n :int)" "(defun main :vec4 ((frag :vec2)) (for (i 0 n)) (return (vec4 1.0)))") {Shader, loop bound is not a constant int !})
(assert-error "const from an input" (sh-src "(definput k :float)" "(defconst c (* k 2.0))" "(defun main :vec4 ((frag :vec2)) (return (vec4 c)))") {Shader, not a constant !})
(assert-error "wrong args to function" (sh-src "(defun f :float ((a :float)) (return a))" "(defun main :vec4 ((frag :vec2)) (return (vec4 (f 1))))") {Shader, wrong args to function !})
(assert-error "recursion" (sh-src "(defun f :float ((a :float)) (return (f a)))" "(defun main :vec4 ((frag :vec2)) (return (vec4 (f 1.0))))") {Shader, unknown function !})
(assert-error "a main that is neither kind" (sh-src "(defun main :float ((a :float)) (return a))") {Shader, no (defun main :vec4 ((frag :vec2)) ...), or (defun main :vec4 () ...) for a vertex shader !})
(assert-error "wrong main" (sh-src "(defun main :vec3 ((frag :vec2)) (return (vec3 1.0)))") {Shader, no (defun main :vec4 ((frag :vec2)) ...), or (defun main :vec4 () ...) for a vertex shader !})
(assert-error "vector input with a default" (sh-src "(definput v :vec2 1.0)" "(defun main :vec4 ((frag :vec2)) (return (vec4 1.0)))") {Shader, a vector or matrix input has no default !})
(assert-error "float input with an int default" (sh-src "(definput k :float 1)" "(defun main :vec4 ((frag :vec2)) (return (vec4 1.0)))") {Shader, wrong input value !})
(assert-error "not a number" (sh-main {(return (vec4 "1.0e3"))}) {Shader, not a number !})
;the declarations were once input, const and global
(assert-error "input is not a declaration" (sh-src "(input k :float)" "(defun main :vec4 ((frag :vec2)) (return (vec4 k)))") {Shader, unknown declaration !})
(assert-error "const is not a declaration" (sh-src "(const c 1.0)" "(defun main :vec4 ((frag :vec2)) (return (vec4 c)))") {Shader, unknown declaration !})
(assert-error "global is not a declaration" (sh-src "(global g 1.0)" "(defun main :vec4 ((frag :vec2)) (return (vec4 g)))") {Shader, unknown declaration !})

;a cond is the ifs it is, as the value of a function and as a statement
(assert-list-eq "a cond as the value is its ifs"
	(last (sh-main "(if (> (:x frag) 1.0) (vec4 1.0) (if (> (:x frag) 0.5) (vec4 0.5) (vec4 0.0)))"))
	(last (sh-main "(cond ((> (:x frag) 1.0) (vec4 1.0)) ((> (:x frag) 0.5) (vec4 0.5)) (:t (vec4 0.0)))")))
(assert-list-eq "a cond as a statement, no :t, is its ifs"
	(last (sh-main "(defq a 0.0)" "(if (> (:x frag) 1.0) (setq a 1.0) (if (> (:x frag) 0.5) (progn (setq a 0.5) (setq a (* a 2.0)))))" "(vec4 a)"))
	(last (sh-main "(defq a 0.0)" "(cond ((> (:x frag) 1.0) (setq a 1.0)) ((> (:x frag) 0.5) (setq a 0.5) (setq a (* a 2.0))))" "(vec4 a)")))
(assert-error "a cond with no clauses" (sh-main "(cond)" "(return (vec4 1.0))") {Shader, a cond with no clauses !})
(assert-error "a cond clause with no forms" (sh-main "(cond ((> (:x frag) 1.0)))" "(return (vec4 1.0))") {Shader, a cond clause is (test form ...) !})
(assert-error "a :t clause before the last" (sh-main "(cond (:t (vec4 1.0)) ((> (:x frag) 1.0) (vec4 0.5)))") {Shader, the :t clause of a cond is its last !})
(assert-error "a cond not returning on every path" (sh-main "(cond ((> (:x frag) 1.0) (vec4 1.0)))") {Shader, function does not return !})

;the last form of a function is its value
(assert-list-eq "last form is the value"
	(last (sh-src "(defun f :float ((a :float)) (return (* a 2.0)))" "(defun main :vec4 ((frag :vec2)) (return (vec4 (f 1.0))))"))
	(last (sh-src "(defun f :float ((a :float)) (* a 2.0))" "(defun main :vec4 ((frag :vec2)) (vec4 (f 1.0)))")))
(assert-list-eq "last form of each arm of a last if"
	(last (sh-main "(if (> (:x frag) 1.0) (return (vec4 1.0)) (return (vec4 0.0)))"))
	(last (sh-main "(if (> (:x frag) 1.0) (vec4 1.0) (vec4 0.0))")))
(assert-list-eq "last form of a last progn, and of an if in it"
	(last (sh-main "(defq a 1.0)" "(if (> a 0.0) (progn (setq a 2.0) (return (vec4 a))) (return (vec4 0.0)))"))
	(last (sh-main "(defq a 1.0)" "(if (> a 0.0) (progn (setq a 2.0) (vec4 a)) (vec4 0.0))")))
(assert-list-eq "a name as the value"
	(last (sh-main "(defq v (vec4 0.5))" "(return v)"))
	(last (sh-main "(defq v (vec4 0.5))" "v")))
(assert-list-eq "a return can still be said"
	(last (sh-main "(if (> (:x frag) 1.0) (return (vec4 1.0)))" "(return (vec4 0.0))"))
	(last (sh-main "(if (> (:x frag) 1.0) (return (vec4 1.0)))" "(vec4 0.0)")))
(assert-error "value of the wrong type" (sh-main "1.0") {Shader, wrong return type !})
(assert-error "a value that is not last" (sh-main "(vec4 1.0)" "(vec4 0.0)") {Shader, unknown statement !})
(assert-error "an if with one arm is no value" (sh-main "(if (> (:x frag) 1.0) (vec4 1.0))") {Shader, function does not return !})
(assert-error "a when is no value" (sh-main "(when (> (:x frag) 1.0) (vec4 1.0))") {Shader, unknown statement !})
(assert-error "a loop is no value" (sh-main "(for (i 0 4) (vec4 1.0))") {Shader, unknown statement !})

(report-header "GPU: shader language, what native code gives")

;scalars
(assert-pixel "arithmetic" '(3.0 -5.0 0.25 14.0)
	(sh-main "(return (vec4 (+ 1.0 2.0) (- 5.0) (/ 1.0 4.0) (+ 2.0 (* 3.0 4.0))))"))
(assert-pixel "ints" '(5.0 3.0 2.0 -6.0)
	(sh-main "(return (vec4 (float (+ 2 3)) (float (/ 7 2)) (float (int 2.7)) (float (* 2 (- 3)))))"))
(assert-pixel "floor fract mod abs" '(-1.0 0.75 1.75 0.25)
	(sh-main "(defq a -0.25)" "(return (vec4 (floor a) (fract a) (mod a 2.0) (abs a)))"))
(assert-pixel "min max clamp mix" '(1.0 2.0 1.0 1.5)
	(sh-main "(defq a 1.0 b 2.0)" "(return (vec4 (min a b) (max a b) (clamp 5.0 0.0 a) (mix a b 0.5)))"))
(assert-pixel "pow" '(8.0 1.4142 0.125 0.0)
	(sh-main "(return (vec4 (pow 2.0 3.0) (pow 2.0 0.5) (pow 4.0 -1.5) (pow 0.0 2.0)))"))
(assert-pixel "sin cos sqrt" '(0.0 1.0 3.0 0.8415)
	(sh-main "(return (vec4 (sin 0.0) (cos 0.0) (sqrt 9.0) (sin 1.0)))"))
(assert-pixel "exp log" '(1.0 2.7183 0.0 0.6931)
	(sh-main "(return (vec4 (exp 0.0) (exp 1.0) (log 1.0) (log 2.0)))"))
(assert-pixel "tanh" '(0.0 0.7616 -0.964 1.0)
	(sh-main "(return (vec4 (tanh 0.0) (tanh 1.0) (tanh -2.0) (tanh 20.0)))"))
(assert-pixel "exp log of a vector" '(0.3679 7.3891 -0.6931 2.3026)
	(sh-main "(defq a (vec2 -1.0 2.0) b (vec2 0.5 10.0))"
		"(return (vec4 (exp a) (log b)))"))
(assert-pixel "compare and logic" '(1.0 1.0 1.0 1.0)
	(sh-main "(defq a 0.0 b 0.0 c 0.0 d 0.0)"
		"(if (and (< 1 2) (<= 2 2) (> 2.0 1.0) (>= 2.0 2.0)) (setq a 1.0))"
		"(if (or (= 1 2) (/= 1 2)) (setq b 1.0))"
		"(if (not (and (= 1 1) (= 1 2))) (setq c 1.0))"
		"(if (= (< 1 2) :t) (setq d 1.0))"
		"(return (vec4 a b c d))"))
;an if of a NaN is as a GPU has it: a NaN is not <, >= nor = anything,
;itself too, and is /= all of them
(assert-pixel "an if of a NaN" '(0.0 0.0 1.0 0.0)
	(sh-main "(defq a 0.0 b 0.0 c 0.0 d 0.0 z 0.0 n (/ z z))"
		"(if (< n 1.0) (setq a 1.0))"
		"(if (>= n 1.0) (setq b 1.0))"
		"(if (/= n n) (setq c 1.0))"
		"(if (= n n) (setq d 1.0))"
		"(return (vec4 a b c d))"))

;vectors
(assert-pixel "vector with vector" '(4.0 10.0 -3.0 0.25)
	(sh-main "(defq a (vec2 1.0 2.0) b (vec2 4.0 8.0))"
		"(return (vec4 (:x (* a b)) (:y (+ a b)) (:x (- a b)) (:y (/ a b))))"))
(assert-pixel "vector with float" '(3.0 3.0 0.0 0.5)
	(sh-main "(defq a (vec2 1.0 2.0))"
		"(return (vec4 (:x (* a 3.0)) (:y (+ a 1.0)) (:y (- a 2.0)) (:x (/ a 2.0))))"))
(assert-pixel "float with vector" '(6.0 2.0 1.0 2.0)
	(sh-main "(defq a (vec2 1.0 2.0))"
		"(return (vec4 (:y (* 3.0 a)) (:x (+ 1.0 a)) (:x (- 2.0 a)) (:y (/ 4.0 a))))"))
(assert-pixel "many args" '(24.0 10.0 -8.0 -1.0)
	(sh-main "(defq a (vec2 1.0 2.0))"
		"(return (vec4 (:y (* 2.0 a 3.0 2.0)) (+ 1.0 2.0 3.0 4.0) (- 1.0 2.0 3.0 4.0) (:x (- a))))"))
(assert-pixel "constructors" '(1.0 2.0 3.0 4.0)
	(sh-main "(defq a (vec2 2.0 3.0))" "(return (vec4 1 a (:x (* (vec3 2.0) 2.0))))"))
(assert-pixel "swizzles" '(3.0 2.0 1.0 4.0)
	(sh-main "(defq a (vec4 1.0 2.0 3.0 4.0) b (:zyx a) c (:yz a))"
		"(return (vec4 (:x b) (:x c) (:z b) (:a a)))"))
(assert-pixel "set components" '(1.0 9.0 8.0 7.0)
	(sh-main "(defq a (vec4 1.0 2.0 3.0 4.0) b a)"
		"(setq (:w a) 7.0 (:zy a) (vec2 8.0 9.0))"
		"(return (vec4 (:x a) (:yzw a)))"))
(assert-pixel "a set makes a new vector" '(1.0 2.0 5.0 2.0)
	(sh-main "(defq a (vec2 1.0 2.0) b a)" "(setq (:x b) 5.0)" "(return (vec4 a b))"))
(assert-pixel "vector floor fract mod abs" '(-2.0 0.5 1.5 1.5)
	(sh-main "(defq a (vec2 -1.5 1.5))"
		"(return (vec4 (:x (floor a)) (:x (fract a)) (:x (mod a 3.0)) (:x (abs a))))"))
(assert-pixel "vector min max clamp mix" '(-1.5 2.0 1.0 0.0)
	(sh-main "(defq a (vec2 -1.5 1.5) b (vec2 2.0 1.0))"
		"(return (vec4 (:x (min a b)) (:x (max a 2.0)) (:y (clamp a 0.0 1.0)) (:x (mix a (- a) 0.5))))"))
(assert-pixel "vector sin sqrt pow mix" '(0.8415 3.0 8.0 2.0)
	(sh-main "(defq a (vec2 1.0 9.0))"
		"(return (vec4 (:x (sin a)) (:y (sqrt a)) (:y (pow (vec2 3.0 2.0) (vec2 2.0 3.0)))"
		"(:x (mix a (vec2 3.0) (vec2 0.5)))))"))
(assert-pixel "dot length normalize" '(25.0 5.0 0.6 0.8)
	(sh-main "(defq a (vec2 3.0 4.0))" "(return (vec4 (dot a a) (length a) (normalize a)))"))
(assert-pixel "cross reflect" '(0.0 0.0 1.0 -1.0)
	(sh-main "(defq a (cross (vec3 1.0 0.0 0.0) (vec3 0.0 1.0 0.0)))"
		"(return (vec4 a (:y (reflect (vec2 1.0 1.0) (vec2 0.0 1.0)))))"))

;control flow
(assert-pixel "if and else" '(1.0 2.0 0.0 1.0)
	(sh-main "(defq a 0.0 b 0.0 c 0.0)"
		"(if (> (:x frag) 0.0) (setq a 1.0) (setq a 2.0))"
		"(if (> (:x frag) 1.0) (setq b 1.0) (progn (defq d 2.0) (setq b d)))"
		"(when (> (:x frag) 1.0) (setq c 1.0) (setq c (+ c 1.0)))"
		"(return (vec4 a b c 1.0))"))
(assert-pixel "loop" '(10.0 4.0 0.0 1.0)
	(sh-main "(defq a 0.0 b 0.0)" "(for (i 1 5) (setq a (+ a (float i)) b (+ b 1.0)))"
		"(return (vec4 a b 0.0 1.0))"))
(assert-pixel "loop with a break" '(3.0 2.0 0.0 1.0)
	(sh-main "(defq a 0.0 b 0.0)"
		"(for (i 0 100) (setq a (+ a 1.0)) (if (>= i 2) (break)) (setq b (+ b 1.0)))"
		"(return (vec4 a b 0.0 1.0))"))
(assert-pixel "loop in a loop, break of the inner" '(6.0 3.0 0.0 1.0)
	(sh-main "(defq a 0.0 b 0.0)"
		"(for (i 0 3) (for (j 0 10) (if (>= j 2) (break)) (setq a (+ a 1.0))) (setq b (+ b 1.0)))"
		"(return (vec4 a b 0.0 1.0))"))
(assert-pixel "return before the end" '(1.0 1.0 1.0 1.0)
	(sh-main "(if (< (:x frag) 1.0) (return (vec4 1.0)))" "(return (vec4 2.0))"))
(assert-pixel "return not taken" '(2.0 2.0 2.0 2.0)
	(sh-main "(if (> (:x frag) 1.0) (return (vec4 1.0)))" "(return (vec4 2.0))"))
(assert-pixel "return from deep in ifs" '(3.0 3.0 3.0 3.0)
	(sh-main "(defq a 1.0)"
		"(when (> a 0.0) (setq a 2.0) (if (> a 1.0) (progn (setq a 3.0) (if (> a 2.0) (return (vec4 a))))))"
		"(return (vec4 9.0))"))
(assert-pixel "return from a loop" '(5.0 5.0 5.0 5.0)
	(sh-main "(defq a 0.0)" "(for (i 0 100) (setq a (+ a 1.0)) (if (>= i 4) (return (vec4 a))))"
		"(return (vec4 9.0))"))
(assert-pixel "return from a loop in a loop" '(7.0 7.0 7.0 7.0)
	(sh-main "(defq a 0.0)"
		"(for (i 0 10) (for (j 0 10) (setq a (+ a 1.0)) (if (and (= i 1) (= j 1)) (return (vec4 a)))) (setq a (- a 5.0)))"
		"(return (vec4 9.0))"))
(assert-pixel "loop that does not return" '(9.0 9.0 9.0 9.0)
	(sh-main "(for (i 0 3) (if (> i 5) (return (vec4 1.0))))" "(return (vec4 9.0))"))

;functions, a parameter is a copy, and locals do not leak between calls
(assert-pixel "function calls" '(4.0 1.0 6.0 2.0)
	(sh-src "(defun twice :float ((a :float)) (setq a (* a 2.0)) (return a))"
		"(defun both :vec2 ((v :vec2)) (defq a 1.0) (setq (:x v) (twice (:x v))) (return (+ v a)))"
		"(defun main :vec4 ((frag :vec2))"
		"(defq a 1.0 v (vec2 1.0 1.0) b (twice (twice a)) w (both v))"
		"(return (vec4 b a (+ (:x w) (:x v) (:y w)) (:y w))))"))

;inputs, constants, globals and the frag coord
(defq prog (sh-src
		"(definput k :float 1.5)" "(definput n :int 3)" "(definput size :vec2)"
		"(defconst two 2.0)" "(defconst four (* two two))"
		"(defglobal g (* k two))" "(defglobal h (+ g (float n)))"
		"(defun main :vec4 ((frag :vec2)) (return (vec4 (+ g four) h (/ frag size))))"))
(assert-pixel "defaults" '(7.0 6.0 0.25 0.25) prog '((size (2.0 2.0))))
(assert-pixel "values given" '(5.0 6.0 0.625 0.875) prog '((size (4.0 4.0)) (n 5) (k 0.5)) 2 3)
(defq native (shader-vp prog)
	out (shader-vp-pixels native (shader-vp-frame prog native '((size (1.0 1.0)))) 1 2 4 4))
(assert-eq "tile size" 6 (length out))
(assert-list-eq "tile order" '((1.5 2.5) (2.5 2.5) (3.5 2.5) (1.5 3.5) (2.5 3.5) (3.5 3.5))
	(map (# (map! (const n2q) (list %0) 2 4)) out))

(report-header "GPU: shader language, GLSL back end")

(assert-eq "GLSL text" (join '(
			"#ifdef GL_ES"
			"precision highp float;"
			"#endif"
			""
			"uniform float k;"
			"uniform int n;"
			"uniform vec2 size;"
			""
			"const float two = 2.0000;"
			"const float four = (two * two);"
			""
			"float g;"
			"float h;"
			""
			"vec4 shader_main(vec2 frag)"
			"{"
			"	return vec4((g + four), h, (frag / size));"
			"}"
			""
			"void main()"
			"{"
			"	g = (k * two);"
			"	h = (g + float(n));"
			"	gl_FragColor = shader_main(gl_FragCoord.xy);"
			"}"
			"") sh_lf) (shader-glsl prog))

(assert-eq "GLSL statements" (join '(
			"vec4 shader_main(vec2 frag)"
			"{"
			"	vec3 c = vec3((-0.2500));"
			"	bool b = ((1 < 2) && (!(frag.x == 1.0000)));"
			"	for (int i = 0; i < 4; i++)"
			"	{"
			"		if ((float(i) > frag.y))"
			"		{"
			"			break;"
			"		}"
			"		else"
			"		{"
			"			c = (c + 0.2500);"
			"		}"
			"	}"
			"	c.zx = (-c.xy);"
			"	return vec4(mix(c, c.zyx, 0.5000), 1.0000);"
			"}") sh_lf)
	(progn
		(defq text (shader-glsl (sh-main
					"(defq c (vec3 -0.25) b (and (< 1 2) (not (= (:x frag) 1.0))))"
					"(for (i 0 4) (if (> (float i) (:y frag)) (break) (setq c (+ c 0.25))))"
					"(setq (:zx c) (- (:xy c)))"
					"(return (vec4 (mix c (:bgr c) 0.5) 1.0))")))
		(defq lines (split text sh_lf))
		(join (slice lines (find "vec4 shader_main(vec2 frag)" lines) (find "void main()" lines)) sh_lf)))

(report-header "GPU: shader language, MSL back end")

(assert-eq "MSL text" (join '(
			"#include <metal_stdlib>"
			"using namespace metal;"
			""
			"template<typename T, typename U> static inline T sh_mod(T x, U y) { return x - y * floor(x / y); }"
			""
			"struct Inputs"
			"{"
			"\tfloat k_;"
			"\tfloat pad_1;"
			"\tfloat pad_2;"
			"\tfloat pad_3;"
			"\tpacked_float3 b_;"
			"\tint n_;"
			"\tpacked_float2 size_;"
			"};"
			""
			"struct Shader"
			"{"
			"\tfloat k_;"
			"\tfloat3 b_;"
			"\tint n_;"
			"\tfloat2 size_;"
			"\tfloat g_;"
			"\tfloat two_ = 2.0000;"
			""
			"\tfloat3 half_(float3 a_)"
			"\t{"
			"\t\treturn sh_mod(max(a_, float3(0.5000)), float3(two_));"
			"\t}"
			""
			"\tfloat4 shader_main(float2 frag_)"
			"\t{"
			"\t\tfloat3 c_ = half_(b_);"
			"\t\tfor (int i_ = 0; i_ < 4; i_++)"
			"\t\t{"
			"\t\t\tif ((float(i_) > g_))"
			"\t\t\t{"
			"\t\t\t\tbreak;"
			"\t\t\t}"
			"\t\t\telse"
			"\t\t\t{"
			"\t\t\t\tc_ = (c_ + 0.2500);"
			"\t\t\t}"
			"\t\t}"
			"\t\tc_.zx = (-c_.xy);"
			"\t\treturn float4(mix(c_, c_.zyx, float3(0.5000)), (frag_.x / size_.x));"
			"\t}"
			""
			"\tvoid shader_init()"
			"\t{"
			"\t\tg_ = (k_ * two_);"
			"\t}"
			"};"
			""
			"struct VertexOut"
			"{"
			"\tfloat4 position [[position]];"
			"\tfloat2 frag;"
			"};"
			""
			"fragment float4 fragment_main(VertexOut in [[stage_in]], constant Inputs &inputs [[buffer(0)]])"
			"{"
			"\tShader s;"
			"\ts.k_ = inputs.k_;"
			"\ts.b_ = float3(inputs.b_);"
			"\ts.n_ = inputs.n_;"
			"\ts.size_ = float2(inputs.size_);"
			"\ts.shader_init();"
			"\treturn s.shader_main(in.frag);"
			"}"
			"") sh_lf)
	(shader-msl (sh-src
			"(definput k :float 1.5)" "(definput b :vec3)" "(definput n :int 3)" "(definput size :vec2)"
			"(defconst two 2.0)"
			"(defglobal g (* k two))"
			"(defun half :vec3 ((a :vec3)) (return (mod (max a 0.5) two)))"
			"(defun main :vec4 ((frag :vec2))"
			"(defq c (half b))"
			"(for (i 0 4) (if (> (float i) g) (break) (setq c (+ c 0.25))))"
			"(setq (:zx c) (- (:xy c)))"
			"(return (vec4 (mix c (:bgr c) 0.5) (/ (:x frag) (:x size)))))")))
(assert-true "MSL vertex shader" (find "vertex VertexOut vertex_main(uint vid [[vertex_id]], constant Target &target [[buffer(0)]])"
		(split (shader-msl-vertex) sh_lf)))

(report-header "GPU: shader language, SPIR-V back end")

;a module is checked as a stream of words here, each instruction of the
;right length and each id made once. That it draws what the other back
;ends draw was seen on a Raspberry Pi 4, see docs/ai_digest/shader_language.md

(defun spvt-words (module)
	(map! (# (logand (code module 4 (* (!) 4)) 0xffffffff)) '() 0 (/ (length module) 4)))

(defun spvt-insts (module)
	;the instructions, each (opcode operand ...), or :nil if they do not
	;end where the module does
	(defq words (spvt-words module) insts (list) i 5 n (length words) ok (> n 5))
	(while (and ok (< i n))
		(defq len (>> (elem-get words i) 16))
		(cond
			((or (= len 0) (> (+ i len) n)) (setq ok :nil))
			(:t (push insts (cat (list (logand (elem-get words i) 0xffff)) (slice words (inc i) (+ i len))))
				(setq i (+ i len)))))
	(if ok insts))

(defun spvt-ids (insts)
	;the ids the instructions make, in order
	(sort (reduce (lambda (ids (op &rest args))
				(cond
					((find op '(17 14 15 16 71 72 62 246 247 249 250 252 253 254 56)) ids)
					((find op '(11 19 20 21 22 23 24 30 32 33 248)) (push ids (first args)))
					((push ids (second args))))) insts (list)) (const -)))

(defun spvt-has? (insts &rest pattern)
	;is there an instruction that starts with these words, :nil is any word
	(some (lambda (inst)
			(and (>= (length inst) (length pattern))
				(every (# (or (not %0) (eql %0 %1))) pattern inst))) insts))

(defq module (shader-spirv-vertex) words (spvt-words module) insts (spvt-insts module))
(assert-list-eq "SPIR-V vertex header" (list 0x07230203 0x00010000 0) (slice words 0 3))
(assert-eq "SPIR-V vertex size" 896 (length module))
(assert-true "SPIR-V vertex stream" insts)
(assert-list-eq "SPIR-V vertex ids" (map! (const !) '() 1 (elem-get words 3)) (spvt-ids insts))
(assert-true "SPIR-V vertex entry point" (apply spvt-has? (cat (list insts 15 0 :nil) (spv-str "vertex_main"))))
(assert-true "SPIR-V vertex index" (spvt-has? insts 71 :nil 11 42))
(assert-true "SPIR-V vertex position" (spvt-has? insts 71 :nil 11 0))
(assert-true "SPIR-V vertex block set" (spvt-has? insts 71 :nil 34 1))

(defq module (shader-spirv (sh-src
			"(definput k :float 1.5 0.0 4.0)"
			"(definput n :int 3)"
			"(definput tint :vec3)"
			"(defconst two 2.0)"
			"(defglobal scale (* k two))"
			"(defun wave :float ((a :float))"
			"	(return (sin (* a scale))))"
			"(defun main :vec4 ((frag :vec2))"
			"	(defq c (vec3 0.0) total 0.0)"
			"	(for (i 0 8)"
			"		(if (> total 3.0) (break))"
			"		(setq total (+ total (wave (float i)))))"
			"	(setq (:x c) total (:yz c) (:xy frag))"
			"	(if (and (> n 2) (not (< k 0.0)))"
			"		(return (vec4 (mix c tint 0.5) 1.0))"
			"		(return (vec4 (clamp c 0.0 1.0) (mod total 2.0)))))"))
	words (spvt-words module) insts (spvt-insts module))
(assert-true "SPIR-V fragment stream" insts)
(assert-list-eq "SPIR-V fragment ids" (map! (const !) '() 1 (elem-get words 3)) (spvt-ids insts))
(assert-true "SPIR-V fragment entry point" (apply spvt-has? (cat (list insts 15 4 :nil) (spv-str "fragment_main"))))
(assert-true "SPIR-V origin upper left" (spvt-has? insts 16 :nil 7))
(assert-true "SPIR-V block set" (spvt-has? insts 71 :nil 34 3))
(assert-true "SPIR-V block offsets" (and (spvt-has? insts 72 :nil 0 35 0)
		(spvt-has? insts 72 :nil 1 35 4) (spvt-has? insts 72 :nil 2 35 16)))
(assert-true "SPIR-V float constant" (spvt-has? insts 43 :nil :nil 0x3f000000))
(assert-true "SPIR-V sin" (spvt-has? insts 12 :nil :nil 1 13))
(assert-true "SPIR-V mix clamp" (and (spvt-has? insts 12 :nil :nil 1 46) (spvt-has? insts 12 :nil :nil 1 43)))
(assert-true "SPIR-V loop" (spvt-has? insts 246))
(assert-eq "SPIR-V ifs" 2 (length (filter (# (= (first %0) 247)) insts)))
(assert-eq "SPIR-V functions" 3 (length (filter (# (= (first %0) 54)) insts)))
(assert-true "SPIR-V set of components" (and (spvt-has? insts 82) (spvt-has? insts 79 :nil :nil :nil :nil 0 3 4)))

(defq module (shader-spirv (shader-load "apps/demos/surface/surface.shader"))
	words (spvt-words module) insts (spvt-insts module))
(assert-true "SPIR-V raymarch stream" insts)
(assert-list-eq "SPIR-V raymarch ids" (map! (const !) '() 1 (elem-get words 3)) (spvt-ids insts))
(assert-eq "SPIR-V raymarch functions" 20 (length (filter (# (= (first %0) 54)) insts)))

(report-header "GPU: shader language, the inputs block")

(defq prog (sh-src
		"(definput a :float 1.5)" "(definput b :vec3)" "(definput c :int -3)" "(definput d :vec2)"
		"(definput e :float 0.2)" "(definput f :vec4)" "(definput g :int 7)"
		"(defun main :vec4 ((frag :vec2)) (return (vec4 a e (:x d) (:z b))))"))
(assert-list-eq "layout" '(80 (a :float 0) (b :vec3 16) (c :int 28) (d :vec2 32)
		(e :float 40) (f :vec4 48) (g :int 64)) (shader-layout prog))
(defq blk (shader-pack prog))
(assert-eq "block size" 80 (length blk))
(assert-list-eq "defaults packed" '(0x3fc00000 0 0 0 0 0 0 0xfffffffd 0 0 0x3e4ccccd 0 0 0 0 0 7 0 0 0)
	(map! (# (get-uint blk (* (!) 4))) '() 0 20))
(defq blk (shader-pack prog (list '(b (1.0 -2.5 16777217)) '(d (0.3 64.0)) (list 'a (n2d 0.75)) '(c 9))))
(assert-list-eq "values packed" '(0x3f400000 0 0 0 0x3f800000 0xc0200000 0x4b800000 9 0x3e99999a 0x42800000 0x3e4ccccd)
	(map! (# (get-uint blk (* (!) 4))) '() 0 11))
(defq vals (shader-unpack prog blk))
(assert-list-eq "unpacked names" '(a b c d e f g) (map (const first) vals))
(assert-list-eq "unpacked ints" '(9 7) (list (second (third vals)) (second (last vals))))
(assert-eq "unpacked float" (n2d 0.75) (second (first vals)))
(assert-list-eq "unpacked vector" (doubles (n2d 1.0) (n2d -2.5) (n2d 16777216)) (second (second vals)))
(assert-pixel "block to the CPU back end" '(0.75 0.2 0.3 16777216.0) prog vals)
(assert-error "wrong size of vector" (shader-pack prog '((d (1.0 2.0 3.0)))) {Shader, wrong size of vector input !})

(report-header "GPU: shader language, VP back end")

;a tile as 32 bit argb pixels, clamped, and with the top row first. A
;byte is the nearest to 255 of the float, as a GPU writes one
(defq prog (sh-src "(definput k :float 4.0)" "(definput n :int 2)"
		"(defun main :vec4 ((frag :vec2)) (return (vec4 (/ frag k) (float n) -1.0)))")
	native (shader-vp prog) frame (shader-vp-frame prog native))
(assert-true "native function" (func? (first native)))
(assert-eq "frame holds the inputs, a float as the 32 bit float it is" (n2d 4.0) (sh-float-to-double (get-uint frame 0)))
(assert-eq "frame holds the ints" 2 (get-long frame 8))
(defq out (shader-vp-argb native frame 0 0 2 2))
(assert-eq "argb size" 16 (length out))
(assert-list-eq "argb tile" '(0xff2020ff 0xff6020ff 0xff2060ff 0xff6060ff)
	(map! (# (get-uint out (* (!) 4))) '() 0 4))
(assert-list-eq "argb tile, top row first" '(0xff20dfff 0xff60dfff 0xff209fff 0xff609fff)
	(map! (# (get-uint (shader-vp-argb native frame 0 0 2 2 4) (* (!) 4))) '() 0 4))
(assert-list-eq "argb tile, inputs given" '(0xff404000 0xffbf4000)
	(map! (# (get-uint (shader-vp-argb native
					(shader-vp-frame prog native '((k 2.0) (n 0))) 0 0 2 1) (* (!) 4))) '() 0 2))
(defq pixels (shader-vp-pixels native frame 1 2 3 4))
(assert-list-eq "pixels tile" '((0.375 0.625) (0.625 0.625) (0.375 0.875) (0.625 0.875))
	(map (# (map! (const n2q) (list %0) 0 2)) pixels))

;every float register is in use at once here, and a call saves them all
(assert-pixel "registers saved over calls" '(13.0 26.0 39.0 52.0)
	(sh-src "(defun f :vec4 ((a :vec4)) (return (* a 2.0)))"
		"(defun main :vec4 ((frag :vec2))"
		"(defq a (vec4 1.0 2.0 3.0 4.0))"
		"(return (+ a (+ (f a) (f (+ a (f (f a))))))))"))
;one more value held and there is no register for it
(defq prog (sh-src "(defun f :vec4 ((a :vec4)) (return (* a 2.0)))"
		"(defun main :vec4 ((frag :vec2))"
		"(defq a (vec4 1.0 2.0 3.0 4.0))"
		"(return (+ a (+ (f a) (+ (f (f a)) (+ a (f (+ a (f (f a))))))))))"))
(assert-error "deep expression, VP back end" (shader-vp prog) {Shader, expression needs too many registers !})
(assert-pixel "sin and pow in the middle of a vector" '(1.8415 4.5403 11.0 0.0)
	(sh-main "(defq a (vec4 1.0 2.0 3.0 4.0))"
		"(return (vec4 (+ (:x a) (sin (:x a))) (+ (* (:y a) 2.0) (cos (:x a)))"
		"(+ (:z a) (pow (:y a) (:z a))) (- (:w a) (pow (:y a) 2.0))))"))

(report-header "GPU: the raymarch shader")

(defq prog (shader-load "apps/demos/surface/surface.shader") text (shader-glsl prog))
(assert-eq "inputs block size" 64 (first (shader-layout prog)))
(assert-list-eq "inputs" '(time resolution arg_aa arg_aa_adaptive arg_aa_debug arg_depth
		arg_aa_limit arg_ao arg_ref arg_shadow arg_bump arg_dis arg_march) (map (const first) (first prog)))
(assert-eq "functions" 19 (length (elem-get prog 3)))
(assert-true "GLSL scene" (found? text (join '(
				"float scene(vec3 p)"
				"{"
				"	float d = 0.0000;"
				"	if ((arg_dis > 0.0000))"
				"	{"
				"		d = (sinusoidal_bump((p * 4.0000)) * arg_dis);"
				"	}"
				"	p = (fract(p) - 0.5000);"
				"	return (sphere(p, vec3(0.0000), 0.3500) + d);"
				"}") sh_lf)))

;pixels of a 64 by 48 frame at time 2.0, against what an Apple M4 Max gave
;for the GLSL text, read back as floats
(defq vals '((time 2.0) (resolution (64.0 48.0))))
(each (lambda ((x y r g b))
		(assert-pixel (cat "pixel " (str x) " " (str y)) (list r g b 1.0) prog vals x y))
	'((48 16 0.6084 0.5918 0.0)
		(22 7 0.0040 0.7230 0.0040)
		(4 33 0.3743 0.1048 0.3734)
		(17 2 0.0033 0.5215 0.5248)
		(50 3 0.4024 0.4024 0.4024)
		(48 45 0.3835 0.0409 0.3835)))

;the second shader, the Raymarch film. Every back end takes it, and the
;native code gives the pixels it gave when they were last worked out by
;hand, in 32 bit floats
(defq film (shader-load "apps/demos/raymarch/film.shader")
	film_vals '((resolution (96.0 96.0)) (cam_z -2.4) (light_x -0.02)))
(assert-true "film, GLSL" (> (length (shader-glsl film)) 1000))
(assert-true "film, MSL" (> (length (shader-msl film)) 1000))
(assert-true "film, SPIR-V" (> (length (shader-spirv film)) 1000))
(each (lambda ((x y &rest want))
		(assert-pixel (cat "film, native code, " (str x) " " (str y)) want film film_vals x y))
	'((48 48 0.0 0.0 0.0 1.0) (10 80 0.1343 0.1340 0.1331 1.0) (70 20 0.0930 0.0746 0.6284 1.0)))

(report-header "GPU: shader language, vertex shaders, varyings and a matrix")

(import "lib/math/matrix.inc")

;a vertex shader, its main takes nothing. It reads the attrs of a vertex
;and its inputs, gives where the vertex is, and sets its varyings
(defq vert (sh-src
		"(definput model :mat4)"
		"(definput view :mat4)"
		"(definput tint :vec3)"
		"(defattr position :vec3)"
		"(defattr normal :vec3)"
		"(defvarying shade :float)"
		"(defvarying color :vec3)"
		"(defvarying unset :vec2)"
		"(defun lit :float ((n :vec3)) (max (dot n (vec3 0.0 0.0 1.0)) 0.0))"
		"(defun main :vec4 ()"
		"	(defq n (* model normal))"
		"	(setq shade (lit n) color (* tint shade))"
		"	(* view model (vec4 position 1.0)))"))
(assert-eq "a vertex shader" :vertex (shader-stage vert))
(assert-list-eq "its attrs" '((position :vec3) (normal :vec3)) (shader-attrs vert))
(assert-list-eq "its varyings" '((shade :float) (color :vec3) (unset :vec2)) (shader-varyings vert))
(assert-eq "a pixel shader" :pixel (shader-stage film))
(assert-eq "a pixel shader has no attrs" 0 (length (shader-attrs film)))

;the types of a matrix
(defun sh-vert-type (&rest lines)
	;the type of the first local of the main of a vertex shader
	(first (third (first (last (first (elem-get (sh-src "(definput m :mat4)" "(definput v :vec4)"
								(cat "(defun main :vec4 () " (join lines " ") ")")) 3)))))))
(test-cases
	(sh-vert-type "(defq a (* m v))" "a") :vec4
	(sh-vert-type "(defq a (* m m))" "v") :mat4
	(sh-vert-type "(defq a (* m m m v))" "a") :vec4
	(sh-vert-type "(defq a (* m (:xyz v)))" "v") :vec3
	(sh-vert-type "(defq a (* m m (:xyz v)))" "v") :vec3)
(assert-error "a vector times a matrix" (sh-vert-type "(defq a (* v m))" "v") {Shader, wrong types !})
(assert-error "a matrix times a vec2" (sh-vert-type "(defq a (* m (:xy v)))" "v") {Shader, wrong types !})
(assert-error "a matrix times a float" (sh-vert-type "(defq a (* m 2.0))" "v") {Shader, wrong types !})
(assert-error "a matrix added" (sh-vert-type "(defq a (+ m m))" "v") {Shader, wrong types !})
(assert-error "a matrix has no components" (sh-vert-type "(defq a (:x m))" "v") {Shader, components of a value that is not a vector !})
(assert-error "a matrix input has no default" (sh-src "(definput m :mat4 1.0)" "(defun main :vec4 () (vec4 0.0))") {Shader, a vector or matrix input has no default !})

;what is not allowed
(assert-error "an attr can not be set"
	(sh-src "(defattr p :vec3)" "(defun main :vec4 () (setq p (vec3 0.0)) (vec4 p 1.0))") {Shader, can only set a local, a parameter or a varying !})
(assert-error "an attr of a matrix"
	(sh-src "(defattr p :mat4)" "(defun main :vec4 () (vec4 0.0))") {Shader, wrong type, a float or a vector !})
(assert-error "a varying of an int"
	(sh-src "(defvarying p :int)" "(defun main :vec4 () (vec4 0.0))") {Shader, wrong type, a float or a vector !})
(assert-error "a pixel shader with an attr"
	(sh-src "(defattr p :vec3)" "(defun main :vec4 ((frag :vec2)) (vec4 p 1.0))") {Shader, a pixel shader has no attrs !})
(assert-error "a pixel shader that sets a varying"
	(sh-src "(defvarying c :vec3)" "(defun main :vec4 ((frag :vec2)) (setq c (vec3 0.0)) (vec4 c 1.0))") {Shader, a pixel shader can not set a varying !})
(assert-error "a varying set to the wrong type"
	(sh-src "(defvarying c :vec3)" "(defun main :vec4 () (setq c 1.0) (vec4 0.0))") {Shader, wrong types !})
(assert-error "a varying can not be a constant"
	(sh-src "(defvarying c :float)" "(defconst k (* c 2.0))" "(defun main :vec4 () (vec4 0.0))") {Shader, not a constant !})
(assert-error "main with the wrong parameters"
	(sh-src "(defun main :vec4 ((a :float)) (vec4 a))") {Shader, no (defun main :vec4 ((frag :vec2)) ...), or (defun main :vec4 () ...) for a vertex shader !})

;the inputs block, a matrix is its 4 columns, on a 16 byte boundary, and
;is given a row at a time
(assert-list-eq "layout with matrices" '(144 (model :mat4 0) (view :mat4 64) (tint :vec3 128))
	(shader-layout vert))
(defq rows (map! (# (n2d (!))) '() 1 17)
	blk (shader-pack vert (list (list 'model rows) '(tint (0.5 0.25 1.0)))))
(assert-eq "block size" 144 (length blk))
(assert-list-eq "a matrix packed a column at a time"
	(map (const sh-double-to-float) '(1 5 9 13 2 6 10 14 3 7 11 15 4 8 12 16))
	(map! (# (get-uint blk (* (!) 4))) '() 0 16))
(assert-list-eq "and comes back a row at a time" rows (second (first (shader-unpack vert blk))))
(assert-error "wrong size of matrix" (shader-pack vert '((model (1.0 2.0 3.0)))) {Shader, wrong size of vector input !})

;native code places vertices. A vertex goes in as its attrs one after
;another, and comes out as where it is, then its varyings. The matrices
;are those of the matrix library, and where a vertex is put is where the
;library puts it
(defq model (mat4x4-mul (mat4x4-translate (n2d 1) (n2d 2) (n2d 3)) (mat4x4-rotx (n2d 0.5)))
	view (mat4x4-frustum (n2d -1) (n2d 1) (n2d 1) (n2d -1) (n2d 2) (n2d 10))
	verts (list (list (doubles (n2d 0.5) (n2d -0.25) (n2d 2)) (doubles (n2d 0) (n2d 0) (n2d 1)))
		(list (doubles (n2d -3) (n2d 1) (n2d 0.75)) (doubles (n2d 0) (n2d 1) (n2d 0))))
	both (mat4x4-mul view model)
	vnative (shader-vp-vertex vert)
	vvals (list (list 'model model) (list 'view view) '(tint (1.0 0.5 0.25)))
	vframe (shader-vp-frame vert vnative vvals)
	flat (apply (const cat) (map (# (apply (const cat) %0)) verts))
	out (shader-vp-place vnative vframe flat))
(assert-true "native function for a vertex shader" (func? (first vnative)))
(assert-list-eq "numbers in and out for a vertex" '(6 10) (rest (rest vnative)))
(assert-eq "numbers out, 10 for each vertex" 20 (length out))
(each (lambda ((position normal))
		(defq at (* (!) 10) pos (slice out at (+ at 4)) shade (elem-get out (+ at 4))
			color (slice out (+ at 5) (+ at 8)) unset (slice out (+ at 8) (+ at 10)))
		(assert-true (cat "where vertex " (str (!)) " is, as the matrix library has it")
			(sh-near? (map (const n2q) pos)
				(map (const n2q) (mat4x4-vec4-mul both (cat position (doubles (n2d 1)))))))
		(defq n (mat4x4-vec3-mul model normal)
			want (max (n2q (third n)) 0.0))
		(assert-true (cat "varying of vertex " (str (!)) ", a float")
			(sh-near? (list (n2q shade)) (list want)))
		(assert-true (cat "varying of vertex " (str (!)) ", a vector")
			(sh-near? (map (const n2q) color) (list want (* want 0.5) (* want 0.25))))
		(assert-list-eq (cat "varying of vertex " (str (!)) ", not set, is 0") '(0.0 0.0)
			(map (const n2q) unset)))
	verts)
(assert-eq "no vertices, nothing out" 0 (length (shader-vp-place vnative vframe (doubles))))
(assert-list-eq "same program, same function" (first vnative) (first (shader-vp-vertex vert)))

;a pixel shader reads varyings, by name, and goes with a vertex shader
;that has them
(defq pix (sh-src
		"(defvarying color :vec3)"
		"(defvarying shade :float)"
		"(defun main :vec4 ((frag :vec2)) (vec4 (* color shade) 1.0))"))
(assert-list-eq "the varyings of a pixel shader" '((color :vec3) (shade :float)) (shader-varyings pix))
(assert-true "a pair" (lmatch? (shader-pair vert pix) (list vert pix)))
(assert-true "a pixel shader with no varyings goes with any vertex shader"
	(shader-pair vert film))
(assert-error "a varying the vertex shader has not got"
	(shader-pair vert (sh-src "(defvarying glow :float)" "(defun main :vec4 ((frag :vec2)) (vec4 glow))")) {Shader, the vertex shader has no such varying !})
(assert-error "a varying of another type"
	(shader-pair vert (sh-src "(defvarying shade :vec2)" "(defun main :vec4 ((frag :vec2)) (vec4 shade shade))")) {Shader, the vertex shader has no such varying !})
(assert-error "two pixel shaders are not a pair" (shader-pair pix pix) {Shader, not a vertex shader !})
(assert-error "two vertex shaders are not a pair" (shader-pair vert vert) {Shader, not a pixel shader !})

;matrices kept in locals and globals, set, and multiplied together
(defq vert2 (sh-src
		"(definput a :mat4)"
		"(definput b :mat4)"
		"(defattr p :vec4)"
		"(defvarying turned :vec3)"
		"(defglobal ab (* a b))"
		"(defun main :vec4 ()"
		"	(defq m ab n b)"
		"	(setq n (* a a b))"
		"	(setq turned (* m (:xyz p)))"
		"	(+ (* m p) (* n p) (* a b p)))")
	vvals (list (list 'a model) (list 'b view))
	pts (list (list (doubles (n2d 1) (n2d 2) (n2d 3) (n2d 1))) (list (doubles (n2d -0.5) (n2d 0.25) (n2d 4) (n2d 1))))
	vnative (shader-vp-vertex vert2)
	out (shader-vp-place vnative (shader-vp-frame vert2 vnative vvals)
		(apply (const cat) (map (const first) pts)))
	;what the matrix library gives
	ab (mat4x4-mul model view) aab (mat4x4-mul (mat4x4-mul model model) view))
(each (lambda ((p))
		(defq m_p (mat4x4-vec4-mul ab p)
			want (cat (nums-add (nums-add m_p (mat4x4-vec4-mul aab p)) m_p)
				(mat4x4-vec3-mul ab (slice p 0 3))))
		(assert-true (cat "matrices in locals and globals, as the matrix library has it, vertex " (str (!)))
			(sh-near? (map (const n2q) want) (map! (const n2q) (list out) (* (!) 7) (* (inc (!)) 7)))))
	pts)
;a vertex function reads its attrs and writes its vertex where they are,
;by two registers. A call of the system must not lose them, nor a function
;of the shader's own that reads an attr, and a varying can be read back
(defq vert3 (sh-src
		"(definput a :mat4)"
		"(defattr p :vec4)"
		"(defattr k :float)"
		"(defvarying wave :vec2)"
		"(defvarying far :float)"
		"(defun bend :float ((x :float)) (+ (sin (* x k)) (pow (abs (:y p)) 1.5)))"
		"(defun main :vec4 ()"
		"	(setq wave (vec2 (bend (:x p)) (cos k)))"
		"	(setq far (+ (:x wave) (:y wave) (:z p)))"
		"	(defq q (* a p))"
		"	(if (> k 1.5) (return (vec4 (:zyx q) far)))"
		"	(+ q (vec4 (:y wave) (bend k) (:w q) far)))")
	pts3 (list (list (doubles (n2d 1) (n2d 2) (n2d 3) (n2d 1)) (n2d 0.5))
		(list (doubles (n2d -0.5) (n2d 0.25) (n2d 4) (n2d 1)) (n2d 2))
		(list (doubles (n2d 0.3) (n2d -1.25) (n2d -2) (n2d 1)) (n2d 1.25)))
	vvals3 (list (list 'a model))
	vnative3 (shader-vp-vertex vert3)
	out3 (shader-vp-place vnative3 (shader-vp-frame vert3 vnative3 vvals3)
		(apply (const cat) (map (# (cat (first %0) (doubles (second %0)))) pts3))))
(assert-eq "sin, pow and a function that reads an attr, the size of what comes out" 21 (length out3))
(each (lambda ((p k))
		;what it is worked out by hand, a power of 1.5 is a number times its root
		(bind '(px py pz &ignore) p)
		(defq bend (lambda (x) (+ (sin (* x k)) (* (abs py) (sqrt (abs py)))))
			wx (bend px) wy (cos k) far (+ wx wy pz) q (mat4x4-vec4-mul model p)
			pos (if (> k (n2d 1.5)) (doubles (third q) (second q) (first q) far)
				(nums-add q (doubles wy (bend k) (elem-get q 3) far))))
		(assert-true (cat "sin, pow and a function that reads an attr, vertex " (str (!)))
			(sh-near? (map (const n2q) (cat pos (doubles wx wy far)))
				(map! (const n2q) (list out3) (* (!) 7) (* (inc (!)) 7)))))
	pts3)
(assert-error "a function can not take a matrix"
	(sh-src "(defun f :vec4 ((m :mat4)) (vec4 0.0))" "(defun main :vec4 () (vec4 0.0))") {Shader, a function can not take a matrix !})
(assert-error "a function can not give a matrix"
	(sh-src "(definput m :mat4)" "(defun f :mat4 () m)" "(defun main :vec4 () (vec4 0.0))") {Shader, a function can not give a matrix !})

(report-header "GPU: shader language, triangles drawn by a pair of shaders")

(import "gui/canvas/lisp.inc")

;triangles drawn as native code. Each way of drawing the one picture must
;draw it the same, to the bit: in parts, in strips, in another order, a
;triangle's vertices taken round. A picture is known by the CRC of its
;pixels, as they were drawn when last looked at. The vertex shader has
;more varyings than the pixel shader reads, and in another order. The
;matrix gives a w that changes with depth, so a varying has perspective
;to get right
(defq tvert (sh-src "(definput m :mat4)" "(defattr p :vec4)" "(defattr c :vec3)" "(defattr k :float)"
		"(defvarying unused :vec2)" "(defvarying glow :float)" "(defvarying col :vec3)"
		"(defun main :vec4 () (setq col c glow k) (* m p))")
	tpix (sh-src "(definput gain :float 1.0)" "(defvarying col :vec3)" "(defvarying glow :float)"
		"(defun main :vec4 ((frag :vec2)) (vec4 (* col glow gain) (/ (:y frag) 20.0)))")
	pipe (shader-vp-pipeline tvert tpix)
	tm (apply (const doubles) (map (const n2d) '(1 0 0 0 0 1 0 0 0 0 1 0 0 0 0.7 1.2)))
	;two that cross in depth, one that faces away, and one behind the eye
	tverts (apply (const doubles) (map (const n2d) '(
				-0.93 -0.81 -0.5 1   1 0 0   1
				0.87 -0.73  0.5 1   0 1 0   0.5
				0.11  0.91  0   1   0 0 1   1
				-0.77 -0.89  0.6 1   1 1 0   1
				0.95 -0.83 -0.7 1   0 1 1   1
				0.03  0.79 0.1  1   1 0 1   0.25
				-0.5  -0.5  -0.6 1   1 1 1   1
				-0.1   0.4  -0.6 1   1 1 1   1
				0.4  -0.5  -0.6 1   1 1 1   1
				0.0   0.0  -3.0 1   1 1 1   1)))
	ttris (nums 0 1 2 3 4 5 6 7 8 0 1 9)
	tvv (list (list 'm tm)) tpv '((gain 0.9)))
(assert-list-eq "where in a placed vertex the pixel shader's varyings are" '(56 64 72 48) (last pipe))

(defun tri-pixels (pixmap)
	;the pixels of a pixmap, as they are saved
	(defq stream (memory-stream))
	(pixmap-write pixmap stream 32)
	(stream-seek stream 0 0)
	(read-blk stream 100000))

(defun tri-drawn (pixels)
	;how many pixels were drawn
	(reduce! (# (if (/= (get-uint pixels (<< (!) 2)) 0) (inc %0) %0)) '() 0 0 (>> (length pixels) 2)))

(defun tri-draw (pipeline tris &optional pvals cull tri_size x y x1 y1 verts)
	;the pixels of triangles drawn on a frame of 20 by 20 of their own
	(defq canvas (Canvas 20 20 1) pixmap (getf canvas +canvas_pixmap 0))
	(shader-vp-draw-tris pipeline (ifn verts tverts) tris pixmap (shader-vp-depth 20 20) tvv (ifn pvals tpv)
		cull (ifn tri_size 3) (ifn x 0) (ifn y 0) (ifn x1 20) (ifn y1 20))
	(slice (tri-pixels pixmap) 0 1600))

(defun tri-only (pixels x y x1 y1)
	;the pixels with all outside that box made 0
	(defq out (cat pixels))
	(each! (lambda () (defq px (% (!) 20) py (/ (!) 20))
			(unless (and (<= x px) (< px x1) (<= y py) (< py y1)) (set-int out (* (!) 4) 0))) '() 0 400)
	out)

(defq drawn (list))
(each (lambda ((title cull x y x1 y1 crc))
		(push drawn (defq got (tri-draw pipe ttris :nil cull 3 x y x1 y1)))
		(assert-eq (cat "the pixels as they were, " title) crc (crc32 got)))
	'(("all of it" :nil 0 0 20 20 3973177273) ("those that face away left out" :t 0 0 20 20 2764376830)
		("those that face us left out" :front 0 0 20 20 2496602895)))
(assert-true "it is a picture" (> (tri-drawn (first drawn)) 100))
(assert-true "one faced away, and without it the picture is another"
	(not (eql (second drawn) (first drawn))))
(assert-true "with only the one that faced away the picture is small"
	(< 0 (tri-drawn (third drawn)) 120))
(each (lambda ((title x y x1 y1))
		(assert-eq (cat "a part of the screen is that part of the whole, " title)
			(tri-only (first drawn) x y x1 y1) (tri-draw pipe ttris :nil :nil 3 x y x1 y1)))
	'(("in the middle" 3 5 17 14) ("partly off the screen" -5 -5 40 8)))

;the nearer triangle is the one seen, whichever is drawn first
(defq tsolid (sh-src "(definput gain :float 1.0)" "(defvarying col :vec3)" "(defvarying glow :float)"
		"(defun main :vec4 ((frag :vec2)) (vec4 (* col glow gain) 1.0))")
	pipe_solid (shader-vp-pipeline tvert tsolid)
	solid (tri-draw pipe_solid ttris))
(defq canvas (Canvas 20 20 1) pixmap (getf canvas +canvas_pixmap 0) depth (shader-vp-depth 20 20))
(shader-vp-draw-tris pipe_solid tverts (nums 3 4 5 0 1 2) pixmap depth tvv tpv)
(shader-vp-draw-tris pipe_solid tverts (nums 6 7 8 0 1 9) pixmap depth tvv tpv)
(assert-eq "solid, the order they are drawn in does not matter, nor how many calls"
	solid (slice (tri-pixels pixmap) 0 1600))
;alpha. The pixel shader of the tests above has an alpha that goes from
;next to nothing at the bottom row to nearly full on at the top, so
;they are all tests of a pixel going over what is there. Here, the two
;quick ways, and what is under a see through pixel
(defun tri-alpha (alpha)
	(sh-src "(definput gain :float 1.0)" "(defvarying col :vec3)" "(defvarying glow :float)"
		(cat "(defun main :vec4 ((frag :vec2)) (vec4 (* col glow gain) " alpha "))")))
(defq tnone (tri-alpha "0.0") thalf (tri-alpha "0.5") tover (tri-alpha "7.0"))
(defq canvas (Canvas 20 20 1) pixmap (getf canvas +canvas_pixmap 0) depth (shader-vp-depth 20 20))
(shader-vp-draw-tris (shader-vp-pipeline tvert tnone) tverts ttris pixmap depth tvv tpv)
(assert-eq "alpha of 0, nothing drawn" 0 (tri-drawn (slice (tri-pixels pixmap) 0 1600)))
(shader-vp-draw-tris pipe_solid tverts ttris pixmap depth tvv tpv)
(assert-eq "alpha of 0 left the depth buffer alone, so what is drawn next is all there"
	solid (slice (tri-pixels pixmap) 0 1600))
(assert-eq "alpha over 1 is full on" solid (tri-draw (shader-vp-pipeline tvert tover) ttris))
;half there, over nothing, a pixel has half its color and half its
;alpha, 0.5 of 255 to the nearest is 128, and 255 * 129 / 256 of white
(defq half_pixels (tri-draw (shader-vp-pipeline tvert thalf) (nums 6 7 8) '((gain 1.0)))
	half_seen (filter (# (/= %0 0)) (map! (# (get-uint half_pixels (* (!) 4))) '() 0 400)))
(assert-true "half there, some pixels" (> (length half_seen) 10))
(assert-true "half there, over nothing, half the color and half the alpha"
	(every (# (= %0 0x80808080)) half_seen))
;and over a solid one that is further off, it is not what either is alone
(defq canvas (Canvas 20 20 1) pixmap (getf canvas +canvas_pixmap 0) depth (shader-vp-depth 20 20))
(shader-vp-draw-tris pipe_solid tverts (nums 3 4 5) pixmap depth tvv tpv)
(defq under_pixels (slice (tri-pixels pixmap) 0 1600))
(shader-vp-draw-tris (shader-vp-pipeline tvert thalf) tverts (nums 6 7 8) pixmap depth tvv '((gain 1.0)))
(defq over_pixels (slice (tri-pixels pixmap) 0 1600)
	mixed (reduce! (# (if (and (/= (get-uint under_pixels (* (!) 4)) 0) (/= (get-uint half_pixels (* (!) 4)) 0)) (push %0 (* (!) 4)) %0)) '() (list) 0 400))
(assert-true "see through over solid, pixels where both are" (> (length mixed) 10))
(assert-true "see through over solid, a mix of the two, and as good as solid"
	(every (# (defq p (get-uint over_pixels %0))
			(and (>= (>> p 24) 0xfe) (/= p (get-uint under_pixels %0)) (/= p (get-uint half_pixels %0)))) mixed))
;a frame drawn a part at a time is the frame
(defq canvas (Canvas 20 20 1) pixmap (getf canvas +canvas_pixmap 0) depth (shader-vp-depth 20 20))
(each (lambda ((x y x1 y1)) (shader-vp-draw-tris pipe tverts ttris pixmap depth tvv tpv :nil 3 x y x1 y1))
	'((0 0 20 7) (0 7 9 20) (9 7 20 20)))
(assert-eq "a frame drawn in three parts" (first drawn) (slice (tri-pixels pixmap) 0 1600))
;and by strips, each with a depth buffer of only its own rows, and the
;vertices as the bytes they are in a message
(defq canvas (Canvas 20 20 1) pixmap (getf canvas +canvas_pixmap 0)
	tbytes (shader-verts-str tverts))
(assert-eq "vertices as bytes" (* (length tverts) 8) (length tbytes))
(each (lambda ((y y1)) (shader-vp-draw-tris pipe tbytes ttris pixmap (shader-vp-depth 20 (- y1 y))
			tvv tpv :nil 3 0 y 20 y1 y))
	'((0 6) (6 13) (13 20)))
(assert-eq "a frame drawn in three strips, each with a depth buffer of its own rows"
	(first drawn) (slice (tri-pixels pixmap) 0 1600))
;the near plane. A triangle with one vertex behind it, or two, is cut,
;and what is in front is drawn, whichever of its vertices comes first
(defq tnear (cat tverts (apply (const doubles) (map (const n2d) '(0.3 0.2 -3.0 1   0 1 0   1)))))
(each (lambda ((title crc &rest tris))
		(defq got (map (# (tri-draw pipe_solid %0 :nil :nil 3 0 0 20 20 tnear)) tris))
		(assert-true (cat "the near plane, " title ", some of it is drawn") (> (tri-drawn (first got)) 0))
		(assert-true (cat "the near plane, " title ", whichever vertex comes first")
			(every (# (eql %0 (first got))) got))
		(assert-eq (cat "the near plane, " title ", the pixels as they were") crc (crc32 (first got))))
	(list (list "one vertex behind" 3909020676 (nums 0 1 9) (nums 1 9 0) (nums 9 0 1))
		(list "one vertex in front" 2309378785 (nums 2 9 10) (nums 9 10 2) (nums 10 2 9))
		(list "cut ones among whole ones" 284662683 (nums 0 1 2 0 1 9 3 4 5 10 2 9 6 7 8))))
(assert-eq "the near plane, one all behind and one all in front, only the one in front"
	(tri-draw pipe_solid (nums 0 1 2) :nil :nil 3 0 0 20 20 tnear)
	(tri-draw pipe_solid (nums 9 10 9 0 1 2) :nil :nil 3 0 0 20 20 tnear))
(assert-eq "the near plane, all behind, nothing drawn" 0
	(tri-drawn (tri-draw pipe_solid (nums 9 10 9) :nil :nil 3 0 0 20 20 tnear)))
;triangles of 4 numbers, as a mesh has them, the 4th is not a vertex
(assert-eq "triangles of 4 numbers"
	(tri-draw pipe (nums 0 1 2 3 4 5)) (tri-draw pipe (nums 0 1 2 99 3 4 5 99) :nil :nil 4))
;nothing to draw
(assert-eq "no triangles, nothing drawn" 0 (tri-drawn (tri-draw pipe (nums))))
;a pixel shader with no varyings goes with it too, and one function of
;each kind serves every pair it is in. Each byte is the nearest to 255 of
;what it gave, 64, 128 and 191, where the solid ones are drawn
(defq flat (sh-main "(vec4 0.25 0.5 0.75 1.0)") pipe2 (shader-vp-pipeline tvert flat)
	flat_pixels (tri-draw pipe2 ttris))
(assert-true "a pixel shader with no varyings"
	(not (some! (# (defq p (get-uint flat_pixels (* (!) 4)))
			(if (= (get-uint solid (* (!) 4)) 0) (/= p 0) (/= p 0xff4080bf))) '() :nil 0 400)))
(assert-list-eq "the one vertex function for both pairs" (first (third pipe)) (first (third pipe2)))
(assert-error "a pair that does not go together" (shader-vp-pipeline tvert
		(sh-src "(defvarying nope :float)" "(defun main :vec4 ((frag :vec2)) (vec4 nope))")) {Shader, the vertex shader has no such varying !})

;the Metal text for a pair, a vertex function and a fragment function
(bind '(mv mf) (shader-msl-pair vert pix))
(each (lambda ((name text want))
		(assert-true (cat "MSL pair, " name) (every (# (find %0 (split text sh_lf))) want)))
	(list
		(list "the vertex function" mv (list
				"vertex VertexOut vertex_main(VertexIn in [[stage_in]], constant Inputs &inputs [[buffer(0)]])"
				"\tfloat4x4 model_;" "\tfloat3 position_ [[attribute(0)]];" "\tfloat3 normal_ [[attribute(1)]];"
				"\tfloat shade_ [[user(locn0)]];" "\tfloat3 color_ [[user(locn1)]];" "\tfloat2 unset_ [[user(locn2)]];"
				"\tout.position = float4(p.x, p.y, (p.z + p.w) * 0.5, p.w);"))
		(list "the fragment function, its varyings by their place in the vertex shader" mf (list
				"\tfloat3 color_ [[user(locn1)]];" "\tfloat shade_ [[user(locn0)]];"
				"\tfloat4 c = clamp(s.shader_main(float2(in.position.x, float2(target.size).y - in.position.y)), 0.0, 1.0);"
				"\tif (c.w < 0.003921568627) discard_fragment();"
				"\treturn float4(c.xyz * c.w, c.w);"))))
(assert-true "MSL pair, a matrix times a vec3" (found? mv "sh_mul3(model_, normal_)"))
(assert-error "MSL pair, two that do not go together" (shader-msl-pair vert
		(sh-src "(defvarying nope :float)" "(defun main :vec4 ((frag :vec2)) (vec4 nope))")) {Shader, the vertex shader has no such varying !})

;the GLSL text for a pair, a vertex shader and a fragment shader
(bind '(gv gf) (shader-glsl-pair vert pix))
(each (lambda ((name text want))
		(assert-true (cat "GLSL pair, " name) (every (# (find %0 (split text sh_lf))) want)))
	(list
		(list "the vertex shader" gv (list
				"precision highp float;" "uniform mat4 model;"
				"attribute vec3 position;" "attribute vec3 normal;"
				"varying float shade;" "varying vec3 color;" "varying vec2 unset;"
				"\tunset = vec2(0.0);" "\tgl_Position = shader_main();"))
		(list "the fragment shader, its varyings by name" gf (list
				"precision highp float;" "varying vec3 color;" "varying float shade;"
				"\tvec4 c = clamp(shader_main(gl_FragCoord.xy), 0.0, 1.0);"
				"\tif (c.a < 0.003921568627) discard;"
				"\tgl_FragColor = vec4(c.rgb * c.a, c.a);"))))
(assert-true "GLSL pair, a matrix times a vec3" (found? gv "sh_mul3(model, normal)"))
(assert-true "GLSL pair, the fragment shader has no attribute" (not (found? gf "attribute")))
(assert-true "GLSL pair, a varying the pixel shader does not read is not in it" (not (found? gf "unset")))
(assert-error "GLSL pair, two that do not go together" (shader-glsl-pair vert
		(sh-src "(defvarying nope :float)" "(defun main :vec4 ((frag :vec2)) (vec4 nope))")) {Shader, the vertex shader has no such varying !})
(assert-error "GLSL pair, the wrong way round" (shader-glsl-pair pix vert) {Shader, not a vertex shader !})
;the two of the Mesh demo, which a GLSL compiler has taken and linked
(bind '(gv gf) (shader-glsl-pair (shader-load "lib/gpu/shaders/mesh_vertex.shader")
		(shader-load "lib/gpu/shaders/mesh_lit.shader")))
(assert-true "GLSL pair, the Mesh demo's vertex shader" (find "\tboth = (lens * model);" (split gv sh_lf)))
(assert-true "GLSL pair, the Mesh demo's fragment shader" (find "uniform vec4 color;" (split gf sh_lf)))

;the SPIR-V modules for a pair. Each is a stream of instructions that
;parses to its end, with every id made once, and has what a vertex and a
;fragment module of a pair must have
(bind '(sv sf) (shader-spirv-pair vert pix))
(each (lambda ((name module model entry has))
		(defq words (spvt-words module) insts (spvt-insts module))
		(assert-true (cat "SPIR-V pair, " name ", stream") insts)
		(assert-list-eq (cat "SPIR-V pair, " name ", ids") (map! (const !) '() 1 (elem-get words 3)) (spvt-ids insts))
		(assert-true (cat "SPIR-V pair, " name ", entry point")
			(apply spvt-has? (cat (list insts 15 model :nil) (spv-str entry))))
		(each (lambda ((what &rest inst))
				(assert-true (cat "SPIR-V pair, " name ", " what) (apply spvt-has? (cat (list insts) inst)))) has))
	(list
		(list "vertex" sv 0 "vertex_main" '(
				("a matrix type" 24 :nil :nil 4)
				("where the vertex is" 71 :nil 11 0)
				("an attr at location 1" 71 :nil 30 1)
				("a varying at location 2" 71 :nil 30 2)
				("its block in set 1" 71 :nil 34 1)
				("a matrix a column at a time" 72 :nil 0 5)
				("16 bytes a column" 72 :nil 0 7 16)
				("a matrix times a matrix" 146)
				("a matrix times a vector" 145)))
		(list "fragment" sf 4 "fragment_main" '(
				("the frag coord" 71 :nil 11 15)
				("origin at the upper left" 16 :nil 7)
				("its blocks in set 3" 71 :nil 34 3)
				("the size of the target at binding 1" 71 :nil 33 1)))))
(assert-error "SPIR-V pair, two that do not go together" (shader-spirv-pair vert
		(sh-src "(defvarying nope :float)" "(defun main :vec4 ((frag :vec2)) (vec4 nope))")) {Shader, the vertex shader has no such varying !})

;a vertex shader, or a pixel shader with varyings, is half of a pair
(assert-error "GLSL, a vertex shader is half of a pair" (shader-glsl vert) {Shader, the GLSL back end has no vertex stage, varyings or matrix yet !})
(assert-error "MSL, a pixel shader with varyings is half of a pair" (shader-msl pix) {Shader, the MSL back end has no vertex stage, varyings or matrix yet !})
(assert-error "SPIR-V, a vertex shader is half of a pair" (shader-spirv vert) {Shader, the SPIR-V back end has no vertex stage, varyings or matrix yet !})
(assert-error "VP, a pixel shader with varyings shades no tile" (shader-vp pix) {Shader, the VP back end has no vertex stage, varyings or matrix yet !})
(assert-error "VP, a vertex shader is not a pixel shader" (shader-vp vert) {Shader, the VP back end has no vertex stage, varyings or matrix yet !})

(report-header "GPU: shader language, functions for Lisp to call")

;a file with no main is functions. Each is a native function that Lisp
;calls, and gives what the matrix library and the sums worked out by hand
;give, near enough, as it works in 32 bit floats
(defq fn_prog (sh-src
		"(defconst half 0.5)"
		"(defglobal twice (* half 4.0))"
		"(defun mul :mat4 ((a :mat4) (b :mat4)) (* a b))"
		"(defun place :vec4 ((m :mat4) (p :vec4)) (* m p))"
		"(defun turn :vec3 ((m :mat4) (n :vec3)) (* m n))"
		"(defun same :mat4 ((m :mat4)) m)"
		"(defun wave :float ((x :float) (k :float)) (+ (sin (* x k)) (pow (abs x) 1.5) half))"
		"(defun mix3 :vec3 ((a :vec3) (b :vec3) (t :float)) (+ (* a (- 1.0 t)) (* b t)))"
		"(defun bend :float ((x :float)) (wave x twice))"
		"(defun count :int ((n :int) (x :float)) (defq c n) (for (i 0 5) (if (> x 0.5) (setq c (+ c i)))) c)"
		"(defun none :float () (* half twice))")
	fn_a (mat4x4-rotx (n2d 0.5))
	fn_b (mat4x4-frustum (n2d -1) (n2d 1) (n2d 1) (n2d -1) (n2d 2) (n2d 6))
	fn_p (doubles (n2d 0.1) (n2d 0.2) (n2d -3) (n2d 1))
	fn_n (doubles (n2d 0) (n2d 0.6) (n2d 0.8)))
(assert-eq "no main, so it is functions" :func (shader-stage fn_prog))
(assert-list-eq "what a function gives and takes" '(place :vec4 ((m :mat4) (p :vec4)))
	(shader-func fn_prog 'place))
(assert-eq "all its functions" 9 (length (shader-funcs fn_prog)))
(defun fn-nums (v)
	;what a function gave, or what is wanted of it, as fixeds
	(map (const n2q) (if (doubles? v) v (list v))))
(defun fn-wave (x k)
	;a power of 1.5 is a number times its root
	(+ (sin (* x k)) (* (abs x) (sqrt (abs x))) (n2d 0.5)))
(each (lambda ((name args want))
		(assert-true (cat "native code, " (str name))
			(sh-near? (fn-nums want) (fn-nums (apply (shader-vp-func fn_prog name) args)))))
	(list (list 'mul (list fn_a fn_b) (mat4x4-mul fn_a fn_b))
		(list 'place (list fn_b fn_p) (mat4x4-vec4-mul fn_b fn_p))
		(list 'turn (list fn_a fn_n) (mat4x4-vec3-mul fn_a fn_n))
		(list 'same (list fn_b) fn_b)
		(list 'wave (list (n2d 0.7) (n2d 3)) (fn-wave (n2d 0.7) (n2d 3)))
		(list 'mix3 (list fn_n (doubles (n2d 1) (n2d 2) (n2d 3)) (n2d 0.25))
			(nums-add (nums-scale fn_n (n2d 0.75)) (nums-scale (doubles (n2d 1) (n2d 2) (n2d 3)) (n2d 0.25))))
		(list 'bend (list (n2d 0.3)) (fn-wave (n2d 0.3) (n2d 2)))
		(list 'count (list 5 (n2d 0.9)) 15) (list 'count (list 5 (n2d 0.1)) 5)
		(list 'none (list) (n2d 1))))
(defq fn_mul (shader-vp-func fn_prog 'mul) fn_place (shader-vp-func fn_prog 'place)
	fn_turn (shader-vp-func fn_prog 'turn))
;a float is a 32 bit float, as a GPU has it, each sum the nearest one
(defq fn_single (sh-src "(defun add :float ((a :float) (b :float)) (+ a b))"
		"(defun over :float ((a :float)) (/ 1.0 a))" "(defun tenth :float () 0.1)"))
(defun fn-single (r)
	;the double that is the nearest 32 bit float to r
	(sh-float-to-double (sh-double-to-float r)))
(assert-eq "a float is 32 bits, 2^24 and 1 is 2^24" (n2d 16777216)
	((shader-vp-func fn_single 'add) (n2d 16777216) (n2d 1)))
(assert-eq "a third is the nearest 32 bit float to it" (fn-single (/ (n2d 1) (n2d 3)))
	((shader-vp-func fn_single 'over) (n2d 3)))
(assert-eq "a tenth, a constant, is the nearest 32 bit float to it" (fn-single (str-to-double "0.1"))
	((shader-vp-func fn_single 'tenth)))
(assert-eq "an arg is made the nearest 32 bit float to it" (fn-single (str-to-double "0.1"))
	((shader-vp-func fn_single 'add) (str-to-double "0.1") (n2d 0)))
;a cond, run, each clause in turn
(defq fn_pick (shader-vp-func (sh-src "(defun pick :float ((x :float)) (cond ((> x 1.0) 2.0) ((> x 0.0) 1.0) (:t 0.0)))") 'pick))
(assert-list-eq "a cond picks its clause" (list (n2d 2) (n2d 1) (n2d 0))
	(map (# (fn_pick (n2d %0))) '(3 0.5 -1)))
;what comes back is new each time, and the args are as they were
(defq fn_before (cat fn_b) fn_one (fn_mul fn_a fn_b) fn_two (fn_mul fn_a fn_b))
(assert-true "a doubles of 16 comes back" (and (doubles? fn_one) (= (length fn_one) 16)))
(elem-set fn_one 0 (n2d 99))
(assert-true "new each time" (not (eql (str fn_one) (str fn_two))))
(assert-eq "the args are as they were" (str fn_before) (str fn_b))
(assert-true "a float comes back as a double" (double? ((shader-vp-func fn_prog 'none))))
(assert-true "an int comes back as a num" (num? ((shader-vp-func fn_prog 'count) 1 (n2d 0))))
(assert-list-eq "same program, same function" fn_mul (shader-vp-func fn_prog 'mul))

;a doubles arg is read where it is, the first three of them, and what is
;given is written where it goes. A function that sets an arg has a copy
;of it, and so has a fourth doubles arg, so no arg is ever changed
(defq fn_more (sh-src
		"(defun sets :vec3 ((a :vec3) (k :float)) (setq a (* a k)) (+ a (vec3 (sin k))))"
		"(defun part :vec3 ((a :vec3)) (setq (:y a) 9.0) a)"
		"(defun four :vec4 ((a :vec4) (b :vec4) (c :vec4) (d :vec4) (k :float)) (+ (* a k) b (* c d)))"
		"(defun chain :mat4 ((a :mat4) (b :mat4) (c :mat4)) (* a b c))"
		"(defun keeps :mat4 ((a :mat4) (b :mat4)) (defq m (* a b)) (setq m (* m a)) m)")
	fn_v (doubles (n2d 1) (n2d 2) (n2d 3) (n2d 4)) fn_w (doubles (n2d 0.5) (n2d -1) (n2d 2) (n2d 0.25))
	fn_n3 (doubles (n2d 0) (n2d 0.6) (n2d 0.8)))
(defq fn_s (sin (n2d 2)))
(each (lambda ((name args want))
		(defq before (map (const str) args) got (apply (shader-vp-func fn_more name) args))
		(assert-true (cat "args where they are, native code, " (str name))
			(sh-near? (fn-nums want) (fn-nums got)))
		(assert-list-eq (cat "args where they are, no arg is changed, " (str name))
			before (map (const str) args)))
	(list (list 'sets (list fn_n3 (n2d 2)) (nums-add (nums-scale fn_n3 (n2d 2)) (doubles fn_s fn_s fn_s)))
		(list 'part (list fn_n3) (doubles (first fn_n3) (n2d 9) (third fn_n3)))
		(list 'four (list fn_v fn_w fn_v fn_w (n2d 3)) (nums-add (nums-add (nums-scale fn_v (n2d 3)) fn_w) (nums-mul fn_v fn_w)))
		(list 'chain (list fn_a fn_b fn_a) (mat4x4-mul (mat4x4-mul fn_a fn_b) fn_a))
		(list 'keeps (list fn_a fn_b) (mat4x4-mul (mat4x4-mul fn_a fn_b) fn_a))))
(assert-true "the same doubles as two args"
	(sh-near? (fn-nums (mat4x4-mul fn_a fn_a)) (fn-nums ((shader-vp-func fn_prog 'mul) fn_a fn_a))))

;what is refused, by the language
(assert-error "no functions at all" (sh-src "(defconst a 1.0)") {Shader, no functions !})
(assert-error "functions have args, not inputs"
	(sh-src "(definput a :float 1.0)" "(defun f :float () a)") {Shader, functions for Lisp have no inputs, attrs or varyings, they have args !})
(assert-error "functions have no attrs" (sh-src "(defattr p :vec3)" "(defun f :float () 1.0)") {Shader, functions for Lisp have no inputs, attrs or varyings, they have args !})
(assert-error "a function with a matrix is not called by another"
	(sh-src "(defun f :vec4 ((m :mat4) (p :vec4)) (* m p))"
		"(defun g :vec4 ((m :mat4) (p :vec4)) (f m p))") {Shader, a function with a matrix is for Lisp to call !})
(assert-error "no such function" (shader-vp-func fn_prog 'nope) {Shader, no such function !})
(assert-error "a shader is not functions" (shader-vp-func (sh-main "(vec4 1.0)") 'main) {Shader, not functions for Lisp, it has a main !})
(assert-error "functions are not a pixel shader" (shader-vp fn_prog) {Shader, the VP back end has no functions for Lisp yet !})
(assert-error "functions are not a vertex shader" (shader-vp-vertex fn_prog) {Shader, not a vertex shader !})
(assert-error "GLSL, not yet" (shader-glsl fn_prog) {Shader, the GLSL back end has no functions for Lisp yet !})
(assert-error "MSL, not yet" (shader-msl fn_prog) {Shader, the MSL back end has no functions for Lisp yet !})
(assert-error "SPIR-V, not yet" (shader-spirv fn_prog) {Shader, the SPIR-V back end has no functions for Lisp yet !})
;and by the native code
(assert-error "too few args" (fn_mul fn_a) {(mul a b) wrong_types})
(assert-error "a doubles of the wrong length" (fn_place fn_b fn_n) {(place m p) wrong_types})
(assert-error "a number where a doubles should be" (fn_place fn_b (n2d 1)) {(place m p) wrong_types})
(assert-error "a fixed where a double should be" ((shader-vp-func fn_prog 'bend) 0.5) {(bend x) wrong_types})

(report-header "GPU: shader language, a file is known by a hash of it")

;a file is loaded with only its head read, and a hash of it taken. Its
;native code is kept under that hash, so a file that has been met before
;costs no reading or checking of its functions
(defq kf_file "apps/demos/surface/surface.shader" kf_prog (shader-load kf_file))
(assert-eq "only the head is read" :lazy (second kf_prog))
(assert-eq "a key of 32 hex digits" 32 (length (elem-get kf_prog 5)))
(assert-eq "the key is the hash of the source, the file and what it imports" (shader-key (shader-text kf_file)) (elem-get kf_prog 5))
(assert-eq "of a file that imports nothing, the hash of the file"
	(shader-key (load "apps/demos/flight/moon_lit.shader")) (elem-get (shader-load "apps/demos/flight/moon_lit.shader") 5))
(assert-eq "the same file, the same key" (elem-get kf_prog 5) (elem-get (shader-load kf_file) 5))
(assert-true "another file, another key"
	(not (eql (elem-get kf_prog 5) (elem-get (shader-load "apps/demos/raymarch/film.shader") 5))))
(assert-true "a byte more, another key" (not (eql (shader-key "abc") (shader-key "abc "))))
;what a caller of a ready made function needs is there without the rest
(assert-eq "its kind" :pixel (shader-stage kf_prog))
(assert-true "its inputs" (nempty? (first kf_prog)))
(assert-true "its block can be packed" (> (length (shader-pack kf_prog)) 0))
(defq kf_vert (shader-load "lib/gpu/shaders/mesh_vertex.shader"))
(assert-list-eq "the attrs of a vertex shader" '((position :vec4) (normal :vec3)) (shader-attrs kf_vert))
(assert-list-eq "and its varyings" '((facing :vec3) (fade :float)) (shader-varyings kf_vert))
;the native function is made once, and after that is found by the key
;with the program still not read
(defq kf_first (shader-vp kf_prog) kf_again (shader-load kf_file) kf_second (shader-vp kf_again))
(assert-eq "found again with only the head read" :lazy (second kf_again))
(assert-list-eq "the same function, and the same size of frame" kf_first kf_second)
;made here first, this test must not lean on another having made it
(shader-vp-pipeline (shader-load "lib/gpu/shaders/mesh_vertex.shader")
	(shader-load "lib/gpu/shaders/mesh_lit.shader"))
(defq kf_pipe (shader-vp-pipeline (shader-load "lib/gpu/shaders/mesh_vertex.shader")
		(shader-load "lib/gpu/shaders/mesh_lit.shader")))
(assert-eq "a pipeline found again, its vertex shader not read" :lazy (second (first kf_pipe)))
(assert-eq "nor its pixel shader" :lazy (second (second kf_pipe)))
;the rest is read when it is asked for, into the same program
(defq kf_same (shader-full kf_again))
(assert-true "all of it, when it is wanted" (list? (second kf_again)))
(assert-list-eq "the same program" kf_again kf_same)
(assert-eq "and what a function gives is there" :vec4
	(second (some (# (if (eql (first %0) 'main) %0)) (shader-funcs kf_again))))
;a program from text has a key too, and is all there from the start
(defq kf_text (sh-main "(vec4 0.25)"))
(assert-eq "from text, a key" 32 (length (elem-get kf_text 5)))
(assert-true "from text, all of it" (list? (second kf_text)))
;a back end that makes text reads the rest for itself
(defq kf_atom (shader-load "apps/science/molecule/atom.shader"))
(assert-eq "before a back end is asked for, only the head" :lazy (second kf_atom))
(shader-glsl kf_atom)
(assert-true "the back end read the rest" (list? (second kf_atom)))

;which make of a back end it is, is a hash of the files it is written in,
;and is part of the name of all it keeps, so a change to a back end can
;not find what the old one made
(assert-eq "a make of 8 hex digits" 8 (length (shader-make '("lib/gpu/vp.inc" "lib/gpu/shader.inc"))))
(assert-eq "the same files, the same make"
	(shader-make '("lib/gpu/vp.inc" "lib/gpu/shader.inc")) (shader-make '("lib/gpu/vp.inc" "lib/gpu/shader.inc")))
(assert-true "other files, another make"
	(not (eql (shader-make '("lib/gpu/vp.inc" "lib/gpu/shader.inc")) (shader-make '("lib/gpu/msl.inc" "lib/gpu/shader.inc")))))
;what a back end makes of a program for the GPU is kept under a name,
;and made only if it is not there
(defq kt_name (cat "test_kept_" (str (pii-time)) "_" (hex-encode (task-mbox)) ".txt")
	kt_made 0
	kt_first (shader-kept-text kt_name (# (setq kt_made (inc kt_made)) "made the once"))
	kt_second (shader-kept-text kt_name (# (setq kt_made (inc kt_made)) "made again")))
(assert-eq "kept text, as it was made" "made the once" kt_first)
(assert-eq "kept text, found the next time" "made the once" kt_second)
(assert-eq "kept text, made the once" 1 kt_made)
(pii-remove (cat (load-path) "lib/gpu/jit/" kt_name))
(defq kt_msl (shader-kept-text (cat "test_kept_" (elem-get kf_prog 5) ".msl") (# (shader-msl kf_prog))))
(assert-eq "the Metal text of a file, kept, is what the back end makes" (shader-msl (shader-load kf_file)) kt_msl)
(pii-remove (cat (load-path) "lib/gpu/jit/test_kept_" (elem-get kf_prog 5) ".msl"))

;an import, (import path): the functions, consts and globals of another
;file, read in its place, each file once, and the key of a program is of
;all of it, so a change to a file it imports is a change of key. The files
;are the test's own, and gone after
(defq si_tag (hex-encode (task-mbox))
	si_lib (cat "tests/scratch/tmp_si_" si_tag "_lib.shader") si_lib2 (cat "tests/scratch/tmp_si_" si_tag "_lib2.shader")
	si_main (cat "tests/scratch/tmp_si_" si_tag "_main.shader") si_dot (cat "tests/scratch/tmp_si_" si_tag "_dot.shader")
	si_a (cat "tests/scratch/tmp_si_" si_tag "_a.shader") si_b (cat "tests/scratch/tmp_si_" si_tag "_b.shader")
	si_bad (cat "tests/scratch/tmp_si_" si_tag "_bad.shader") si_late (cat "tests/scratch/tmp_si_" si_tag "_late.shader")
	si_use_bad (cat "tests/scratch/tmp_si_" si_tag "_use_bad.shader")
	si_twice "(defun twice :float ((x :float)) (* x 2.0))"
	si_body "(defun main :vec4 ((frag :vec2)) (vec4 (twice 0.25)))")
(defun si-imp (file) (cat "(import " (str "\q") file (str "\q") ")" sh_lf))
(save si_twice si_lib)
(defq si_thrice "(defun thrice :float ((x :float)) (+ x (twice x)))")
(save (cat (si-imp si_lib) si_thrice) si_lib2)
;the first library twice, as itself and by the second, read once
(save (cat (si-imp si_lib) (si-imp si_lib2) si_body) si_main)
(defq si_loaded (shader-full (shader-load si_main)) si_pasted (sh-src si_twice si_thrice si_body))
(assert-eq "an imported function is the GLSL of one written in" (shader-glsl si_pasted) (shader-glsl si_loaded))
(assert-list-eq "and draws the same" '(0.5 0.5 0.5 0.5) (sh-pixel si_loaded))
(defq si_key (elem-get (shader-load si_main) 5))
(save "(defun twice :float ((x :float)) (+ x x))" si_lib)
(assert-true "a change to a file it imports is a change of key" (not (eql si_key (elem-get (shader-load si_main) 5))))
(save (cat "(import " (str "\q") "./" (slice si_lib (length "tests/scratch/") -1) (str "\q") ")" sh_lf si_body) si_dot)
(assert-true "a path with ./ is from the folder of the file" (shader-load si_dot))
(save (cat (si-imp si_b) "(defun fa :float ((x :float)) x)") si_a)
(save (cat (si-imp si_a) "(defun fb :float ((x :float)) x)") si_b)
(assert-error "a loop of imports" (shader-load si_a) {Shader, a loop of imports !})
(save "(definput level :float)" si_bad)
(save (cat (si-imp si_bad) si_body) si_use_bad)
(assert-error "an imported file has no inputs" (shader-load si_use_bad) {Shader, an imported file has only functions, consts and globals !})
(save (cat si_twice sh_lf (si-imp si_lib2) si_body) si_late)
(assert-error "an import comes first" (shader-load si_late) {Shader, an import comes before any other declaration !})
(each (const pii-remove) (list si_lib si_lib2 si_main si_dot si_a si_b si_bad si_late si_use_bad))
