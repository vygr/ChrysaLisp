(report-header "GPU: shader language, type checker")

(import "lib/gpu/glsl.inc")
(import "lib/gpu/msl.inc")
(import "lib/gpu/spirv.inc")
(import "lib/gpu/cpu.inc")
(import "lib/gpu/vp.inc")

(defq sh_lf (ascii-char 10))

(defun sh-src (&rest lines)
	;a program from the lines of its source
	(shader-compile (shader-read (string-stream (join lines sh_lf)))))

(defun sh-main (&rest lines)
	;a program that is a main with this body
	(sh-src (cat "(defun main :vec4 ((frag :vec2)) " (join lines " ") ")")))

(defun sh-pixel (program &optional vals x y)
	;the vec4 the CPU back end gives for one pixel, as fixeds
	(setd x 0 y 0)
	(map (const n2f) (first (apply (shader-cpu program)
		(cat (list x y (inc x) (inc y)) (shader-cpu-args program vals))))))

(defun sh-pixel-vp (program &optional vals x y)
	;the vec4 the VP back end gives for one pixel, as fixeds
	(setd x 0 y 0)
	(defq native (shader-vp program))
	(map (const n2f) (first (shader-vp-pixels native
		(shader-vp-frame program native vals) x y (inc x) (inc y)))))

(defun sh-near? (a b)
	(and (= (length a) (length b))
		(every (# (< (abs (- %0 %1)) 0.0015)) a b)))

(defmacro assert-pixel (name expected program &rest args)
	; (assert-pixel name expected program [vals x y])
	;both the CPU back end and the VP back end must give the pixel
	(defq res (gensym) exp (gensym) prg (gensym))
	`(progn
		(defq ,prg ,program ,exp ,expected ,res (sh-pixel ,prg ~args))
		(if (sh-near? ,exp ,res)
			(test-pass ,name)
			(test-fail ,name ,exp ,res))
		(setq ,res (sh-pixel-vp ,prg ~args))
		(if (sh-near? ,exp ,res)
			(test-pass (cat ,name ", native"))
			(test-fail (cat ,name ", native") ,exp ,res))))

;the tree for a small program
(defq prog (sh-src
	"(definput k :float 1.5 0.0 4.0)"
	"(definput n :int 3)"
	"(definput size :vec2)"
	"(defconst two 2.0)"
	"(defglobal g (* k two))"
	"(defun half :float ((a :float)) (return (* a 0.5)))"
	"(defun main :vec4 ((frag :vec2)) (return (vec4 (half g) frag 1.0)))"))
(bind '(inputs consts globals funcs &rest _) prog)
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
(assert-error "unknown name" (sh-main "(return (vec4 nope))"))
(assert-error "unknown function" (sh-main "(return (nope 1.0))"))
(assert-error "unknown statement" (sh-main "(print 1.0)" "(return (vec4 1.0))"))
(assert-error "int with float" (sh-main "(return (vec4 (+ 1 2.0)))"))
(assert-error "vec2 with vec3" (sh-main "(return (vec4 (+ (vec2 1.0) (vec3 1.0)) 1.0))"))
(assert-error "test is not a bool" (sh-main "(if 1.0 (return (vec4 1.0)))" "(return (vec4 0.0))"))
(assert-error "wrong return type" (sh-main "(return 1.0)"))
(assert-error "no return" (sh-main "(defq a 1.0)"))
(assert-error "no return on one path" (sh-main "(if (> (:x frag) 1.0) (return (vec4 1.0)))"))
(assert-error "local hides a parameter" (sh-main "(defq frag 1.0)" "(return (vec4 1.0))"))
(assert-error "local hides a local" (sh-main "(defq a 1.0)" "(when (> a 0.0) (defq a 2.0))" "(return (vec4 a))"))
(assert-error "local out of scope" (sh-main "(when (> (:x frag) 0.0) (defq a 2.0))" "(return (vec4 a))"))
(assert-error "local is a reserved word" (sh-main "(defq sin 1.0)" "(return (vec4 1.0))"))
(assert-error "setq changes the type" (sh-main "(defq a 1.0)" "(setq a 1)" "(return (vec4 a))"))
(assert-error "setq of a loop counter" (sh-main "(for (i 0 4) (setq i 2))" "(return (vec4 1.0))"))
(assert-error "setq of a const" (sh-src "(defconst c 1.0)" "(defun main :vec4 ((frag :vec2)) (setq c 2.0) (return (vec4 c)))"))
(assert-error "setq of an input" (sh-src "(definput k :float)" "(defun main :vec4 ((frag :vec2)) (setq k 2.0) (return (vec4 k)))"))
(assert-error "component out of range" (sh-main "(return (vec4 (:z frag)))"))
(assert-error "component set twice" (sh-main "(defq a (vec2 1.0))" "(setq (:xx a) (vec2 1.0))" "(return (vec4 a a))"))
(assert-error "break outside a loop" (sh-main "(break)" "(return (vec4 1.0))"))
(assert-error "loop bound is not a constant" (sh-src "(definput n :int)" "(defun main :vec4 ((frag :vec2)) (for (i 0 n)) (return (vec4 1.0)))"))
(assert-error "const from an input" (sh-src "(definput k :float)" "(defconst c (* k 2.0))" "(defun main :vec4 ((frag :vec2)) (return (vec4 c)))"))
(assert-error "wrong args to function" (sh-src "(defun f :float ((a :float)) (return a))" "(defun main :vec4 ((frag :vec2)) (return (vec4 (f 1))))"))
(assert-error "recursion" (sh-src "(defun f :float ((a :float)) (return (f a)))" "(defun main :vec4 ((frag :vec2)) (return (vec4 (f 1.0))))"))
(assert-error "no main" (sh-src "(defun f :float ((a :float)) (return a))"))
(assert-error "wrong main" (sh-src "(defun main :vec3 ((frag :vec2)) (return (vec3 1.0)))"))
(assert-error "vector input with a default" (sh-src "(definput v :vec2 1.0)" "(defun main :vec4 ((frag :vec2)) (return (vec4 1.0)))"))
(assert-error "float input with an int default" (sh-src "(definput k :float 1)" "(defun main :vec4 ((frag :vec2)) (return (vec4 1.0)))"))
(assert-error "not a number" (sh-main {(return (vec4 "1.0e3"))}))
;the declarations were once input, const and global
(assert-error "input is not a declaration" (sh-src "(input k :float)" "(defun main :vec4 ((frag :vec2)) (return (vec4 k)))"))
(assert-error "const is not a declaration" (sh-src "(const c 1.0)" "(defun main :vec4 ((frag :vec2)) (return (vec4 c)))"))
(assert-error "global is not a declaration" (sh-src "(global g 1.0)" "(defun main :vec4 ((frag :vec2)) (return (vec4 g)))"))

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
(assert-error "value of the wrong type" (sh-main "1.0"))
(assert-error "a value that is not last" (sh-main "(vec4 1.0)" "(vec4 0.0)"))
(assert-error "an if with one arm is no value" (sh-main "(if (> (:x frag) 1.0) (vec4 1.0))"))
(assert-error "a when is no value" (sh-main "(when (> (:x frag) 1.0) (vec4 1.0))"))
(assert-error "a loop is no value" (sh-main "(for (i 0 4) (vec4 1.0))"))

(report-header "GPU: shader language, CPU back end")

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
(assert-pixel "compare and logic" '(1.0 1.0 1.0 1.0)
	(sh-main "(defq a 0.0 b 0.0 c 0.0 d 0.0)"
		"(if (and (< 1 2) (<= 2 2) (> 2.0 1.0) (>= 2.0 2.0)) (setq a 1.0))"
		"(if (or (= 1 2) (/= 1 2)) (setq b 1.0))"
		"(if (not (and (= 1 1) (= 1 2))) (setq c 1.0))"
		"(if (= (< 1 2) :t) (setq d 1.0))"
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
(assert-list-eq "args" (list (n2r 1.5) 3 (reals (n2r 0) (n2r 0))) (shader-cpu-args prog))
(defq out (apply (shader-cpu prog) (cat '(1 2 4 4) (shader-cpu-args prog '((size (1.0 1.0)))))))
(assert-eq "tile size" 6 (length out))
(assert-list-eq "tile order" '((1.5 2.5) (2.5 2.5) (3.5 2.5) (1.5 3.5) (2.5 3.5) (3.5 3.5))
	(map (# (map (const n2f) (slice %0 2 4))) out))

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
	(map (# (logand (code module 4 (* %0 4)) 0xffffffff)) (range 0 (/ (length module) 4))))

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
			((find op '(17 14 15 16 71 72 62 246 247 249 250 253 254 56)) ids)
			((find op '(11 19 20 21 22 23 30 32 33 248)) (push ids (first args)))
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
(assert-list-eq "SPIR-V vertex ids" (range 1 (elem-get words 3)) (spvt-ids insts))
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
(assert-list-eq "SPIR-V fragment ids" (range 1 (elem-get words 3)) (spvt-ids insts))
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

(defq module (shader-spirv (shader-load "lib/gpu/shaders/raymarch.shader"))
	words (spvt-words module) insts (spvt-insts module))
(assert-true "SPIR-V raymarch stream" insts)
(assert-list-eq "SPIR-V raymarch ids" (range 1 (elem-get words 3)) (spvt-ids insts))
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
	(map (# (get-uint blk (* %0 4))) (range 0 20)))
(defq blk (shader-pack prog (list '(b (1.0 -2.5 16777217)) '(d (0.3 64.0)) (list 'a (n2r 0.75)) '(c 9))))
(assert-list-eq "values packed" '(0x3f400000 0 0 0 0x3f800000 0xc0200000 0x4b800000 9 0x3e99999a 0x42800000 0x3e4ccccd)
	(map (# (get-uint blk (* %0 4))) (range 0 11)))
(defq vals (shader-unpack prog blk))
(assert-list-eq "unpacked names" '(a b c d e f g) (map (const first) vals))
(assert-list-eq "unpacked ints" '(9 7) (list (second (third vals)) (second (last vals))))
(assert-eq "unpacked float" (n2r 0.75) (second (first vals)))
(assert-list-eq "unpacked vector" (reals (n2r 1.0) (n2r -2.5) (n2r 16777216)) (second (second vals)))
(assert-pixel "block to the CPU back end" '(0.75 0.2 0.3 16777216.0) prog vals)
(assert-error "wrong size of vector" (shader-pack prog '((d (1.0 2.0 3.0)))))

(report-header "GPU: shader language, VP back end")

;a tile as 32 bit argb pixels, clamped, and with the top row first
(defq prog (sh-src "(definput k :float 4.0)" "(definput n :int 2)"
		"(defun main :vec4 ((frag :vec2)) (return (vec4 (/ frag k) (float n) -1.0)))")
	native (shader-vp prog) frame (shader-vp-frame prog native))
(assert-true "native function" (func? (first native)))
(assert-eq "frame holds the inputs" (n2r 4.0) (get-real frame 0))
(assert-eq "frame holds the ints" 2 (get-long frame 8))
(defq out (shader-vp-argb native frame 0 0 2 2))
(assert-eq "argb size" 16 (length out))
(assert-list-eq "argb tile" '(0xff1f1fff 0xff5f1fff 0xff1f5fff 0xff5f5fff)
	(map (# (get-uint out (* %0 4))) (range 0 4)))
(assert-list-eq "argb tile, top row first" '(0xff1fdfff 0xff5fdfff 0xff1f9fff 0xff5f9fff)
	(map (# (get-uint (shader-vp-argb native frame 0 0 2 2 4) (* %0 4))) (range 0 4)))
(assert-list-eq "argb tile, inputs given" '(0xff3f3f00 0xffbf3f00)
	(map (# (get-uint (shader-vp-argb native
		(shader-vp-frame prog native '((k 2.0) (n 0))) 0 0 2 1) (* %0 4))) (range 0 2)))
(assert-list-eq "same program, same function" (first native) (first (shader-vp prog)))
(defq pixels (shader-vp-pixels native frame 1 2 3 4))
(assert-list-eq "pixels tile" '((0.375 0.625) (0.625 0.625) (0.375 0.875) (0.625 0.875))
	(map (# (map (const n2f) (slice %0 0 2))) pixels))

;every float register is in use at once here, and a call saves them all
(assert-pixel "registers saved over calls" '(13.0 26.0 39.0 52.0)
	(sh-src "(defun f :vec4 ((a :vec4)) (return (* a 2.0)))"
		"(defun main :vec4 ((frag :vec2))"
		"(defq a (vec4 1.0 2.0 3.0 4.0))"
		"(return (+ a (+ (f a) (f (+ a (f (f a))))))))"))
;one more value held and there is no register for it, the CPU back end
;has no such limit
(defq prog (sh-src "(defun f :vec4 ((a :vec4)) (return (* a 2.0)))"
	"(defun main :vec4 ((frag :vec2))"
	"(defq a (vec4 1.0 2.0 3.0 4.0))"
	"(return (+ a (+ (f a) (+ (f (f a)) (+ a (f (+ a (f (f a))))))))))"))
(assert-list-eq "deep expression, CPU back end" '(18.0 36.0 54.0 72.0) (sh-pixel prog))
(assert-error "deep expression, VP back end" (shader-vp prog))
(assert-pixel "sin and pow in the middle of a vector" '(1.8415 4.5403 11.0 0.0)
	(sh-main "(defq a (vec4 1.0 2.0 3.0 4.0))"
		"(return (vec4 (+ (:x a) (sin (:x a))) (+ (* (:y a) 2.0) (cos (:x a)))"
		"(+ (:z a) (pow (:y a) (:z a))) (- (:w a) (pow (:y a) 2.0))))"))

(report-header "GPU: the raymarch shader")

(defq prog (shader-load "lib/gpu/shaders/raymarch.shader") text (shader-glsl prog))
(assert-eq "inputs block size" 64 (first (shader-layout prog)))
(assert-list-eq "inputs" '(time resolution arg_aa arg_aa_adaptive arg_aa_debug arg_depth
	arg_aa_limit arg_ao arg_ref arg_shadow arg_bump arg_dis arg_march) (map (const first) (first prog)))
(assert-eq "functions" 19 (length (elem-get prog 3)))
(assert-true "GLSL scene" (find (join '(
	"float scene(vec3 p)"
	"{"
	"	float d = 0.0000;"
	"	if ((arg_dis > 0.0000))"
	"	{"
	"		d = (sinusoidal_bump((p * 4.0000)) * arg_dis);"
	"	}"
	"	p = (fract(p) - 0.5000);"
	"	return (sphere(p, vec3(0.0000), 0.3500) + d);"
	"}") sh_lf) text))

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
;native code gives the pixels the Lisp back end does
(defq film (shader-load "apps/demos/raymarch/film.shader")
	film_vals '((resolution (96.0 96.0)) (cam_z -2.4) (light_x -0.02)))
(assert-true "film, GLSL" (> (length (shader-glsl film)) 1000))
(assert-true "film, MSL" (> (length (shader-msl film)) 1000))
(assert-true "film, SPIR-V" (> (length (shader-spirv film)) 1000))
(each (lambda ((x y))
	(assert-true (cat "film, native code and Lisp agree, " (str x) " " (str y))
		(sh-near? (sh-pixel film film_vals x y) (sh-pixel-vp film film_vals x y))))
	'((48 48) (10 80) (70 20)))

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
(assert-error "a vector times a matrix" (sh-vert-type "(defq a (* v m))" "v"))
(assert-error "a matrix times a vec2" (sh-vert-type "(defq a (* m (:xy v)))" "v"))
(assert-error "a matrix times a float" (sh-vert-type "(defq a (* m 2.0))" "v"))
(assert-error "a matrix added" (sh-vert-type "(defq a (+ m m))" "v"))
(assert-error "a matrix has no components" (sh-vert-type "(defq a (:x m))" "v"))
(assert-error "a matrix input has no default" (sh-src "(definput m :mat4 1.0)" "(defun main :vec4 () (vec4 0.0))"))

;what is not allowed
(assert-error "an attr can not be set"
	(sh-src "(defattr p :vec3)" "(defun main :vec4 () (setq p (vec3 0.0)) (vec4 p 1.0))"))
(assert-error "an attr of a matrix"
	(sh-src "(defattr p :mat4)" "(defun main :vec4 () (vec4 0.0))"))
(assert-error "a varying of an int"
	(sh-src "(defvarying p :int)" "(defun main :vec4 () (vec4 0.0))"))
(assert-error "a pixel shader with an attr"
	(sh-src "(defattr p :vec3)" "(defun main :vec4 ((frag :vec2)) (vec4 p 1.0))"))
(assert-error "a pixel shader that sets a varying"
	(sh-src "(defvarying c :vec3)" "(defun main :vec4 ((frag :vec2)) (setq c (vec3 0.0)) (vec4 c 1.0))"))
(assert-error "a varying set to the wrong type"
	(sh-src "(defvarying c :vec3)" "(defun main :vec4 () (setq c 1.0) (vec4 0.0))"))
(assert-error "a varying can not be a constant"
	(sh-src "(defvarying c :float)" "(defconst k (* c 2.0))" "(defun main :vec4 () (vec4 0.0))"))
(assert-error "main with the wrong parameters"
	(sh-src "(defun main :vec4 ((a :float)) (vec4 a))"))

;the inputs block, a matrix is its 4 columns, on a 16 byte boundary, and
;is given a row at a time
(assert-list-eq "layout with matrices" '(144 (model :mat4 0) (view :mat4 64) (tint :vec3 128))
	(shader-layout vert))
(defq rows (map (const n2r) (range 1 17))
	blk (shader-pack vert (list (list 'model rows) '(tint (0.5 0.25 1.0)))))
(assert-eq "block size" 144 (length blk))
(assert-list-eq "a matrix packed a column at a time"
	(map (const sh-real-to-float) '(1 5 9 13 2 6 10 14 3 7 11 15 4 8 12 16))
	(map (# (get-uint blk (* %0 4))) (range 0 16)))
(assert-list-eq "and comes back a row at a time" rows (second (first (shader-unpack vert blk))))
(assert-error "wrong size of matrix" (shader-pack vert '((model (1.0 2.0 3.0)))))

;the reference, the Lisp back end. The matrices are those of the matrix
;library, and where it puts a vertex is where the library does
(defq place (shader-cpu-vertex vert)
	model (mat4x4-mul (Mat4x4-translate (n2r 1) (n2r 2) (n2r 3)) (Mat4x4-rotx (n2r 0.5)))
	view (Mat4x4-frustum (n2r -1) (n2r 1) (n2r 1) (n2r -1) (n2r 2) (n2r 10))
	verts (list (list (reals (n2r 0.5) (n2r -0.25) (n2r 2)) (reals (n2r 0) (n2r 0) (n2r 1)))
		(list (reals (n2r -3) (n2r 1) (n2r 0.75)) (reals (n2r 0) (n2r 1) (n2r 0))))
	placed (apply place (cat (list verts)
		(shader-cpu-args vert (list (list 'model model) (list 'view view) '(tint (1.0 0.5 0.25))))))
	both (mat4x4-mul view model))
(assert-eq "a vertex out for each in" 2 (length placed))
(each (lambda ((position normal) (pos shade color unset))
	(assert-true (cat "where vertex " (str (!)) " is, as the matrix library has it")
		(sh-near? (map (const n2f) pos)
			(map (const n2f) (mat4x4-vec4-mul both (cat position (reals (n2r 1)))))))
	(defq n (mat4x4-vec3-mul model normal)
		want (max (n2f (third n)) 0.0))
	(assert-true (cat "varying of vertex " (str (!)) ", a float")
		(sh-near? (list (n2f shade)) (list want)))
	(assert-true (cat "varying of vertex " (str (!)) ", a vector")
		(sh-near? (map (const n2f) color) (list want (* want 0.5) (* want 0.25))))
	(assert-list-eq (cat "varying of vertex " (str (!)) ", not set, is 0") '(0.0 0.0)
		(map (const n2f) unset)))
	verts placed)

;a pixel shader reads varyings, by name, and goes with a vertex shader
;that has them
(defq pix (sh-src
	"(defvarying color :vec3)"
	"(defvarying shade :float)"
	"(defun main :vec4 ((frag :vec2)) (vec4 (* color shade) 1.0))"))
(assert-list-eq "the varyings of a pixel shader" '((color :vec3) (shade :float)) (shader-varyings pix))
(assert-list-eq "the pixel shader given its varyings" '(0.4 0.2 0.1 1.0)
	(sh-pixel pix '((color (0.8 0.4 0.2)) (shade 0.5))))
(assert-true "a pair" (lmatch? (shader-pair vert pix) (list vert pix)))
(assert-true "a pixel shader with no varyings goes with any vertex shader"
	(shader-pair vert film))
(assert-error "a varying the vertex shader has not got"
	(shader-pair vert (sh-src "(defvarying glow :float)" "(defun main :vec4 ((frag :vec2)) (vec4 glow))")))
(assert-error "a varying of another type"
	(shader-pair vert (sh-src "(defvarying shade :vec2)" "(defun main :vec4 ((frag :vec2)) (vec4 shade shade))")))
(assert-error "two pixel shaders are not a pair" (shader-pair pix pix))
(assert-error "two vertex shaders are not a pair" (shader-pair vert vert))
(assert-error "the reference places with a vertex shader" (shader-cpu-vertex pix))
(assert-error "and shades with a pixel shader" (shader-cpu vert))

;the back ends that have no vertex stage yet say so
(assert-error "GLSL, not yet" (shader-glsl vert))
(assert-error "MSL, not yet" (shader-msl pix))
(assert-error "SPIR-V, not yet" (shader-spirv vert))
(assert-error "VP, not yet" (shader-vp pix))
