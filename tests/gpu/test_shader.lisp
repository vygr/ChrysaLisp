(report-header "GPU: shader language, type checker")

(import "lib/gpu/glsl.inc")
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
	"(input k :float 1.5 0.0 4.0)"
	"(input n :int 3)"
	"(input size :vec2)"
	"(const two 2.0)"
	"(global g (* k two))"
	"(defun half :float ((a :float)) (return (* a 0.5)))"
	"(defun main :vec4 ((frag :vec2)) (return (vec4 (half g) frag 1.0)))"))
(bind '(inputs consts globals funcs) prog)
(assert-list-eq "inputs" '((k :float 1.5 0.0 4.0) (n :int 3 :nil :nil) (size :vec2 0 :nil :nil)) inputs)
(assert-list-eq "const" '((two :float (:float :lit "2.0000"))) consts)
(assert-eq "global type" :float (second (first globals)))
(assert-list-eq "function names" '(half main) (map (const first) funcs))
(assert-list-eq "function types" '(:float :vec4) (map (const second) funcs))
(assert-list-eq "return statement" '(:return (:float :op * ((:float :var a :param) (:float :lit "0.5000"))))
	(first (last (first funcs))))

;float literals are decimals, to 4 places from the reader, or as given in a str
(defq prog (sh-src "(const a 0.001)" "(const b -43758.5453)" {(const c "3.1415926535898")} {(const d "7")}
	"(defun main :vec4 ((frag :vec2)) (return (vec4 a b c d)))"))
(assert-list-eq "decimal literals" '("0.0010" "-43758.5453" "3.1415926535898" "7.0")
	(map (# (third (third %0))) (second prog)))

;types of the ops
(defun sh-local-type (program)
	;the type of the first local of main
	(first (third (first (last (first (last program)))))))

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
(assert-error "setq of a const" (sh-src "(const c 1.0)" "(defun main :vec4 ((frag :vec2)) (setq c 2.0) (return (vec4 c)))"))
(assert-error "setq of an input" (sh-src "(input k :float)" "(defun main :vec4 ((frag :vec2)) (setq k 2.0) (return (vec4 k)))"))
(assert-error "component out of range" (sh-main "(return (vec4 (:z frag)))"))
(assert-error "component set twice" (sh-main "(defq a (vec2 1.0))" "(setq (:xx a) (vec2 1.0))" "(return (vec4 a a))"))
(assert-error "break outside a loop" (sh-main "(break)" "(return (vec4 1.0))"))
(assert-error "loop bound is not a constant" (sh-src "(input n :int)" "(defun main :vec4 ((frag :vec2)) (for (i 0 n)) (return (vec4 1.0)))"))
(assert-error "const from an input" (sh-src "(input k :float)" "(const c (* k 2.0))" "(defun main :vec4 ((frag :vec2)) (return (vec4 c)))"))
(assert-error "wrong args to function" (sh-src "(defun f :float ((a :float)) (return a))" "(defun main :vec4 ((frag :vec2)) (return (vec4 (f 1))))"))
(assert-error "recursion" (sh-src "(defun f :float ((a :float)) (return (f a)))" "(defun main :vec4 ((frag :vec2)) (return (vec4 (f 1.0))))"))
(assert-error "no main" (sh-src "(defun f :float ((a :float)) (return a))"))
(assert-error "wrong main" (sh-src "(defun main :vec3 ((frag :vec2)) (return (vec3 1.0)))"))
(assert-error "vector input with a default" (sh-src "(input v :vec2 1.0)" "(defun main :vec4 ((frag :vec2)) (return (vec4 1.0)))"))
(assert-error "float input with an int default" (sh-src "(input k :float 1)" "(defun main :vec4 ((frag :vec2)) (return (vec4 1.0)))"))
(assert-error "not a number" (sh-main {(return (vec4 "1.0e3"))}))

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
	"(input k :float 1.5)" "(input n :int 3)" "(input size :vec2)"
	"(const two 2.0)" "(const four (* two two))"
	"(global g (* k two))" "(global h (+ g (float n)))"
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

(report-header "GPU: shader language, the inputs block")

(defq prog (sh-src
	"(input a :float 1.5)" "(input b :vec3)" "(input c :int -3)" "(input d :vec2)"
	"(input e :float 0.2)" "(input f :vec4)" "(input g :int 7)"
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

(report-header "GPU: the raymarch shader")

(defq prog (shader-load "lib/gpu/shaders/raymarch.shader") text (shader-glsl prog))
(assert-eq "inputs block size" 64 (first (shader-layout prog)))
(assert-list-eq "inputs" '(time resolution arg_aa arg_aa_adaptive arg_aa_debug arg_depth
	arg_aa_limit arg_ao arg_ref arg_shadow arg_bump arg_dis arg_march) (map (const first) (first prog)))
(assert-eq "functions" 19 (length (last prog)))
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
