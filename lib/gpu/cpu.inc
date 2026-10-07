;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shader language, CPU back end.
; A program becomes a Lisp lambda that shades a tile of pixels,
; or, for a vertex shader, one that places a list of vertices,
; with no GPU. A float is a real, a vector is a reals, an int is
; a num, a matrix is a reals of 16, a row at a time, as
; lib/math/matrix.inc has one. Vectors are never changed in place,
; so they can be shared, and constants are folded into the code.
;
; It is the reference, what the other back ends are checked by.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./shader.inc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; run time, called by the lambda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +cpu_0 (n2r 0) +cpu_1 (n2r 1) +cpu_2 (n2r 2) +cpu_-1 (n2r -1) +cpu_1/2 (n2r 0.5))

(defun cpu-splat (f n)
	(case n (2 (reals f f)) (3 (reals f f f)) (:t (reals f f f f))))

(defun cpu-vmap (f v)
	(apply (const reals) (map f v)))

(defun cpu-mod (x y)
	(- x (* y (floor (/ x y)))))

(defun cpu-vmod (x y)
	(nums-sub x (nums-mul y (fixeds-floor (nums-div x y)))))

(defun cpu-mix (a b f)
	(+ a (* (- b a) f)))

(defun cpu-vmix (a b f)
	(nums-add a (nums-scale (nums-sub b a) f)))

(defun cpu-vmixv (a b f)
	(nums-add a (nums-mul (nums-sub b a) f)))

(defun cpu-norm (v)
	(nums-scale v (recip (sqrt (nums-dot v v)))))

(defun cpu-length (v)
	(sqrt (nums-dot v v)))

(defun cpu-reflect (i n)
	(nums-sub i (nums-scale n (* (nums-dot n i) +cpu_2))))

(defun cpu-cross ((x1 y1 z1) (x2 y2 z2))
	(reals (- (* y1 z2) (* z1 y2)) (- (* z1 x2) (* x1 z2)) (- (* x1 y2) (* y1 x2))))

(defun cpu-pow (x y)
	;whole part of the power by squaring, the rest by repeated roots
	(cond
		((<= x +cpu_0) +cpu_0)
		(:t (defq n (floor y) f (- y n) i (abs (n2i n)) r +cpu_1 s x)
			(while (> i 0)
				(if (odd? i) (setq r (* r s)))
				(setq s (* s s) i (>> i 1)))
			(if (< n +cpu_0) (setq r (recip r)))
			(setq s x i 0)
			(while (and (> f +cpu_0) (< (++ i) 32))
				(setq s (sqrt s) f (* f +cpu_2))
				(when (>= f +cpu_1)
					(setq r (* r s) f (- f +cpu_1))))
			r)))

(defun cpu-vpow (x y)
	(apply (const reals) (map (const cpu-pow) x y)))

(defun cpu-mat-vec (m v)
	;a matrix times a vec4, each row of the matrix dotted with it
	(reals (nums-dot (slice m 0 4) v) (nums-dot (slice m 4 8) v)
		(nums-dot (slice m 8 12) v) (nums-dot (slice m 12 16) v)))

(defun cpu-mat-vec3 (m v)
	;the 3 by 3 of a matrix times a vec3
	(reals (nums-dot (slice m 0 3) v) (nums-dot (slice m 4 7) v) (nums-dot (slice m 8 11) v)))

(defun cpu-mat-mul (a b)
	;a matrix times a matrix, each row of the one dotted with each
	;column of the other
	(defq cols (map (# (reals (elem-get b %0) (elem-get b (+ %0 4))
		(elem-get b (+ %0 8)) (elem-get b (+ %0 12)))) (range 0 4)))
	(apply (const reals) (map (# (nums-dot (slice a (* (/ %0 4) 4) (+ (* (/ %0 4) 4) 4))
		(elem-get cols (% %0 4)))) (range 0 16))))

(defun cpu-zero (type)
	;what a value of the type is before it is set
	(cond
		((eql type :mat4) (apply (const reals) (map (lambda (_) +cpu_0) (range 0 16))))
		((shader-dim type) (cpu-splat +cpu_0 (shader-dim type)))
		((eql type :int) 0)
		(+cpu_0)))

(defun cpu-swz (v idxs)
	(apply (const reals) (map (# (elem-get v %0)) idxs)))

(defun cpu-swz-set (v idxs val)
	(setq v (cat v))
	(if (real? val)
		(elem-set v (first idxs) val)
		(each (# (elem-set v %0 %1)) idxs val))
	v)

;;;;;;;;;;;;;;
; the compiler
;;;;;;;;;;;;;;

(defun cpu-const? (form)
	(not (or (sym? form) (list? form))))

(defun cpu-fold (form)
	;run it now if all the args are constants
	(if (and (list? form) (every (const cpu-const?) (rest form))) (eval form) form))

(defun cpu-splat-form (form n)
	(cpu-fold (list (const cpu-splat) form n)))

(defun cpu-arith (i a ta b tb)
	;one step of + - * /, by the types of the two sides
	(defq va (shader-dim ta) vb (shader-dim tb))
	(cpu-fold (cond
		((eql ta :mat4)
			(list (cond ((eql tb :mat4) (const cpu-mat-mul)) ((eql tb :vec3) (const cpu-mat-vec3))
				((const cpu-mat-vec))) a b))
		((and va vb)
			(list (elem-get (static-q (nums-add nums-sub nums-mul nums-div)) i) a b))
		(va (case i
			(2 (list (const nums-scale) a b))
			(3 (list (const nums-scale) a (cpu-fold (list (const recip) b))))
			(:t (list (elem-get (static-q (nums-add nums-sub)) i) a (cpu-splat-form b va)))))
		(vb (case i
			(2 (list (const nums-scale) b a))
			(:t (list (elem-get (static-q (nums-add nums-sub nums-mul nums-div)) i)
				(cpu-splat-form a vb) b))))
		((list (elem-get (static-q (+ - * /)) i) a b)))))

(defun cpu-op (op type args types)
	;a built in op
	(defq a (first args) b (second args) c (third args)
		ta (first types) tb (second types) tc (third types) va (shader-dim ta))
	(cond
		((defq i (find op '(+ - * /)))
			(cond
				((> (length args) 1)
					(each (# (setq a (cpu-arith i a ta %0 %1)
						ta (if (shader-dim ta) ta %1))) (rest args) (rest types))
					a)
				(va (list (const nums-scale) a +cpu_-1))
				((list (const neg) a))))
		((defq i (find op '(< <= > >= = /=)))
			(cond
				((not (eql ta :bool)) (list (elem-get (static-q (< <= > >= = /=)) i) a b))
				((= i 4) (list (const eql) a b))
				((list (const if) (list (const eql) a b) :nil :t))))
		((eql op 'and)
			(reduce (# (list (const if) %0 %1)) args))
		((eql op 'or)
			(reduce (# (list (const if) %0 :t %1)) args))
		((eql op 'not) (list (const if) a :nil :t))
		((eql op 'float) (if (eql ta :int) (list (const n2r) a) a))
		((eql op 'int) (if (eql ta :int) a (list (const n2i) a)))
		((find op '(vec2 vec3 vec4))
			;ints are made floats, then a splat, or a join of the parts
			(setq args (map (# (if (eql %1 :int) (cpu-fold (list (const n2r) %0)) %0)) args types))
			(cond
				((and (= (length args) 1) (not va))
					(cpu-splat-form a (shader-dim type)))
				((notany (const shader-dim) types)
					(cat (list (const reals)) args))
				((cat (list (const cat)) (map (# (if (shader-dim %1) %0
					(cpu-fold (list (const reals) %0)))) args types)))))
		((defq i (find op '(sin cos sqrt floor fract abs)))
			(if va
				(case i
					(3 (list (const fixeds-floor) a))
					(4 (list (const fixeds-frac) a))
					(5 (list (const nums-abs) a))
					(:t (list (const cpu-vmap) (elem-get (static-q (sin cos sqrt)) i) a)))
				(list (elem-get (static-q (sin cos sqrt floor frac abs)) i) a)))
		((defq i (find op '(min max mod)))
			(cond
				((not va) (list (elem-get (static-q (min max cpu-mod)) i) a b))
				((list (elem-get (static-q (nums-min nums-max cpu-vmod)) i) a
					(if (shader-dim tb) b (cpu-splat-form b va))))))
		((eql op 'pow)
			(list (if va (const cpu-vpow) (const cpu-pow)) a b))
		((eql op 'clamp)
			(cond
				((not va) (list (const min) (list (const max) a b) c))
				((list (const nums-min) (list (const nums-max) a
						(if (shader-dim tb) b (cpu-splat-form b va)))
					(if (shader-dim tc) c (cpu-splat-form c va))))))
		((eql op 'mix)
			(list (cond ((not va) (const cpu-mix)) ((shader-dim tc) (const cpu-vmixv))
				((const cpu-vmix))) a b c))
		((eql op 'dot) (list (const nums-dot) a b))
		((eql op 'cross) (list (const cpu-cross) a b))
		((eql op 'length) (list (if va (const cpu-length) (const abs)) a))
		((eql op 'normalize) (list (const cpu-norm) a))
		((eql op 'reflect) (list (const cpu-reflect) a b))))

(defun cpu-expr (node)
	(bind '(type tag a &optional b) node)
	(case tag
		(:lit (if (eql type :float) (str-to-real a) a))
		(:var (if (eql b :const) (second (some (# (if (eql (first %0) a) %0)) cvals)) a))
		(:swz (defq v (cpu-expr b))
			(cond
				((= (length a) 1)
					(cpu-fold (list (const elem-get) v (first a))))
				((every (const =) a (range (first a) (+ (first a) (length a))))
					(cpu-fold (list (const slice) v (first a) (inc (last a)))))
				((cpu-fold (list (const cpu-swz) v (list (const quote) a))))))
		(:user (cat (list (second (some (# (if (eql (first %0) a) %0)) fvals)))
			(map cpu-expr b)))
		(:t (cpu-fold (cpu-op a type (map cpu-expr b) (map (const first) b))))))

(defun cpu-exits? (block in_loop)
	;does this block hold a return, or a break of the loop it is in ?
	(some (lambda ((tag &optional a b c d))
		(case tag
			(:return :t)
			(:break (not in_loop))
			(:if (or (cpu-exits? b in_loop) (cpu-exits? c in_loop)))
			(:for (cpu-exits? d :t))
			(:t :nil))) block))

(defun cpu-loop-return? (block)
	;does this block hold a return from inside a loop ?
	(some (lambda ((tag &optional a b c d))
		(case tag
			(:if (or (cpu-loop-return? b) (cpu-loop-return? c)))
			(:for (cpu-exits? d :t))
			(:t :nil))) block))

(defun cpu-block (block flag)
	;forms for a block. With no flag it is the body of the function,
	;and the value of the last form is the return value. With a flag
	;it is the body of a loop, and the flag is set to leave the loop.
	(cond
		((empty? block) (list))
		(:t (bind '(tag &optional a b c d) (first block))
			(defq more (rest block))
			(case tag
				(:defq (cat (list (list (const defq) a (cpu-expr b))) (cpu-block more flag)))
				(:setq (cat (list (list (const setq) a (cpu-expr b))) (cpu-block more flag)))
				(:setsw (cat (list (list (const setq) a
						(list (const cpu-swz-set) a (list (const quote) b) (cpu-expr c))))
					(cpu-block more flag)))
				(:return (if flag
					(list (list (const setq) '_rv (cpu-expr a) flag :ret))
					(list (cpu-expr a))))
				(:break (list (list (const setq) flag :t)))
				(:if (defq test (cpu-expr a))
					(if (or (cpu-exits? b :nil) (cpu-exits? c :nil))
						;the rest of the block goes inside both sides
						(list (list (const if) test
							(cat (list (const progn)) (cpu-block (cat b more) flag))
							(cat (list (const progn)) (cpu-block (cat c more) flag))))
						(cat (list (cat (list (const if) test
								(cat (list (const progn)) (cpu-block b flag)))
								(if (nempty? c) (list (cat (list (const progn)) (cpu-block c flag))) (list))))
							(cpu-block more flag))))
				(:for (defq loop_flag (sym (cat "_b" (str (++ flag_num))))
						loop (list
							(list (const defq) a (cpu-fold (list (const -) (cpu-expr b) 1)) loop_flag :nil)
							(cat (list (const while) (list (const if) loop_flag :nil
									(list (const <) (list (const setq) a (list (const +) a 1)) (cpu-expr c))))
								(cpu-block d loop_flag))))
					(if (cpu-exits? d :t)
						;a return in the loop, so leave this block as well
						(cat loop (list (list (const if) (list (const eql) loop_flag :ret)
							(if flag (list (const setq) flag :ret) '_rv)
							(cat (list (const progn)) (cpu-block more flag)))))
						(cat loop (cpu-block more flag))))))))

(defun cpu-funcs (program)
	; -> the lambda of main
	;the constants worked out, and each function a lambda
	(bind '(inputs consts globals funcs &rest _) program)
	(each (lambda ((name type e))
		(push cvals (list name (if (cpu-const? (defq v (cpu-expr e))) v (eval v))))) consts)
	(each (lambda ((name type params block))
		(push fvals (list name (cat (list (const lambda) (map (const first) params))
			(if (cpu-loop-return? block) (list (list (const defq) '_rv :nil)) (list))
			(cpu-block block :nil))))) funcs)
	(second (some (# (if (eql (first %0) 'main) %0)) fvals)))

(defun shader-cpu-vertex (program)
	; (shader-cpu-vertex program) -> lambda
	; (lambda verts input ...) -> ((position varying ...) ...)
	;the lambda places each vertex of a list. A vertex is a list of its
	;attrs, in the order the shader has them. For each it gives where the
	;vertex is, a vec4, as main gave it, then what main left each varying
	;as, in the order the shader has them. A varying that is not set is 0.
	(unless (eql (shader-stage program) :vertex) (sh-err "not a vertex shader" 'main))
	(bind '(inputs consts globals funcs &rest _) program)
	(defq cvals (list) fvals (list) flag_num 0 main (cpu-funcs program)
		varyings (shader-varyings program))
	(cat (list (const lambda) (cat '(_verts) (map (const first) inputs)))
		(map (lambda ((name type e)) (list (const defq) name (cpu-expr e))) globals)
		(list
			(static-qq (defq _out (list) _i -1))
			(cat (list (const while) (static-qq (< (setq _i (+ _i 1)) (length _verts)))
					(list (const bind) (list (const quote) (map (const first) (shader-attrs program)))
						(static-qq (elem-get _verts _i))))
				(map (lambda ((name type)) (list (const defq) name (cpu-zero type))) varyings)
				(list (list (const push) '_out
					(cat (list (const list) (list main)) (map (const first) varyings)))))
			'_out)))

(defun shader-cpu (program)
	; (shader-cpu program) -> lambda
	; (lambda x y x1 y1 input ... varying ...) -> (vec4 ...)
	;the lambda shades the pixels of the tile, row by row, the
	;centre of pixel x y is at frag coord x + 0.5, y + 0.5. A pixel
	;shader with varyings is given a value for each, after the inputs,
	;and every pixel of the tile has those.
	(unless (eql (shader-stage program) :pixel) (sh-err "not a pixel shader" 'main))
	(bind '(inputs consts globals funcs &rest _) program)
	(defq cvals (list) fvals (list) flag_num 0 main (cpu-funcs program))
	(cat (list (const lambda) (cat '(_x _y _x1 _y1) (map (const first) inputs)
			(map (const first) (shader-varyings program))))
		(map (lambda ((name type e)) (list (const defq) name (cpu-expr e))) globals)
		(list
			(static-qq (defq _out (list) _yy (- _y 1)))
			(list (const while) (static-qq (< (setq _yy (+ _yy 1)) _y1))
				(static-qq (defq _xx (- _x 1)))
				(list (const while) (static-qq (< (setq _xx (+ _xx 1)) _x1))
					(list (const push) '_out (list main
						(static-qq (reals (+ (n2r _xx) ,+cpu_1/2) (+ (n2r _yy) ,+cpu_1/2))))))
				(static-qq (task-slice)))
			'_out)))

(defun shader-cpu-args (program &optional vals)
	; (shader-cpu-args program [((name val) ...)]) -> (val ...)
	;the input args for the lambda, defaults for those not given, then a
	;value for each varying of a pixel shader, 0 for those not given
	(map (lambda ((name type &optional val lo hi))
		(if (defq v (some (# (if (eql (first %0) name) %0)) vals))
			(setq val (second v)))
		;a varying has no default, it is 0
		(unless val (setq val 0))
		(cond
			((eql type :int) (n2i val))
			((eql type :float) (shader-real val))
			((reals? val) val)
			((apply (const reals) (map (const shader-real)
				(if (seq? val) val (map (lambda (_) 0) (range 0 (shader-size type)))))))))
		(cat (first program)
			(if (eql (shader-stage program) :pixel) (shader-varyings program) (list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; triangles, the reference
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cpu-share (pixel share)
	;that much of a 32 bit argb pixel, a share of 0 to 256
	(+ (logand (>> (* (logand pixel 0xff00ff) share) 8) 0xff00ff)
		(<< (logand (>> (* (logand (>> pixel 8) 0xff00ff) share) 8) 0xff00ff) 8)))

(defun shader-cpu-tris (vertex pixel verts tris width height &optional vvals pvals cull tri_size x y x1 y1)
	; (shader-cpu-tris vertex pixel verts tris width height [vvals pvals cull tri_size x y x1 y1]) -> str
	;triangles drawn with a vertex shader and a pixel shader, in Lisp, a
	;pixel at a time. It is slow, and is what the native code is checked
	;by. The args are those of (shader-vp-draw-tris), with the size of the
	;frame in place of a pixmap. The result is the pixels, row by row, the
	;top row first, each a 32 bit argb, 0 where nothing was drawn.
	(shader-pair vertex pixel)
	(setd tri_size 3 x 0 y 0 x1 width y1 height)
	(defq sizes (map (# (shader-size (second %0))) (shader-attrs vertex))
		stride (reduce (const +) sizes 0) at 0
		;each vertex as a list of its attrs
		vlist (map (lambda (v)
			(setq at (* v stride))
			(map (lambda (n) (defq a (slice verts at (+ at n)))
				(setq at (+ at n)) (if (= n 1) (first a) a)) sizes))
			(range 0 (/ (length verts) (max stride 1))))
		placed (apply (eval (shader-cpu-vertex vertex)) (cat (list vlist) (shader-cpu-args vertex vvals)))
		shade (eval (shader-cpu pixel)) inputs (slice (shader-cpu-args pixel pvals) 0 (length (first pixel)))
		;where in what a vertex was placed as each varying of the pixel shader is
		picks (map (lambda ((name type))
			(inc (some (# (if (eql (first %0) name) (!))) (shader-varyings vertex))))
			(shader-varyings pixel))
		out (str-alloc (* width height 4)) depth (map (lambda (_) +cpu_2) (range 0 (* width height)))
		fw (n2r width) fh (n2r height))
	(each (# (set-long out %0 0)) (range 0 (length out) 8))
	(each (lambda ((i0 i1 i2 &rest _))
		(defq vs (list (elem-get placed i0) (elem-get placed i1) (elem-get placed i2))
			ws (map (# (elem-get (first %0) 3)) vs))
		(when (every (# (> %0 (const (str-to-real "0.000001")))) ws)
			(defq iw (map (const recip) ws)
				sx (map (# (* (+ (* (first (first %0)) %1) +cpu_1) +cpu_1/2 fw)) vs iw)
				sy (map (# (* (- +cpu_1 (* (second (first %0)) %1)) +cpu_1/2 fh)) vs iw)
				zs (map (# (* (third (first %0)) %1)) vs iw))
			(bind '(x0 x1s x2) sx) (bind '(y0 y1s y2) sy)
			(defq area (- (* (- x1s x0) (- y2 y0)) (* (- y1s y0) (- x2 x0))))
			(unless (or (= area +cpu_0)
					(and cull (if (eql cull :front) (< area +cpu_0) (> area +cpu_0))))
				;the share of each vertex at a point, as the native code has
				;it, a number for the corner of the screen and a step for a
				;pixel along and a pixel down, in the same order of working
				(defq ia (recip area)
					edges (map (lambda (ax ay bx by)
						(defq dx (- bx ax) dy (- by ay))
						(list (* (- (* dy ax) (* dx ay)) ia) (neg (* dy ia)) (* dx ia)))
						(list x1s x2 x0) (list y1s y2 y0) (list x2 x0 x1s) (list y2 y0 y1s)))
				(each (lambda (py)
					(defq fy (+ (n2r py) +cpu_1/2)
						rows (map (lambda ((at kx ky)) (+ (* ky fy) at)) edges))
					(each (lambda (px)
						(defq fx (+ (n2r px) +cpu_1/2))
						(bind '(b0 b1 b2) (map (lambda ((at kx ky) row) (+ (* kx fx) row)) edges rows))
						(when (and (>= b0 +cpu_0) (>= b1 +cpu_0) (>= b2 +cpu_0))
							(defq z (+ (* b0 (first zs)) (* b1 (second zs)) (* b2 (third zs)))
								di (+ (* py width) px))
							(when (and (>= z +cpu_-1) (<= z +cpu_1) (< z (elem-get depth di)))
								(defq q0 (* b0 (first iw)) q1 (* b1 (second iw)) q2 (* b2 (third iw))
									r (recip (+ q0 q1 q2)) q0 (* q0 r) q1 (* q1 r) q2 (* q2 r)
									vary (map (lambda (pick)
										(defq a (elem-get (first vs) pick) b (elem-get (second vs) pick)
											c (elem-get (third vs) pick))
										(if (real? a)
											(+ (* a q0) (* b q1) (* c q2))
											(nums-add (nums-add (nums-scale a q0) (nums-scale b q1))
												(nums-scale c q2)))) picks)
									col (first (apply shade (cat (list px (- height 1 py) (inc px) (- height py))
										inputs vary))))
								;alpha of 0 is not drawn, full on is written,
								;and between goes over what is there as
								;:canvas :plot has it, in whole numbers
								(defq alpha (n2i (* (min (max (elem-get col 3) +cpu_0) +cpu_1) (const (n2r 255))))
									pixel (reduce (# (+ (<< %0 8)
										(n2i (* (min (max %1 +cpu_0) +cpu_1) (const (n2r 255))))))
										(slice col 0 3) 0xff))
								(when (/= alpha 0)
									(when (/= alpha 255)
										(setq pixel (+ (<< alpha 24)
											(cpu-share (logand pixel 0xffffff) (+ alpha (>> alpha 7)))
											(cpu-share (get-uint out (* di 4)) (- 255 alpha)))))
									(elem-set depth di z)
									(set-int out (* di 4) pixel)))))
						(range (max x 0) (min x1 width))))
					(range (max y 0) (min y1 height))))))
		(partition tris tri_size))
	out)
