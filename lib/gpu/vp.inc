;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shader language, VP back end.
; A program becomes VP source for a native function that shades a
; tile of pixels with no GPU. The source is assembled once, for the
; CPU the node runs on, and kept under obj/.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./shader.inc")
(import "gui/pixmap/struct.inc")
(import "././collections/collections.inc")

;there is no recursion, so every variable of every function has a slot
;of its own in one frame, 8 bytes for each float or int. The frame is a
;str the caller gives, with the inputs written at the start of it, so
;none of this is on the task's stack. :r13 holds the frame, :r12 the
;constants. A value being worked on is in registers, a float in each
;of :f0 to :f15, an int or bool in :r2 to :r10, and any that are live
;over a call are saved on the stack around it. A matrix is never in
;registers, it is 16 slots of the frame, a row at a time, and is known by
;where it is.

(defq +sv_fregs ''(:f0 :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12 :f13 :f14 :f15)
	+sv_rregs ''(:r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10)
	+sv_sin_trash ''(:f0 :f1 :f2 :f3)
	+sv_1 (n2r 1) +sv_hpi (str-to-real "1.5707963267948966")
	;part of the name of the native function, so a change to this file
	;that changes the code must change this
	;this much work, a pixel is 1, and a native function gives the other
	;tasks of its node a turn
	+sv_work 16384
	;and this many pixels of a tile of a pixel shader, they are far more
	;work each
	+sv_tile_work 4096
	+sv_version 14)

(defun sv-emit (&rest form)
	(push code form))

(defun sv-q (label)
	(list 'quote label))

(defun sv-new-label ()
	(sym (cat "sl_" (str (++ label_num)))))

(defun sv-getf ()
	(if (empty? free_f) (sh-err "expression needs too many registers" :float))
	(push live (defq r (pop free_f)))
	r)

(defun sv-getr ()
	(if (empty? free_r) (sh-err "expression needs too many registers" :int))
	(push live (defq r (pop free_r)))
	r)

(defun sv-put (r)
	(setq live (filter (# (not (eql %0 r))) live))
	(push (if (find r +sv_fregs) free_f free_r) r))

(defun sv-puts (regs)
	(each (const sv-put) regs))

(defun sv-slots (type)
	;room in the frame for a value of this type
	(defq offset frame_size)
	(setq frame_size (+ frame_size (* (shader-size type) 8)))
	offset)

(defun sv-var (name)
	;the frame offset of a variable, the last one of that name declared
	(second (some! (# (if (eql (first %0) name) %0)) (list vars) :nil -1 0)))

(defun sv-load (type offset)
	(if (sh-float? type)
		(map (# (sv-emit 'vp-cpy-if :r13 (+ offset (* %0 8)) (defq r (sv-getf))) r)
			(range 0 (ifn (shader-dim type) 1)))
		(progn (sv-emit 'vp-cpy-ir :r13 offset (defq r (sv-getr))) (list r))))

(defun sv-store (regs type offset)
	(if (sh-float? type)
		(each (# (sv-emit 'vp-cpy-fi %0 :r13 (+ offset (* (!) 8)))) regs)
		(sv-emit 'vp-cpy-ri (first regs) :r13 offset)))

(defun sv-mat-copy (from to)
	;a matrix from one place in the frame to another
	(defq r (sv-getf))
	(each (# (sv-emit 'vp-cpy-if :r13 (+ from (* %0 8)) r)
		(sv-emit 'vp-cpy-fi r :r13 (+ to (* %0 8)))) (range 0 16))
	(sv-put r))

(defun sv-mat (node)
	;code for an expression that is a matrix, where in the frame it is. A
	;variable is where it is kept. A product is worked out into new slots,
	;each number of it a row of the one by a column of the other.
	(bind '(type tag a &optional b) node)
	(cond
		((eql tag :var) (sv-var a))
		(:t (defq at (sv-mat (first b)))
			(each (lambda (arg)
				(defq with (sv-mat arg) out (sv-slots :mat4)
					acc (sv-getf) x (sv-getf) y (sv-getf))
				(each (lambda (i)
					(defq row (/ i 4) col (% i 4))
					(each (lambda (k)
						(sv-emit 'vp-cpy-if :r13 (+ at (* (+ (* row 4) k) 8)) (if (= k 0) acc x))
						(sv-emit 'vp-cpy-if :r13 (+ with (* (+ (* k 4) col) 8)) y)
						(sv-emit 'vp-mul-ff y (if (= k 0) acc x))
						(if (/= k 0) (sv-emit 'vp-add-ff x acc))) (range 0 4))
					(sv-emit 'vp-cpy-fi acc :r13 (+ out (* i 8)))) (range 0 16))
				(sv-puts (list acc x y))
				(setq at out)) (rest b))
			at)))

(defun sv-mat-vec (at regs)
	;a matrix times a vector that is in registers, to new registers. A
	;vec3 is by the 3 by 3 of the matrix
	(defq n (length regs) x (sv-getf)
		out (map (lambda (row)
			(defq acc (sv-getf))
			(each (lambda (k)
				(sv-emit 'vp-cpy-if :r13 (+ at (* (+ (* row 4) k) 8)) (if (= k 0) acc x))
				(sv-emit 'vp-mul-ff (elem-get regs k) (if (= k 0) acc x))
				(if (/= k 0) (sv-emit 'vp-add-ff x acc))) (range 0 n))
			acc) (range 0 n)))
	(sv-put x)
	(sv-puts regs)
	out)

(defun sv-define (name type e)
	;a new variable, set to the value of an expression
	(cond
		((eql type :mat4)
			;a product is in slots of its own already, and they are the
			;variable. Another variable is copied, it may be set later
			(defq at (sv-mat e))
			(when (eql (second e) :var)
				(sv-mat-copy at (defq to (sv-slots :mat4)))
				(setq at to))
			(push vars (list name at)))
		(:t (defq regs (sv-expr e) offset (sv-slots type))
			(push vars (list name offset))
			(sv-store regs type offset)
			(sv-puts regs))))

(defun sv-const (val)
	;a float constant, to a new register
	(sv-emit 'vp-cpy-if :r12 (list 'fn-const (logior val)) (defq r (sv-getf)))
	r)

(defun sv-spill (regs)
	;save the live registers, all of them or those of a list
	(defq saved (if (eql regs :t) (cat live) (filter (# (find %0 regs)) live)))
	(when (nempty? saved)
		(sv-emit 'vp-alloc (* (length saved) 8))
		(each (# (sv-emit (if (find %0 +sv_fregs) 'vp-cpy-fi 'vp-cpy-ri) %0 :rsp (* (!) 8))) saved))
	saved)

(defun sv-unspill (saved)
	(when (nempty? saved)
		(each (# (if (find %0 +sv_fregs)
			(sv-emit 'vp-cpy-if :rsp (* (!) 8) %0)
			(sv-emit 'vp-cpy-ir :rsp (* (!) 8) %0))) saved)
		(sv-emit 'vp-free (* (length saved) 8))))

(defun sv-src (regs i)
	;a float goes with every component of a vector
	(elem-get regs (if (= (length regs) 1) 0 i)))

(defun sv-floor (r)
	;floor of a register, in place
	(defq ri (sv-getr) rf (sv-getf) skip (sv-new-label))
	(sv-emit 'vp-cvt-fr r ri)
	(sv-emit 'vp-cvt-rf ri rf)
	(sv-emit 'vp-ble-ff r rf (sv-q skip))
	(sv-emit 'vp-sub-ff (defq one (sv-const +sv_1)) rf)
	(sv-emit 'vp-label (sv-q skip))
	(sv-emit 'vp-cpy-ff rf r)
	(sv-puts (list ri rf one)))

(defun sv-sin (r is_cos)
	;sin or cos of a register, which is given up, to a new register
	(sv-put r)
	(defq saved (sv-spill +sv_sin_trash))
	(unless (eql r :f0) (sv-emit 'vp-cpy-ff r :f0))
	(when is_cos
		(sv-emit 'vp-cpy-if :r12 (list 'fn-const (logior +sv_hpi)) :f1)
		(sv-emit 'vp-add-ff :f1 :f0))
	(sv-emit 'call :sys_math :r_sin (sv-q '(:f0)) (sv-q '(:f0)))
	(defq r (sv-getf))
	(unless (eql r :f0) (sv-emit 'vp-cpy-ff :f0 r))
	(sv-unspill saved)
	r)

(defun sv-pow (x y)
	;x to the power y, both kept, to a new register
	(defq saved (sv-spill :t))
	(sv-emit 'vp-cpy-if :rsp (* (find x saved) 8) :f0)
	(sv-emit 'vp-cpy-if :rsp (* (find y saved) 8) :f1)
	(sv-emit 'call :sys_math :r_pow (sv-q '(:f0 :f1)) (sv-q '(:f0)))
	(defq r (sv-getf))
	(unless (eql r :f0) (sv-emit 'vp-cpy-ff :f0 r))
	(sv-unspill saved)
	r)

(defun sv-arith (i a ta b tb)
	;one step of + - * /, by the types of the two sides
	(defq va (shader-dim ta) vb (shader-dim tb)
		op (elem-get '(vp-add-ff vp-sub-ff vp-mul-ff vp-div-ff) i))
	(cond
		((eql ta :int)
			(cond
				((= i 3)
					(sv-emit 'vp-ext-rr (first a) (defq tmp (sv-getr)))
					(sv-emit 'vp-div-rrr (first b) tmp (first a))
					(sv-put tmp))
				((sv-emit (elem-get '(vp-add-rr vp-sub-rr vp-mul-rr) i) (first b) (first a))))
			(sv-puts b) a)
		((or va (not vb))
			;the same shape, or a vector with a float
			(each (# (sv-emit op (sv-src b (!)) %0)) a)
			(sv-puts b) a)
		((or (= i 0) (= i 2))
			;float with a vector, either way round
			(each (# (sv-emit op (first a) %0)) b)
			(sv-puts a) b)
		((= i 1)
			(each (# (sv-emit 'vp-neg-ff %0 %0) (sv-emit 'vp-add-ff (first a) %0)) b)
			(sv-puts a) b)
		(:t (defq tmp (sv-getf))
			(each (# (sv-emit 'vp-cpy-ff (first a) tmp)
				(sv-emit 'vp-div-ff %0 tmp)
				(sv-emit 'vp-cpy-ff tmp %0)) b)
			(sv-put tmp)
			(sv-puts a) b)))

(defun sv-calls? (node)
	;does this expression call a function of the shader ?
	(bind '(type tag a &optional b) node)
	(case tag
		(:user :t)
		(:swz (sv-calls? b))
		(:op (some sv-calls? b))
		(:t :nil)))

(defun sv-op (node)
	;a built in op
	(bind '(type & op args) node)
	(defq types (map (const first) args) ta (first types) va (shader-dim ta))
	(cond
		((find :mat4 types)
			;matrices times a vector, the vector is last, and each matrix
			;is applied to it in turn, the nearest first
			(defq v (sv-expr (last args)))
			(reach (# (setq v (sv-mat-vec (sv-mat %0) v))) (slice args 0 -2))
			v)
		((defq i (find op '(+ - * /)))
			(defq a (sv-expr (first args)))
			(cond
				((> (length args) 1)
					(each (# (setq a (sv-arith i a ta (sv-expr %0) %1)
						ta (if (shader-dim ta) ta %1))) (rest args) (rest types)))
				((eql ta :int)
					(sv-emit 'vp-cpy-rr (first a) (defq tmp (sv-getr)))
					(sv-emit 'vp-xor-rr (first a) (first a))
					(sv-emit 'vp-sub-rr tmp (first a))
					(sv-put tmp))
				((each (# (sv-emit 'vp-neg-ff %0 %0)) a)))
			a)
		((find op '(< <= > >= = /= and or not))
			;a bool as a value
			(defq r (sv-getr) l (sv-new-label))
			(sv-emit 'vp-cpy-cr 1 r)
			(sv-branch node l :t)
			(sv-emit 'vp-cpy-cr 0 r)
			(sv-emit 'vp-label (sv-q l))
			(list r))
		((eql op 'float)
			(defq a (sv-expr (first args)))
			(cond
				((eql ta :int)
					(sv-emit 'vp-cvt-rf (first a) (defq r (sv-getf)))
					(sv-puts a) (list r))
				(a)))
		((eql op 'int)
			(defq a (sv-expr (first args)))
			(cond
				((eql ta :float)
					(sv-emit 'vp-cvt-fr (first a) (defq r (sv-getr)))
					(sv-puts a) (list r))
				(a)))
		((find op '(vec2 vec3 vec4))
			(defq regs (list))
			(each (lambda (arg)
				(defq a (sv-expr arg))
				(cond
					((eql (first arg) :int)
						(sv-emit 'vp-cvt-rf (first a) (defq r (sv-getf)))
						(sv-puts a) (push regs r))
					((each (# (push regs %0)) a)))) args)
			(while (< (length regs) (shader-dim type))
				(sv-emit 'vp-cpy-ff (first regs) (defq r (sv-getf)))
				(push regs r))
			regs)
		((defq i (find op '(sin cos sqrt abs floor fract)))
			(defq a (sv-expr (first args)))
			(case i
				((0 1) (defq n (length a))
					(times n (push a (sv-sin (first a) (= i 1)))
						(setq a (rest a)))
					a)
				(2 (each (# (sv-emit 'vp-sqrt-ff %0 %0)) a) a)
				(3 (each (# (sv-emit 'vp-abs-ff %0 %0)) a) a)
				(4 (each (const sv-floor) a) a)
				(:t (defq tmp (sv-getf))
					(each (# (sv-emit 'vp-cpy-ff %0 tmp)
						(sv-floor tmp)
						(sv-emit 'vp-sub-ff tmp %0)) a)
					(sv-put tmp) a)))
		((defq i (find op '(min max mod)))
			(defq a (sv-expr (first args)) b (sv-expr (second args)))
			(cond
				((< i 2)
					(each (# (sv-emit (if (= i 0) 'vp-min-ff 'vp-max-ff) (sv-src b (!)) %0)) a))
				(:t (defq tmp (sv-getf))
					(each (# (sv-emit 'vp-cpy-ff %0 tmp)
						(sv-emit 'vp-div-ff (sv-src b (!)) tmp)
						(sv-floor tmp)
						(sv-emit 'vp-mul-ff (sv-src b (!)) tmp)
						(sv-emit 'vp-sub-ff tmp %0)) a)
					(sv-put tmp)))
			(sv-puts b) a)
		((eql op 'pow)
			(defq a (sv-expr (first args)) b (sv-expr (second args))
				regs (map (const sv-pow) a b))
			(sv-puts a) (sv-puts b) regs)
		((eql op 'clamp)
			(defq a (sv-expr (first args)) b (sv-expr (second args)) c (sv-expr (third args)))
			(each (# (sv-emit 'vp-max-ff (sv-src b (!)) %0)
				(sv-emit 'vp-min-ff (sv-src c (!)) %0)) a)
			(sv-puts b) (sv-puts c) a)
		((eql op 'mix)
			(defq a (sv-expr (first args)) b (sv-expr (second args)) c (sv-expr (third args)))
			(each (# (sv-emit 'vp-sub-ff %0 %1)
				(sv-emit 'vp-mul-ff (sv-src c (!)) %1)
				(sv-emit 'vp-add-ff %1 %0)) a b)
			(sv-puts b) (sv-puts c) a)
		((eql op 'dot)
			(defq a (sv-expr (first args)) b (sv-expr (second args)))
			(each (# (sv-emit 'vp-mul-ff %1 %0)) a b)
			(each (# (sv-emit 'vp-add-ff %0 (first a))) (rest a))
			(sv-puts b) (sv-puts (rest a)) (list (first a)))
		((eql op 'length)
			(defq a (sv-expr (first args)))
			(cond
				(va (each (# (sv-emit 'vp-mul-ff %0 %0)) a)
					(each (# (sv-emit 'vp-add-ff %0 (first a))) (rest a))
					(sv-emit 'vp-sqrt-ff (first a) (first a))
					(sv-puts (rest a)) (list (first a)))
				(:t (sv-emit 'vp-abs-ff (first a) (first a)) a)))
		((eql op 'normalize)
			(defq a (sv-expr (first args)) acc (sv-getf) tmp (sv-getf))
			(sv-emit 'vp-cpy-ff (first a) acc)
			(sv-emit 'vp-mul-ff acc acc)
			(each (# (sv-emit 'vp-cpy-ff %0 tmp)
				(sv-emit 'vp-mul-ff tmp tmp)
				(sv-emit 'vp-add-ff tmp acc)) (rest a))
			(sv-emit 'vp-sqrt-ff acc acc)
			(each (# (sv-emit 'vp-div-ff acc %0)) a)
			(sv-put acc) (sv-put tmp) a)
		((eql op 'cross)
			(defq a (sv-expr (first args)) b (sv-expr (second args)) tmp (sv-getf)
				regs (map (lambda (i j)
					(sv-emit 'vp-cpy-ff (elem-get a i) (defq r (sv-getf)))
					(sv-emit 'vp-mul-ff (elem-get b j) r)
					(sv-emit 'vp-cpy-ff (elem-get a j) tmp)
					(sv-emit 'vp-mul-ff (elem-get b i) tmp)
					(sv-emit 'vp-sub-ff tmp r)
					r) '(1 2 0) '(2 0 1)))
			(sv-put tmp) (sv-puts a) (sv-puts b) regs)
		((eql op 'reflect)
			;i - 2 * dot(n, i) * n
			(defq a (sv-expr (first args)) b (sv-expr (second args)) acc (sv-getf) tmp (sv-getf))
			(sv-emit 'vp-cpy-ff (first a) acc)
			(sv-emit 'vp-mul-ff (first b) acc)
			(each (# (sv-emit 'vp-cpy-ff %0 tmp)
				(sv-emit 'vp-mul-ff %1 tmp)
				(sv-emit 'vp-add-ff tmp acc)) (rest a) (rest b))
			(sv-emit 'vp-add-ff acc acc)
			(each (# (sv-emit 'vp-mul-ff acc %1)
				(sv-emit 'vp-sub-ff %1 %0)) a b)
			(sv-put acc) (sv-put tmp) (sv-puts b) a)))

(defun sv-expr (node)
	;code for an expression, the registers that hold the value
	(bind '(type tag a &optional b) node)
	(case tag
		(:lit (cond
			((eql type :float) (list (sv-const (str-to-real a))))
			(:t (sv-emit 'vp-cpy-cr (cond ((eql type :int) a) (a 1) (0)) (defq r (sv-getr)))
				(list r))))
		(:var (sv-load type (sv-var a)))
		(:swz (defq regs (sv-expr b) used (list)
				out (map (# (defq r (elem-get regs %0))
					(cond
						((find r used)
							(sv-emit 'vp-cpy-ff r (defq r (sv-getf))) r)
						((push used r) r))) a))
			(sv-puts (filter (# (not (find %0 used))) regs))
			out)
		(:user (bind '(& label offsets result) (some (# (if (eql (first %0) a) %0)) finfo))
			;a value can go to its parameter slot at once if no later
			;arg can call a function, else all are held till the end
			(defq held (list))
			(each (lambda (arg offset)
				(push held (list (sv-expr arg) (first arg) offset))
				(unless (some sv-calls? (slice b (inc (!)) -1))
					(each (lambda ((regs type offset))
						(sv-store regs type offset)
						(sv-puts regs)) held)
					(clear held))) b offsets)
			(defq saved (sv-spill :t))
			(sv-emit 'vp-call (sv-q label))
			(defq regs (sv-load type result))
			(sv-unspill saved)
			regs)
		(:t (sv-op node))))

(defun sv-branch (node label sense)
	;code to jump to the label if the bool is as sense says
	(bind '(type tag a &optional b) node)
	(cond
		((and (eql tag :op) (defq i (find a '(< <= > >= = /=))))
			(defq x (sv-expr (first b)) y (sv-expr (second b))
				i (if sense i (elem-get '(3 2 1 0 5 4) i)))
			(sv-emit (elem-get (if (eql (first (first b)) :float)
					'(vp-blt-ff vp-ble-ff vp-bgt-ff vp-bge-ff vp-beq-ff vp-bne-ff)
					'(vp-blt-rr vp-ble-rr vp-bgt-rr vp-bge-rr vp-beq-rr vp-bne-rr)) i)
				(first y) (first x) (sv-q label))
			(sv-puts x) (sv-puts y))
		((and (eql tag :op) (eql a 'not))
			(sv-branch (first b) label (not sense)))
		((and (eql tag :op) (find a '(and or)))
			(cond
				((eql (eql a 'and) (not sense))
					;any one of them settles it
					(each (# (sv-branch %0 label sense)) b))
				(:t (defq skip (sv-new-label))
					(each (# (sv-branch %0 skip (not sense))) (most b))
					(sv-branch (last b) label sense)
					(sv-emit 'vp-label (sv-q skip)))))
		(:t (defq r (sv-expr node))
			(sv-emit (if sense 'vp-bne-cr 'vp-beq-cr) 0 (first r) (sv-q label))
			(sv-puts r))))

(defun sv-block (block)
	(each (lambda ((tag &optional a b c d))
		(case tag
			(:defq (sv-define a (first b) b))
			(:setq (cond
				((eql (first b) :mat4) (sv-mat-copy (sv-mat b) (sv-var a)))
				(:t (defq regs (sv-expr b))
					(sv-store regs (first b) (sv-var a))
					(sv-puts regs))))
			(:setsw (defq regs (sv-expr c) offset (sv-var a))
				(each (# (sv-emit 'vp-cpy-fi %0 :r13 (+ offset (* %1 8)))) regs b)
				(sv-puts regs))
			(:if (defq l_else (sv-new-label))
				(sv-branch a l_else :nil)
				(sv-block b)
				(cond
					((nempty? c)
						(sv-emit 'vp-jmp (sv-q (defq l_end (sv-new-label))))
						(sv-emit 'vp-label (sv-q l_else))
						(sv-block c)
						(sv-emit 'vp-label (sv-q l_end)))
					((sv-emit 'vp-label (sv-q l_else)))))
			(:for (defq offset (sv-slots :int) l_top (sv-new-label) l_exit (sv-new-label)
					regs (sv-expr b))
				(push vars (list a offset))
				(sv-store regs :int offset)
				(sv-puts regs)
				(sv-emit 'vp-label (sv-q l_top))
				(defq regs (sv-load :int offset) end (sv-expr c))
				(sv-emit 'vp-bge-rr (first end) (first regs) (sv-q l_exit))
				(sv-puts regs) (sv-puts end)
				(push loops l_exit)
				(sv-block d)
				(pop loops)
				(defq regs (sv-load :int offset))
				(sv-emit 'vp-add-cr 1 (first regs))
				(sv-store regs :int offset)
				(sv-puts regs)
				(sv-emit 'vp-jmp (sv-q l_top))
				(sv-emit 'vp-label (sv-q l_exit)))
			(:break (sv-emit 'vp-jmp (sv-q (last loops))))
			(:return (defq regs (sv-expr a))
				(sv-store regs (first a) ret_offset)
				(sv-puts regs)
				(sv-emit 'vp-jmp (sv-q ret_label))))) block))

(defun sv-program (program prefix)
	; -> (main_param_offsets main_ret_offset)
	;the code of a program, after its inputs have been given their slots.
	;A label that sets its constants and globals, then each function at a
	;label of its own. The labels start with the prefix, so that the two
	;programs of a pair can be in the one native function.
	(bind '(inputs consts globals funcs &rest _) program)
	(sv-emit 'vp-label (sv-q (sym (cat prefix "h_init"))))
	(each (lambda ((name type e)) (sv-define name type e)) (cat consts globals))
	(sv-emit 'vp-ret)
	(defq num_globals (length vars))
	(each (lambda ((name type params block))
		(setq vars (slice vars 0 num_globals))
		(defq label (sym (cat prefix "f_" name))
			offsets (map (lambda ((pname ptype))
				(push vars (list pname (defq offset (sv-slots ptype)))) offset) params))
		(setq ret_offset (sv-slots type) ret_label (sv-new-label))
		(push finfo (list name label offsets ret_offset))
		(sv-emit 'vp-label (sv-q label))
		(sv-block block)
		(sv-emit 'vp-label (sv-q ret_label))
		(sv-emit 'vp-ret)) funcs)
	(bind '(& & offsets ret) (some (# (if (eql (first %0) 'main) %0)) finfo))
	(list offsets ret))

(defun sv-code-text ()
	;the code so far as lines of VP source
	(defq lf (ascii-char 10))
	(join (map (# (if (eql (first %0) 'vp-label) (str %0) (cat (ascii-char 9) (str %0)))) code) lf))

(defun sv-vertex-source (program name)
	; -> (text frame_size attr_slots out_slots)
	;the VP source of the native function that places vertices
	(defq code (list) live (list) free_f (cat +sv_fregs) free_r (cat +sv_rregs)
		vars (list) finfo (list) loops (list) frame_size 0 label_num 0
		ret_offset 0 ret_label :nil)
	;the frame starts with the inputs, then the attrs of the vertex being
	;placed, then its varyings, then what the loop keeps
	(each (lambda ((name type &rest _)) (push vars (list name (sv-slots type)))) (first program))
	(defq attr_at frame_size)
	(each (lambda ((name type)) (push vars (list name (sv-slots type)))) (shader-attrs program))
	(defq vary_at frame_size attr_slots (/ (- vary_at attr_at) 8))
	(each (lambda ((name type)) (push vars (list name (sv-slots type)))) (shader-varyings program))
	(defq vary_slots (/ (- frame_size vary_at) 8))
	(bind '(s_in s_out s_n) (map (lambda (_) (sv-slots :int)) (range 0 3)))
	(bind '(& ret) (sv-program program "v"))
	(defq lf (ascii-char 10) text (cat
{(include "lib/asm/func.inc")
(include "sys/math/class.inc")
(include "class/num/class.inc")
(include "class/str/class.inc")
(include "class/array/class.inc")
(include "class/lisp/class.inc")
(include "sys/task/class.inc")

(def-func "} name {")
	;inputs
	;:r0 = lisp object (ptr)
	;:r1 = args list object (ptr)
	;outputs
	;:r0 = lisp object (ptr)
	;:r1 = return value object (ptr)
	;trashes
	;:r1-:r14, :f0-:f15
	;info
	;(place frame verts out count is_str) -> :nil

	(entry '(:r0 :r1))

	(errorif-lisp-args-sig 'error :r1 5)

	(vp-push :r0)
	(list-bind-args :r1 '(:r13 :r6 :r7 :r8 :r9) '(:str :obj :obj :num :num))
	;a constant, so that there is a table of them, a shader may have none
	(fn-const 0)
	(vp-lea-p 'fn_consts :r12)
	;the vertices are a reals, or the bytes of one in a str
	(vpif '(:r9 = 0))
		(vp-cpy-ir :r6 +array_begin :r6)
	(else)
		(vp-add-cr +str_data :r6)
	(endif)
	(vp-cpy-ir :r7 +array_begin :r7)
	(vp-cpy-ri :r6 :r13 } (str s_in) {)
	(vp-cpy-ri :r7 :r13 } (str s_out) {)
	(vp-cpy-ri :r8 :r13 } (str s_n) {)
	(vp-call 'vh_init)
(vp-label 'sh_vloop)
	(vp-cpy-ir :r13 } (str s_n) { :r2)
	(vp-ble-cr 0 :r2 'sh_vdone)
	(vp-sub-cr 1 :r2)
	(vp-cpy-ri :r2 :r13 } (str s_n) {)
	;every so many vertices the other tasks of the node have a turn. All
	;that is kept is in the frame
	(vp-and-cr 2047 :r2)
	(vpif '(:r2 = 0))
		(vp-push :r13)
		(call :sys_task :sleep '(0))
		(vp-pop :r13)
		(vp-lea-p 'fn_consts :r12)
	(endif)
	;the attrs of this vertex, and its varyings start as 0
	(vp-cpy-ir :r13 } (str s_in) { :r2)
	(each (lambda (i)
		(vp-cpy-ir :r2 (* i 8) :r3)
		(vp-cpy-ri :r3 :r13 (+ } (str attr_at) { (* i 8)))) (range 0 } (str attr_slots) {))
	(vp-add-cr } (str (* attr_slots 8)) { :r2)
	(vp-cpy-ri :r2 :r13 } (str s_in) {)
	(vp-xor-rr :r3 :r3)
	(each (lambda (i)
		(vp-cpy-ri :r3 :r13 (+ } (str vary_at) { (* i 8)))) (range 0 } (str vary_slots) {))
	(vp-call 'vf_main)
	;where it is, then its varyings
	(vp-cpy-ir :r13 } (str s_out) { :r2)
	(each (lambda (i)
		(vp-cpy-ir :r13 (+ } (str ret) { (* i 8)) :r3)
		(vp-cpy-ri :r3 :r2 (* i 8))) (range 0 4))
	(each (lambda (i)
		(vp-cpy-ir :r13 (+ } (str vary_at) { (* i 8)) :r3)
		(vp-cpy-ri :r3 :r2 (+ 32 (* i 8)))) (range 0 } (str vary_slots) {))
	(vp-add-cr } (str (+ 32 (* vary_slots 8))) { :r2)
	(vp-cpy-ri :r2 :r13 } (str s_out) {)
	(vp-jmp 'sh_vloop)
(vp-label 'sh_vdone)
	(vp-pop :r0)
	(assign '((:r0 +lisp_sym_nil)) '(:r1))
	(class/obj/ref :r1 :r2)

	(exit '(:r0 :r1))
	(vp-ret)

} (sv-code-text) {

(errorcase
(vp-label 'error)
	(jump :lisp :repl_error '(:r1 "(place frame verts out count is_str)" +error_msg_wrong_types))
	(signature '(:str :obj :obj :num :num)))

(def-func-end)
}))
	(list text frame_size attr_slots (+ 4 vary_slots)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; triangles, filled by a pixel shader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the native function of a pixel shader that fills triangles. It is given
;vertices that have been placed, by any vertex shader, and for each
;triangle works out the pixels it covers. A pixel that is nearer than what
;is there already, by the depth buffer, has its varyings worked out from
;those of the three vertices, and the pixel shader is run for it.
;
;All that is kept from pixel to pixel is in the frame, the pixel shader has
;every register. The numbers the caller sets are slots of the frame too,
;it is told where they are.

(defun sv-fill-code ()
	;the code that walks the triangles, to the code list. The slots are
	;those of the bindings in (sv-fill-source)
	(defun E (&rest form) (push code form))
	(defun L (slot reg) (E 'vp-cpy-if :r13 slot reg))
	(defun S (reg slot) (E 'vp-cpy-fi reg :r13 slot))
	(defun LI (slot reg) (E 'vp-cpy-ir :r13 slot reg))
	(defun SI (reg slot) (E 'vp-cpy-ri reg :r13 slot))
	(defun K (val reg) (E 'vp-cpy-if :r12 (list 'fn-const (logior (n2r val))) reg))
	(defun Q (label) (list 'quote label))
	(defun T (k j) (+ tv_at (* (+ (* k tv_n) j) 8)))
	(defun lerp (ka kb da db kd)
		;vertex kd is where the edge from vertex ka, in front of the near
		;plane, to vertex kb, behind it, crosses the plane
		(E 'vp-cpy-ff da :f5) (E 'vp-cpy-ff da :f4) (E 'vp-sub-ff db :f4) (E 'vp-div-ff :f4 :f5)
		(each (lambda (j)
			(L (T ka j) :f6) (L (T kb j) :f7)
			(E 'vp-sub-ff :f6 :f7) (E 'vp-mul-ff :f5 :f7) (E 'vp-add-ff :f6 :f7)
			(S :f7 (T kd j))) (range 0 tv_n)))
	(E 'vp-call (Q 'sh_init))
	(E 'vp-xor-rr :r2 :r2)
	(SI :r2 s_pend)
	(E 'vp-label (Q 'sr_tri))
	;a triangle that was cut by the near plane can be two, and the other
	;one is waiting, vertices 0, 2 and 3 of what was cut
	(LI s_pend :r2)
	(E 'vp-beq-cr 0 :r2 (Q 'sr_fetch))
	(E 'vp-xor-rr :r2 :r2)
	(SI :r2 s_pend)
	(each (lambda (j)
		(L (T 2 j) :f6) (S :f6 (T 1 j))
		(L (T 3 j) :f6) (S :f6 (T 2 j))) (range 0 tv_n))
	(E 'vp-jmp (Q 'sr_setup))
	(E 'vp-label (Q 'sr_fetch))
	;the next triangle, its three vertices. A triangle is some work done,
	;whether or not it is drawn
	(E 'vp-cpy-cr 32 :r2)
	(E 'vp-call (Q 'sr_work))
	(LI s_ntris :r2)
	(E 'vp-ble-cr 0 :r2 (Q 'sr_done))
	(E 'vp-sub-cr 1 :r2)
	(SI :r2 s_ntris)
	(LI s_tris :r2)
	(E 'vp-cpy-ir :r2 0 :r3) (E 'vp-cpy-ir :r2 8 :r4) (E 'vp-cpy-ir :r2 16 :r5)
	(LI s_tstride :r6)
	(E 'vp-add-rr :r6 :r2)
	(SI :r2 s_tris)
	(LI s_stride :r6) (LI s_placed :r7)
	(each (lambda (r)
		(E 'vp-mul-rr :r6 r) (E 'vp-add-rr :r7 r))
		'(:r3 :r4 :r5))
	;the near plane. A vertex is in front of it if its z + w is not less
	;than 0. With none in front the triangle is left out
	(K 0 :f3)
	(E 'vp-xor-rr :r8 :r8)
	(each (lambda (r d bit label)
		(E 'vp-cpy-if r 16 d) (E 'vp-cpy-if r 24 :f6) (E 'vp-add-ff :f6 d)
		(E 'vp-blt-ff :f3 d (Q label))
		(E 'vp-add-cr bit :r8)
		(E 'vp-label (Q label)))
		'(:r3 :r4 :r5) '(:f0 :f1 :f2) '(1 2 4) '(sr_out0 sr_out1 sr_out2))
	(E 'vp-beq-cr 0 :r8 (Q 'sr_tri))
	;with one or two behind it, the vertices are taken round, the same
	;way round, till the one in front is the first, or the one behind
	;is the last
	(E 'vp-beq-cr 2 :r8 (Q 'sr_rot1))
	(E 'vp-beq-cr 6 :r8 (Q 'sr_rot1))
	(E 'vp-beq-cr 4 :r8 (Q 'sr_rot2))
	(E 'vp-beq-cr 5 :r8 (Q 'sr_rot2))
	(E 'vp-jmp (Q 'sr_rotted))
	(E 'vp-label (Q 'sr_rot1))
	(E 'vp-cpy-rr :r3 :r6) (E 'vp-cpy-rr :r4 :r3) (E 'vp-cpy-rr :r5 :r4) (E 'vp-cpy-rr :r6 :r5)
	(E 'vp-cpy-ff :f0 :f4) (E 'vp-cpy-ff :f1 :f0) (E 'vp-cpy-ff :f2 :f1) (E 'vp-cpy-ff :f4 :f2)
	(E 'vp-jmp (Q 'sr_rotted))
	(E 'vp-label (Q 'sr_rot2))
	(E 'vp-cpy-rr :r5 :r6) (E 'vp-cpy-rr :r4 :r5) (E 'vp-cpy-rr :r3 :r4) (E 'vp-cpy-rr :r6 :r3)
	(E 'vp-cpy-ff :f2 :f4) (E 'vp-cpy-ff :f1 :f2) (E 'vp-cpy-ff :f0 :f1) (E 'vp-cpy-ff :f4 :f0)
	(E 'vp-label (Q 'sr_rotted))
	;the three are copied to the frame, where each is and the varyings
	;the pixel shader reads, so a pixel has them without a sum to find
	;them, and a triangle that is cut has somewhere to be
	(each (lambda (r k)
		(each (lambda (j) (E 'vp-cpy-if r (* j 8) :f6) (S :f6 (T k j))) (range 0 4))
		(each (lambda (i)
			(LI (+ s_vary (* i 8)) :r6)
			(E 'vp-cpy-rr r :r7) (E 'vp-add-rr :r6 :r7) (E 'vp-cpy-if :r7 0 :f6)
			(S :f6 (T k (+ 4 i)))) (range 0 vary_slots)))
		'(:r3 :r4 :r5) '(0 1 2))
	(E 'vp-beq-cr 7 :r8 (Q 'sr_setup))
	(E 'vp-beq-cr 1 :r8 (Q 'sr_clip1))
	(E 'vp-beq-cr 2 :r8 (Q 'sr_clip1))
	(E 'vp-beq-cr 4 :r8 (Q 'sr_clip1))
	;the last is behind. What is left has four corners, the first two,
	;where the edge from the second crosses, and where the edge from the
	;first does. It is two triangles, and the second waits
	(lerp 0 2 :f0 :f2 3)
	(lerp 1 2 :f1 :f2 2)
	(E 'vp-cpy-cr 1 :r2)
	(SI :r2 s_pend)
	(E 'vp-jmp (Q 'sr_setup))
	;only the first is in front. What is left is it, and where its two
	;edges cross
	(E 'vp-label (Q 'sr_clip1))
	(lerp 0 1 :f0 :f1 1)
	(lerp 0 2 :f0 :f2 2)
	(E 'vp-label (Q 'sr_setup))
	;where each is on the screen, row 0 at the top, its depth, and 1 / w
	(LI s_vw :r8) (E 'vp-cvt-rf :r8 :f12)
	(LI s_vh :r8) (E 'vp-cvt-rf :r8 :f13)
	(K 1 :f14) (K 0.5 :f15)
	(E 'vp-cpy-if :r12 (list 'fn-const (logior (str-to-real "0.000001"))) :f11)
	(each (lambda (k sx sy iw z)
		(L (T k 3) :f6)
		(E 'vp-ble-ff :f11 :f6 (Q 'sr_tri))
		(E 'vp-cpy-ff :f14 :f7) (E 'vp-div-ff :f6 :f7) (S :f7 iw)
		(L (T k 0) sx) (E 'vp-mul-ff :f7 sx) (E 'vp-add-ff :f14 sx)
		(E 'vp-mul-ff :f15 sx) (E 'vp-mul-ff :f12 sx)
		(L (T k 1) :f8) (E 'vp-mul-ff :f7 :f8)
		(E 'vp-cpy-ff :f14 sy) (E 'vp-sub-ff :f8 sy)
		(E 'vp-mul-ff :f15 sy) (E 'vp-mul-ff :f13 sy)
		(L (T k 2) :f8) (E 'vp-mul-ff :f7 :f8) (S :f8 z))
		'(0 1 2) '(:f0 :f2 :f4) '(:f1 :f3 :f5)
		(list f_iw0 f_iw1 f_iw2) (list f_z0 f_z1 f_z2))
	;twice its area, with a sign that says which way it faces. One of no
	;area is left out, and one that faces away if they are to be
	(E 'vp-cpy-ff :f2 :f6) (E 'vp-sub-ff :f0 :f6)
	(E 'vp-cpy-ff :f5 :f7) (E 'vp-sub-ff :f1 :f7) (E 'vp-mul-ff :f7 :f6)
	(E 'vp-cpy-ff :f3 :f7) (E 'vp-sub-ff :f1 :f7)
	(E 'vp-cpy-ff :f4 :f8) (E 'vp-sub-ff :f0 :f8) (E 'vp-mul-ff :f8 :f7)
	(E 'vp-sub-ff :f7 :f6)
	(K 0 :f9)
	(E 'vp-beq-ff :f9 :f6 (Q 'sr_tri))
	(LI s_cull :r8)
	(E 'vp-beq-cr 0 :r8 (Q 'sr_faces))
	(E 'vp-beq-cr 2 :r8 (Q 'sr_front))
	(E 'vp-bgt-ff :f9 :f6 (Q 'sr_tri))
	(E 'vp-jmp (Q 'sr_faces))
	(E 'vp-label (Q 'sr_front))
	(E 'vp-blt-ff :f9 :f6 (Q 'sr_tri))
	(E 'vp-label (Q 'sr_faces))
	(E 'vp-cpy-ff :f14 :f10) (E 'vp-div-ff :f6 :f10)
	;the pixels it may cover, kept to the part of the screen asked for
	(E 'vp-cpy-ff :f0 :f6) (E 'vp-min-ff :f2 :f6) (E 'vp-min-ff :f4 :f6)
	(E 'vp-cpy-ff :f0 :f7) (E 'vp-max-ff :f2 :f7) (E 'vp-max-ff :f4 :f7)
	(E 'vp-cpy-ff :f1 :f8) (E 'vp-min-ff :f3 :f8) (E 'vp-min-ff :f5 :f8)
	(E 'vp-cpy-ff :f1 :f9) (E 'vp-max-ff :f3 :f9) (E 'vp-max-ff :f5 :f9)
	(each (lambda (lo hi c0 c1 smin smax)
		(LI c0 :r8) (E 'vp-cvt-rf :r8 :f11) (E 'vp-max-ff :f11 lo)
		(LI c1 :r10) (E 'vp-cvt-rf :r10 :f11) (E 'vp-min-ff :f11 hi)
		(E 'vp-blt-ff lo hi (Q 'sr_tri))
		(E 'vp-cvt-fr lo :r8) (SI :r8 smin)
		(E 'vp-cvt-fr hi :r9) (E 'vp-add-cr 1 :r9) (E 'vp-min-rr :r10 :r9) (SI :r9 smax))
		'(:f6 :f8) '(:f7 :f9) (list s_cx0 s_cy0) (list s_cx1 s_cy1)
		(list s_minx s_y) (list s_maxx s_maxy))
	;for each edge, the share of the triangle a point has from the vertex
	;across from it. It is a number for the corner of the screen, and what
	;it changes by for a pixel along and a pixel down. The three shares of
	;a point add up to 1, and one that is less than 0 says the point is
	;outside. The share at a pixel is worked out from where the pixel is,
	;not stepped to from the pixel before, so it is the same number
	;whichever part of the screen is being drawn, and a frame drawn in
	;strips is the frame drawn whole, to the bit
	(each (lambda (ax ay bx by at kx ky)
		(E 'vp-cpy-ff bx :f11) (E 'vp-sub-ff ax :f11)
		(E 'vp-cpy-ff by :f7) (E 'vp-sub-ff ay :f7)
		(E 'vp-cpy-ff :f7 :f12) (E 'vp-mul-ff ax :f12)
		(E 'vp-cpy-ff :f11 :f13) (E 'vp-mul-ff ay :f13)
		(E 'vp-sub-ff :f13 :f12) (E 'vp-mul-ff :f10 :f12) (S :f12 at)
		(E 'vp-mul-ff :f10 :f7) (E 'vp-neg-ff :f7 :f7) (S :f7 kx)
		(E 'vp-mul-ff :f10 :f11) (S :f11 ky))
		'(:f2 :f4 :f0) '(:f3 :f5 :f1) '(:f4 :f0 :f2) '(:f5 :f1 :f3)
		(list f_e0 f_e1 f_e2) (list f_kx0 f_kx1 f_kx2) (list f_ky0 f_ky1 f_ky2))
	;a row, the shares at the left edge of the screen in this row
	(E 'vp-label (Q 'sr_row))
	(LI s_y :r2) (LI s_maxy :r3)
	(E 'vp-bge-rr :r3 :r2 (Q 'sr_tri))
	;the pixels of the row are work to be done
	(LI s_maxx :r2) (LI s_minx :r3) (E 'vp-sub-rr :r3 :r2)
	(E 'vp-call (Q 'sr_work))
	(LI s_y :r2)
	(K 0.5 :f3) (E 'vp-cvt-rf :r2 :f2) (E 'vp-add-ff :f3 :f2)
	(each (lambda (at ky row) (L ky :f0) (E 'vp-mul-ff :f2 :f0) (L at :f1) (E 'vp-add-ff :f1 :f0) (S :f0 row))
		(list f_e0 f_e1 f_e2) (list f_ky0 f_ky1 f_ky2) (list f_row0 f_row1 f_row2))
	(LI s_minx :r2) (SI :r2 s_x)
	;a pixel
	(E 'vp-label (Q 'sr_px))
	(LI s_x :r2) (LI s_maxx :r3)
	(E 'vp-bge-rr :r3 :r2 (Q 'sr_rowend))
	(K 0.5 :f3) (E 'vp-cvt-rf :r2 :f4) (E 'vp-add-ff :f3 :f4)
	(each (lambda (row kx e) (L kx e) (E 'vp-mul-ff :f4 e) (L row :f3) (E 'vp-add-ff :f3 e))
		(list f_row0 f_row1 f_row2) (list f_kx0 f_kx1 f_kx2) '(:f0 :f1 :f2))
	(K 0 :f3)
	(E 'vp-blt-ff :f3 :f0 (Q 'sr_next))
	(E 'vp-blt-ff :f3 :f1 (Q 'sr_next))
	(E 'vp-blt-ff :f3 :f2 (Q 'sr_next))
	;its depth, -1 to 1 is in view, as a number of 32 bits. It is drawn
	;if that is less than what the depth buffer has
	(L f_z0 :f4) (E 'vp-mul-ff :f0 :f4)
	(L f_z1 :f5) (E 'vp-mul-ff :f1 :f5) (E 'vp-add-ff :f5 :f4)
	(L f_z2 :f5) (E 'vp-mul-ff :f2 :f5) (E 'vp-add-ff :f5 :f4)
	(K -1 :f5) (E 'vp-blt-ff :f5 :f4 (Q 'sr_next))
	(K 1 :f5) (E 'vp-bgt-ff :f5 :f4 (Q 'sr_next))
	(E 'vp-add-ff :f5 :f4)
	(K 2147483647 :f6) (E 'vp-mul-ff :f6 :f4)
	(E 'vp-cvt-fr :f4 :r4)
	(LI s_y :r5) (LI s_dy :r6) (E 'vp-sub-rr :r6 :r5) (LI s_vw :r6)
	(E 'vp-mul-rr :r6 :r5) (E 'vp-add-rr :r2 :r5) (E 'vp-shl-cr 2 :r5)
	(LI s_depth :r6) (E 'vp-add-rr :r6 :r5)
	(E 'vp-cpy-ir-ui :r5 0 :r6)
	(E 'vp-bge-rr :r6 :r4 (Q 'sr_next))
	(SI :r4 s_zi) (SI :r5 s_dptr)
	;the shares, each over the w of its vertex, and made to add up to 1
	;again, are how much of each vertex a varying has here, with the
	;perspective right
	(L f_iw0 :f4) (E 'vp-mul-ff :f4 :f0)
	(L f_iw1 :f4) (E 'vp-mul-ff :f4 :f1)
	(L f_iw2 :f4) (E 'vp-mul-ff :f4 :f2)
	(E 'vp-cpy-ff :f0 :f4) (E 'vp-add-ff :f1 :f4) (E 'vp-add-ff :f2 :f4)
	(K 1 :f5) (E 'vp-div-ff :f4 :f5)
	(E 'vp-mul-ff :f5 :f0) (E 'vp-mul-ff :f5 :f1) (E 'vp-mul-ff :f5 :f2)
	(each (lambda (i)
		(L (T 0 (+ 4 i)) :f4) (E 'vp-mul-ff :f0 :f4)
		(L (T 1 (+ 4 i)) :f5) (E 'vp-mul-ff :f1 :f5)
		(E 'vp-add-ff :f5 :f4)
		(L (T 2 (+ 4 i)) :f5) (E 'vp-mul-ff :f2 :f5)
		(E 'vp-add-ff :f5 :f4)
		(S :f4 (+ vary_at (* i 8)))) (range 0 vary_slots))
	;the frag coord, the centre of the pixel, y up
	(K 0.5 :f5)
	(E 'vp-cvt-rf :r2 :f4) (E 'vp-add-ff :f5 :f4) (S :f4 frag)
	(LI s_y :r6) (LI s_vh :r7)
	(E 'vp-sub-rr :r6 :r7) (E 'vp-sub-cr 1 :r7)
	(E 'vp-cvt-rf :r7 :f4) (E 'vp-add-ff :f5 :f4) (S :f4 (+ frag 8))
	(E 'vp-call (Q 'sf_main))
	;the alpha of the pixel, 0 to 255. One of 0 is not there at all, and
	;is the quick way out
	(K 0 :f1) (K 1 :f2) (K 255 :f3)
	(L (+ ret 24) :f0)
	(E 'vp-max-ff :f1 :f0) (E 'vp-min-ff :f2 :f0) (E 'vp-mul-ff :f3 :f0)
	(E 'vp-cvt-fr :f0 :r4)
	(E 'vp-beq-cr 0 :r4 (Q 'sr_next))
	;the pixel, a 32 bit argb
	(E 'vp-cpy-rr :r4 :r7)
	(each (lambda (offset)
		(L offset :f0)
		(E 'vp-max-ff :f1 :f0) (E 'vp-min-ff :f2 :f0) (E 'vp-mul-ff :f3 :f0)
		(E 'vp-cvt-fr :f0 :r5)
		(E 'vp-shl-cr 8 :r7) (E 'vp-add-rr :r5 :r7)) (list ret (+ ret 8) (+ ret 16)))
	(LI s_y :r5) (LI s_pstride :r6) (E 'vp-mul-rr :r6 :r5)
	(LI s_x :r2) (E 'vp-cpy-rr :r2 :r6) (E 'vp-shl-cr 2 :r6) (E 'vp-add-rr :r6 :r5)
	(LI s_pix :r6) (E 'vp-add-rr :r6 :r5)
	;full on is the other quick way, it is just written
	(E 'vp-beq-cr 255 :r4 (Q 'sr_solid))
	;see through. It goes over what is there as a pixel a canvas plots
	;does, :canvas :plot, its alpha multiplied in, and what is there,
	;which has had its own multiplied in, left with the rest
	(E 'vp-cpy-rr :r7 :r1)
	(E 'call :pixmap :to_premul (Q '(:r1)) (Q '(:r1)))
	(E 'vp-cpy-rr :r1 :r7)
	(E 'vp-cpy-cr 0xff :r8) (E 'vp-sub-rr :r4 :r8)
	(E 'vp-cpy-cr 0x00ff00ff :r2) (E 'vp-cpy-cr 0xff00ff00 :r3)
	(E 'vp-cpy-ir-ui :r5 0 :r6) (E 'vp-cpy-rr :r6 :r9)
	(E 'vp-and-rr :r2 :r6) (E 'vp-and-rr :r3 :r9)
	(E 'vp-mul-rr :r8 :r6) (E 'vp-mul-rr :r8 :r9)
	(E 'vp-shr-cr 8 :r6) (E 'vp-shr-cr 8 :r9)
	(E 'vp-and-rr :r2 :r6) (E 'vp-and-rr :r3 :r9)
	(E 'vp-add-rr :r9 :r6) (E 'vp-add-rr :r6 :r7)
	(E 'vp-label (Q 'sr_solid))
	(E 'vp-cpy-ri-i :r7 :r5 0)
	(LI s_zi :r4) (LI s_dptr :r5)
	(E 'vp-cpy-ri-i :r4 :r5 0)
	;the next pixel
	(E 'vp-label (Q 'sr_next))
	(LI s_x :r2) (E 'vp-add-cr 1 :r2) (SI :r2 s_x)
	(E 'vp-jmp (Q 'sr_px))
	;the next row
	(E 'vp-label (Q 'sr_rowend))
	(LI s_y :r2) (E 'vp-add-cr 1 :r2) (SI :r2 s_y)
	(E 'vp-jmp (Q 'sr_row))
	;:r2 of work has been done. When enough has, the other tasks of the
	;node are given a turn. It is called where nothing is in a register,
	;all that is kept is in the frame, so there is only the frame itself
	;to keep hold of
	(E 'vp-label (Q 'sr_work))
	(LI s_work :r3)
	(E 'vp-add-rr :r2 :r3)
	(SI :r3 s_work)
	(E 'vp-blt-cr +sv_work :r3 (Q 'sr_worked))
	(E 'vp-xor-rr :r3 :r3)
	(SI :r3 s_work)
	(E 'vp-push :r13)
	(E 'call :sys_task :sleep (Q '(0)))
	(E 'vp-pop :r13)
	(E 'vp-lea-p (Q 'fn_consts) :r12)
	(E 'vp-label (Q 'sr_worked))
	(E 'vp-ret)
	(E 'vp-label (Q 'sr_done)))

(defun sv-fill-source (program name)
	; -> (text frame_size params vary_slots)
	;the VP source of the native function that fills triangles with a
	;pixel shader. params is where in the frame the caller's numbers go,
	;(ntris stride tstride vw vh cx0 cy0 cx1 cy1 cull dy vary), vary is the
	;first of a slot for each number of the varyings, where in a placed
	;vertex that number is, in bytes
	(defq code (list) live (list) free_f (cat +sv_fregs) free_r (cat +sv_rregs)
		vars (list) finfo (list) loops (list) frame_size 0 label_num 0
		ret_offset 0 ret_label :nil)
	;the frame starts with the inputs, then the varyings of the pixel
	;being shaded, then what the walk of the triangles keeps
	(each (lambda ((name type &rest _)) (push vars (list name (sv-slots type)))) (first program))
	(defq vary_at frame_size)
	(each (lambda ((name type)) (push vars (list name (sv-slots type)))) (shader-varyings program))
	(defq vary_slots (/ (- frame_size vary_at) 8))
	(bind '(s_ntris s_stride s_tstride s_vw s_vh s_cx0 s_cy0 s_cx1 s_cy1 s_cull s_dy
			s_placed s_tris s_pix s_pstride s_depth s_pend
			s_minx s_maxx s_y s_maxy s_x s_zi s_dptr s_work
			f_iw0 f_iw1 f_iw2 f_z0 f_z1 f_z2 f_row0 f_row1 f_row2 f_e0 f_e1 f_e2
			f_kx0 f_kx1 f_kx2 f_ky0 f_ky1 f_ky2)
		(map (lambda (_) (sv-slots :int)) (range 0 43)))
	(defq s_vary frame_size)
	(times vary_slots (sv-slots :int))
	;four vertices, where each is and its varyings, the three of the
	;triangle being drawn and one more for a triangle that is cut
	(defq tv_at frame_size tv_n (+ 4 vary_slots))
	(times (* 4 tv_n) (sv-slots :int))
	(bind '((frag) ret) (sv-program program "s"))
	(defq funcs_text (sv-code-text))
	(setq code (list))
	(sv-fill-code)
	(defq lf (ascii-char 10) text (cat
{(include "lib/asm/func.inc")
(include "sys/math/class.inc")
(include "class/num/class.inc")
(include "gui/pixmap/class.inc")
(include "class/str/class.inc")
(include "class/array/class.inc")
(include "class/lisp/class.inc")
(include "sys/task/class.inc")

(def-func "} name {")
	;inputs
	;:r0 = lisp object (ptr)
	;:r1 = args list object (ptr)
	;outputs
	;:r0 = lisp object (ptr)
	;:r1 = return value object (ptr)
	;trashes
	;:r1-:r14, :f0-:f15
	;info
	;(fill frame placed tris pixmap depth) -> :nil

	(entry '(:r0 :r1))

	(errorif-lisp-args-sig 'error :r1 5)

	(vp-push :r0)
	(list-bind-args :r1 '(:r13 :r6 :r7 :r8 :r9) '(:str :obj :obj :obj :str))
	(vp-lea-p 'fn_consts :r12)
	(vp-cpy-ir :r6 +array_begin :r6)
	(vp-cpy-ir :r7 +array_begin :r7)
	(vp-cpy-ir-ui :r8 } (str +pixmap_stride) { :r10)
	(vp-cpy-ir :r8 } (str +pixmap_data) { :r8)
	(vp-cpy-ri :r6 :r13 } (str s_placed) {)
	(vp-cpy-ri :r7 :r13 } (str s_tris) {)
	(vp-cpy-ri :r8 :r13 } (str s_pix) {)
	(vp-cpy-ri :r10 :r13 } (str s_pstride) {)
	(vp-cpy-ri :r9 :r13 } (str s_depth) {)
} (sv-code-text) {
	(vp-pop :r0)
	(assign '((:r0 +lisp_sym_nil)) '(:r1))
	(class/obj/ref :r1 :r2)

	(exit '(:r0 :r1))
	(vp-ret)

} funcs_text {

(errorcase
(vp-label 'error)
	(jump :lisp :repl_error '(:r1 "(fill frame placed tris pixmap depth)" +error_msg_wrong_types))
	(signature '(:str :obj :obj :obj :str)))

(def-func-end)
}))
	(list text frame_size
		(list s_ntris s_stride s_tstride s_vw s_vh s_cx0 s_cy0 s_cx1 s_cy1 s_cull s_dy s_vary)
		vary_slots))

(defun sv-source (program name)
	; -> (text frame_size)
	;the VP source of the native function for a program
	(sh-pixel-only program "VP")
	(bind '(inputs consts globals funcs &rest _) program)
	(defq code (list) live (list) free_f (cat +sv_fregs) free_r (cat +sv_rregs)
		vars (list) finfo (list) loops (list) frame_size 0 label_num 0
		ret_offset 0 ret_label :nil)
	;the frame starts with the inputs, then what the tile loop keeps
	(each (lambda ((name type &rest _)) (push vars (list name (sv-slots type)))) inputs)
	(bind '(s_x0 s_x1 s_x s_y s_y1 s_mode s_height s_out s_skip s_work)
		(map (lambda (_) (sv-slots :int)) (range 0 10)))
	(bind '((frag) ret) (sv-program program "s"))
	(defq lf (ascii-char 10) text (cat
{(include "lib/asm/func.inc")
(include "sys/math/class.inc")
(include "class/num/class.inc")
(include "class/str/class.inc")
(include "class/lisp/class.inc")
(include "sys/task/class.inc")

(def-func "} name {")
	;inputs
	;:r0 = lisp object (ptr)
	;:r1 = args list object (ptr)
	;outputs
	;:r0 = lisp object (ptr)
	;:r1 = return value object (ptr)
	;trashes
	;:r1-:r14, :f0-:f15
	;info
	;(shade x y x1 y1 frame out mode height) -> :nil

	(entry '(:r0 :r1))

	(errorif-lisp-args-sig 'error :r1 8)

	(vp-push :r0)
	(list-bind-args :r1 '(:r2 :r3 :r4 :r5 :r13 :r6 :r7 :r8)
		'(:num :num :num :num :str :obj :num :num))
	(vp-lea-p 'fn_consts :r12)
	;where the pixels go, and what is skipped at the end of each row. A
	;string is filled row after row. In modes 2 and 3 out is a pixmap,
	;and the tile is drawn where it belongs on it
	(vp-xor-rr :r9 :r9)
	(vp-beq-cr 2 :r7 'sh_pixmap)
	(vp-beq-cr 3 :r7 'sh_pixmap)
	(vp-add-cr +str_data :r6)
	(vp-jmp 'sh_out)
(vp-label 'sh_pixmap)
	(vp-cpy-ir-ui :r6 } (str +pixmap_stride) { :r9)
	(vp-cpy-ir :r6 } (str +pixmap_data) { :r6)
	(vp-cpy-rr :r3 :r10)
	(vp-mul-rr :r9 :r10)
	(vp-add-rr :r10 :r6)
	(vp-cpy-rr :r2 :r10)
	(vp-shl-cr 2 :r10)
	(vp-add-rr :r10 :r6)
	(vp-cpy-rr :r4 :r10)
	(vp-sub-rr :r2 :r10)
	(vp-shl-cr 2 :r10)
	(vp-sub-rr :r10 :r9)
(vp-label 'sh_out)
	(vp-cpy-ri :r9 :r13 } (str s_skip) {)
	(vp-cpy-ri :r2 :r13 } (str s_x0) {)
	(vp-cpy-ri :r3 :r13 } (str s_y) {)
	(vp-cpy-ri :r4 :r13 } (str s_x1) {)
	(vp-cpy-ri :r5 :r13 } (str s_y1) {)
	(vp-cpy-ri :r6 :r13 } (str s_out) {)
	(vp-cpy-ri :r7 :r13 } (str s_mode) {)
	(vp-cpy-ri :r8 :r13 } (str s_height) {)
	(vp-call 'sh_init)
(vp-label 'sh_yloop)
	;after so many pixels, a row counts as its pixels, the other tasks of
	;the node have a turn. All that is kept is in the frame
	(vp-cpy-ir :r13 } (str s_work) { :r2)
	(vp-cpy-ir :r13 } (str s_x1) { :r3)
	(vp-cpy-ir :r13 } (str s_x0) { :r4)
	(vp-add-rr :r3 :r2)
	(vp-sub-rr :r4 :r2)
	(vpif '(:r2 >= } (str +sv_tile_work) {))
		(vp-xor-rr :r2 :r2)
		(vp-cpy-ri :r2 :r13 } (str s_work) {)
		(vp-push :r13)
		(call :sys_task :sleep '(0))
		(vp-pop :r13)
		(vp-lea-p 'fn_consts :r12)
	(else)
		(vp-cpy-ri :r2 :r13 } (str s_work) {)
	(endif)
	(vp-cpy-ir :r13 } (str s_y) { :r2)
	(vp-cpy-ir :r13 } (str s_y1) { :r3)
	(vp-bge-rr :r3 :r2 'sh_done)
	(vp-cpy-ir :r13 } (str s_x0) { :r2)
	(vp-cpy-ri :r2 :r13 } (str s_x) {)
(vp-label 'sh_xloop)
	(vp-cpy-ir :r13 } (str s_x) { :r2)
	(vp-cpy-ir :r13 } (str s_x1) { :r3)
	(vp-bge-rr :r3 :r2 'sh_xend)
	;the frag coord, the centre of the pixel, y up if a height is given
	(vp-cpy-if :r12 (fn-const (n2r 0.5)) :f2)
	(vp-cvt-rf :r2 :f0)
	(vp-add-ff :f2 :f0)
	(vp-cpy-fi :f0 :r13 } (str frag) {)
	(vp-cpy-ir :r13 } (str s_y) { :r3)
	(vp-cpy-ir :r13 } (str s_height) { :r4)
	(vp-beq-cr 0 :r4 'sh_noflip)
	(vp-sub-rr :r3 :r4)
	(vp-sub-cr 1 :r4)
	(vp-cpy-rr :r4 :r3)
(vp-label 'sh_noflip)
	(vp-cvt-rf :r3 :f0)
	(vp-add-ff :f2 :f0)
	(vp-cpy-fi :f0 :r13 } (str (+ frag 8)) {)
	(vp-call 'sf_main)
	(vp-cpy-ir :r13 } (str s_out) { :r2)
	(vp-cpy-ir :r13 } (str s_mode) { :r3)
	(vp-bne-cr 0 :r3 'sh_argb)
	;mode 0, the 4 floats as they are
	(vp-cpy-if :r13 } (str ret) { :f0)
	(vp-cpy-if :r13 } (str (+ ret 8)) { :f1)
	(vp-cpy-if :r13 } (str (+ ret 16)) { :f2)
	(vp-cpy-if :r13 } (str (+ ret 24)) { :f3)
	(vp-cpy-fi :f0 :r2 0)
	(vp-cpy-fi :f1 :r2 8)
	(vp-cpy-fi :f2 :r2 16)
	(vp-cpy-fi :f3 :r2 24)
	(vp-add-cr 32 :r2)
	(vp-jmp 'sh_next)
(vp-label 'sh_argb)
	;modes 1 and 2, a 32 bit argb pixel, alpha full on. In mode 3 the
	;alpha is the one main gave
	(vp-cpy-if :r12 (fn-const (n2r 0)) :f1)
	(vp-cpy-if :r12 (fn-const (n2r 1)) :f2)
	(vp-cpy-if :r12 (fn-const (n2r 255)) :f3)
	(vp-cpy-cr 0xff :r4)
	(vp-bne-cr 3 :r3 'sh_rgb)
	(vp-cpy-if :r13 } (str (+ ret 24)) { :f0)
	(vp-max-ff :f1 :f0)
	(vp-min-ff :f2 :f0)
	(vp-mul-ff :f3 :f0)
	(vp-cvt-fr :f0 :r4)
(vp-label 'sh_rgb)
	(each (lambda (offset)
		(vp-cpy-if :r13 offset :f0)
		(vp-max-ff :f1 :f0)
		(vp-min-ff :f2 :f0)
		(vp-mul-ff :f3 :f0)
		(vp-cvt-fr :f0 :r5)
		(vp-shl-cr 8 :r4)
		(vp-add-rr :r5 :r4)) '(} (str ret) " " (str (+ ret 8)) " " (str (+ ret 16)) {))
	(vp-cpy-ri-i :r4 :r2 0)
	(vp-add-cr 4 :r2)
(vp-label 'sh_next)
	(vp-cpy-ri :r2 :r13 } (str s_out) {)
	(vp-cpy-ir :r13 } (str s_x) { :r2)
	(vp-add-cr 1 :r2)
	(vp-cpy-ri :r2 :r13 } (str s_x) {)
	(vp-jmp 'sh_xloop)
(vp-label 'sh_xend)
	(vp-cpy-ir :r13 } (str s_out) { :r2)
	(vp-cpy-ir :r13 } (str s_skip) { :r3)
	(vp-add-rr :r3 :r2)
	(vp-cpy-ri :r2 :r13 } (str s_out) {)
	(vp-cpy-ir :r13 } (str s_y) { :r2)
	(vp-add-cr 1 :r2)
	(vp-cpy-ri :r2 :r13 } (str s_y) {)
	(vp-jmp 'sh_yloop)
(vp-label 'sh_done)
	(vp-pop :r0)
	(assign '((:r0 +lisp_sym_nil)) '(:r1))
	(class/obj/ref :r1 :r2)

	(exit '(:r0 :r1))
	(vp-ret)

} (sv-code-text) {

(errorcase
(vp-label 'error)
	(jump :lisp :repl_error '(:r1 "(shade x y x1 y1 frame out mode height)" +error_msg_wrong_types))
	(signature '(:num :num :num :num :str :obj :num :num)))

(def-func-end)
}))
	(list text frame_size))

(defun sv-native (name text)
	; -> :nil | func
	;the native function of this name, assembled from the text if this
	;CPU has not got it yet
	(bind '(& *cpu* *abi*) (map (const sym) (split (load-path) "/")))
	(defq obj (cat "obj/" *cpu* "/" *abi* "/" name))
	;the lock is held to bind as well, so that no task can load a
	;function that another is part way through writing
	(lock-claim-rpc obj)
	(when (= (age obj) 0)
		(save text (cat obj ".vp"))
		(within-compile-env (# (include (cat obj ".vp")))))
	(defq func (if (/= (age obj) 0) (ffi name)))
	(lock-release-rpc obj)
	(unless func (sh-err "could not assemble" name))
	func)

(defun sv-name (letter program)
	;the name of the native function of a program
	(cat "lib/gpu/jit/" letter (str (logand (hash (str +sv_version program)) 0xffffffffffff))))

(defun shader-vp (program)
	; (shader-vp program) -> (shade frame_size)
	;the native function for a pixel shader. It is assembled if this is
	;the first time this CPU has been given this program.
	(defq name (sv-name "s" program))
	(bind '(text frame_size) (sv-source program name))
	(list (sv-native name text) frame_size))

(defun shader-vp-vertex (program)
	; (shader-vp-vertex program) -> (place frame_size attr_slots out_slots)
	;the native function for a vertex shader, it places vertices. A vertex
	;is attr_slots numbers, its attrs one after another, and what comes
	;out for it is out_slots numbers, where it is, 4, then its varyings.
	(unless (eql (shader-stage program) :vertex) (sh-err "not a vertex shader" 'main))
	(defq name (sv-name "v" program))
	(bind '(text frame_size attr_slots out_slots) (sv-vertex-source program name))
	(list (sv-native name text) frame_size attr_slots out_slots))

(defun shader-vp-fill (program)
	; (shader-vp-fill program) -> (fill frame_size params vary_slots)
	;the native function for a pixel shader that fills triangles with it
	(unless (eql (shader-stage program) :pixel) (sh-err "not a pixel shader" 'main))
	(defq name (sv-name "t" program))
	(bind '(text frame_size params vary_slots) (sv-fill-source program name))
	(list (sv-native name text) frame_size params vary_slots))

(defun shader-vp-pipeline (vertex pixel)
	; (shader-vp-pipeline vertex pixel) -> pipeline
	;a vertex shader and a pixel shader as native code, to draw triangles
	;with. Each is a function of its own, so a vertex shader is assembled
	;once however many pixel shaders it is used with, and a pixel shader
	;once however many vertex shaders. What ties the two is where in a
	;placed vertex each varying of the pixel shader is.
	(shader-pair vertex pixel)
	(defq place (shader-vp-vertex vertex) fill (shader-vp-fill pixel)
		at 32 where (map (lambda ((name type))
			(defq o at) (setq at (+ at (* (shader-size type) 8))) (list name o))
			(shader-varyings vertex)))
	(list vertex pixel place fill
		(apply (const cat) (cat (list (list)) (map (lambda ((name type))
			(defq o (second (some (# (if (eql (first %0) name) %0)) where)))
			(map (# (+ o (* %0 8))) (range 0 (shader-size type))))
			(shader-varyings pixel))))))

(defun shader-vp-depth (width height)
	; (shader-vp-depth width height) -> depth
	;a depth buffer for a pixmap of this size, with nothing in it, all of
	;it as far away as can be. A new one is made for each frame, and it is
	;a copy of one that is kept, the one copy of the bytes. What is kept is
	;all but the last 4 bytes, so that joining them on is always a new
	;str, the one that is kept is never the one that is handed out
	(defq n (* width height 4))
	(cat (memoize n (progn
		(defq far (const (char -1 8)))
		(while (< (length far) n) (setq far (cat far far)))
		(slice far 0 (max 0 (- n 4)))) 31) (char -1 4)))

(defun shader-vp-draw-tris (pipeline verts tris pixmap depth &optional vvals pvals cull tri_size x y x1 y1 depth_y)
	; (shader-vp-draw-tris pipeline verts tris pixmap depth [vvals pvals cull tri_size x y x1 y1 depth_y]) -> pixmap
	;draw triangles on a 32 bit pixmap. verts is a reals, the attrs of a
	;vertex one after another, vertex after vertex, or the bytes of one
	;in a str. tris is a nums, three
	;numbers of vertices for each triangle, the first three of every
	;tri_size, 3 if not given. vvals and pvals are the inputs of the two
	;shaders. Where a vertex shader puts a vertex, x and y of -1 to 1 are
	;the edges of the pixmap, y up, and z of -1 to 1 is in view, nearest
	;first. With cull a triangle whose vertices go round clockwise as seen
	;is left out, it faces away. A cull of :front leaves out those that go
	;round the other way, for a mesh made the other way round, or a view
	;that is turned over. Only the pixels of x y x1 y1 are drawn,
	;all of the pixmap if they are not given, so that a frame can be drawn
	;a part at a time, or by several tasks. A task that draws rows y to y1
	;only can have a depth buffer of just those rows, and says so with a
	;depth_y of y, the row its depth buffer starts at. A pixel whose alpha
	;is 0 is not drawn, and leaves the depth buffer alone. One that is
	;full on is written. One between goes over what is there, which is
	;taken to have had its alpha multiplied in, and its depth is kept as
	;any other, so what is see through is to be drawn after what is not,
	;the furthest first.
	;A triangle that the near plane goes through, where z is -w, is cut by
	;it, and what is in front is drawn.
	(bind '(vertex pixel place fill offsets) pipeline)
	(defq width (getf pixmap +pixmap_width 0) height (getf pixmap +pixmap_height 0))
	(setd tri_size 3 x 0 y 0 x1 width y1 height depth_y 0)
	(defq placed (sv-place-again place (shader-vp-frame vertex place vvals) verts)
		frame (shader-vp-frame pixel fill pvals))
	(bind '(s_ntris s_stride s_tstride s_vw s_vh s_cx0 s_cy0 s_cx1 s_cy1 s_cull s_dy s_vary) (third fill))
	(each (# (set-long frame %0 %1))
		(list s_ntris s_stride s_tstride s_vw s_vh s_cx0 s_cy0 s_cx1 s_cy1 s_cull s_dy)
		(list (/ (length tris) tri_size) (* (elem-get place 3) 8) (* tri_size 8) width height
			(max x 0) (max y depth_y 0) (min x1 width) (min y1 height)
			(cond ((not cull) 0) ((eql cull :front) 2) (1)) depth_y))
	(each (# (set-long frame (+ s_vary (* (!) 8)) %0)) offsets)
	(if (and (nempty? tris) (nempty? placed))
		((first fill) frame placed tris pixmap depth))
	pixmap)

(defun sv-reals (n)
	;a reals of n numbers, all 0
	(defq out (reals (n2r 0) (n2r 0) (n2r 0) (n2r 0)))
	(while (< (length out) n) (setq out (cat out out)))
	(slice out 0 n))

(defun shader-vp-place (native frame verts)
	; (shader-vp-place native frame verts) -> reals
	;place the vertices of a reals, the attrs of one after another, vertex
	;after vertex. The result is a reals, for each vertex where it is, 4
	;numbers, then its varyings. The vertices can be a str, the bytes of
	;such a reals, 8 to a number, as (shader-verts-str) makes, which is
	;how they are when they have come in a message.
	(bind '(place & attr_slots out_slots) native)
	(defq count (/ (if (str? verts) (/ (length verts) 8) (length verts)) (max attr_slots 1))
		out (sv-reals (* count out_slots)))
	(if (> count 0) (place frame verts out count (if (str? verts) 1 0)))
	out)

(defun sv-place-again (native frame verts)
	;as (shader-vp-place), for a result that is used at once and let go
	;of, as a draw does. The reals the last such call of that size gave
	;is used again, every number of it is written, so there is no new one
	;to make and clear for each object of each frame
	(bind '(place & attr_slots out_slots) native)
	(defq count (/ (if (str? verts) (/ (length verts) 8) (length verts)) (max attr_slots 1))
		out (memoize (* count out_slots) (sv-reals (* count out_slots)) 31))
	(if (> count 0) (place frame verts out count (if (str? verts) 1 0)))
	out)

(defun shader-verts-str (verts)
	; (shader-verts-str reals) -> str
	;the bytes of a reals of vertices, to send in a message
	(defq out (str-alloc (* (length verts) 8)))
	(each (# (set-real out (* (!) 8) %0)) verts)
	out)

(defun shader-vp-frame (program native &optional vals)
	; (shader-vp-frame program native [((name val) ...)]) -> frame
	;the frame the native function works in, with the inputs set
	(defq frame (str-alloc (second native)) offset 0)
	(each (lambda ((name type val &rest _))
		(if (defq v (some (# (if (eql (first %0) name) %0)) vals))
			(setq val (second v)))
		(cond
			((eql type :int) (set-long frame offset (n2i val)))
			((eql type :float) (set-real frame offset (shader-real val)))
			((seq? val) (each (# (set-real frame (+ offset (* (!) 8)) (shader-real %0))) val)))
		(setq offset (+ offset (* (shader-size type) 8)))) (first program))
	frame)

(defun shader-vp-argb (native frame x y x1 y1 &optional height)
	; (shader-vp-argb native frame x y x1 y1 [height]) -> str
	;the pixels of a tile, row by row, each a 32 bit argb. If the height
	;of the frame is given then row 0 is the top row, as a canvas has it.
	(defq out (str-alloc (* (- x1 x) (- y1 y) 4)))
	((first native) x y x1 y1 frame out 1 (ifn height 0))
	out)

(defun shader-vp-draw (native frame pixmap x y x1 y1 &optional height alpha)
	; (shader-vp-draw native frame pixmap x y x1 y1 [height alpha]) -> :nil | pixmap
	;a tile drawn straight onto a 32 bit pixmap, where it belongs on it,
	;with no copy of the pixels in between. :nil if the tile is not all
	;inside the pixmap. The pixels are full on, so they are right for a
	;pixmap that is premultiplied as well as one that is not. With alpha
	;the alpha of a pixel is the one main gave, for a shader that is to be
	;seen through. A pixmap is premultiplied, so such a shader gives its
	;color times its alpha.
	(when (and (<= 0 x) (< x x1) (<= x1 (getf pixmap +pixmap_width 0))
			(<= 0 y) (< y y1) (<= y1 (getf pixmap +pixmap_height 0)))
		((first native) x y x1 y1 frame pixmap (if alpha 3 2) (ifn height 0))
		pixmap))

(defun shader-vp-pixels (native frame x y x1 y1)
	; (shader-vp-pixels native frame x y x1 y1) -> (vec4 ...)
	;the pixels of a tile, row by row, each a reals of 4
	(defq out (str-alloc (* (- x1 x) (- y1 y) 32)))
	((first native) x y x1 y1 frame out 0 0)
	(map (# (reals (get-real out %0) (get-real out (+ %0 8))
		(get-real out (+ %0 16)) (get-real out (+ %0 24)))) (range 0 (length out) 32)))
