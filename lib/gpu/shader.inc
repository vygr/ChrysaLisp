;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shader language, s-expression source to a checked, typed tree.
; The tree is what the back ends take, see glsl.inc and cpu.inc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;a program is (inputs consts globals funcs)
;	input, (name type default min max)
;	const and global, (name type expr)
;	func, (name type ((name type) ...) block)
;an expr is (type tag ...)
;	(type :lit value), a float value is a str of the decimal number
;	(type :var name kind), kind is :input :const :global :param :local :loop
;	(type :swz (idx ...) expr)
;	(type :user name (expr ...))
;	(type :op name (expr ...))
;a block is a list of statements
;	(:defq name expr)
;	(:setq name expr)
;	(:setsw name (idx ...) expr)
;	(:if expr block block)
;	(:for name expr expr block)
;	(:break)
;	(:return expr)

; these lists are not + constants, a prebound list would be run as a form
(defq *sh_types* '(:float :int :bool :vec2 :vec3 :vec4)
	*sh_input_types* '(:float :int :vec2 :vec3 :vec4)
	*sh_arith* '(+ - * /)
	*sh_order* '(< <= > >=)
	*sh_equal* '(= /=)
	*sh_logic* '(and or)
	*sh_ctors* '(vec2 vec3 vec4)
	*sh_gen1* '(sin cos sqrt floor fract abs)
	*sh_gen2* '(min max mod)
	*sh_others* '(not float int pow clamp mix dot cross length normalize reflect)
	*sh_words* '(definput defconst defglobal defun defq setq if when progn for break return))

(defun sh-err (msg form)
	(throw (cat "Shader, " msg " !") form))

(defun shader-dim (type)
	; (shader-dim type) -> :nil | 2 | 3 | 4
	(case type (:vec2 2) (:vec3 3) (:vec4 4) (:t :nil)))

(defun sh-vec (n)
	(elem-get '(:float :float :vec2 :vec3 :vec4) n))

(defun sh-float? (type)
	(or (eql type :float) (shader-dim type)))

(defun sh-op? (name)
	(some (# (find name %0)) (list *sh_arith* *sh_order* *sh_equal* *sh_logic*
		*sh_ctors* *sh_gen1* *sh_gen2* *sh_others*)))

(defun sh-find (name)
	(some (# (if (eql (first %0) name) %0)) syms))

(defun sh-func (name)
	(some (# (if (eql (first %0) name) %0)) funcs))

(defun sh-name (name form)
	;a new name must be a plain symbol, and must not hide anything
	(cond
		((not (sym? name)) (sh-err "name is not a symbol" form))
		((find (first name) ":_+") (sh-err "name starts with a reserved character" form))
		((or (sh-op? name) (find name *sh_words*)) (sh-err "name is a reserved word" form))
		((or (sh-find name) (sh-func name)) (sh-err "name is already in use" form)))
	name)

(defun sh-type (type form)
	(unless (find type *sh_types*) (sh-err "unknown type" form))
	type)

(defun sh-op-type (op types)
	;result type of a built in op, or :nil if the types are wrong
	(defq n (length types) a (first types) b (second types) c (third types))
	(cond
		((find op *sh_arith*)
			(and (> n (if (eql op '-) 0 1))
				(or (sh-float? a) (eql a :int))
				(reduce (# (cond
					((not %0) :nil)
					((eql %0 %1) %0)
					((and (shader-dim %0) (eql %1 :float)) %0)
					((and (eql %0 :float) (shader-dim %1)) %1))) types)))
		((find op *sh_order*)
			(and (= n 2) (eql a b) (find a '(:float :int)) :bool))
		((find op *sh_equal*)
			(and (= n 2) (eql a b) (find a '(:float :int :bool)) :bool))
		((find op *sh_logic*)
			(and (> n 1) (every (# (eql %0 :bool)) types) :bool))
		((eql op 'not)
			(and (= n 1) (eql a :bool) :bool))
		((eql op 'float)
			(and (= n 1) (find a '(:float :int)) :float))
		((eql op 'int)
			(and (= n 1) (find a '(:float :int)) :int))
		((find op *sh_ctors*)
			(defq d (+ (find op *sh_ctors*) 2)
				s (reduce (# (if %0 (cond
					((find %1 '(:float :int)) (inc %0))
					((shader-dim %1) (+ %0 (shader-dim %1)))))) types 0))
			(and s (> n 0) (or (= s d) (and (= n 1) (= s 1))) (sh-vec d)))
		((find op *sh_gen1*)
			(and (= n 1) (sh-float? a) a))
		((find op *sh_gen2*)
			(and (= n 2) (sh-float? a) (or (eql a b) (eql b :float)) a))
		((eql op 'pow)
			(and (= n 2) (sh-float? a) (eql a b) a))
		((eql op 'clamp)
			(and (= n 3) (sh-float? a)
				(or (and (eql a b) (eql a c)) (and (eql b :float) (eql c :float))) a))
		((eql op 'mix)
			(and (= n 3) (sh-float? a) (eql a b) (or (eql a c) (eql c :float)) a))
		((eql op 'dot)
			(and (= n 2) (shader-dim a) (eql a b) :float))
		((eql op 'cross)
			(and (= n 2) (eql a :vec3) (eql b :vec3) a))
		((eql op 'length)
			(and (= n 1) (sh-float? a) :float))
		((eql op 'normalize)
			(and (= n 1) (shader-dim a) a))
		((eql op 'reflect)
			(and (= n 2) (shader-dim a) (eql a b) a))))

(defun sh-swizzle (name type form)
	;component indices of a swizzle like :xyz or :rgb
	(defq d (shader-dim type) idxs (map (# (cond
		((defq i (find %0 "xyzw")) i)
		((defq i (find %0 "rgba")) i)
		((sh-err "unknown component" form)))) (rest name)))
	(unless d (sh-err "components of a value that is not a vector" form))
	(unless (<= 1 (length idxs) 4) (sh-err "wrong number of components" form))
	(unless (every (# (< %0 d)) idxs) (sh-err "component out of range" form))
	idxs)

(defun sh-decimal (form)
	;the Lisp reader gives a float literal as a 16.16 fixed, which is good
	;to 4 decimal places, so that is what it is rounded to. A number that
	;needs more is written as a str, and is taken as it stands.
	(cond
		((str? form)
			(unless (and (nempty? form) (every (# (find %0 "0123456789.-")) form)
					(<= (length (split form ".")) 2) (not (find "-" (rest form))))
				(sh-err "not a number" form))
			(if (find "." form) form (cat form ".0")))
		(:t (defq n (n2i (+ (* (abs form) 10000.0) 0.5)))
			(cat (if (< form 0) "-" "") (str (/ n 10000)) "."
				(pad (str (% n 10000)) 4 "0000")))))

(defun sh-expr (form)
	(cond
		((real? form) (sh-err "real number literal" form))
		((fixed? form) (list :float :lit (sh-decimal form)))
		((num? form) (list :int :lit form))
		((eql form :t) (list :bool :lit :t))
		((eql form :nil) (list :bool :lit :nil))
		((and (str? form) (not (sym? form))) (list :float :lit (sh-decimal form)))
		((sym? form)
			(unless (defq e (sh-find form)) (sh-err "unknown name" form))
			(if (and const_only (not (eql (third e) :const)))
				(sh-err "not a constant" form))
			(list (second e) :var form (third e)))
		((not (and (list? form) (nempty? form) (sym? (defq op (first form)))))
			(sh-err "not an expression" form))
		((eql (first op) ":")
			(unless (= (length form) 2) (sh-err "wrong number of args" form))
			(defq arg (sh-expr (second form))
				idxs (sh-swizzle op (first arg) form))
			(list (sh-vec (length idxs)) :swz idxs arg))
		((defq f (sh-func op))
			(if const_only (sh-err "not a constant" form))
			(defq args (map sh-expr (rest form)))
			(unless (and (= (length args) (length (third f)))
					(every (# (eql (first %0) (second %1))) args (third f)))
				(sh-err "wrong args to function" form))
			(list (second f) :user op args))
		((sh-op? op)
			(defq args (map sh-expr (rest form)))
			(unless (defq type (sh-op-type op (map (const first) args)))
				(sh-err "wrong types" form))
			(list type :op op args))
		((sh-err "unknown function" form))))

(defun sh-const-int (form)
	;a loop bound, an int literal or an int constant
	(defq e (sh-expr form))
	(unless (and (eql (first e) :int)
			(or (eql (second e) :lit) (eql (last e) :const)))
		(sh-err "loop bound is not a constant int" form))
	e)

(defun sh-pairs (form)
	(defq args (rest form))
	(if (or (empty? args) (odd? (length args)))
		(sh-err "wrong number of args" form))
	(partition args 2))

(defun sh-block (forms)
	;statements in a scope of their own
	(defq mark (length syms) out (list))
	(each (# (sh-stmt %0 out)) forms)
	(setq syms (slice syms 0 mark))
	out)

(defun sh-stmt (form out)
	(unless (and (list? form) (nempty? form)) (sh-err "not a statement" form))
	(defq op (first form) n (dec (length form)))
	(cond
		((eql op 'defq)
			(each (lambda ((name val))
				(sh-name name form)
				(defq e (sh-expr val))
				(push syms (list name (first e) :local))
				(push out (list :defq name e))) (sh-pairs form)))
		((eql op 'setq)
			(each (lambda ((name val))
				(defq idxs :nil e (sh-expr val))
				(when (and (list? name) (= (length name) 2))
					(setq idxs (first name) name (second name)))
				(unless (and (sym? name) (defq v (sh-find name)))
					(sh-err "unknown name" form))
				(unless (find (third v) '(:local :param))
					(sh-err "can only set a local or a parameter" form))
				(cond
					(idxs
						(setq idxs (sh-swizzle idxs (second v) form))
						(unless (and (eql (first e) (sh-vec (length idxs)))
								(= (length (unique idxs)) (length idxs)))
							(sh-err "wrong types" form))
						(push out (list :setsw name idxs e)))
					((eql (first e) (second v))
						(push out (list :setq name e)))
					((sh-err "wrong types" form)))) (sh-pairs form)))
		((find op '(if when))
			(unless (if (eql op 'if) (<= 2 n 3) (>= n 2))
				(sh-err "wrong number of args" form))
			(defq e (sh-expr (second form)))
			(unless (eql (first e) :bool) (sh-err "test is not a bool" form))
			(push out (if (eql op 'if)
				(list :if e (sh-block (list (third form)))
					(if (= n 3) (sh-block (list (last form))) (list)))
				(list :if e (sh-block (slice form 2 -1)) (list)))))
		((eql op 'progn)
			(each (# (push out %0)) (sh-block (rest form))))
		((eql op 'for)
			(unless (and (>= n 1) (list? (defq head (second form)))
					(= (length head) 3))
				(sh-err "wrong loop header" form))
			(defq name (first head) mark (length syms)
				start (sh-const-int (second head)) end (sh-const-int (third head)))
			(sh-name name form)
			(push syms (list name :int :loop))
			(++ loop_depth)
			(push out (list :for name start end (sh-block (slice form 2 -1))))
			(-- loop_depth)
			(setq syms (slice syms 0 mark)))
		((eql op 'break)
			(unless (and (= n 0) (> loop_depth 0)) (sh-err "break outside a loop" form))
			(push out (list :break)))
		((eql op 'return)
			(unless (= n 1) (sh-err "wrong number of args" form))
			(defq e (sh-expr (second form)))
			(unless (eql (first e) ret_type) (sh-err "wrong return type" form))
			(push out (list :return e)))
		((sh-err "unknown statement" form))))

(defun sh-returns? (block)
	;does every path through the block end at a return ?
	(and (nempty? block)
		(case (first (defq s (last block)))
			(:return :t)
			(:if (and (sh-returns? (third s)) (sh-returns? (last s))))
			(:t :nil))))

(defun sh-literal (type val form)
	;default, min or max of an input
	(cond
		((not val) (if (eql type :float) 0.0 0))
		((eql type :int)
			(if (or (fixed? val) (not (num? val))) (sh-err "wrong input value" form))
			val)
		((and (eql type :float) (fixed? val) (not (real? val))) val)
		((sh-err "wrong input value" form))))

(defun shader-compile (forms)
	; (shader-compile forms) -> program
	(defq syms (list) funcs (list) inputs (list) consts (list) globals (list)
		const_only :nil ret_type :nil loop_depth 0)
	(each (lambda (form)
		(unless (and (list? form) (nempty? form)) (sh-err "not a declaration" form))
		(defq op (first form) n (dec (length form)) name (second form))
		(cond
			((eql op 'definput)
				(unless (<= 2 n 5) (sh-err "wrong number of args" form))
				(sh-name name form)
				(unless (find (defq type (third form)) *sh_input_types*)
					(sh-err "wrong input type" form))
				(if (and (shader-dim type) (> n 2))
					(sh-err "a vector input has no default" form))
				(bind '(& & & &optional val lo hi) form)
				(push inputs (list name type (sh-literal type val form)
					(if lo (sh-literal type lo form)) (if hi (sh-literal type hi form))))
				(push syms (list name type :input)))
			((find op '(defconst defglobal))
				(unless (= n 2) (sh-err "wrong number of args" form))
				(sh-name name form)
				(setq const_only (eql op 'defconst))
				(defq e (sh-expr (third form)))
				(setq const_only :nil)
				(push (if (eql op 'defconst) consts globals) (list name (first e) e))
				(push syms (list name (first e) (if (eql op 'defconst) :const :global))))
			((eql op 'defun)
				(unless (and (>= n 3) (list? (defq params (elem-get form 3))))
					(sh-err "wrong function header" form))
				(sh-name name form)
				(setq ret_type (sh-type (third form) form))
				(defq mark (length syms))
				(each (# (unless (and (list? %0) (= (length %0) 2))
						(sh-err "wrong parameter" form))
					(sh-name (first %0) form)
					(sh-type (second %0) form)
					(push syms (list (first %0) (second %0) :param))) params)
				(defq block (sh-block (slice form 4 -1)))
				(setq syms (slice syms 0 mark))
				(unless (sh-returns? block) (sh-err "function does not return" form))
				(push funcs (list name ret_type params block)))
			((sh-err "unknown declaration" form)))) forms)
	(unless (and (defq f (sh-func 'main)) (eql (second f) :vec4)
			(lmatch? (map (const second) (third f)) '(:vec2)))
		(sh-err "no (defun main :vec4 ((frag :vec2)) ...)" 'main))
	(list inputs consts globals funcs))

(defun shader-read (stream)
	; (shader-read stream) -> forms
	(defq forms (list) next (ascii-code " "))
	(while (/= next -1)
		(bind '(form next) (read stream next))
		(unless (and (= next -1) (not form)) (push forms form)))
	forms)

(defun shader-load (file)
	; (shader-load file) -> program
	(unless (defq stream (file-stream file)) (sh-err "no such file" file))
	(shader-compile (shader-read stream)))

;;;;;;;;;;;;;;;;;;
; the inputs block
;;;;;;;;;;;;;;;;;;

;the values of the inputs for a frame travel as one block of bytes, laid
;out by the std140 rules of a GPU uniform block. A float is 32 bits, an
;int is 32 bits, and a vector is 2, 3 or 4 floats, a vec2 on an 8 byte
;boundary, a vec3 or vec4 on a 16 byte boundary.

(defun shader-layout (program)
	; (shader-layout program) -> (size (name type offset) ...)
	(defq offset 0 fields (map (lambda ((name type &rest _))
		(defq d (ifn (shader-dim type) 1) a (elem-get '(0 4 8 16 16) d))
		(setq offset (+ (defq o (align offset a)) (* d 4)))
		(list name type o)) (first program)))
	(cat (list (align offset 16)) fields))

(defun shader-real (val)
	; (shader-real num) -> real
	;a fixed is rounded as a float literal of the language is
	(cond
		((real? val) val)
		((fixed? val) (str-to-real (sh-decimal val)))
		((n2r val))))

(defun sh-real-to-float (val)
	;the bits of a 32 bit float, from a real, a fixed or an int
	(defq val (get-long (set-real (str-alloc 8) 0 (shader-real val)) 0)
		e (- (logand (>> val 52) 0x7ff) 896)
		m (>> (+ (logand val 0xfffffffffffff) 0x10000000) 29))
	;a tie rounds to even, then the mantissa may have carried
	(if (and (= (logand val 0x1fffffff) 0x10000000) (odd? m)) (setq m (dec m)))
	(if (= m 0x800000) (setq m 0 e (inc e)))
	(cond
		((<= e 0) 0)
		((>= e 255) (logior (<< (>> val 63) 31) 0x7f800000))
		((logior (<< (>> val 63) 31) (<< e 23) m))))

(defun sh-float-to-real (val)
	;a real from the bits of a 32 bit float
	(defq e (logand (>> val 23) 0xff))
	(get-real (set-long (str-alloc 8) 0 (if (= e 0) 0
		(logior (<< (logand (>> val 31) 1) 63) (<< (+ e 896) 52)
			(<< (logand val 0x7fffff) 29)))) 0))

(defun shader-pack (program &optional vals)
	; (shader-pack program [((name val) ...)]) -> block
	;a value not given is the default of the input. A float can be a
	;real, a fixed or an int, a vector is a sequence of them.
	(bind '(size &rest fields) (shader-layout program))
	(defq block (str-alloc size))
	(each (# (set-long block %0 0)) (range 0 size 8))
	(each (lambda ((name type offset) (& & val &rest _))
		(if (defq v (some (# (if (eql (first %0) name) %0)) vals))
			(setq val (second v)))
		(cond
			((eql type :int) (set-int block offset (n2i val)))
			((eql type :float) (set-int block offset (sh-real-to-float val)))
			((seq? val)
				(unless (= (length val) (shader-dim type))
					(sh-err "wrong size of vector input" name))
				(each (# (set-int block (+ offset (* (!) 4)) (sh-real-to-float %0))) val))))
		fields (first program))
	block)

(defun shader-unpack (program block)
	; (shader-unpack program block) -> ((name val) ...)
	;a float comes back as a real, a vector as a reals
	(map (lambda ((name type offset))
		(list name (cond
			((eql type :int) (get-int block offset))
			((eql type :float) (sh-float-to-real (get-uint block offset)))
			((apply (const reals) (map (# (sh-float-to-real (get-uint block (+ offset (* %0 4)))))
				(range 0 (shader-dim type)))))))) (rest (shader-layout program))))
