;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shader language, s-expression source to a checked, typed tree.
; The tree is what the back ends take, see glsl.inc and cpu.inc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././crypto/sha256.inc")
(import "././collections/collections.inc")

;a program is (inputs consts globals funcs stage key text). key is a hash
;of the source, and text the source, of a program from a file that has
;only had its head read, see (shader-load) and (shader-full)
;	input, (name type default min max)
;	const and global, (name type expr)
;	func, (name type ((name type) ...) block)
;	stage, (kind attrs varyings), kind is :pixel, :vertex or :func, an
;	attr and a varying are (name type)
;
;a file with no main is not a shader, it is functions, kind :func. Each
;can be made into a native function that Lisp calls, with numbers,
;vectors and matrices as its args and its value, (shader-vp-func). It is
;not Lisp compiled. It is a way in to what the machine has that the
;language can not reach by itself, for work that is all numbers. Such a
;file has no inputs, attrs or varyings, a function of it can take a
;matrix and give one, and one that does is for Lisp to call, not another
;function.
;
;there are two kinds of shader, each a file of its own. A pixel shader
;gives the color of a pixel, its main takes the frag coord. A vertex
;shader gives where a vertex is, its main takes nothing, it reads the
;attrs of the vertex, and what it sets its varyings to is what a pixel
;shader that has varyings of those names reads, spread over the triangle.
;Any vertex shader goes with any pixel shader whose varyings it has.
;an expr is (type tag ...)
;	(type :lit value), a float value is a str of the decimal number
;	(type :var name kind), kind is :input :const :global :param :local :loop
;		:attr :varying
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
(defq *sh_types* '(:float :int :bool :vec2 :vec3 :vec4 :mat4)
	*sh_input_types* '(:float :int :vec2 :vec3 :vec4 :mat4)
	*sh_vertex_types* '(:float :vec2 :vec3 :vec4)
	*sh_arith* '(+ - * /)
	*sh_order* '(< <= > >=)
	*sh_equal* '(= /=)
	*sh_logic* '(and or)
	*sh_ctors* '(vec2 vec3 vec4)
	*sh_gen1* '(sin cos sqrt floor fract abs)
	*sh_gen2* '(min max mod)
	*sh_others* '(not float int pow clamp mix dot cross length normalize reflect)
	*sh_words* '(definput defconst defglobal defattr defvarying defun
		defq setq if when progn for break return))

(defun sh-err (msg form)
	(throw (cat "Shader, " msg " !") form))

(defun shader-dim (type)
	; (shader-dim type) -> :nil | 2 | 3 | 4
	(case type (:vec2 2) (:vec3 3) (:vec4 4) (:t :nil)))

(defun shader-size (type)
	; (shader-size type) -> 1 | 2 | 3 | 4 | 16
	;how many numbers a value of the type is
	(cond ((eql type :mat4) 16) ((shader-dim type)) (1)))

(defun shader-stage (program)
	; (shader-stage program) -> :pixel | :vertex | :func
	(first (elem-get program 4)))

(defun shader-funcs (program)
	; (shader-funcs program) -> ((name type ((name type) ...)) ...)
	;the functions of a program, what each gives and takes
	(map (# (slice %0 0 3)) (elem-get program 3)))

(defun shader-func (program name)
	; (shader-func program name) -> (name type ((name type) ...))
	;a function of a program that Lisp can call
	(unless (eql (shader-stage program) :func) (sh-err "not functions for Lisp, it has a main" name))
	(unless (defq f (some (# (if (eql (first %0) name) %0)) (shader-funcs program)))
		(sh-err "no such function" name))
	f)

(defun shader-attrs (program)
	; (shader-attrs program) -> ((name type) ...)
	;what a vertex shader reads of each vertex
	(second (elem-get program 4)))

(defun shader-varyings (program)
	; (shader-varyings program) -> ((name type) ...)
	;what a vertex shader sets for the pixel shader, or a pixel shader reads
	(third (elem-get program 4)))

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
		((find :mat4 types)
			;a matrix times a matrix is a matrix, times a vec4 is a vec4,
			;the vector on the right. Times a vec3 is a vec3, by the 3 by
			;3 of the matrix, so a direction is turned and not moved.
			;That is all a matrix does
			(and (eql op '*) (> n 1)
				(some! (# (eql %0 :mat4)) (list types) :t 0 -2)
				(if (find (last types) '(:mat4 :vec4 :vec3)) (last types))))
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
	(defq d (shader-dim type) idxs (map! (# (cond
		((defq i (find %0 "xyzw")) i)
		((defq i (find %0 "rgba")) i)
		((sh-err "unknown component" form)))) (list name) 1))
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
	;an expression, checked and typed. One can have others in it to any
	;depth, and no function here calls itself: what is still to be done is
	;a list, the next thing last, an expression or the making of one whose
	;args are done, and what has been made so far is another
	(defq stack (list (list :expr form)) vs (list))
	(while (defq item (pop stack))
		(bind '(kind form &optional op f) item)
		(case kind
			(:swz (defq arg (pop vs) idxs (sh-swizzle op (first arg) form))
				(push vs (list (sh-vec (length idxs)) :swz idxs arg)))
			(:user (defq n (dec (length form)) args (slice vs (- (length vs) n) -1))
				(times n (pop vs))
				(unless (and (= (length args) (length (third f)))
						(every (# (eql (first %0) (second %1))) args (third f)))
					(sh-err "wrong args to function" form))
				;a matrix is never in registers, so is not handed on
				(if (or (eql (second f) :mat4) (some (# (eql (second %0) :mat4)) (third f)))
					(sh-err "a function with a matrix is for Lisp to call" form))
				(push vs (list (second f) :user op args)))
			(:op (defq n (dec (length form)) args (slice vs (- (length vs) n) -1))
				(times n (pop vs))
				(unless (defq type (sh-op-type op (map (const first) args)))
					(sh-err "wrong types" form))
				(push vs (list type :op op args)))
			(:t (cond
				((real? form) (sh-err "real number literal" form))
				((fixed? form) (push vs (list :float :lit (sh-decimal form))))
				((num? form) (push vs (list :int :lit form)))
				((eql form :t) (push vs (list :bool :lit :t)))
				((eql form :nil) (push vs (list :bool :lit :nil)))
				((and (str? form) (not (sym? form))) (push vs (list :float :lit (sh-decimal form))))
				((sym? form)
					(unless (defq e (sh-find form)) (sh-err "unknown name" form))
					(if (and const_only (not (eql (third e) :const)))
						(sh-err "not a constant" form))
					(push vs (list (second e) :var form (third e))))
				((not (and (list? form) (nempty? form) (sym? (defq op (first form)))))
					(sh-err "not an expression" form))
				((eql (first op) ":")
					(unless (= (length form) 2) (sh-err "wrong number of args" form))
					(push stack (list :swz form op) (list :expr (second form))))
				((defq f (sh-func op))
					(if const_only (sh-err "not a constant" form))
					;the args are done first, in order, the first is put on last
					(push stack (list :user form op f))
					(each! (# (push stack (list :expr %0))) (list form) -1 1))
				((sh-op? op)
					(push stack (list :op form op))
					(each! (# (push stack (list :expr %0))) (list form) -1 1))
				((sh-err "unknown function" form))))))
	(first vs))

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

(defun sh-stmt (form out frames)
	;a statement, checked and typed, onto out. One that has blocks in it
	;does not do them, a function that calls itself has no stack to spare.
	;It leaves each on frames for (sh-block) to do next, with what is to
	;be made of it when it is done
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
				(unless (find (third v) '(:local :param :varying))
					(sh-err "can only set a local, a parameter or a varying" form))
				(if (eql (third v) :varying) (setq sets_varying :t))
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
			(push frames (if (eql op 'if)
				(list (list (third form)) 0 (list) (length syms)
					(list :then e out (if (= n 3) (list (last form)))))
				(list (slice form 2 -1) 0 (list) (length syms) (list :then e out :nil)))))
		((eql op 'progn)
			(push frames (list (rest form) 0 (list) (length syms) (list :progn out))))
		((eql op 'for)
			(unless (and (>= n 1) (list? (defq head (second form)))
					(= (length head) 3))
				(sh-err "wrong loop header" form))
			(defq name (first head) mark (length syms)
				start (sh-const-int (second head)) end (sh-const-int (third head)))
			(sh-name name form)
			(push syms (list name :int :loop))
			(++ loop_depth)
			(push frames (list (slice form 2 -1) 0 (list) (length syms)
				(list :for out name start end mark))))
		((eql op 'break)
			(unless (and (= n 0) (> loop_depth 0)) (sh-err "break outside a loop" form))
			(push out (list :break)))
		((eql op 'return)
			(unless (= n 1) (sh-err "wrong number of args" form))
			(defq e (sh-expr (second form)))
			(unless (eql (first e) ret_type) (sh-err "wrong return type" form))
			(push out (list :return e)))
		((sh-err "unknown statement" form))))

(defun sh-block (forms)
	;statements in a scope of their own. A block can have blocks in it, and
	;they are done here one at a time, with a list as the stack of those
	;that are part done, as (flatten) does it. A frame is a block: its
	;forms, how far through them it is, its statements so far, how long
	;syms was when it began, and what is to be made of it when it is done
	(defq result :nil frames (list (list forms 0 (list) (length syms) (list :top))))
	(while (nempty? frames)
		(bind '(forms i out mark made) (defq frame (last frames)))
		(cond
			((< i (length forms))
				;its next statement, which may leave a block on frames
				(elem-set frame 1 (inc i))
				(sh-stmt (elem-get forms i) out frames))
			(:t ;the block is done, its names go, and it is put where it belongs
				(pop frames)
				(setq syms (slice syms 0 mark))
				(case (first made)
					(:top (setq result out))
					(:then (bind '(& e into other) made)
						;the one arm of an if. The other is done next, if it has one
						(if other
							(push frames (list other 0 (list) (length syms) (list :else e into out)))
							(push into (list :if e out (list)))))
					(:else (bind '(& e into arm) made)
						(push into (list :if e arm out)))
					(:progn (each (# (push (second made) %0)) out))
					(:for (bind '(& into name start end was) made)
						(-- loop_depth)
						(setq syms (slice syms 0 was))
						(push into (list :for name start end out)))))))
	result)

(defun sh-tail (form)
	;the last form of a function is its value, as in Lisp, so if it is not
	;a statement it is returned. And so for the last form of each arm of an
	;if that is last, and of a progn that is last. The back ends still see
	;a return, they are statement languages.
	;An arm can be an if again. Each form that is to be looked at is a
	;place, a list and where in it, on a list of those still to do
	(defq top (list form) todo (list (list top 0)))
	(while (defq spot (pop todo))
		(bind '(held idx) spot)
		(defq form (elem-get held idx))
		(cond
			((not (and (list? form) (nempty? form)))
				(elem-set held idx (list 'return form)))
			((eql (first form) 'if)
				(when (<= 3 (length form) 4)
					(elem-set held idx (defq arms (cat form)))
					(each! (lambda (&) (push todo (list arms (!)))) (list arms) 2)))
			((eql (first form) 'progn)
				(when (> (length form) 1)
					(elem-set held idx (defq arms (cat form)))
					(push todo (list arms (dec (length arms))))))
			((find (first form) '(defq setq when for break return)))
			((elem-set held idx (list 'return form)))))
	(first top))

(defun sh-returns? (block)
	;does every path through the block end at a return ? The arms of an
	;if that is last are blocks to ask it of in turn, on a list
	(defq stack (list block) all :t)
	(while (and all (defq block (pop stack)))
		(cond
			((empty? block) (setq all :nil))
			(:t (case (first (defq s (last block)))
				(:return)
				(:if (push stack (third s) (last s)))
				(:t (setq all :nil))))))
	all)

(defun sh-fold (node fnc)
	; (sh-fold node fnc) -> value
	;what a back end makes of an expression, from the bottom up. fnc is
	;(lambda (node args) ...) -> value, called for each node with what was
	;made of those under it, in order. A back end's own function for an
	;expression then calls nothing that calls it, the walk is here, with a
	;list as the stack of what is still to do, as (flatten) does it
	(defq stack (list (list node :nil)) vs (list))
	(while (defq item (pop stack))
		(bind '(node done) item)
		(defq kids (case (second node) (:swz (list (last node))) ((:user :op) (last node)) (:t '())))
		(cond
			((or done (empty? kids))
				(defq n (length kids) args (slice vs (- (length vs) n) -1))
				(times n (pop vs))
				(push vs (fnc node args)))
			(:t (push stack (list node :t))
				(each! (# (push stack (list %0 :nil))) (list kids) -1 0))))
	(first vs))

(defun sh-literal (type val form)
	;default, min or max of an input
	(cond
		((not val) (if (eql type :float) 0.0 0))
		((eql type :int)
			(if (or (fixed? val) (not (num? val))) (sh-err "wrong input value" form))
			val)
		((and (eql type :float) (fixed? val) (not (real? val))) val)
		((sh-err "wrong input value" form))))

(defun shader-key (text)
	; (shader-key text) -> str
	;what a source is known by, a hash of it. The native code made from a
	;source is kept under its key, so a source that has been met before
	;is found again without a line of it being read
	(apply (const cat) (map! (# (byte-to-hex-str (code %0))) (list (sha256 text)) 0 16)))

(defun shader-make (files)
	; (shader-make files) -> str
	;which make of a back end this is, a hash of the files it is written
	;in, 8 hex digits. It is part of the name of all that a back end
	;keeps, so that a change to the back end is a change of name, and
	;what the old one made is not found. Worked out once by a task.
	(memoize (str files)
		(slice (shader-key (apply (const cat) (map (# (ifn (load %0) "")) files))) 0 8) 11))

(defun shader-kept-text (name make)
	; (shader-kept-text name lambda) -> str
	;what a back end makes of a program, text or a module, kept in a
	;file of that name beside the native code, and made only if it is
	;not there
	(defq path (cat (load-path) "lib/gpu/jit/" name))
	(or (load path)
		(progn (defq text (make)) (save text path) text)))

(defun shader-compile (forms &optional head_only)
	; (shader-compile forms [head_only]) -> program
	;with head_only only what a caller of a ready made function needs is
	;taken, the inputs, the attrs and varyings, the kind, and what each
	;function gives and takes. No body is read or checked, and the consts
	;and globals are :lazy. (shader-full) reads the rest when it is wanted.
	(defq syms (list) funcs (list) inputs (list) consts (list) globals (list)
		attrs (list) varyings (list) sets_varying :nil
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
				(if (and (> (shader-size type) 1) (> n 2))
					(sh-err "a vector or matrix input has no default" form))
				(bind '(& & & &optional val lo hi) form)
				(push inputs (list name type (sh-literal type val form)
					(if lo (sh-literal type lo form)) (if hi (sh-literal type hi form))))
				(push syms (list name type :input)))
			((find op '(defattr defvarying))
				(unless (= n 2) (sh-err "wrong number of args" form))
				(sh-name name form)
				(unless (find (defq type (third form)) *sh_vertex_types*)
					(sh-err "wrong type, a float or a vector" form))
				(push (if (eql op 'defattr) attrs varyings) (list name type))
				(push syms (list name type (if (eql op 'defattr) :attr :varying))))
			((and head_only (find op '(defconst defglobal))))
			((and head_only (eql op 'defun))
				(unless (and (>= n 3) (list? (elem-get form 3))) (sh-err "wrong function header" form))
				(push funcs (list name (third form) (elem-get form 3))))
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
				(defq body (slice form 4 -1))
				(if (nempty? body)
					(setq body (cat (slice body 0 -2) (list (sh-tail (last body))))))
				(defq block (sh-block body))
				(setq syms (slice syms 0 mark))
				(unless (sh-returns? block) (sh-err "function does not return" form))
				(push funcs (list name ret_type params block)))
			((sh-err "unknown declaration" form)))) forms)
	;main says which kind it is, a pixel shader's takes the frag coord, a
	;vertex shader's takes nothing. With no main it is functions for Lisp
	;to call
	(defq f (sh-func 'main))
	(cond
		((not f)
			(if (empty? funcs) (sh-err "no functions" 'main))
			(if (or (nempty? inputs) (nempty? attrs) (nempty? varyings))
				(sh-err "functions for Lisp have no inputs, attrs or varyings, they have args" 'main)))
		(:t (unless (and (eql (second f) :vec4)
					(or (empty? (third f)) (lmatch? (map (const second) (third f)) '(:vec2))))
				(sh-err "no (defun main :vec4 ((frag :vec2)) ...), or (defun main :vec4 () ...) for a vertex shader" 'main))
			;a matrix is an input, a global or a local of a shader, it
			;is not handed to a function or back from one
			(each (lambda ((name type params &rest _))
				(if (eql type :mat4) (sh-err "a function can not give a matrix" name))
				(if (some (# (eql (second %0) :mat4)) params)
					(sh-err "a function can not take a matrix" name))) funcs)))
	(defq kind (cond ((not f) :func) ((empty? (third f)) :vertex) (:pixel)))
	(when (eql kind :pixel)
		(if (nempty? attrs) (sh-err "a pixel shader has no attrs" (first (first attrs))))
		(if sets_varying (sh-err "a pixel shader can not set a varying" 'main)))
	(if head_only
		(list inputs :lazy :lazy funcs (list kind attrs varyings) :nil :nil)
		(list inputs consts globals funcs (list kind attrs varyings) (shader-key (str forms)) :nil)))

(defun shader-read (stream)
	; (shader-read stream) -> forms
	(defq forms (list) next (ascii-code " "))
	(while (/= next -1)
		(bind '(form next) (read stream next))
		(unless (and (= next -1) (not form)) (push forms form)))
	forms)

(defun shader-full (program)
	; (shader-full program) -> program
	;a program with all of it there. One that was loaded with only its
	;head has the rest read and checked now, into the same program
	(when (eql (second program) :lazy)
		(defq full (shader-compile (shader-read (string-stream (elem-get program 6)))))
		(each (# (elem-set program %0 (elem-get full %0))) '(0 1 2 3 4)))
	program)

(defun sh-pixel-only (program back_end)
	;for a back end that has not got the vertex stage yet. It has only
	;pixel shaders with no varyings and no matrix in them
	(if (eql (shader-stage program) :func)
		(sh-err (cat "the " back_end " back end has no functions for Lisp yet") 'main))
	(when (or (eql (shader-stage program) :vertex) (nempty? (shader-varyings program))
			(some (# (eql (second %0) :mat4)) (first program)))
		(sh-err (cat "the " back_end " back end has no vertex stage, varyings or matrix yet") 'main)))

(defun shader-pair (vertex pixel)
	; (shader-pair vertex pixel) -> (vertex pixel)
	;a vertex shader and a pixel shader that go together. Every varying
	;the pixel shader reads must be one the vertex shader has, of the same
	;type. The vertex shader may have more, they are not used.
	(unless (eql (shader-stage vertex) :vertex) (sh-err "not a vertex shader" 'main))
	(unless (eql (shader-stage pixel) :pixel) (sh-err "not a pixel shader" 'main))
	(each (lambda ((name type))
		(unless (some (# (and (eql (first %0) name) (eql (second %0) type)))
				(shader-varyings vertex))
			(sh-err "the vertex shader has no such varying" name)))
		(shader-varyings pixel))
	(list vertex pixel))

(defun shader-load (file)
	; (shader-load file) -> program
	;a program from a file. Only its head is read, and a hash of the file
	;taken, which is what its native code is kept under. The rest is read
	;and checked when a back end has to make something from it,
	;(shader-full), so a file whose native code is there already costs a
	;hash and a few lines
	(unless (defq text (load file)) (sh-err "no such file" file))
	(defq program (shader-compile (shader-read (string-stream text)) :t))
	(elem-set program 5 (shader-key text))
	(elem-set program 6 text)
	program)

;;;;;;;;;;;;;;;;;;
; the inputs block
;;;;;;;;;;;;;;;;;;

;the values of the inputs for a frame travel as one block of bytes, laid
;out by the std140 rules of a GPU uniform block. A float is 32 bits, an
;int is 32 bits, and a vector is 2, 3 or 4 floats, a vec2 on an 8 byte
;boundary, a vec3 or vec4 on a 16 byte boundary. A mat4 is its 4 columns,
;each a vec4. A matrix is given, and comes back, as ChrysaLisp has one, 16
;numbers a row at a time, as lib/math/matrix.inc makes them.

(defun shader-layout (program)
	; (shader-layout program) -> (size (name type offset) ...)
	(defq offset 0 fields (map (lambda ((name type &rest _))
		(defq d (shader-size type) a (if (= d 16) 16 (elem-get '(0 4 8 16 16) d)))
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
			((not (seq? val)))
			((/= (length val) (shader-size type))
				(sh-err "wrong size of vector input" name))
			((eql type :mat4)
				;a row at a time to a column at a time
				(each (# (set-int block (+ offset (* (+ (* (% (!) 4) 4) (/ (!) 4)) 4))
					(sh-real-to-float %0))) val))
			((each (# (set-int block (+ offset (* (!) 4)) (sh-real-to-float %0))) val))))
		fields (first program))
	block)

(defun shader-unpack (program block)
	; (shader-unpack program block) -> ((name val) ...)
	;a float comes back as a real, a vector as a reals
	(map! (lambda ((name type offset))
		(list name (cond
			((eql type :int) (get-int block offset))
			((eql type :float) (sh-float-to-real (get-uint block offset)))
			((eql type :mat4)
				(apply (const reals) (map (# (sh-float-to-real (get-uint block
					(+ offset (* (+ (* (% %0 4) 4) (/ %0 4)) 4))))) (range 0 16))))
			((apply (const reals) (map (# (sh-float-to-real (get-uint block (+ offset (* %0 4)))))
				(range 0 (shader-dim type)))))))) (list (shader-layout program)) 1))
