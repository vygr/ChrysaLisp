;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shader language, SPIR-V binary back end.
; The module is a fragment shader for Vulkan, as SDL's GPU interface
; wants it. The inputs are one uniform block, set 3 binding 0, laid
; out as the inputs block. No compiler is called on, the words of the
; module are made here.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./shader.inc")

;every value of a SPIR-V module has an id, and is made once. An input,
;a constant, a global, a parameter and a local are each a variable, read
;with a load and set with a store, the driver's compiler makes registers
;of them. Control flow is structured, an if has a merge block, a loop
;has a merge block and a continue block.

;the state of a module being made is bound by (spv-module-vars), and
;the functions here see it
(defmacro spv-module-vars ()
	'(defq spv_bound 0 spv_decor (list) spv_types (list) spv_code (list)
		spv_fvars (list) spv_body (list) spv_loops (list) spv_term :nil
		spv_tmap (Lmap) spv_gvars (Lmap) spv_lvars (Lmap) spv_fmap (Lmap)
		spv_glsl 1))

(defun spv-id ()
	(setq spv_bound (inc spv_bound)))

(defun spv-op (dest opcode &rest words)
	;an instruction, its first word has its length and its opcode
	(push dest (+ (<< (inc (length words)) 16) opcode))
	(each (# (push dest %0)) words)
	dest)

(defun spv-str (s)
	;the words of a string, it ends with a zero byte
	(setq s (cat s (slice (const (char 0 4)) 0 (- 4 (logand (length s) 3)))))
	(map (# (logand (code s 4 (* %0 4)) 0xffffffff)) (range 0 (/ (length s) 4))))

(defun spv-memo (key fnc)
	;the id of a type or a constant, made the first time it is asked for
	(or (. spv_tmap :find key)
		(progn (defq id (fnc)) (. spv_tmap :insert key id) id)))

(defun spv-type (type)
	(spv-memo type (lambda ()
		(defq ft (if (shader-dim type) (spv-type :float)) id (spv-id))
		(case type
			(:void (spv-op spv_types 19 id))
			(:bool (spv-op spv_types 20 id))
			(:int (spv-op spv_types 21 id 32 1))
			(:float (spv-op spv_types 22 id 32))
			(:t (spv-op spv_types 23 id ft (shader-dim type))))
		id)))

(defun spv-ptr (key type_id class)
	;a pointer type, class is 1 input, 2 uniform, 3 output, 6 private, 7 function
	(spv-memo (cat "ptr " (str key) " " (str class)) (lambda ()
		(defq id (spv-id))
		(spv-op spv_types 32 id class type_id)
		id)))

(defun spv-const (type bits)
	;a float is given as the bits of it
	(setq bits (logand bits 0xffffffff))
	(spv-memo (cat "const " (str type) " " (str bits)) (lambda ()
		(defq tid (spv-type type) id (spv-id))
		(if (eql type :bool)
			(spv-op spv_types (if (= bits 0) 42 41) tid id)
			(spv-op spv_types 43 tid id bits))
		id)))

(defun spv-null (type)
	(spv-memo (cat "null " (str type)) (lambda ()
		(defq tid (spv-type type) id (spv-id))
		(spv-op spv_types 46 tid id)
		id)))

(defun spv-ftype (type params)
	(spv-memo (cat "func " (str type) " " (join (map (const str) params) " ")) (lambda ()
		(defq tid (spv-type type) pids (map (const spv-type) params) id (spv-id))
		(apply (const spv-op) (cat (list spv_types 33 id tid) pids))
		id)))

(defun spv-global (name type)
	;a variable of the module
	(defq id (spv-id))
	(spv-op spv_types 59 (spv-ptr type (spv-type type) 6) id 6)
	(. spv_gvars :insert name (list id type))
	id)

(defun spv-local (name type)
	;a variable of the function, they all go at its start
	(defq id (spv-id))
	(spv-op spv_fvars 59 (spv-ptr type (spv-type type) 7) id 7)
	(. spv_lvars :insert name (list id type))
	id)

(defun spv-var (name)
	; (spv-var name) -> (id type)
	(or (. spv_lvars :find name) (. spv_gvars :find name)))

(defun spv-emit (type opcode &rest args)
	;an instruction that gives a value of this type, to the block
	(defq tid (spv-type type) id (spv-id))
	(apply (const spv-op) (cat (list spv_body opcode tid id) args))
	id)

(defun spv-ext (type inst &rest args)
	;an instruction of the GLSL.std.450 set
	(apply (const spv-emit) (cat (list type 12 spv_glsl inst) args)))

(defun spv-splat (id from to)
	;a float that goes with a vector is made a vector
	(if (and (shader-dim to) (not (shader-dim from)))
		(apply (const spv-emit) (cat (list to 80) (map (lambda (_) id) (range 0 (shader-dim to)))))
		id))

(defun spv-fold (type opcode ids types)
	;an op of two, over more than two
	(defq acc (spv-splat (first ids) (first types) type))
	(each (lambda (id ty)
		(setq acc (spv-emit type opcode acc (spv-splat id ty type)))) (rest ids) (rest types))
	acc)

(defun spv-expr (node)
	; (spv-expr node) -> id
	(bind '(type tag a &optional b) node)
	(case tag
		(:lit (cond
			((eql type :bool) (spv-const :bool (if a 1 0)))
			((eql type :int) (spv-const :int a))
			((spv-const :float (sh-real-to-float (str-to-real a))))))
		(:var (spv-emit type 61 (first (spv-var a))))
		(:swz (defq v (spv-expr b))
			(if (= (length a) 1)
				(spv-emit type 81 v (first a))
				(apply (const spv-emit) (cat (list type 79 v v) a))))
		(:user (apply (const spv-emit) (cat (list type 57 (. spv_fmap :find a)) (map spv-expr b))))
		(:t (defq types (map (const first) b) ids (map spv-expr b)
				ta (first types) ia (first ids) ib (second ids) ic (third ids) i :nil)
			(cond
				((defq i (find a '(+ - * /)))
					(if (= (length ids) 1)
						(spv-emit type (if (eql type :int) 126 127) ia)
						(spv-fold type (elem-get (if (eql type :int)
							'(128 130 132 135) '(129 131 133 136)) i) ids types)))
				((defq i (find a '(< <= > >=)))
					(spv-emit :bool (elem-get (if (eql ta :int)
						'(177 179 173 175) '(184 188 186 190)) i) ia ib))
				((defq i (find a '(= /=)))
					(spv-emit :bool (elem-get (case ta
						(:int '(170 171)) (:bool '(164 165)) (:t '(180 183))) i) ia ib))
				;both sides of an and and an or are worked out
				((eql a 'and) (spv-fold :bool 167 ids types))
				((eql a 'or) (spv-fold :bool 166 ids types))
				((eql a 'not) (spv-emit :bool 168 ia))
				((eql a 'float) (if (eql ta :int) (spv-emit :float 111 ia) ia))
				((eql a 'int) (if (eql ta :float) (spv-emit :int 110 ia) ia))
				((find a '(vec2 vec3 vec4))
					(defq parts (map (lambda (id ty)
						(if (eql ty :int) (spv-emit :float 111 id) id)) ids types))
					(cond
						((> (length parts) 1) (apply (const spv-emit) (cat (list type 80) parts)))
						((shader-dim ta) ia)
						((spv-splat (first parts) :float type))))
				((defq i (find a '(sin cos sqrt floor fract abs)))
					(spv-ext type (elem-get '(13 14 31 8 10 4) i) ia))
				((defq i (find a '(min max)))
					(spv-ext type (elem-get '(37 40) i) ia (spv-splat ib (second types) type)))
				((eql a 'mod) (spv-emit type 141 ia (spv-splat ib (second types) type)))
				((eql a 'pow) (spv-ext type 26 ia ib))
				((eql a 'clamp) (spv-ext type 43 ia
					(spv-splat ib (second types) type) (spv-splat ic (third types) type)))
				((eql a 'mix) (spv-ext type 46 ia ib (spv-splat ic (third types) type)))
				((eql a 'dot) (spv-emit type 148 ia ib))
				((eql a 'cross) (spv-ext type 68 ia ib))
				((eql a 'length) (spv-ext type 66 ia))
				((eql a 'normalize) (spv-ext type 69 ia))
				((eql a 'reflect) (spv-ext type 71 ia ib))
				((sh-err "no SPIR-V for" a))))))

(defun spv-block (block)
	;the statements of a block, up to the one that leaves it
	(some (lambda ((tag &optional a b c d))
		(case tag
			(:defq (defq v (spv-expr b))
				(spv-op spv_body 62 (spv-local a (first b)) v))
			(:setq (defq v (spv-expr b))
				(spv-op spv_body 62 (first (spv-var a)) v))
			(:setsw (bind '(p vt) (spv-var a))
				(defq n (shader-dim vt) old (spv-emit vt 61 p) v (spv-expr c))
				(spv-op spv_body 62 p (if (= (length b) 1)
					(spv-emit vt 82 v old (first b))
					(apply (const spv-emit) (cat (list vt 79 old v)
						(map (lambda (i) (if (defq k (find i b)) (+ n k) i)) (range 0 n)))))))
			(:if (defq ci (spv-expr a) lt (spv-id) lm (spv-id) le (if (nempty? c) (spv-id) lm))
				(spv-op spv_body 247 lm 0)
				(spv-op spv_body 250 ci lt le)
				(spv-op spv_body 248 lt)
				(spv-block b)
				(unless spv_term (spv-op spv_body 249 lm))
				(when (nempty? c)
					(spv-op spv_body 248 le)
					(setq spv_term :nil)
					(spv-block c)
					(unless spv_term (spv-op spv_body 249 lm)))
				(spv-op spv_body 248 lm)
				(setq spv_term :nil))
			(:for (defq iv (spv-local a :int) lh (spv-id) lc (spv-id) lb (spv-id)
					ln (spv-id) lm (spv-id))
				(spv-op spv_body 62 iv (spv-expr b))
				(spv-op spv_body 249 lh)
				(spv-op spv_body 248 lh)
				(spv-op spv_body 246 lm ln 0)
				(spv-op spv_body 249 lc)
				(spv-op spv_body 248 lc)
				(defq cur (spv-emit :int 61 iv) lim (spv-expr c))
				(spv-op spv_body 250 (spv-emit :bool 177 cur lim) lb lm)
				(spv-op spv_body 248 lb)
				(push spv_loops lm)
				(spv-block d)
				(pop spv_loops)
				(unless spv_term (spv-op spv_body 249 ln))
				(spv-op spv_body 248 ln)
				(spv-op spv_body 62 iv (spv-emit :int 128 (spv-emit :int 61 iv) (spv-const :int 1)))
				(spv-op spv_body 249 lh)
				(spv-op spv_body 248 lm)
				(setq spv_term :nil))
			(:break (spv-op spv_body 249 (last spv_loops))
				(setq spv_term :t))
			(:return (spv-op spv_body 254 (spv-expr a))
				(setq spv_term :t)))
		spv_term) block))

(defun spv-function (id type ptypes fnc)
	;a function, fnc is given the ids of its parameters and makes its body
	(defq pids (map (lambda (_) (spv-id)) ptypes) ftype (spv-ftype type ptypes))
	(spv-op spv_code 54 (spv-type type) id 0 ftype)
	(each (# (spv-op spv_code 55 (spv-type %0) %1)) ptypes pids)
	(spv-op spv_code 248 (spv-id))
	(setq spv_fvars (list) spv_body (list) spv_lvars (Lmap) spv_term :nil)
	(fnc pids)
	(each (# (push spv_code %0)) spv_fvars)
	(each (# (push spv_code %0)) spv_body)
	(spv-op spv_code 56))

(defun spv-block-var (types offsets set)
	;a uniform block of these members, -> (var_id ptr_type_id ...) the
	;pointer types are those of the members
	(defq tids (map (const spv-type) types) sid (spv-id) vid (spv-id))
	(apply (const spv-op) (cat (list spv_types 30 sid) tids))
	(spv-op spv_decor 71 sid 2)
	(each (# (spv-op spv_decor 72 sid (!) 35 %0)) offsets)
	(spv-op spv_types 59 (spv-ptr "block" sid 2) vid 2)
	(spv-op spv_decor 71 vid 34 set)
	(spv-op spv_decor 71 vid 33 0)
	(cat (list vid) (map (# (spv-ptr %0 %1 2)) types tids)))

(defun spv-module (model entry name interface)
	;the bytes of the module, model is 0 vertex, 4 fragment
	(defq head (list) words (list 0x07230203 0x00010000 0 (inc spv_bound) 0))
	(spv-op head 17 1)
	(apply (const spv-op) (cat (list head 11 spv_glsl) (spv-str "GLSL.std.450")))
	(spv-op head 14 0 1)
	(apply (const spv-op) (cat (list head 15 model entry) (spv-str name) interface))
	;a fragment has its origin at the upper left
	(if (= model 4) (spv-op head 16 entry 7))
	(each (lambda (part) (each (# (push words %0)) part))
		(list head spv_decor spv_types spv_code))
	(join (map (# (char %0 4)) words) ""))

(defun shader-spirv (program)
	; (shader-spirv program) -> str
	;the entry point is fragment_main
	(bind '(inputs consts globals funcs) program)
	(spv-module-vars)
	(setq spv_glsl (spv-id))
	(defq entry (spv-id) frag_in (spv-id) colour_out (spv-id)
		layout (rest (shader-layout program)) block :nil)
	(spv-op spv_types 59 (spv-ptr :vec2 (spv-type :vec2) 1) frag_in 1)
	(spv-op spv_decor 71 frag_in 30 0)
	(spv-op spv_types 59 (spv-ptr :vec4 (spv-type :vec4) 3) colour_out 3)
	(spv-op spv_decor 71 colour_out 30 0)
	(if (nempty? layout)
		(setq block (spv-block-var (map (const second) layout) (map (const third) layout) 3)))
	(each (lambda ((name type &rest _)) (spv-global name type)) (cat inputs consts globals))
	(each (lambda ((name &rest _)) (. spv_fmap :insert name (spv-id))) funcs)
	(each (lambda ((name type params block))
		(spv-function (. spv_fmap :find name) type (map (const second) params) (lambda (pids)
			(each (lambda ((pname ptype) pid)
				(spv-op spv_body 62 (spv-local pname ptype) pid)) params pids)
			(spv-block block)
			(unless spv_term (spv-op spv_body 254 (spv-null type))))))
		funcs)
	;the inputs are copied from the block, the constants and the globals
	;are set, then main is called
	(spv-function entry :void (list) (lambda (pids)
		(each (lambda ((name type &rest _))
			(defq p (spv-id))
			(spv-op spv_body 65 (elem-get block (inc (!))) p (first block) (spv-const :int (!)))
			(spv-op spv_body 62 (first (spv-var name)) (spv-emit type 61 p))) inputs)
		(each (lambda ((name type e))
			(spv-op spv_body 62 (first (spv-var name)) (spv-expr e))) (cat consts globals))
		(spv-op spv_body 62 colour_out
			(spv-emit :vec4 57 (. spv_fmap :find 'main) (spv-emit :vec2 61 frag_in)))
		(spv-op spv_body 253)))
	(spv-module 4 entry "fragment_main" (list frag_in colour_out)))

(defun shader-spirv-vertex ()
	; (shader-spirv-vertex) -> str
	;the vertex shader that goes with every fragment shader, one triangle
	;that covers the target. Its uniform, set 1 binding 0, is the size of
	;the target, and it gives each pixel its frag coord, with y up. The
	;entry point is vertex_main.
	(spv-module-vars)
	(setq spv_glsl (spv-id))
	(defq entry (spv-id) index_in (spv-id) position_out (spv-id) frag_out (spv-id)
		block (spv-block-var '(:vec2) '(0) 1))
	(spv-op spv_types 59 (spv-ptr :int (spv-type :int) 1) index_in 1)
	(spv-op spv_decor 71 index_in 11 42)
	(spv-op spv_types 59 (spv-ptr :vec4 (spv-type :vec4) 3) position_out 3)
	(spv-op spv_decor 71 position_out 11 0)
	(spv-op spv_types 59 (spv-ptr :vec2 (spv-type :vec2) 3) frag_out 3)
	(spv-op spv_decor 71 frag_out 30 0)
	(spv-function entry :void (list) (lambda (pids)
		(defq two (spv-const :int 2) f2 (spv-const :float 0x40000000)
			f1 (spv-const :float 0x3f800000) f0 (spv-const :float 0)
			vid (spv-emit :int 61 index_in)
			x (spv-emit :float 111 (spv-emit :int 199 (spv-emit :int 196 vid (spv-const :int 1)) two))
			y (spv-emit :float 111 (spv-emit :int 199 vid two))
			p (spv-id))
		(spv-op spv_body 62 position_out (spv-emit :vec4 80
			(spv-emit :float 131 (spv-emit :float 133 x f2) f1)
			(spv-emit :float 131 (spv-emit :float 133 y f2) f1) f0 f1))
		(spv-op spv_body 65 (second block) p (first block) (spv-const :int 0))
		(spv-op spv_body 62 frag_out (spv-emit :vec2 133
			(spv-emit :vec2 80 x y) (spv-emit :vec2 61 p)))
		(spv-op spv_body 253)))
	(spv-module 0 entry "vertex_main" (list index_in position_out frag_out)))
