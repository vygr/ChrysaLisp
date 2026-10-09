;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shader language, SPIR-V binary back end.
; The module is a fragment shader for Vulkan, as SDL's GPU interface
; wants it. The inputs are one uniform block, set 3 binding 0, laid
; out as the inputs block. No compiler is called on, the words of the
; module are made here. A vertex shader and a pixel shader that go
; together are a vertex module and a fragment module, (shader-spirv-pair).
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
	;the id of a type, made the first time it is asked for. A vector is of
	;floats, and a matrix of vectors, its columns, and those are made here
	;if they are not there, this does not call itself for them. The ids
	;are given out in the order they always were, the float, then this
	;type, then the vector a matrix is of
	(or (. spv_tmap :find type)
		(progn
			(defq dim (shader-dim type)
				ft (if dim (or (. spv_tmap :find :float)
					(progn (defq fid (spv-id))
						(spv-op spv_types 22 fid 32)
						(. spv_tmap :insert :float fid)
						fid)))
				id (spv-id))
			(case type
				(:void (spv-op spv_types 19 id))
				(:bool (spv-op spv_types 20 id))
				(:int (spv-op spv_types 21 id 32 1))
				(:float (spv-op spv_types 22 id 32))
				;a matrix is its columns, 4 of vec4, and for a vec3, 3 of vec3
				((:mat4 :mat3)
					(defq vt (if (eql type :mat4) :vec4 :vec3)
						col (or (. spv_tmap :find vt)
							(progn (defq vid (spv-id))
								(spv-op spv_types 23 vid ft (shader-dim vt))
								(. spv_tmap :insert vt vid)
								vid)))
					(spv-op spv_types 24 id col (if (eql type :mat4) 4 3)))
				(:t (spv-op spv_types 23 id ft dim)))
			(. spv_tmap :insert type id)
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
		(apply (const spv-emit) (cat (list to 80) (map (lambda (&) id) (range 0 (shader-dim to)))))
		id))

(defun spv-fold (type opcode ids types)
	;an op of two, over more than two
	(defq acc (spv-splat (first ids) (first types) type))
	(each! (lambda (id ty)
		(setq acc (spv-emit type opcode acc (spv-splat id ty type)))) (list ids types) 1)
	acc)

(defun spv-expr (node)
	; (spv-expr node) -> id
	(sh-fold node (lambda (node args)
		(bind '(type tag a &optional b) node)
		(case tag
			(:lit (cond
				((eql type :bool) (spv-const :bool (if a 1 0)))
				((eql type :int) (spv-const :int a))
				((spv-const :float (sh-real-to-float (str-to-real a))))))
			(:var (spv-emit type 61 (first (spv-var a))))
			(:swz (defq v (first args))
				(if (= (length a) 1)
					(spv-emit type 81 v (first a))
					(apply (const spv-emit) (cat (list type 79 v v) a))))
			(:user (apply (const spv-emit) (cat (list type 57 (. spv_fmap :find a)) args)))
			(:t (defq types (map (const first) b) ids args
					ta (first types) ia (first ids) ib (second ids) ic (third ids) i :nil)
				(cond
					((find :mat4 types)
						;matrices, made the one, then times the vector, if it ends
						;with one. A vec3 is by the 3 by 3 of the matrix, its first
						;three columns, cut to three
						(defq m (reduce (# (spv-emit :mat4 146 %0 %1))
							(if (eql type :mat4) ids (slice ids 0 -2))))
						(cond
							((eql type :mat4) m)
							((eql type :vec4) (spv-emit :vec4 145 m (last ids)))
							(:t (spv-emit :vec3 145
								(apply (const spv-emit) (cat (list :mat3 80) (map (lambda (col)
									(defq c (spv-emit :vec4 81 m col))
									(spv-emit :vec3 79 c c 0 1 2)) '(0 1 2))))
								(last ids)))))
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
					((sh-err "no SPIR-V for" a))))))))

(defun spv-block (block)
	;the statements of a block, up to the one that leaves it. An if and a
	;for have blocks in them, and no function here calls itself: what is
	;still to be done is a list, the next thing last, a statement, or what
	;comes after a block that was put there before it. A statement that
	;leaves, a break or a return, has the rest of its own block taken off
	(defq stack (map (# (list :stmt %0)) (reverse block)))
	(while (defq item (pop stack))
		(bind '(kind p0 &optional p1 p2 p3) item)
		(case kind
			(:else ;the first block of an if is done, p0 its merge, p1 its else, p2 that block
				(unless spv_term (spv-op spv_body 249 p0))
				(cond
					((nempty? p2)
						(spv-op spv_body 248 p1)
						(setq spv_term :nil)
						(push stack (list :endif p0))
						(each (# (push stack (list :stmt %0))) (reverse p2)))
					(:t (spv-op spv_body 248 p0)
						(setq spv_term :nil))))
			(:endif (unless spv_term (spv-op spv_body 249 p0))
				(spv-op spv_body 248 p0)
				(setq spv_term :nil))
			(:next ;the block of a for is done, p0 its counter, p1 p2 p3 its labels
				(pop spv_loops)
				(unless spv_term (spv-op spv_body 249 p2))
				(spv-op spv_body 248 p2)
				(spv-op spv_body 62 p0 (spv-emit :int 128 (spv-emit :int 61 p0) (spv-const :int 1)))
				(spv-op spv_body 249 p1)
				(spv-op spv_body 248 p3)
				(setq spv_term :nil))
			(:t (bind '(tag &optional a b c d) p0)
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
						(push stack (list :else lm le c))
						(each (# (push stack (list :stmt %0))) (reverse b)))
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
						(push stack (list :next iv lh ln lm))
						(each (# (push stack (list :stmt %0))) (reverse d)))
					(:break (spv-op spv_body 249 (last spv_loops))
						(setq spv_term :t))
					(:return (spv-op spv_body 254 (spv-expr a))
						(setq spv_term :t)))
				;it left, the rest of its block is not done
				(when spv_term
					(while (and (nempty? stack) (eql (first (last stack)) :stmt)) (pop stack))))))
	spv_term)

(defun spv-function (id type ptypes fnc)
	;a function, fnc is given the ids of its parameters and makes its body
	(defq pids (map (lambda (&) (spv-id)) ptypes) ftype (spv-ftype type ptypes))
	(spv-op spv_code 54 (spv-type type) id 0 ftype)
	(each (# (spv-op spv_code 55 (spv-type %0) %1)) ptypes pids)
	(spv-op spv_code 248 (spv-id))
	(setq spv_fvars (list) spv_body (list) spv_lvars (Lmap) spv_term :nil)
	(fnc pids)
	(each (# (push spv_code %0)) spv_fvars)
	(each (# (push spv_code %0)) spv_body)
	(spv-op spv_code 56))

(defun spv-block-var (types offsets set &optional binding)
	;a uniform block of these members, -> (var_id ptr_type_id ...) the
	;pointer types are those of the members. A matrix is a column at a
	;time, each 16 bytes on from the last
	(defq tids (map (const spv-type) types) sid (spv-id) vid (spv-id))
	(apply (const spv-op) (cat (list spv_types 30 sid) tids))
	(spv-op spv_decor 71 sid 2)
	(each (# (spv-op spv_decor 72 sid (!) 35 %0)
		(when (eql %1 :mat4)
			(spv-op spv_decor 72 sid (!) 5)
			(spv-op spv_decor 72 sid (!) 7 16))) offsets types)
	(spv-op spv_types 59 (spv-ptr (cat "block " (str sid)) sid 2) vid 2)
	(spv-op spv_decor 71 vid 34 set)
	(spv-op spv_decor 71 vid 33 (ifn binding 0))
	(cat (list vid) (map (# (spv-ptr %0 %1 2)) types tids)))

(defun spv-program (program)
	; (spv-program program) -> block
	;the inputs block of a program, a variable for each of its inputs,
	;constants and globals, and its functions
	(bind '(inputs consts globals funcs &rest _) (shader-full program))
	(defq layout (rest (shader-layout program)) block :nil)
	(if (nempty? layout)
		(setq block (spv-block-var (map (const second) layout) (map (const third) layout)
			(if (eql (shader-stage program) :vertex) 1 3))))
	(each (lambda ((name type &rest _)) (spv-global name type)) (cat inputs consts globals))
	(each (lambda ((name &rest _)) (. spv_fmap :insert name (spv-id))) funcs)
	(each (lambda ((name type params block))
		(spv-function (. spv_fmap :find name) type (map (const second) params) (lambda (pids)
			(each (lambda ((pname ptype) pid)
				(spv-op spv_body 62 (spv-local pname ptype) pid)) params pids)
			(spv-block block)
			(unless spv_term (spv-op spv_body 254 (spv-null type))))))
		funcs)
	block)

(defun spv-start (program block)
	;at the start of the entry point the inputs are copied from the block,
	;and the constants and the globals are set
	(bind '(inputs consts globals &rest _) program)
	(each (lambda ((name type &rest _))
		(defq p (spv-id))
		(spv-op spv_body 65 (elem-get block (inc (!))) p (first block) (spv-const :int (!)))
		(spv-op spv_body 62 (first (spv-var name)) (spv-emit type 61 p))) inputs)
	(each (lambda ((name type e))
		(spv-op spv_body 62 (first (spv-var name)) (spv-expr e))) (cat consts globals)))

(defun spv-port (name type class decor value)
	;a variable that comes in to the module, class 1, or goes out, class
	;3, at a location, decor 30, or as a built in, decor 11. A shader's
	;name for it, if it has one, reads and sets it as it is
	(defq id (spv-id))
	(spv-op spv_types 59 (spv-ptr type (spv-type type) class) id class)
	(spv-op spv_decor 71 id decor value)
	(if name (. spv_gvars :insert name (list id type)))
	id)

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
	(sh-pixel-only program "SPIR-V")
	(spv-module-vars)
	(setq spv_glsl (spv-id))
	(defq entry (spv-id)
		frag_in (spv-port :nil :vec2 1 30 0) colour_out (spv-port :nil :vec4 3 30 0)
		block (spv-program program))
	;the inputs are copied from the block, the constants and the globals
	;are set, then main is called
	(spv-function entry :void (list) (lambda (pids)
		(spv-start program block)
		(spv-op spv_body 62 colour_out
			(spv-emit :vec4 57 (. spv_fmap :find 'main) (spv-emit :vec2 61 frag_in)))
		(spv-op spv_body 253)))
	(spv-module 4 entry "fragment_main" (list frag_in colour_out)))

(defun shader-spirv-pair (vertex pixel)
	; (shader-spirv-pair vertex pixel) -> (vertex_module fragment_module)
	;a vertex shader and a pixel shader that go together, to draw
	;triangles with. The entry points are vertex_main and fragment_main.
	;
	;The vertex module takes the attrs of a vertex at locations, in the
	;order the shader has them, and its inputs as a block, set 1 binding
	;0. It hands on each of its varyings, at a location. Where it puts a
	;vertex has z of -1 to 1 in view, a GPU of this kind has 0 to 1, so z
	;is moved as it leaves.
	;
	;The fragment module takes the varyings the pixel shader reads, at
	;the place each has in the vertex shader's list, its inputs as a
	;block, set 3 binding 0, and the size of the target, set 3 binding 1,
	;from which, and where the pixel is, comes the frag coord, y up. A
	;pixel whose alpha is under 1 in 255 is not drawn. The color leaves
	;with its alpha multiplied in, for a target that is blended that way.
	(shader-pair vertex pixel)
	(defq varyings (shader-varyings vertex))
	;the vertex module
	(spv-module-vars)
	(setq spv_glsl (spv-id))
	(defq entry (spv-id) ports (list)
		position_out (spv-port :nil :vec4 3 11 0))
	(each (lambda ((name type)) (push ports (spv-port name type 1 30 (!)))) (shader-attrs vertex))
	(each (lambda ((name type)) (push ports (spv-port name type 3 30 (!)))) varyings)
	(defq block (spv-program vertex))
	(spv-function entry :void (list) (lambda (pids)
		(spv-start vertex block)
		;a varying that is not set is 0
		(each (lambda ((name type)) (spv-op spv_body 62 (first (spv-var name)) (spv-null type))) varyings)
		(defq p (spv-emit :vec4 57 (. spv_fmap :find 'main))
			z (spv-emit :float 81 p 2) w (spv-emit :float 81 p 3)
			half (spv-const :float 0x3f000000))
		(spv-op spv_body 62 position_out (spv-emit :vec4 80
			(spv-emit :float 81 p 0) (spv-emit :float 81 p 1)
			(spv-emit :float 133 (spv-emit :float 129 z w) half) w))
		(spv-op spv_body 253)))
	(defq vertex_module (spv-module 0 entry "vertex_main" (cat (list position_out) ports)))
	;the fragment module
	(spv-module-vars)
	(setq spv_glsl (spv-id))
	(defq entry (spv-id) ports (list)
		coord_in (spv-port :nil :vec4 1 11 15) colour_out (spv-port :nil :vec4 3 30 0))
	(each (lambda ((name type))
		(push ports (spv-port name type 1 30 (some (# (if (eql (first %0) name) (!))) varyings))))
		(shader-varyings pixel))
	(defq block (spv-program pixel) target (spv-block-var '(:vec2) '(0) 3 1))
	(spv-function entry :void (list) (lambda (pids)
		(spv-start pixel block)
		(defq coord (spv-emit :vec4 61 coord_in) size (spv-id))
		(spv-op spv_body 65 (second target) size (first target) (spv-const :int 0))
		(defq c (spv-emit :vec4 57 (. spv_fmap :find 'main) (spv-emit :vec2 80
			(spv-emit :float 81 coord 0)
			(spv-emit :float 131 (spv-emit :float 81 (spv-emit :vec2 61 size) 1) (spv-emit :float 81 coord 1)))))
		;each part kept to 0 to 1. A pixel whose alpha is under 1 in 255
		;is not drawn, and the color leaves with its alpha multiplied in
		(defq f0 (spv-const :float 0) f1 (spv-const :float 0x3f800000)
			parts (map (# (spv-ext :float 43 (spv-emit :float 81 c %0) f0 f1)) '(0 1 2 3))
			alpha (elem-get parts 3) lk (spv-id) lm (spv-id)
			thin (spv-emit :bool 184 alpha (spv-const :float 0x3b808081)))
		(spv-op spv_body 247 lm 0)
		(spv-op spv_body 250 thin lk lm)
		(spv-op spv_body 248 lk)
		(spv-op spv_body 252)
		(spv-op spv_body 248 lm)
		(spv-op spv_body 62 colour_out (spv-emit :vec4 80
			(spv-emit :float 133 (first parts) alpha) (spv-emit :float 133 (second parts) alpha)
			(spv-emit :float 133 (third parts) alpha) alpha))
		(spv-op spv_body 253)))
	(list vertex_module (spv-module 4 entry "fragment_main" (cat (list coord_in colour_out) ports))))

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
