(report-header "GPU: shader language, a texture, the color of a picture at a place")

(import "gui/lisp.inc")
(import "lib/gpu/glsl.inc")
(import "lib/gpu/msl.inc")
(import "lib/gpu/spirv.inc")
(import "lib/gpu/cpu.inc")
(import "lib/gpu/vp.inc")

(defq tx_lf "\n")

(defun tx-src (&rest lines)
	;a program from the lines of its source
	(shader-compile (shader-read (string-stream (join lines tx_lf)))))

;a picture of 8 by 4, no two pixels the same, every one of them solid
(defun tx-color (i)
	(+ 0xff000000 (* i 0x060504) (<< (logand i 3) 8)))

(defq tx_canvas (Canvas 8 4 1))
(each (lambda (i) (.-> tx_canvas (:set_color (tx-color i)) (:plot (% i 8) (/ i 8)))) (range 0 32))
(defq tx_pixmap (getf tx_canvas +canvas_pixmap 0)
	tx_prog (tx-src "(definput resolution :vec2)" "(definput turn :float 1.5)" "(deftexture pic)"
		"(defun main :vec4 ((frag :vec2))"
		"	(defq uv (/ frag resolution))"
		"	(texture pic (- (* uv turn) (vec2 0.25 0.0))))")
	tx_vals (list (list 'resolution '(16.0 8.0)) (list 'pic tx_pixmap)))

;the language
(assert-list-eq "a pixel shader says what textures it has, in order" '((pic) (more))
	(shader-textures (tx-src "(deftexture pic)" "(deftexture more)"
		"(defun main :vec4 ((frag :vec2)) (+ (texture pic frag) (texture more frag)))")))
(assert-list-eq "and one with none has none" '()
	(shader-textures (tx-src "(defun main :vec4 ((frag :vec2)) (vec4 1.0))")))
(assert-list-eq "they are known from the head of a file alone" '((pic))
	(shader-textures (shader-compile (shader-read (string-stream
		"(deftexture pic) (defun main :vec4 ((frag :vec2)) (texture pic frag))")) :t)))
(assert-error "a texture is read at a vec2" (tx-src "(deftexture pic)" "(defun main :vec4 ((frag :vec2)) (texture pic 1.0))"))
(assert-error "what is read is a texture" (tx-src "(defun main :vec4 ((frag :vec2)) (texture frag frag))"))
(assert-error "a texture is no value for a local" (tx-src "(deftexture pic)" "(defun main :vec4 ((frag :vec2)) (defq a pic) (vec4 1.0))"))
(assert-error "nor for sums" (tx-src "(deftexture pic)" "(defun main :vec4 ((frag :vec2)) (vec4 (+ pic 1.0)))"))
(assert-error "nor to hand to a function" (tx-src "(deftexture pic)" "(defun f :float ((a :float)) a)"
	"(defun main :vec4 ((frag :vec2)) (vec4 (f pic)))"))
(assert-error "its name is its own" (tx-src "(deftexture pic)" "(definput pic :float)" "(defun main :vec4 ((frag :vec2)) (vec4 1.0))"))
(assert-error "texture is a word of the language" (tx-src "(defun main :vec4 ((frag :vec2)) (defq texture 1.0) (vec4 1.0))"))
(assert-error "a vertex shader has none" (tx-src "(deftexture pic)" "(defun main :vec4 () (vec4 1.0))"))
(assert-error "nor do functions for Lisp" (tx-src "(deftexture pic)" "(defun f :float ((a :float)) a)"))

;the Lisp back end, the reference, and the native code, the same to the bit
(defq tx_ref (apply (eval (shader-cpu tx_prog)) (cat (list 0 0 16 8) (shader-cpu-args tx_prog tx_vals)))
	tx_native (shader-vp tx_prog)
	tx_got (shader-vp-pixels tx_native (shader-vp-frame tx_prog tx_native tx_vals) 0 0 16 8))
(assert-eq "a tile of 16 by 8 is shaded" 128 (length tx_ref))
(assert-true "every pixel of it is one the picture has"
	(every (lambda (p) (some (lambda (i)
			(defq c (tx-color i))
			(eql (str p) (str (reals (/ (n2r (logand (>> c 16) 0xff)) (n2r 255)) (/ (n2r (logand (>> c 8) 0xff)) (n2r 255))
				(/ (n2r (logand c 0xff)) (n2r 255)) (n2r 1))))) (range 0 32)))
		(list (first tx_ref) (elem-get tx_ref 37) (last tx_ref))))
(assert-true "it goes round again outside 0 to 1, more than 8 of the 32 pixels are met, and more than once"
	(< 8 (length (unique (map (const str) tx_ref))) 128))
(assert-eq "native code gives the same pixels as the reference, to the bit"
	(str tx_ref) (str tx_got))
(assert-eq "a texture that is not given is no picture, and is nothing, in the reference"
	"(0x0 0x0 0x0 0x0)" (str (first (apply (eval (shader-cpu tx_prog))
		(cat (list 0 0 1 1) (shader-cpu-args tx_prog (list (list 'resolution '(16.0 8.0)))))))))
(assert-eq "and in native code"
	"(0x0 0x0 0x0 0x0)" (str (first (shader-vp-pixels tx_native
		(shader-vp-frame tx_prog tx_native (list (list 'resolution '(16.0 8.0)))) 0 0 1 1))))

;triangles: a quad with the picture on it, by the pair as native code
;and by the reference
(defq tx_vert (tx-src "(defattr position :vec3)" "(defattr coord :vec2)" "(defvarying uv :vec2)"
		"(defun main :vec4 () (setq uv coord) (vec4 position 1.0))")
	tx_pix (tx-src "(deftexture pic)" "(defvarying uv :vec2)"
		"(defun main :vec4 ((frag :vec2)) (texture pic uv))")
	tx_verts (apply (const reals) (map (const n2r) '(
		-0.75 -0.75 0.0 0.0 1.0   0.75 -0.75 0.0 1.0 1.0   0.75 0.75 0.0 1.0 0.0
		-0.75 -0.75 0.0 0.0 1.0   0.75 0.75 0.0 1.0 0.0   -0.75 0.75 0.0 0.0 0.0)))
	tx_tris (nums 0 1 2 3 4 5) tx_soft (Canvas 32 32 1))
(shader-vp-draw-tris (shader-vp-pipeline tx_vert tx_pix) tx_verts tx_tris (getf tx_soft +canvas_pixmap 0)
	(shader-vp-depth 32 32) :nil (list (list 'pic tx_pixmap)) :t)
(defq tx_want (third (shader-texture (getf tx_soft +canvas_pixmap 0)))
	tx_ref (shader-cpu-tris tx_vert tx_pix tx_verts tx_tris 32 32 :nil (list (list 'pic tx_pixmap)) :t))
(assert-eq "a quad of 24 by 24 is drawn, of a frame of 32" 576
	(length (filter (# (/= (get-uint tx_want %0) 0)) (range 0 (length tx_want) 4))))
(assert-true "the top left of the quad is the top left of the picture, and the bottom right the bottom right"
	(and (= (get-uint tx_want (* 4 (+ 4 (* 4 32)))) 0xff000000)
		(= (get-uint tx_want (* 4 (+ 27 (* 27 32)))) (tx-color 31))))
(assert-true "native triangles give the same pixels as the reference" (eql tx_want tx_ref))

;the text of the other back ends
(defq tx_glsl (shader-glsl tx_prog) tx_msl (shader-msl tx_prog)
	tx_pair (shader-msl-pair tx_vert tx_pix))
(assert-true "GLSL: a sampler, and a read of it"
	(and (found? tx_glsl "uniform sampler2D pic;") (found? tx_glsl "texture2D(pic, ")))
(assert-true "MSL: a texture and a sampler are members, and it is sampled"
	(and (found? tx_msl "texture2d<float> pic_;") (found? tx_msl "sampler pic_s;") (found? tx_msl "pic_.sample(pic_s, ")))
(assert-true "MSL: they come to the fragment function at number 0, and are handed to the shader"
	(and (found? (second tx_pair) "texture2d<float> tex_0 [[texture(0)]], sampler smp_0 [[sampler(0)]]")
		(found? (second tx_pair) "s.pic_ = tex_0;") (found? (second tx_pair) "s.pic_s = smp_0;")))
(assert-true "MSL: the vertex function has none" (not (found? (first tx_pair) "texture2d")))

;SPIR-V, the words of the module
(defun tx-insts (module)
	;the instructions, each (opcode operand ...)
	(defq words (map (# (logand (code module 4 (* %0 4)) 0xffffffff)) (range 0 (/ (length module) 4)))
		insts (list) i 5)
	(while (< i (length words))
		(defq len (max 1 (>> (elem-get words i) 16)))
		(push insts (cat (list (logand (elem-get words i) 0xffff)) (slice words (inc i) (min (length words) (+ i len)))))
		(setq i (+ i len)))
	insts)

(defun tx-has (insts op)
	;the instructions of that opcode
	(filter (# (= (first %0) op)) insts))

(defq tx_spv (tx-insts (second (shader-spirv-pair tx_vert tx_pix)))
	tx_image (first (tx-has tx_spv 25)) tx_sampled (first (tx-has tx_spv 27)))
(assert-true "SPIR-V: a 2D image of floats, that is sampled" (and tx_image (eql (str (slice tx_image 3 -1)) "(1 0 0 0 1 0)")))
(assert-true "SPIR-V: and the type of it read with a place" (and tx_sampled (= (third tx_sampled) (second tx_image))))
(defq tx_var (some (# (and (= (length %0) 4) (= (elem-get %0 3) 0) (third %0))) (tx-has tx_spv 59)))
(assert-true "SPIR-V: a variable of it, a constant of the module" tx_var)
(assert-true "SPIR-V: at set 2, binding 0, where SDL's GPU interface looks"
	(and (some (# (eql (str (rest %0)) (str (list tx_var 34 2)))) (tx-has tx_spv 71))
		(some (# (eql (str (rest %0)) (str (list tx_var 33 0)))) (tx-has tx_spv 71))))
(assert-eq "SPIR-V: it is read the once, at a place" 1 (length (tx-has tx_spv 87)))
(assert-true "SPIR-V: the vertex module has none" (empty? (tx-has (tx-insts (first (shader-spirv-pair tx_vert tx_pix))) 25)))
