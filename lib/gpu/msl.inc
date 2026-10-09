;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shader language, Metal Shading Language text back end.
; The text is a fragment shader for Metal. The inputs are one
; uniform buffer, at [[buffer(0)]], laid out as the inputs block.
; A vertex shader and a pixel shader that go together are a vertex
; function and a fragment function, (shader-msl-pair).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./shader.inc")

;MSL has no globals that can be set, and a function can not see the
;uniforms of the entry point. So the shader is a struct, the inputs,
;constants and globals are its members, and the functions its methods.

(defun msl-type (type)
	(case type
		(:vec2 "float2") (:vec3 "float3") (:vec4 "float4") (:mat4 "float4x4")
		(:t (rest (str type)))))

(defun msl-name (name)
	;a name is given a trailing _ so it can not be a word of MSL, half say
	(if (eql name 'main) "shader_main"
		(cat (apply (const cat) (map (# (if (eql %0 "-") "_" %0)) (str name))) "_")))

(defun msl-num (val)
	(if (eql (first (setq val (str val))) "-") (cat "(" val ")") val))

(defun msl-expr (node)
	(sh-fold node (lambda (node args)
		(bind '(type tag a &optional b) node)
		(case tag
			(:lit (cond
				((eql type :bool) (if a "true" "false"))
				((msl-num a))))
			(:var (msl-name a))
			(:swz (cat (first args) "." (apply (const cat) (map (# (elem-get "xyzw" %0)) a))))
			(:user (cat (msl-name a) "(" (join args ", ") ")"))
			(:t (cond
				((and (eql a '*) (eql type :vec3) (eql (first (first b)) :mat4))
					;matrices, then a vec3, which is turned by the 3 by 3
					(cat "sh_mul3(" (join (slice args 0 -2) " * ") ", " (last args) ")"))
				((defq i (find a '(+ - * / < <= > >= = /= and or)))
					(if (= (length args) 1)
						(cat "(-" (first args) ")")
						(cat "(" (join args (elem-get
							'(" + " " - " " * " " / " " < " " <= " " > " " >= " " == " " != " " && " " || ") i)) ")")))
				((eql a 'not) (cat "(!" (first args) ")"))
				((find a '(vec2 vec3 vec4)) (cat (msl-type type) "(" (join args ", ") ")"))
				((find a '(min max mod clamp mix))
					;a float that goes with a vector is made a vector
					(cat (if (eql a 'mod) "sh_mod" (str a)) "(" (join (map (# (if (and (shader-dim type)
						(eql (first %1) :float)) (cat (msl-type type) "(" %0 ")") %0)) args b) ", ") ")"))
				((cat (str a) "(" (join args ", ") ")"))))))))

(defun msl-block (block indent)
	;the lines of a block. An if and a for have blocks in them: what is
	;still to come is a list, the next thing last, a statement and how far
	;in it is, or a line that closes what was opened before it
	(defq stack (map (# (list :stmt %0 indent)) (reverse block)))
	(while (defq item (pop stack))
		(bind '(kind p0 &optional indent) item)
		(cond
			((eql kind :line) (push lines p0))
			(:t (defq tab (pad "" indent "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"))
				(bind '(tag &optional a b c d) p0)
				(case tag
					(:defq (push lines (cat tab (msl-type (first b)) " " (msl-name a) " = " (msl-expr b) ";")))
					(:setq (push lines (cat tab (msl-name a) " = " (msl-expr b) ";")))
					(:setsw (push lines (cat tab (msl-name a) "."
						(apply (const cat) (map (# (elem-get "xyzw" %0)) b)) " = " (msl-expr c) ";")))
					(:if (push lines (cat tab "if (" (msl-expr a) ")") (cat tab "{"))
						(when (nempty? c)
							(push stack (list :line (cat tab "}")))
							(each (# (push stack (list :stmt %0 (inc indent)))) (reverse c))
							(push stack (list :line (cat tab "{")) (list :line (cat tab "else"))))
						(push stack (list :line (cat tab "}")))
						(each (# (push stack (list :stmt %0 (inc indent)))) (reverse b)))
					(:for (defq i (msl-name a))
						(push lines (cat tab "for (int " i " = " (msl-expr b) "; " i " < " (msl-expr c) "; " i "++)")
							(cat tab "{"))
						(push stack (list :line (cat tab "}")))
						(each (# (push stack (list :stmt %0 (inc indent)))) (reverse d)))
					(:break (push lines (cat tab "break;")))
					(:return (push lines (cat tab "return " (msl-expr a) ";"))))))))

(defun msl-inputs (program lines)
	;the inputs block, packed types and pad words give the std140 offsets.
	;A matrix is on a 16 byte boundary as it is, and is its columns
	(defq offset 0 pads 0)
	(push lines "struct Inputs" "{")
;the layout but for its first, the size of the block
	(each! (lambda ((name type at))
		(while (< offset at)
			(push lines (cat "\tfloat pad_" (str (++ pads)) ";"))
			(setq offset (+ offset 4)))
		(push lines (cat "\t" (if (shader-dim type) (cat "packed_" (msl-type type)) (msl-type type))
			" " (msl-name name) ";"))
		(setq offset (+ offset (* (shader-size type) 4)))) (list (shader-layout program)) 1)
	;a block with nothing in it is still a block
	(if (empty? (first program)) (push lines "\tfloat pad_0;"))
	(push lines "};" ""))

(defun msl-struct (program lines)
	;the shader as a struct. The inputs, the globals, the attrs and the
	;varyings are members, the constants are members with a value, and the
	;functions are methods
	(bind '(inputs consts globals funcs &rest _) (shader-full program))
	(push lines "struct Shader" "{")
	(each (lambda ((name type &rest _))
		(push lines (cat "\t" (msl-type type) " " (msl-name name) ";")))
		(cat inputs globals (shader-attrs program) (shader-varyings program)))
	(each (lambda ((name type e))
		(push lines (cat "\t" (msl-type type) " " (msl-name name) " = " (msl-expr e) ";"))) consts)
	(each (lambda ((name type params block))
		(push lines "" (cat "\t" (msl-type type) " " (msl-name name) "("
			(join (map (# (cat (msl-type (second %0)) " " (msl-name (first %0)))) params) ", ") ")") "\t{")
		(msl-block block 2)
		(push lines "\t}")) funcs)
	;the globals are set from the inputs, then main is called
	(push lines "" "\tvoid shader_init()" "\t{")
	(each (lambda ((name type e))
		(push lines (cat "\t\t" (msl-name name) " = " (msl-expr e) ";"))) globals)
	(push lines "\t}" "};" ""))

(defun msl-set-inputs (program lines)
	;the members that are inputs, from the block
	(each (lambda ((name type &rest _))
		(push lines (cat "\ts." (msl-name name) " = " (if (shader-dim type)
			(cat (msl-type type) "(inputs." (msl-name name) ")")
			(cat "inputs." (msl-name name))) ";"))) (first program)))

(defq *msl_head* '("#include <metal_stdlib>" "using namespace metal;" ""
	"template<typename T, typename U> static inline T sh_mod(T x, U y) { return x - y * floor(x / y); }" "")
	;a pair has matrices, a matrix times a vec3 is by its 3 by 3
	*msl_pair_head* (cat *msl_head* '(
	"static inline float3 sh_mul3(float4x4 m, float3 v) { return float3x3(m[0].xyz, m[1].xyz, m[2].xyz) * v; }" "")))

(defun shader-msl (program)
	; (shader-msl program) -> str
	;the entry point is fragment_main
	(sh-pixel-only program "MSL")
	(defq lines (cat *msl_head*))
	(msl-inputs program lines)
	(msl-struct program lines)
	(push lines
		"struct VertexOut" "{" "\tfloat4 position [[position]];" "\tfloat2 frag;" "};" ""
		"fragment float4 fragment_main(VertexOut in [[stage_in]], constant Inputs &inputs [[buffer(0)]])"
		"{" "\tShader s;")
	(msl-set-inputs program lines)
	(push lines "\ts.shader_init();" "\treturn s.shader_main(in.frag);" "}" "")
	(join lines (ascii-char 10)))

(defun shader-msl-pair (vertex pixel)
	; (shader-msl-pair vertex pixel) -> (vertex_text fragment_text)
	;a vertex shader and a pixel shader that go together, to draw
	;triangles with. The entry points are vertex_main and fragment_main.
	;
	;The vertex function takes the attrs of a vertex as [[attribute(n)]],
	;in the order the shader has them, and its inputs at [[buffer(0)]].
	;What it hands on is each of its varyings, [[user(locn n)]]. Where it
	;puts a vertex has z of -1 to 1 in view, a GPU of this kind has 0 to
	;1, so z is moved as it leaves.
	;
	;The fragment function takes the varyings the pixel shader reads, by
	;the place each has in the vertex shader's list, its inputs at
	;[[buffer(0)]], and the size of the target at [[buffer(1)]], from
	;which, and where the pixel is, comes the frag coord, y up. A pixel
	;whose alpha is under 1 in 255 is not drawn. The color leaves with its
	;alpha multiplied in, for a target that is blended that way.
	(shader-pair vertex pixel)
	(defq lines (cat *msl_pair_head*) varyings (shader-varyings vertex))
	(msl-inputs vertex lines)
	(msl-struct vertex lines)
	(push lines "struct VertexIn" "{")
	(each (lambda ((name type))
		(push lines (cat "\t" (msl-type type) " " (msl-name name) " [[attribute(" (str (!)) ")]];")))
		(shader-attrs vertex))
	(push lines "};" "" "struct VertexOut" "{" "\tfloat4 position [[position]];")
	(each (lambda ((name type))
		(push lines (cat "\t" (msl-type type) " " (msl-name name) " [[user(locn" (str (!)) ")]];")))
		varyings)
	(push lines "};" ""
		"vertex VertexOut vertex_main(VertexIn in [[stage_in]], constant Inputs &inputs [[buffer(0)]])"
		"{" "\tShader s;")
	(msl-set-inputs vertex lines)
	(each (lambda ((name type))
		(push lines (cat "\ts." (msl-name name) " = in." (msl-name name) ";"))) (shader-attrs vertex))
	(each (lambda ((name type))
		(push lines (cat "\ts." (msl-name name) " = " (msl-type type) "(0.0);"))) varyings)
	(push lines "\ts.shader_init();" "\tfloat4 p = s.shader_main();" "\tVertexOut out;"
		"\tout.position = float4(p.x, p.y, (p.z + p.w) * 0.5, p.w);")
	(each (lambda ((name type))
		(push lines (cat "\tout." (msl-name name) " = s." (msl-name name) ";"))) varyings)
	(push lines "\treturn out;" "}" "")
	(defq vertex_text (join lines (ascii-char 10)) lines (cat *msl_pair_head*))
	(msl-inputs pixel lines)
	(msl-struct pixel lines)
	(push lines "struct FragmentIn" "{" "\tfloat4 position [[position]];")
	(each (lambda ((name type))
		(push lines (cat "\t" (msl-type type) " " (msl-name name) " [[user(locn"
			(str (some (# (if (eql (first %0) name) (!))) varyings)) ")]];")))
		(shader-varyings pixel))
	(push lines "};" "" "struct Target" "{" "\tpacked_float2 size;" "};" ""
		(cat "fragment float4 fragment_main(FragmentIn in [[stage_in]], constant Inputs &inputs [[buffer(0)]],"
			" constant Target &target [[buffer(1)]])")
		"{" "\tShader s;")
	(msl-set-inputs pixel lines)
	(each (lambda ((name type))
		(push lines (cat "\ts." (msl-name name) " = in." (msl-name name) ";"))) (shader-varyings pixel))
	(push lines "\ts.shader_init();"
		"\tfloat4 c = clamp(s.shader_main(float2(in.position.x, float2(target.size).y - in.position.y)), 0.0, 1.0);"
		"\tif (c.w < 0.003921568627) discard_fragment();"
		"\treturn float4(c.xyz * c.w, c.w);" "}" "")
	(list vertex_text (join lines (ascii-char 10))))

(defun shader-msl-vertex ()
	; (shader-msl-vertex) -> str
	;the vertex shader that goes with every fragment shader, one triangle
	;that covers the target. Its uniform is the size of the target, and it
	;gives each pixel its frag coord, with y up. The entry point is vertex_main.
	(join '("#include <metal_stdlib>" "using namespace metal;" ""
		"struct VertexOut" "{" "\tfloat4 position [[position]];" "\tfloat2 frag;" "};" ""
		"struct Target" "{" "\tpacked_float2 size;" "};" ""
		"vertex VertexOut vertex_main(uint vid [[vertex_id]], constant Target &target [[buffer(0)]])"
		"{"
		"\tfloat2 p = float2(float((vid << 1) & 2), float(vid & 2));"
		"\tVertexOut out;"
		"\tout.position = float4(p * 2.0 - 1.0, 0.0, 1.0);"
		"\tout.frag = p * float2(target.size);"
		"\treturn out;"
		"}" "") (ascii-char 10)))
