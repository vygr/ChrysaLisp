;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shader language, Metal Shading Language text back end.
; The text is a fragment shader for Metal. The inputs are one
; uniform buffer, at [[buffer(0)]], laid out as the inputs block.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./shader.inc")

;MSL has no globals that can be set, and a function can not see the
;uniforms of the entry point. So the shader is a struct, the inputs,
;constants and globals are its members, and the functions its methods.

(defun msl-type (type)
	(case type
		(:vec2 "float2") (:vec3 "float3") (:vec4 "float4")
		(:t (rest (str type)))))

(defun msl-name (name)
	;a name is given a trailing _ so it can not be a word of MSL, half say
	(if (eql name 'main) "shader_main"
		(cat (apply (const cat) (map (# (if (eql %0 "-") "_" %0)) (str name))) "_")))

(defun msl-num (val)
	(if (eql (first (setq val (str val))) "-") (cat "(" val ")") val))

(defun msl-expr (node)
	(bind '(type tag a &optional b) node)
	(case tag
		(:lit (cond
			((eql type :bool) (if a "true" "false"))
			((msl-num a))))
		(:var (msl-name a))
		(:swz (cat (msl-expr b) "." (apply (const cat) (map (# (elem-get "xyzw" %0)) a))))
		(:user (cat (msl-name a) "(" (join (map msl-expr b) ", ") ")"))
		(:t (defq args (map msl-expr b))
			(cond
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
				((cat (str a) "(" (join args ", ") ")"))))))

(defun msl-block (block indent)
	(defq tab (pad "" indent "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"))
	(each (lambda ((tag &optional a b c d))
		(case tag
			(:defq (push lines (cat tab (msl-type (first b)) " " (msl-name a) " = " (msl-expr b) ";")))
			(:setq (push lines (cat tab (msl-name a) " = " (msl-expr b) ";")))
			(:setsw (push lines (cat tab (msl-name a) "."
				(apply (const cat) (map (# (elem-get "xyzw" %0)) b)) " = " (msl-expr c) ";")))
			(:if (push lines (cat tab "if (" (msl-expr a) ")") (cat tab "{"))
				(msl-block b (inc indent))
				(push lines (cat tab "}"))
				(when (nempty? c)
					(push lines (cat tab "else") (cat tab "{"))
					(msl-block c (inc indent))
					(push lines (cat tab "}"))))
			(:for (defq i (msl-name a))
				(push lines (cat tab "for (int " i " = " (msl-expr b) "; " i " < " (msl-expr c) "; " i "++)")
					(cat tab "{"))
				(msl-block d (inc indent))
				(push lines (cat tab "}")))
			(:break (push lines (cat tab "break;")))
			(:return (push lines (cat tab "return " (msl-expr a) ";"))))) block))

(defun shader-msl (program)
	; (shader-msl program) -> str
	;the entry point is fragment_main
	(bind '(inputs consts globals funcs) program)
	(defq offset 0 pads 0 lines (list "#include <metal_stdlib>" "using namespace metal;" ""
		"template<typename T, typename U> static inline T sh_mod(T x, U y) { return x - y * floor(x / y); }" ""
		"struct Inputs" "{"))
	;the inputs block, packed types and pad words give the std140 offsets
	(each (lambda ((name type at))
		(while (< offset at)
			(push lines (cat "\tfloat pad_" (str (++ pads)) ";"))
			(setq offset (+ offset 4)))
		(push lines (cat "\t" (if (shader-dim type) (cat "packed_" (msl-type type)) (msl-type type))
			" " (msl-name name) ";"))
		(setq offset (+ offset (* (ifn (shader-dim type) 1) 4)))) (rest (shader-layout program)))
	(push lines "};" "" "struct Shader" "{")
	(each (lambda ((name type &rest _))
		(push lines (cat "\t" (msl-type type) " " (msl-name name) ";"))) (cat inputs globals))
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
	(push lines "\t}" "};" ""
		"struct VertexOut" "{" "\tfloat4 position [[position]];" "\tfloat2 frag;" "};" ""
		"fragment float4 fragment_main(VertexOut in [[stage_in]], constant Inputs &inputs [[buffer(0)]])"
		"{" "\tShader s;")
	(each (lambda ((name type &rest _))
		(push lines (cat "\ts." (msl-name name) " = " (if (shader-dim type)
			(cat (msl-type type) "(inputs." (msl-name name) ")")
			(cat "inputs." (msl-name name))) ";"))) inputs)
	(push lines "\ts.shader_init();" "\treturn s.shader_main(in.frag);" "}" "")
	(join lines (ascii-char 10)))

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
