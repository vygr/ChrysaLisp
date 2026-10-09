;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shader language, GLSL text back end.
; The text is a fragment shader that GLSL ES 1.00 and GLSL 1.20
; both take. Each input is a uniform of the same name.
; A vertex shader and a pixel shader that go together are a vertex
; shader and a fragment shader of the same two, (shader-glsl-pair).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./shader.inc")

(defun glsl-type (type)
	(rest (str type)))

(defun glsl-name (name)
	(if (eql name 'main) "shader_main"
		(apply (const cat) (map (# (if (eql %0 "-") "_" %0)) (str name)))))

(defun glsl-num (val)
	(if (eql (first (setq val (str val))) "-") (cat "(" val ")") val))

(defun glsl-expr (node)
	(sh-fold node (lambda (node args)
		(bind '(type tag a &optional b) node)
		(case tag
			(:lit (cond
				((eql type :bool) (if a "true" "false"))
				((glsl-num a))))
			(:var (glsl-name a))
			(:swz (cat (first args) "." (apply (const cat) (map (# (elem-get "xyzw" %0)) a))))
			(:user (cat (glsl-name a) "(" (join args ", ") ")"))
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
				((cat (str a) "(" (join args ", ") ")"))))))))

(defun glsl-block (block indent)
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
					(:defq (push lines (cat tab (glsl-type (first b)) " " (glsl-name a) " = " (glsl-expr b) ";")))
					(:setq (push lines (cat tab (glsl-name a) " = " (glsl-expr b) ";")))
					(:setsw (push lines (cat tab (glsl-name a) "."
						(apply (const cat) (map (# (elem-get "xyzw" %0)) b)) " = " (glsl-expr c) ";")))
					(:if (push lines (cat tab "if (" (glsl-expr a) ")") (cat tab "{"))
						(when (nempty? c)
							(push stack (list :line (cat tab "}")))
							(each (# (push stack (list :stmt %0 (inc indent)))) (reverse c))
							(push stack (list :line (cat tab "{")) (list :line (cat tab "else"))))
						(push stack (list :line (cat tab "}")))
						(each (# (push stack (list :stmt %0 (inc indent)))) (reverse b)))
					(:for (defq i (glsl-name a))
						(push lines (cat tab "for (int " i " = " (glsl-expr b) "; " i " < " (glsl-expr c) "; " i "++)")
							(cat tab "{"))
						(push stack (list :line (cat tab "}")))
						(each (# (push stack (list :stmt %0 (inc indent)))) (reverse d)))
					(:break (push lines (cat tab "break;")))
					(:return (push lines (cat tab "return " (glsl-expr a) ";"))))))))

(defun glsl-decls (program)
	;the inputs as uniforms, the constants, the globals and the functions
	;of a program, onto the lines of whoever calls
	(bind '(inputs consts globals funcs &rest _) (shader-full program))
	(each (lambda ((name type &rest _))
		(push lines (cat "uniform " (glsl-type type) " " (glsl-name name) ";"))) inputs)
	(push lines "")
	(each (lambda ((name type e))
		(push lines (cat "const " (glsl-type type) " " (glsl-name name) " = " (glsl-expr e) ";"))) consts)
	(push lines "")
	(each (lambda ((name type e))
		(push lines (cat (glsl-type type) " " (glsl-name name) ";"))) globals)
	(each (lambda ((name type params block))
		(push lines "" (cat (glsl-type type) " " (glsl-name name) "("
			(join (map (# (cat (glsl-type (second %0)) " " (glsl-name (first %0)))) params) ", ") ")") "{")
		(glsl-block block 1)
		(push lines "}")) funcs)
	globals)

(defun shader-glsl (program)
	; (shader-glsl program) -> str
	(sh-pixel-only program "GLSL")
	(defq lines (list "#ifdef GL_ES" "precision highp float;" "#endif" "")
		globals (glsl-decls program))
	;the globals are set from the inputs, then main is called
	(push lines "" "void main()" "{")
	(each (lambda ((name type e))
		(push lines (cat "\t" (glsl-name name) " = " (glsl-expr e) ";"))) globals)
	(push lines "\tgl_FragColor = shader_main(gl_FragCoord.xy);" "}" "")
	(join lines (ascii-char 10)))

(defq *glsl_pair_head* '("#ifdef GL_ES" "precision highp float;" "#endif" ""
	"vec3 sh_mul3(mat4 m, vec3 v) { return mat3(m[0].xyz, m[1].xyz, m[2].xyz) * v; }" ""))

(defun shader-glsl-pair (vertex pixel)
	; (shader-glsl-pair vertex pixel) -> (vertex_text fragment_text)
	;a vertex shader and a pixel shader that go together, to draw
	;triangles with, as a vertex shader and a fragment shader of GLSL.
	;
	;The vertex shader has each attr of a vertex as an attribute, and each
	;of its varyings as a varying, of the same names, and its inputs as
	;uniforms. A varying it does not set is 0. Where it puts a vertex is
	;gl_Position as it is, z of -1 to 1 in view is what GL has.
	;
	;The fragment shader has the varyings the pixel shader reads, by name,
	;and its inputs as uniforms. The frag coord is GL's own, y up. A pixel
	;whose alpha is under 1 in 255 is not drawn. The color leaves with its
	;alpha multiplied in, for a target that is blended that way.
	;
	;An input the two both have, of the one name, is the one uniform, as
	;GL links them, and both are given the same precision so that it can.
	(shader-pair vertex pixel)
	(defq lines (cat *glsl_pair_head*) varyings (shader-varyings vertex))
	(each (lambda ((name type))
		(push lines (cat "attribute " (glsl-type type) " " (glsl-name name) ";"))) (shader-attrs vertex))
	(each (lambda ((name type))
		(push lines (cat "varying " (glsl-type type) " " (glsl-name name) ";"))) varyings)
	(push lines "")
	(defq globals (glsl-decls vertex))
	(push lines "" "void main()" "{")
	(each (lambda ((name type))
		(push lines (cat "\t" (glsl-name name) " = " (glsl-type type) "(0.0);"))) varyings)
	(each (lambda ((name type e))
		(push lines (cat "\t" (glsl-name name) " = " (glsl-expr e) ";"))) globals)
	(push lines "\tgl_Position = shader_main();" "}" "")
	(defq vertex_text (join lines (ascii-char 10)) lines (cat *glsl_pair_head*))
	(each (lambda ((name type))
		(push lines (cat "varying " (glsl-type type) " " (glsl-name name) ";"))) (shader-varyings pixel))
	(push lines "")
	(setq globals (glsl-decls pixel))
	(push lines "" "void main()" "{")
	(each (lambda ((name type e))
		(push lines (cat "\t" (glsl-name name) " = " (glsl-expr e) ";"))) globals)
	(push lines "\tvec4 c = clamp(shader_main(gl_FragCoord.xy), 0.0, 1.0);"
		"\tif (c.a < 0.003921568627) discard;"
		"\tgl_FragColor = vec4(c.rgb * c.a, c.a);" "}" "")
	(list vertex_text (join lines (ascii-char 10))))
