;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shader language, GLSL text back end.
; The text is a fragment shader that GLSL ES 1.00 and GLSL 1.20
; both take. Each input is a uniform of the same name.
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
	(bind '(type tag a &optional b) node)
	(case tag
		(:lit (cond
			((eql type :bool) (if a "true" "false"))
			((glsl-num a))))
		(:var (glsl-name a))
		(:swz (cat (glsl-expr b) "." (apply (const cat) (map (# (elem-get "xyzw" %0)) a))))
		(:user (cat (glsl-name a) "(" (join (map glsl-expr b) ", ") ")"))
		(:t (defq args (map glsl-expr b))
			(cond
				((defq i (find a '(+ - * / < <= > >= = /= and or)))
					(if (= (length args) 1)
						(cat "(-" (first args) ")")
						(cat "(" (join args (elem-get
							'(" + " " - " " * " " / " " < " " <= " " > " " >= " " == " " != " " && " " || ") i)) ")")))
				((eql a 'not) (cat "(!" (first args) ")"))
				((cat (str a) "(" (join args ", ") ")"))))))

(defun glsl-block (block indent)
	(defq tab (pad "" indent "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"))
	(each (lambda ((tag &optional a b c d))
		(case tag
			(:defq (push lines (cat tab (glsl-type (first b)) " " (glsl-name a) " = " (glsl-expr b) ";")))
			(:setq (push lines (cat tab (glsl-name a) " = " (glsl-expr b) ";")))
			(:setsw (push lines (cat tab (glsl-name a) "."
				(apply (const cat) (map (# (elem-get "xyzw" %0)) b)) " = " (glsl-expr c) ";")))
			(:if (push lines (cat tab "if (" (glsl-expr a) ")") (cat tab "{"))
				(glsl-block b (inc indent))
				(push lines (cat tab "}"))
				(when (nempty? c)
					(push lines (cat tab "else") (cat tab "{"))
					(glsl-block c (inc indent))
					(push lines (cat tab "}"))))
			(:for (defq i (glsl-name a))
				(push lines (cat tab "for (int " i " = " (glsl-expr b) "; " i " < " (glsl-expr c) "; " i "++)")
					(cat tab "{"))
				(glsl-block d (inc indent))
				(push lines (cat tab "}")))
			(:break (push lines (cat tab "break;")))
			(:return (push lines (cat tab "return " (glsl-expr a) ";"))))) block))

(defun shader-glsl (program)
	; (shader-glsl program) -> str
	(sh-pixel-only program "GLSL")
	(bind '(inputs consts globals funcs &rest _) (shader-full program))
	(defq lines (list "#ifdef GL_ES" "precision highp float;" "#endif" ""))
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
	;the globals are set from the inputs, then main is called
	(push lines "" "void main()" "{")
	(each (lambda ((name type e))
		(push lines (cat "\t" (glsl-name name) " = " (glsl-expr e) ";"))) globals)
	(push lines "\tgl_FragColor = shader_main(gl_FragCoord.xy);" "}" "")
	(join lines (ascii-char 10)))
