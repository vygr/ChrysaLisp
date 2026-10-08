(import "lib/options/options.inc")
(import "lib/gpu/shader.inc")
(import "lib/gpu/glsl.inc")
(import "lib/gpu/msl.inc")
(import "lib/gpu/spirv.inc")
(import "lib/gpu/cpu.inc")
(import "lib/gpu/vp.inc")

(defq usage `(
(("-h" "--help")
"Usage: shader [options] file

    options:
        -h --help: this help info.
        -t --target name: what to show, default glsl.
            glsl    GLSL text, for OpenGL and WebGL.
            msl     Metal Shading Language text, for Apple.
            spirv   a SPIR-V module, for Vulkan, as a listing.
            vp      VP assembler source, the native code back end.
                    Of a pixel shader, a vertex shader, or each
                    function of a file of functions for Lisp.
            cpu     the Lisp the CPU back end runs, of any of them.
            tree    the checked, typed tree the back ends are given.
        -v --vertex: the vertex shader that goes with every
            fragment shader, for msl and spirv.
        -p --pair file: the other shader of a pair, a vertex shader
            and a pixel shader that go together, in either order, for
            glsl, msl and spirv. Both halves are shown. With -o they
            are written to the file's name with .vert and .frag on.
        -o --out file: write it to a file. A spirv module is
            then written as the binary a driver, or spirv-dis,
            takes, not as a listing.

    Compiles a shader in the shader language, and shows what
    it is compiled to, or writes it to a file for use outside
    ChrysaLisp. See docs/ai_digest/shader_language.md.

    shader lib/gpu/shaders/raymarch.shader
    shader -t vp lib/gpu/shaders/mesh_vertex.shader
    shader -t msl -o raymarch.metal lib/gpu/shaders/raymarch.shader
    shader -t spirv -o raymarch.spv lib/gpu/shaders/raymarch.shader
    shader -p lib/gpu/shaders/mesh_lit.shader lib/gpu/shaders/mesh_vertex.shader")
(("-t" "--target") ,(opt-str 'opt_t))
(("-v" "--vertex") ,(opt-flag 'opt_v))
(("-p" "--pair") ,(opt-str 'opt_p))
(("-o" "--out") ,(opt-str 'opt_o))
))

;the names of the SPIR-V instructions the back end makes
(defq *spv_names* (scatter (Fmap 31)
	11 "OpExtInstImport" 12 "OpExtInst" 14 "OpMemoryModel" 15 "OpEntryPoint"
	16 "OpExecutionMode" 17 "OpCapability" 19 "OpTypeVoid" 20 "OpTypeBool"
	21 "OpTypeInt" 22 "OpTypeFloat" 23 "OpTypeVector" 30 "OpTypeStruct"
	32 "OpTypePointer" 33 "OpTypeFunction" 41 "OpConstantTrue"
	42 "OpConstantFalse" 43 "OpConstant" 46 "OpConstantNull" 54 "OpFunction"
	55 "OpFunctionParameter" 56 "OpFunctionEnd" 57 "OpFunctionCall"
	59 "OpVariable" 61 "OpLoad" 62 "OpStore" 65 "OpAccessChain" 71 "OpDecorate"
	72 "OpMemberDecorate" 79 "OpVectorShuffle" 80 "OpCompositeConstruct"
	81 "OpCompositeExtract" 82 "OpCompositeInsert" 110 "OpConvertFToS"
	111 "OpConvertSToF" 126 "OpSNegate" 127 "OpFNegate" 128 "OpIAdd"
	129 "OpFAdd" 130 "OpISub" 131 "OpFSub" 132 "OpIMul" 133 "OpFMul"
	135 "OpSDiv" 136 "OpFDiv" 141 "OpFMod" 148 "OpDot" 164 "OpLogicalEqual"
	165 "OpLogicalNotEqual" 166 "OpLogicalOr" 167 "OpLogicalAnd"
	168 "OpLogicalNot" 170 "OpIEqual" 171 "OpINotEqual" 173 "OpSGreaterThan"
	175 "OpSGreaterThanEqual" 177 "OpSLessThan" 179 "OpSLessThanEqual"
	180 "OpFOrdEqual" 183 "OpFUnordNotEqual" 184 "OpFOrdLessThan"
	186 "OpFOrdGreaterThan" 188 "OpFOrdLessThanEqual"
	190 "OpFOrdGreaterThanEqual" 196 "OpShiftLeftLogical" 199 "OpBitwiseAnd"
	246 "OpLoopMerge" 247 "OpSelectionMerge" 248 "OpLabel" 249 "OpBranch"
	250 "OpBranchConditional" 253 "OpReturn" 254 "OpReturnValue"))

(defun spirv-operands (op args)
	;the words of an instruction as text. The two instructions the back
	;end makes that hold a name have it shown as the name.
	(defq at (case op (11 1) (15 2) (:t :nil)))
	(cond
		(at (defq text (apply (const cat) (map! (# (char %0 4)) (list args) at))
				end (find (ascii-char 0) text)
				used (inc (/ end 4)))
			;the words before the name, the name, the words after, into the
			;one list
			(map! (const str) (list args) (+ at used) -1
				(push (map! (const str) (list args) 0 at)
					(cat (ascii-char 34) (slice text 0 end) (ascii-char 34)))))
		((map (const str) args))))

(defun spirv-listing (module)
	;an instruction to a line, its name and its words. It is a plain
	;listing, the ids are not named, spirv-dis does that from the binary
	(defq words (map (# (logand (code module 4 (* %0 4)) 0xffffffff))
			(range 0 (/ (length module) 4)))
		lines (list (cat "; SPIR-V, " (str (length module)) " bytes, "
			(str (dec (elem-get words 3))) " ids")) i 5 n (length words))
	(while (< i n)
		(defq len (max 1 (>> (elem-get words i) 16)) op (logand (elem-get words i) 0xffff))
		(push lines (join (cat (list (ifn (. *spv_names* :find op) (cat "Op" (str op))))
			(spirv-operands op (slice words (inc i) (+ i len)))) " "))
		(setq i (+ i len)))
	(join lines (ascii-char 10)))

(defun tree-listing (program)
	;the program, a declaration to a line
	(bind '(inputs consts globals funcs &rest _) (shader-full program))
	(defq lines (list))
	(each (# (push lines (cat "input " (str %0)))) inputs)
	(each (# (push lines (cat "const " (str %0)))) consts)
	(each (# (push lines (cat "global " (str %0)))) globals)
	(each (lambda ((name type params block))
		(push lines "" (cat "func " (str name) " " (str type) " " (str params)))
		(each (# (push lines (cat (ascii-char 9) (str %0)))) block)) funcs)
	(join lines (ascii-char 10)))

(defun vp-listing (program)
	;the VP source of the native code of a program, whatever kind it is.
	;A vertex shader places vertices. A pixel shader that reads varyings
	;fills triangles, one that reads none shades a tile. A file of
	;functions is a native function for each, one after another
	(defq name "lib/gpu/jit/shader" stage (shader-stage program))
	(cond
		((eql stage :vertex) (first (sv-vertex-source program name)))
		((eql stage :func)
			(join (map (lambda ((fname &rest _))
				(sv-func-source program fname (cat name "_" (str fname))))
				(shader-funcs program)) (ascii-char 10)))
		((nempty? (shader-varyings program)) (first (sv-fill-source program name)))
		((first (sv-source program name)))))

(defun cpu-listing (program)
	;the Lisp the reference back end runs, whatever kind the program is
	(defq stage (shader-stage program))
	(cond
		((eql stage :vertex) (str (shader-cpu-vertex program)))
		((eql stage :func)
			(join (map (lambda ((fname &rest _))
				(cat ";" (str fname) (ascii-char 10) (str (shader-cpu-func program fname))))
				(shader-funcs program)) (ascii-char 10)))
		((str (shader-cpu program)))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_t "glsl" opt_v :nil opt_p :nil opt_o :nil args (options stdio usage)))
		(cond
			((and opt_p (= (length args) 2))
				;a pair, the two files in either order
				(defq one (shader-load (second args)) two (shader-load opt_p) target (sym opt_t)
					vertex (if (eql (shader-stage one) :vertex) one two)
					pixel (if (eql (shader-stage one) :vertex) two one)
					pair (cond
						((eql target 'glsl) (shader-glsl-pair vertex pixel))
						((eql target 'msl) (shader-msl-pair vertex pixel))
						((eql target 'spirv) (shader-spirv-pair vertex pixel))))
				(cond
					((not pair) (print (second (first usage))))
					(opt_o (save (first pair) (cat opt_o ".vert")) (save (second pair) (cat opt_o ".frag")))
					((eql target 'spirv)
						(print "; the vertex module") (print (spirv-listing (first pair)))
						(print "; the fragment module") (print (spirv-listing (second pair))))
					(:t (print "// the vertex shader") (print (first pair))
						(print "// the fragment shader") (print (second pair)))))
			((and (not opt_v) (/= (length args) 2))
				(print (second (first usage))))
			(:t (defq program (if (> (length args) 1) (shader-load (second args)))
					target (sym opt_t)
					out (cond
						((eql target 'glsl) (shader-glsl program))
						((eql target 'msl)
							(if opt_v (shader-msl-vertex) (shader-msl program)))
						((eql target 'spirv)
							(defq module (if opt_v (shader-spirv-vertex) (shader-spirv program)))
							(if opt_o module (spirv-listing module)))
						((eql target 'vp) (vp-listing program))
						((eql target 'cpu) (cpu-listing program))
						((eql target 'tree) (tree-listing program))))
				(cond
					((not out) (print (second (first usage))))
					(opt_o (save out opt_o))
					((print out)))))))
