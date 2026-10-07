;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shader language, a shader on the GPU of the GUI.
; For a GUI app, which runs on the node that has the GUI. The host
; GUI driver says which shading language it takes, if any.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./msl.inc")
(import "./spirv.inc")

(defun shader-gui (program)
	; (shader-gui program) -> :nil | shader
	;a shader the GPU can draw into a canvas, with (. canvas :shade shader
	;block), where block is from (shader-pack). :nil if this host can not.
	;The driver builds it in its own time, till then :shade draws nothing.
	(case (canvas-shader-format)
		(1 (defq shader (canvas-shader-create (shader-msl-vertex) (shader-msl program)))
			(if (/= shader 0) shader))
		(2 (defq shader (canvas-shader-create (shader-spirv-vertex) (shader-spirv program)))
			(if (/= shader 0) shader))
		(:t :nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; triangles, on the GPU of the GUI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shader-gui-pair (vertex pixel &optional cull)
	; (shader-gui-pair vertex pixel [cull]) -> :nil | pair
	;a vertex shader and a pixel shader that the GPU can draw triangles
	;into a canvas with, (shader-gui-frame). :nil if this host can not.
	;With cull a triangle that faces away is left out, :front those that
	;face us. The driver builds it in its own time.
	;
	;The driver is told what it is by 16 bytes before the code of the
	;vertex shader, a name, how many attrs a vertex has, the cull, and how
	;many floats each attr is.
	(defq head (cat "CLMESH" (char (length (shader-attrs vertex)) 1)
			(char (cond ((not cull) 0) ((eql cull :front) 2) (1)) 1)
			(apply (const cat) (map (# (char (shader-size (second %0)) 1)) (shader-attrs vertex))))
		head (cat head (slice (const (char 0 8)) 0 (- 16 (length head)))))
	(case (canvas-shader-format)
		(1 (bind '(vertex_text fragment_text) (shader-msl-pair vertex pixel))
			(defq pair (canvas-shader-create (cat head vertex_text) fragment_text))
			(if (/= pair 0) pair))
		(2 (bind '(vertex_module fragment_module) (shader-spirv-pair vertex pixel))
			(defq pair (canvas-shader-create (cat head vertex_module) fragment_module))
			(if (/= pair 0) pair))
		(:t :nil)))

(defun shader-gui-frame (canvas pair draws)
	; (shader-gui-frame canvas pair draws) -> :nil | :error | canvas
	;a frame of triangles drawn by the GPU into the texture of a canvas,
	;with a depth buffer. draws is a list of (verts count vblock pblock),
	;the vertices of a mesh, as the bytes of (shader-verts-str), how many
	;vertices that is, and the blocks of (shader-pack) for the two shaders.
	;:nil is the GPU still busy with the last frame, or the driver still
	;building the pair, nothing was drawn, try again. :error is a pair the
	;driver could not build.
	;
	;It goes to the driver as the one block. A count, then for each draw,
	;8 bytes each, how many vertices, the lengths of the two blocks and of
	;the vertices, then the blocks, each made up to a whole 8 bytes, and
	;the vertices.
	(defun pad8 (block)
		(cat block (slice (const (char 0 8)) 0 (- (logand (+ (length block) 7) -8) (length block)))))
	(. canvas :shade pair (apply (const cat) (cat (list (char (length draws) 8))
		(map (lambda ((verts count vblock pblock))
			;a shader with no inputs still has a block, of nothing
			(if (empty? vblock) (setq vblock (const (char 0 8))))
			(if (empty? pblock) (setq pblock (const (char 0 8))))
			(cat (char count 8) (char (length vblock) 8) (char (length pblock) 8) (char (length verts) 8)
				(pad8 vblock) (pad8 pblock) verts)) draws)))))
