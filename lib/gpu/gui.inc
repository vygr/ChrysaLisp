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
	;face us. The driver builds it in its own time. It is let go of with
	;(canvas-shader-destroy).
	(defq layout (cat (char (length (shader-attrs vertex)) 1)
			(char (cond ((not cull) 0) ((eql cull :front) 2) (1)) 1)
			(apply (const cat) (map (# (char (shader-size (second %0)) 1)) (shader-attrs vertex))))
		pair (case (canvas-shader-format)
			(1 (bind '(vertex_text fragment_text) (shader-msl-pair vertex pixel))
				(canvas-pair-create vertex_text fragment_text layout))
			(2 (bind '(vertex_module fragment_module) (shader-spirv-pair vertex pixel))
				(canvas-pair-create vertex_module fragment_module layout))
			(:t 0)))
	(if (/= pair 0) pair))

(defun shader-gui-mesh (verts)
	; (shader-gui-mesh verts) -> :nil | mesh
	;the vertices of a mesh, kept on the GPU, to be drawn with a pair
	;whose vertex shader has those attrs. verts is the bytes of
	;(shader-verts-str). :nil if this host can not. It is let go of with
	;(canvas-mesh-destroy).
	(defq mesh (canvas-mesh-create verts))
	(if (/= mesh 0) mesh))

(defun shader-gui-frame (canvas draws)
	; (shader-gui-frame canvas draws) -> :nil | :error | canvas
	;a frame of triangles drawn by the GPU into the texture of a canvas,
	;with a depth buffer. draws is a list of (pair mesh vblock pblock), a
	;pair, a mesh, and the blocks of (shader-pack) for the two shaders of
	;the pair. :nil is the GPU still busy with the last frame, or the
	;driver still building a pair, nothing was drawn, try again. :error is
	;a pair the driver could not build.
	(defun pad8 (block)
		(cat block (slice (const (char 0 8)) 0 (- (logand (+ (length block) 7) -8) (length block)))))
	(. canvas :shade_tris (apply (const cat) (cat (list (char (length draws) 8))
		(map (lambda ((pair mesh vblock pblock))
			;a shader with no inputs still has a block, of nothing
			(if (empty? vblock) (setq vblock (const (char 0 8))))
			(if (empty? pblock) (setq pblock (const (char 0 8))))
			(cat (char pair 8) (char mesh 8) (char (length vblock) 8) (char (length pblock) 8)
				(pad8 vblock) (pad8 pblock))) draws)))))
