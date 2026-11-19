;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shader language, triangles on the GPU of this node.
; A pair, a vertex shader and a pixel shader, made for the GPU the node
; has, MSL or SPIR-V as (gpu-format) says, to draw meshes with,
; (gpu-draw) of lib/gpu/compute.inc. The CPU draws the same pair with
; (shader-vp-draw-tris), lib/gpu/vp.inc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./msl.inc")
(import "./spirv.inc")
(import "./compute.inc")

;what a back end makes of a pair of files for the GPU is kept, under the
;hash of the files and of the back end, as native code is. So a pair that
;has been met before is handed to the driver with nothing of it read
(defq +render_msl_by ''("lib/gpu/msl.inc" "lib/gpu/shader.inc")
	+render_spirv_by ''("lib/gpu/spirv.inc" "lib/gpu/shader.inc"))

(defun shader-pair-text (vertex pixel by ext make &optional kind)
	; (shader-pair-text vertex pixel by ext make [kind]) -> (vertex_stage fragment_stage)
	;the two stages of a pair, kept, both made if either is not there.
	;The fragment stage is written last, so if it is there both are. What
	;is kept of another make goes, the first time. kind is a word for a
	;pair made another way of the same two shaders, kept beside it
	(shader-sweep "gp" (list (shader-make +render_msl_by) (shader-make +render_spirv_by)))
	(defq name (cat "p" (shader-make by) (ifn kind "") "_" (elem-get vertex 5) "_" (elem-get pixel 5))
		path (cat (load-path) "lib/gpu/jit/" name))
	(cond
		((and (defq fragment (load (cat path ".f" ext))) (defq vertex_stage (load (cat path ".v" ext))))
			(list vertex_stage fragment))
		(:t (defq both (make))
			(save (first both) (cat path ".v" ext))
			(save (second both) (cat path ".f" ext))
			both)))

(defun shader-gpu-pair (vertex pixel &optional cull flat)
	; (shader-gpu-pair vertex pixel [cull flat]) -> :nil | pair
	;a vertex shader and a pixel shader that this node's GPU can draw
	;triangles with, (gpu-draw). :nil if it has none. With cull a triangle
	;that faces away is left out, :front those that face us. A flat pair
	;has no depth buffer and puts what it draws where it goes, not over
	;what is there: for a frame that is all a pixel shader, which can then
	;be drawn some rows at a time. The host builds it on a thread of its
	;own. It is let go of with (gpu-pair-free)
	(defq layout (cat (char (length (shader-attrs vertex)) 1)
			(char (+ (cond ((not cull) 0) ((eql cull :front) 2) (1)) (if flat 4 0)) 1)
			(apply (const cat) (map (# (char (shader-size (second %0)) 1)) (shader-attrs vertex)))
			(char (length (shader-textures pixel)) 1)))
	(case (gpu-format)
		(1 (bind '(vertex_text fragment_text) (shader-pair-text vertex pixel +render_msl_by ".msl"
					;check off, the two it is called with are the two this was
					(# (shader-msl-pair vertex pixel flat)) (if flat "f")))
			(gpu-pair vertex_text fragment_text layout))
		(2 (bind '(vertex_module fragment_module) (shader-pair-text vertex pixel +render_spirv_by ".spv"
					;check off, as above
					(# (shader-spirv-pair vertex pixel flat)) (if flat "f")))
			(gpu-pair vertex_module fragment_module layout))))
