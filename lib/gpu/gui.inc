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
	(case (canvas-shader-format)
		(1 (defq shader (canvas-shader-create (shader-msl-vertex) (shader-msl program)))
			(if (/= shader 0) shader))
		(2 (defq shader (canvas-shader-create (shader-spirv-vertex) (shader-spirv program)))
			(if (/= shader 0) shader))
		(:t :nil)))
