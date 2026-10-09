;;;;;;;;;;;;
; cwb loader
;;;;;;;;;;;;

;a .cwb file is a whiteboard document, shapes, lib/cwb/doc.inc. As an
;image it is those shapes drawn, at the size the document says it is, on
;its background, which is nothing unless it was given one. So where one
;is shown, the image section of a page of the Docs app say, what is behind
;it shows round its shapes.

(import "gui/canvas/lisp.inc")
(import "././cwb/doc.inc")

;module
(env-push)

(defun CWB-info (stream)
	; (CWB-info stream) -> (width height type) | (-1 -1 -1)
	(if (and (defq doc (cwb-load stream)) (> (. doc :find :width) 0) (> (. doc :find :height) 0))
		(list (. doc :find :width) (. doc :find :height) 32)
		'(-1 -1 -1)))

(defun CWB-load (stream &optional scale)
	; (CWB-load stream [scale]) -> :nil | canvas
	;each pixel is scale by scale samples, default 1
	(setd scale 1)
	(when (and (defq doc (cwb-load stream)) (> (. doc :find :width) 0) (> (. doc :find :height) 0))
		(defq canvas (Canvas (. doc :find :width) (. doc :find :height) scale))
		(.-> canvas (:set_canvas_flags +canvas_flag_antialias) (:fill (. doc :find :background)))
		(cwb-draw canvas doc (if (= scale 1) :nil (cwb-mat-scale scale)))
		(if (= scale 1) canvas
			(progn
				;drawn big, and made the size it is to be
				(bind '(w h) (. canvas :pref_size))
				(. (Canvas w h 1) :resize canvas)))))

;module
(export-symbols '(CWB-load CWB-info))
(env-pop)
