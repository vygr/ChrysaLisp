;lisp bindings
(ffi canvas-swap "gui/canvas/lisp_swap" 0)
(ffi canvas-fill "gui/canvas/lisp_fill" 0)
(ffi canvas-load "gui/canvas/lisp_load" 0)
;(ffi canvas-save "gui/canvas/lisp_save" 0)
(ffi canvas-fbox "gui/canvas/lisp_fbox" 0)
(ffi canvas-fpoly "gui/canvas/lisp_fpoly" 0)
(ffi canvas-plot "gui/canvas/lisp_plot" 0)
(ffi canvas-next-frame "gui/canvas/lisp_next_frame" 0)
(ffi create-canvas "gui/canvas/lisp_create" 0)

(defun-bind canvas-set-color (canvas argb)
	(set-field canvas  (const canvas_color) (const (neg int_size)) argb))

(defun-bind canvas-set-flags (canvas flags)
	(set-field canvas  (const canvas_flags) (const (neg int_size)) flags))
