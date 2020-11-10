;;;;;;;;;;;;;;;
; canvas widget
;;;;;;;;;;;;;;;

;lisp bindings
(ffi canvas-swap "gui/canvas/lisp_swap" 0)
(ffi canvas-fill "gui/canvas/lisp_fill" 0)
(ffi canvas-info "gui/canvas/lisp_info" 0)
(ffi canvas-load "gui/canvas/lisp_load" 0)
(ffi canvas-save "gui/canvas/lisp_save" 0)
(ffi canvas-fbox "gui/canvas/lisp_fbox" 0)
(ffi canvas-fpoly "gui/canvas/lisp_fpoly" 0)
(ffi canvas-plot "gui/canvas/lisp_plot" 0)
(ffi canvas-next-frame "gui/canvas/lisp_next_frame" 0)
(ffi canvas-resize "gui/canvas/lisp_resize" 0)
(ffi canvas-to-argb32 "gui/canvas/lisp_to_argb32" 0)
(ffi canvas-from-argb32 "gui/canvas/lisp_from_argb32" 0)
(ffi create-canvas "gui/canvas/lisp_create" 0)

(defun canvas-set-color (canvas argb)
	; (canvas-set-color canvas argb) -> canvas
	(set-field canvas (const canvas_color) (const (neg int_size)) argb))

(defun canvas-set-flags (canvas flags)
	; (canvas-set-flags canvas flags) -> canvas
	(set-field canvas (const canvas_flags) (const (neg int_size)) flags))

;load flags
(defq +load_flag_shared+ 1 +load_flag_film+ 2 +load_flag_noswap+ 4)

(class Canvas (width height scale)
	; (Canvas) -> canvas
	; overide the default 'this' env with a canvas component
	(defq this (create-canvas width height scale))

	(method :pref_size (this)
		; (. canvas :pref_size) -> (width height)
		(defq canvas_scale (get-field this (const canvas_scale) (const (neg int_size))))
		(list (/ (get-field this (const canvas_width) (const (neg int_size))) canvas_scale)
			(/ (get-field this (const canvas_height) (const (neg int_size))) canvas_scale)))
	)
