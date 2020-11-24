;;;;;;;;;;;;;;;
; canvas widget
;;;;;;;;;;;;;;;

(import "gui/view/lisp.inc")

;lisp bindings
(ffi canvas-info "gui/canvas/lisp_info" 0)
(ffi canvas-to-argb32 "gui/canvas/lisp_to_argb32" 0)
(ffi canvas-from-argb32 "gui/canvas/lisp_from_argb32" 0)

;load flags
(defq +load_flag_shared+ 1 +load_flag_film+ 2 +load_flag_noswap+ 4)

(defclass Canvas-base ()
	(defq this (View))

	(defmethod :save (this file format)
		; (. canvas :save file format) -> nil | canvas
		((ffi _ "gui/canvas/lisp_save" 0) this file format))

	(defmethod :swap (this)
		; (. canvas :swap) -> canvas
		((ffi _ "gui/canvas/lisp_swap" 0) this))

	(defmethod :next_frame (this)
		; (. canvas :next_frame) -> canvas
		((ffi _ "gui/canvas/lisp_next_frame" 0) this))

	(defmethod :resize (this that)
		; (. canvas :resize canvas) -> canvas
		((ffi _ "gui/canvas/lisp_resize" 0) this that))

	(defmethod :fill (this argb)
		; (. canvas :fill argb) -> canvas
		((ffi _ "gui/canvas/lisp_fill" 0) this argb))

	(defmethod :set_canvas_flags (this flags)
		; (. canvas :set_canvas_flags flags) -> canvas
		(set-field this (const canvas_flags) (const (neg int_size)) flags))

	(defmethod :set_color (this argb)
		; (. canvas :set_color argb) -> canvas
		(set-field this (const canvas_color) (const (neg int_size)) argb))

	(defmethod :plot (this x y)
		; (. canvas :plot x y) -> canvas
		((ffi _ "gui/canvas/lisp_plot" 0) this x y))

	(defmethod :fbox (this x y width height)
		; (. canvas :fbox x y width height) -> canvas
		((ffi _ "gui/canvas/lisp_fbox" 0) this x y))

	(defmethod :fpoly (this x y mode paths)
		; (. canvas :fpoly x y mode paths) -> canvas
		((ffi _ "gui/canvas/lisp_fpoly" 0) this x y mode paths))

	(defmethod :pref_size (this)
		; (. canvas :pref_size) -> (width height)
		(defq canvas_scale (get-field this (const canvas_scale) (const (neg int_size)))
			pixmap (get-field this (const canvas_pixmap) 0))
		(list (/ (get-field pixmap (const pixmap_width) (const (neg int_size))) canvas_scale)
			(/ (get-field pixmap (const pixmap_height) (const (neg int_size))) canvas_scale)))
	)

(defclass Canvas (width height scale)
	; (Canvas width height scale) -> canvas
	; overide the default 'this' env with a canvas component
	; inherit from Canvas-base !
	(defq this ((ffi _ "gui/canvas/lisp_create" 0) width height scale))
	(each (lambda ((key val)) (def this key val)) (tolist (Canvas-base)))
	)

(defclass Canvas-from-file (file flags)
	; (Canvas-from-file file flags) -> nil | canvas
	; overide the default 'this' env with a canvas component
	; inherit from Canvas-base !
	(defq this ((ffi _ "gui/canvas/lisp_load" 0) file flags))
	(if this (each (lambda ((key val)) (def this key val)) (tolist (Canvas-base))))
	)
