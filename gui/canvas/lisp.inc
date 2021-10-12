;;;;;;;;;;;;;;;
; canvas widget
;;;;;;;;;;;;;;;

(import "gui/view/lisp.inc")
(import "gui/texture/lisp.inc")
(import "gui/pixmap/lisp.inc")

;lisp bindings
(ffi canvas-to-argb32 "gui/canvas/lisp_to_argb32" 0)
(ffi canvas-from-argb32 "gui/canvas/lisp_from_argb32" 0)

;init pixmap shared cache map
(defq _ (env))
(while (penv _) (setq _ (penv _)))
(def _ '*pixmap_cache* (lmap))

(structure +canvas +view_size
	(ptr pixmap edges texture)
	(pptr edges_start)
	(pubyte coverage)
	(uint scale cx cy cx1 cy1 color flags))

(bits +load_flag 0
	(bit shared film noswap))

(bits +canvas_flag 0
	(bit antialias))

(enums +winding 0
	(enum odd_even none_zero))

(defun canvas-darker (col)
	; (canvas-darker col) -> col
	(+ (logand col 0xff000000) (>> (logand col 0xfefefe) 1)))

(defun canvas-brighter (col)
	; (canvas-brighter col) -> col
	(+ (logand col 0xff000000) (>> (logand col 0xfefefe) 1) 0x808080))

(defclass Canvas-base () (View)
	(def this :color 0)

	(defmethod :draw (this)
		; (. canvas :draw) -> canvas
		(when (defq texture (getf this +canvas_texture 0))
			(bind '(tid tw th) (texture-metrics texture))
			(bind '(w h) (. this :get_size))
			(defq ox (def? :offset_x this) oy (def? :offset_y this))
			(unless ox (setq ox (>>> (- w tw) 1)))
			(unless oy (setq oy (>>> (- h th) 1)))
			(. this :ctx_blit tid +argb_white ox oy tw th))
		this)

	(defmethod :save (this file format)
		; (. canvas :save file format) -> nil | canvas
		(if (pixmap-save (getf this +canvas_pixmap 0) file format) this))

	(defmethod :swap (this)
		; (. canvas :swap) -> canvas
		(. ((const (ffi _ "gui/canvas/lisp_swap" 0)) this) :dirty))

	(defmethod :next_frame (this)
		; (. canvas :next_frame) -> canvas
		((const (ffi _ "gui/canvas/lisp_next_frame" 0)) this))

	(defmethod :resize (this that)
		; (. canvas :resize canvas) -> canvas
		((const (ffi _ "gui/canvas/lisp_resize" 0)) this that))

	(defmethod :fill (this argb)
		; (. canvas :fill argb) -> canvas
		((const (ffi _ "gui/canvas/lisp_fill" 0)) this argb))

	(defmethod :set_canvas_flags (this flags)
		; (. canvas :set_canvas_flags flags) -> canvas
		(setf this +canvas_flags flags 0))

	(defmethod :set_color (this argb)
		; (. canvas :set_color argb) -> canvas
		(setf this +canvas_color argb 0))

	(defmethod :get_color (this)
		; (. canvas :get_color) -> argb
		(getf this +canvas_color 0))

	(defmethod :plot (this x y)
		; (. canvas :plot x y) -> canvas
		((const (ffi _ "gui/canvas/lisp_plot" 0)) this x y))

	(defmethod :fbox (this x y width height)
		; (. canvas :fbox x y width height) -> canvas
		((const (ffi _ "gui/canvas/lisp_fbox" 0)) this x y))

	(defmethod :fpoly (this x y winding_mode paths)
		; (. canvas :fpoly x y winding_mode paths) -> canvas
		((const (ffi _ "gui/canvas/lisp_fpoly" 0)) this x y winding_mode paths))

	(defmethod :ftri (this tri_path)
		; (. canvas :ftri tri) -> canvas
		((const (ffi _ "gui/canvas/lisp_ftri" 0)) this tri_path))

	(defmethod :pref_size (this)
		; (. canvas :pref_size) -> (width height)
		(defq canvas_scale (getf this +canvas_scale 0)
			pixmap (getf this +canvas_pixmap 0))
		(list (/ (getf pixmap +pixmap_width 0) canvas_scale)
			(/ (getf pixmap +pixmap_height 0) canvas_scale)))

	(defmethod :get_clip (this)
		; (. canvas :get_clip) -> (cx cy cx1 cy1)
		(list (getf this +canvas_cx 0) (getf this +canvas_cy 0)
			 (getf this +canvas_cx1 0) (getf this +canvas_cy1 0)))
	)

(defclass Canvas (width height scale) (Canvas-base)
	; (Canvas width height scale) -> canvas
	; overide the default 'this' env with a Canvas component
	(defq super this this ((const (ffi _ "gui/canvas/lisp_create" 0)) width height scale))
	(each (lambda ((key val)) (def this key val)) (tolist super)))

(defclass Canvas-from-pixmap (pixmap) (Canvas-base)
	; (Canvas-from-pixmap pixmap) -> canvas
	; overide the default 'this' env with a Canvas component
	(defq super this this ((const (ffi _ "gui/canvas/lisp_create_pixmap" 0)) pixmap))
	(each (lambda ((key val)) (def this key val)) (tolist super)))

(defun Canvas-from-file (file flags)
	; (Canvas-from-file file flags) -> nil | canvas
	(defq this nil)
	(when (/= (logand flags +load_flag_shared) 0)
		;check cache first
		(when (defq this (. *pixmap_cache* :find file))
			(defq this (Canvas-from-pixmap this))))
	(when (and (not this)
			(defq stream (if (= (logand flags +load_flag_film) 0)
				(file-stream file) (string-stream (load file)))))
		;load and maybe add to cache
		(defq this
			(cond
				((ends-with ".svg" file)
					(SVG-Canvas stream 2))
				((or (ends-with ".cpm" file) (ends-with ".flm" file))
					(if (defq this ((const (ffi _ "gui/pixmap/lisp_load_cpm" 0)) stream))
						(Canvas-from-pixmap this)))
				((ends-with ".tga" file)
					(if (defq this ((const (ffi _ "gui/pixmap/lisp_load_tga" 0)) stream))
						(Canvas-from-pixmap this)))))
		(when this
			(defq pixmap (getf this +canvas_pixmap 0))
			(if (/= (logand flags +load_flag_shared) 0)
				(. *pixmap_cache* :insert file pixmap))
			(if (/= (logand flags +load_flag_film) 0)
				(setf pixmap +pixmap_stream stream 0))
			(if (= (logand flags +load_flag_noswap) 0)
				(. this :swap))))
	this)
