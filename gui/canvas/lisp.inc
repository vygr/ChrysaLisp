;;;;;;;;;;;;;;;
; canvas widget
;;;;;;;;;;;;;;;

(import "././view/lisp.inc")
(import "././texture/lisp.inc")
(import "././pixmap/lisp.inc")

;lisp bindings
(ffi "gui/canvas/lisp_to_argb32" canvas-to-argb32)
(ffi "gui/canvas/lisp_from_argb32" canvas-from-argb32)

;init pixmap shared cache map
(def *root_env* '*pixmap_cache* (Lmap))

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

	(defmethod :draw ()
		; (. canvas :draw) -> canvas
		(when (defq texture (getf this +canvas_texture 0))
			(bind '(tid tw th) (texture-metrics texture))
			(bind '(w h) (. this :get_size))
			(defq ox (def? :offset_x this) oy (def? :offset_y this))
			(unless ox (setq ox (>>> (- w tw) 1)))
			(unless oy (setq oy (>>> (- h th) 1)))
			(. this :ctx_blit tid +argb_white ox oy tw th))
		this)

	(defmethod :save (file format)
		; (. canvas :save file format) -> :nil | canvas
		(if (pixmap-save (getf this +canvas_pixmap 0) file format) this))

	(defmethod :swap (flags)
		; (. canvas :swap flags) -> canvas
		(. ((const (ffi "gui/canvas/lisp_swap")) this flags) :dirty))

	(defmethod :next_frame ()
		; (. canvas :next_frame) -> canvas
		((const (ffi "gui/canvas/lisp_next_frame")) this))

	(defmethod :resize (that)
		; (. canvas :resize canvas) -> canvas
		((const (ffi "gui/canvas/lisp_resize")) this that))

	(defmethod :fill (argb)
		; (. canvas :fill argb) -> canvas
		((const (ffi "gui/canvas/lisp_fill")) this argb))

	(defmethod :set_canvas_flags (flags)
		; (. canvas :set_canvas_flags flags) -> canvas
		(setf this +canvas_flags flags 0))

	(defmethod :set_color (argb)
		; (. canvas :set_color argb) -> canvas
		(setf this +canvas_color argb 0))

	(defmethod :get_color ()
		; (. canvas :get_color) -> argb
		(getf this +canvas_color 0))

	(defmethod :plot (x y)
		; (. canvas :plot x y) -> canvas
		((const (ffi "gui/canvas/lisp_plot")) this x y))

	(defmethod :fbox (x y width height)
		; (. canvas :fbox x y width height) -> canvas
		((const (ffi "gui/canvas/lisp_fbox")) this x y width height))

	(defmethod :fpoly (x y winding_mode paths)
		; (. canvas :fpoly x y winding_mode paths) -> canvas
		((const (ffi "gui/canvas/lisp_fpoly")) this x y winding_mode paths))

	(defmethod :ftri (tri_path)
		; (. canvas :ftri tri) -> canvas
		((const (ffi "gui/canvas/lisp_ftri")) this tri_path))

	(defmethod :constraint ()
		; (. canvas :constraint) -> (width height)
		(defq canvas_scale (getf this +canvas_scale 0)
			pixmap (getf this +canvas_pixmap 0))
		(list (/ (getf pixmap +pixmap_width 0) canvas_scale)
			(/ (getf pixmap +pixmap_height 0) canvas_scale)))

	(defmethod :get_clip ()
		; (. canvas :get_clip) -> (cx cy cx1 cy1)
		(list (getf this +canvas_cx 0) (getf this +canvas_cy 0)
			 (getf this +canvas_cx1 0) (getf this +canvas_cy1 0)))
	)

(defclass Canvas (width height scale) (Canvas-base)
	; (Canvas width height scale) -> canvas
	; override the default 'this' env with a Canvas component
	(defq super this this ((const (ffi "gui/canvas/lisp_create")) width height scale))
	(each (lambda ((key val)) (def this key val)) (tolist super)))

(defclass Canvas-pixmap (pixmap) (Canvas-base)
	; (Canvas-pixmap pixmap) -> canvas
	; override the default 'this' env with a Canvas component
	(defq super this this ((const (ffi "gui/canvas/lisp_create_pixmap")) pixmap))
	(each (lambda ((key val)) (def this key val)) (tolist super)))

(defun canvas-load (file flags)
	; (canvas-load file flags) -> :nil | canvas
	(defq this :nil)
	(when (/= (logand flags +load_flag_shared) 0)
		;check cache first
		(when (defq this (. *pixmap_cache* :find file))
			(defq this (Canvas-pixmap this))))
	(when (and (not this)
			(defq stream (if (= (logand flags +load_flag_film) 0)
				(file-stream file) (string-stream (load file)))))
		;load and maybe add to cache
		(defq this
			(cond
				((ends-with ".svg" file)
					(SVG-Canvas stream 2))
				((or (ends-with ".cpm" file) (ends-with ".flm" file))
					(if (defq this ((const (ffi "gui/pixmap/lisp_load_cpm")) stream))
						(Canvas-pixmap this)))
				((ends-with ".tga" file)
					(if (defq this ((const (ffi "gui/pixmap/lisp_load_tga")) stream))
						(Canvas-pixmap this)))))
		(when this
			(defq pixmap (getf this +canvas_pixmap 0))
			(if (/= (logand flags +load_flag_shared) 0)
				(. *pixmap_cache* :insert file pixmap))
			(if (/= (logand flags +load_flag_film) 0)
				(setf pixmap +pixmap_stream stream 0))
			(if (= (logand flags +load_flag_noswap) 0)
				(. this :swap 0))))
	this)

(defun canvas-flush ()
	; (canvas-flush)
	;flush any shared pixmaps that have no users.
	;4 refs are held by the cache Lmap and this loop !
	(defq new_cache (Lmap))
	(. *pixmap_cache* :each (lambda (key val)
		(if (> (getf val +obj_count 0) 4)
			(. new_cache :insert key val))))
	(def *root_env* '*pixmap_cache* new_cache))
