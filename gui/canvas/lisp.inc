;;;;;;;;;;;;;;;
; canvas widget
;;;;;;;;;;;;;;;

(import "./struct.inc")
(import "././view/lisp.inc")
(import "././texture/lisp.inc")
(import "././pixmap/lisp.inc")
(import "lib/image/svg.inc")
(import "lib/image/cwb.inc")
(import "lib/image/tga.inc")
(import "lib/image/cpm.inc")

;init pixmap shared cache map
(def *root_env* '*pixmap_cache* (Lmap))

(defun canvas-darker (col)
	; (canvas-darker col) -> col
	(num (+ (logand col 0xff000000) (>> (logand col 0xfefefe) 1))))

(defun canvas-brighter (col)
	; (canvas-brighter col) -> col
	(num (+ (logand col 0xff000000) (>> (logand col 0xfefefe) 1) 0x808080)))

(defun canvas-shade (col)
	; (canvas-shade col) -> col
	;a little darker, a quarter of the way to black, the edge of a thing
	;that is in shadow
	(num (- col (>> (logand col 0xfcfcfc) 2))))

(defun canvas-tint (col)
	; (canvas-tint col) -> col
	;a little brighter, a quarter of the way to white, the edge of a thing
	;that the light is on
	(num (+ col (>> (logand (lognot col) 0xfcfcfc) 2))))

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

	(defmethod :swap (flags)
		; (. canvas :swap flags) -> canvas
		;+swap_write, the pixmap to the texture, to which a +pixmap_mode
		;and any +swap_flag can be added. Or +swap_read, the texture back
		;to the pixmap.
		(. ((const (ffi "gui/canvas/lisp_swap")) this flags) :dirty))

	(defmethod :free ()
		; (. canvas :free) -> canvas
		;let go of the pixmap, the texture stays as it is. For a canvas
		;that is only ever shown, it can not be drawn on after this.
		((const (ffi "gui/canvas/lisp_swap")) this (const (+ +swap_flag_free +swap_flag_keep))))

	(defmethod :shade (shader block &optional x y x1 y1)
		; (. canvas :shade shader block [x y x1 y1]) -> :nil | :error | canvas
		;the GPU draws the shader into the texture of the canvas, all of
		;it, or the part given, in pixels of the texture. One draw is on
		;the go at a time, :nil is the GPU still busy with the last, or
		;the driver still building the shader, it has drawn nothing, try
		;again. :error is a shader the driver could not build. A frame a
		;slow GPU takes long over is best drawn as strips, so the GUI can
		;be drawn in between.
		(setd x 0 y 0 x1 0 y1 0)
		(cond
			((num? (defq drawn ((const (ffi "gui/canvas/lisp_shade")) this shader block x y x1 y1))) :error)
			(drawn (. this :dirty))))

	(defmethod :shade_tris (frame)
		; (. canvas :shade_tris frame) -> :nil | :error | canvas
		;the GPU draws a frame of triangles into the texture of the
		;canvas, with a depth buffer. The frame is a count, 8 bytes, then
		;for each thing drawn, 8 bytes each, its pair, its mesh, the
		;length of the vertex shader's block and of the pixel shader's,
		;then the two blocks, each made up to a whole 8 bytes. :nil is
		;the GPU still busy, or a pair still being built, nothing was
		;drawn, try again. :error is a pair the driver could not build.
		(cond
			((num? (defq drawn ((const (ffi "gui/canvas/lisp_shade")) this 0 frame 0 0 0 0))) :error)
			(drawn (. this :dirty))))

	(defmethod :exchange (that)
		; (. canvas :exchange that) -> canvas
		;the two canvases exchange what they show, their textures. So one
		;can be off screen, drawn into by a shader a strip at a time, and
		;put on show when the frame is whole. They must be the same size.
		(defq tex (getf this +canvas_texture 0) flags (getf this +canvas_flags 0)
			that_flags (getf that +canvas_flags 0)
			mask (const +canvas_flag_shader) keep (const (logxor +canvas_flag_shader -1)))
		(setf this +canvas_texture (getf that +canvas_texture 0) 0)
		(setf that +canvas_texture tex 0)
		(setf this +canvas_flags (logior (logand flags keep) (logand that_flags mask)) 0)
		(setf that +canvas_flags (logior (logand that_flags keep) (logand flags mask)) 0)
		(. this :dirty))

	(defmethod :next_frame ()
		; (. canvas :next_frame) -> canvas
		((const (ffi "gui/canvas/lisp_next_frame")) this))

	(defmethod :resize (that)
		; (. canvas :resize canvas) -> canvas
		((const (ffi "gui/canvas/lisp_resize")) this that))

	(defmethod :flip_x (that)
		; (. canvas :flip_x canvas) -> canvas
		((const (ffi "gui/canvas/lisp_flip_x")) this that))

	(defmethod :fill (argb)
		; (. canvas :fill argb) -> canvas
		((const (ffi "gui/canvas/lisp_fill")) this argb))

	(defmethod :tile (data x1 y1 x2 y2)
		; (. canvas :tile data x1 y1 x2 y2) -> area
		((const (ffi "gui/canvas/lisp_tile")) this data x1 y1 x2 y2))

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

	(defmethod :constraint ()
		; (. canvas :constraint) -> (width height)
		;with the pixmap freed the size is that of the texture
		(cond
			((defq pixmap (getf this +canvas_pixmap 0))
				(defq canvas_scale (getf this +canvas_scale 0))
				(list (/ (getf pixmap +pixmap_width 0) canvas_scale)
					(/ (getf pixmap +pixmap_height 0) canvas_scale)))
			((defq texture (getf this +canvas_texture 0))
				(rest (texture-metrics texture)))
			((list 0 0))))

	(defmethod :get_clip ()
		; (. canvas :get_clip) -> (cx cy cx1 cy1)
		(getf-> this (+canvas_cx 0) (+canvas_cy 0)
			 (+canvas_cx1 0) (+canvas_cy1 0)))

	(defmethod :set_clip (x y x1 y1)
		; (. canvas :set_clip x y x1 y1) -> canvas
		;what is drawn from here on is kept to this part of the pixmap. It
		;is cut down to the pixmap, and with no pixmap it is left alone.
		(when (defq pixmap (getf this +canvas_pixmap 0))
			(setq x (max x 0) y (max y 0)
				x1 (min x1 (getf pixmap +pixmap_width 0))
				y1 (min y1 (getf pixmap +pixmap_height 0)))
			(when (and (<= x x1) (<= y y1))
				(setf this +canvas_cx x 0)
				(setf this +canvas_cy y 0)
				(setf this +canvas_cx1 x1 0)
				(setf this +canvas_cy1 y1 0)))
		this)
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

(defun canvas-shared (width height scale &optional key)
	; (canvas-shared width height scale [key]) -> :nil | canvas
	;a canvas with its pixels in shared memory, that the nodes of this
	;machine can all draw on. With no key the pixels are made, and
	;(canvas-key) is the key of them. Given that key, and the same size,
	;a task on another node has a canvas on the same pixels. :nil if the
	;host has no shared memory, or there are no pixels of that key.
	(when (defq pixmap (pixmap-shared (* width scale) (* height scale) (ifn key 0)))
		(defq canvas (Canvas-pixmap pixmap))
		;a canvas made of a pixmap takes its size as a view from the pixmap,
		;and with a scale it is that much smaller
		(setf canvas +canvas_scale scale 0)
		(. canvas :set_size width height)
		canvas))

(defun canvas-key (canvas)
	; (canvas-key canvas) -> 0 | key
	;the key of the shared memory the pixels of a canvas are in, 0 if
	;they are its own
	(if (defq pixmap (getf canvas +canvas_pixmap 0)) (pixmap-key pixmap) 0))

(defun canvas-info (file)
	; (canvas-info file) -> (width height type) | (-1 -1 -1)
	(or (if (defq stream (file-stream file))
			(some (# (if (ends-with %0 file) (%1 stream)))
				'(".cpm" ".flm" ".tga" ".svg" ".cwb")
				(const `'(,CPM-info ,CPM-info ,TGA-info ,SVG-info ,CWB-info))))
		(list -1 -1 -1)))

(defun canvas-save (canvas file type &rest optionals)
	; (canvas-save canvas file type [optionals...]) -> :nil | canvas
	(if (defq stream (file-stream file +file_open_write))
		(some (# (if (ends-with %0 file)
					(apply %1 (cat (list canvas stream type) optionals))))
			'(".cpm") (const `'(,CPM-save)))))

(defun canvas-load (file flags &optional swap_mode)
	; (canvas-load file flags [swap_mode]) -> :nil | canvas
	(defq this :nil)
	(setd swap_mode +pixmap_mode_normal)
	(when (bits? flags +load_flag_shared)
		;check cache first
		(when (defq this (. *pixmap_cache* :find file))
			(defq this (Canvas-pixmap this))))
	(when (and (not this)
			(defq stream (if (bits? flags +load_flag_film)
				(string-stream (load file)) (file-stream file))))
		;load and maybe add to cache
		(defq this
			(some (# (if (ends-with %0 file) (%1 stream)))
				'(".svg" ".cwb" ".tga" ".cpm" ".flm")
				(const `'(,SVG-load ,CWB-load ,TGA-load ,CPM-load ,CPM-load))))
		(when this
			(defq pixmap (getf this +canvas_pixmap 0))
			(if (bits? flags +load_flag_shared)
				(. *pixmap_cache* :insert file pixmap))
			(if (bits? flags +load_flag_film)
				(setf pixmap +pixmap_stream stream 0))
			(ifn (bits? flags +load_flag_noswap)
				(. this :swap (if (bits? flags +load_flag_free)
					(+ swap_mode +swap_flag_free) swap_mode)))))
	this)

(defun canvas-shader-format ()
	; (canvas-shader-format) -> 0 | 1 | 2
	;the shading language the host GUI driver takes for a shader,
	;0 if it can not draw one, 1 for MSL, 2 for SPIR-V
	((const (ffi "gui/canvas/lisp_shader_format"))))

(defun canvas-shader-create (vertex fragment)
	; (canvas-shader-create vertex fragment) -> 0 | shader
	;a shader from the text of its vertex and fragment stages
	((const (ffi "gui/canvas/lisp_shader_create")) vertex fragment))

(defun canvas-shader-destroy (shader)
	; (canvas-shader-destroy shader) -> shader
	;a shader, or a pair
	((const (ffi "gui/canvas/lisp_shader_destroy")) shader))

(defun canvas-pair-create (vertex fragment layout)
	; (canvas-pair-create vertex fragment layout) -> 0 | pair
	;a vertex shader and a pixel shader that draw triangles, from the text
	;of their stages. The layout is a byte for how many attrs a vertex
	;has, a byte for the cull, 0 none, 1 what faces away, 2 what faces us,
	;then a byte for the floats of each attr
	((const (ffi "gui/canvas/lisp_pair_create")) vertex fragment layout))

(defun canvas-mesh-create (verts)
	; (canvas-mesh-create verts) -> 0 | mesh
	;the vertices of a mesh, kept on the GPU, from the bytes of a reals,
	;the attrs of a vertex one after another
	((const (ffi "gui/canvas/lisp_mesh_create")) verts))

(defun canvas-mesh-destroy (mesh)
	; (canvas-mesh-destroy mesh) -> mesh
	((const (ffi "gui/canvas/lisp_mesh_destroy")) mesh))

(defun canvas-flush ()
	; (canvas-flush)
	;flush any shared pixmaps that have no users.
	;4 refs are held by the cache Lmap and this loop !
	(defq new_cache (Lmap))
	(. *pixmap_cache* :each (lambda (key val)
		(if (> (getf val +obj_count 0) 4)
			(. new_cache :insert key val))))
	(def *root_env* '*pixmap_cache* new_cache))
