;;;;;;;;;;;;;;;
; canvas widget
;;;;;;;;;;;;;;;

(import "./struct.inc")
(import "././view/lisp.inc")
(import "././texture/lisp.inc")
(import "././pixmap/lisp.inc")

;init pixmap shared cache map
(def *root_env* '*pixmap_cache* (Lmap))

;the kinds of picture file there are, each (ext info load save). A loader,
;lib/image/, says what it is for as it is read, (canvas-loader), so this
;file names none of them and needs none of them to be there as it is
;read. They import this, they make a canvas, and this imports them, last
;of all, so that whoever has a canvas can load a picture: whichever of
;the two is come to first, it works
(defq *canvas_loaders* (list))

(defun canvas-loader (ext info load &optional save)
	; (canvas-loader ext info load [save])
	;files whose names end ext are read by these, (info stream) and
	;(load stream), and written by (save canvas stream type ...) if given
	(push *canvas_loaders* (list ext info load save)))

(defun canvas-loader-of (file)
	; (canvas-loader-of file) -> :nil | (ext info load save)
	(some (# (if (ends-with (first %0) file) %0)) *canvas_loaders*))

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

(defun canvas-paint (canvas kind data)
	; (canvas-paint canvas kind data) -> canvas
	;the canvas's span made a paint's, gui/canvas/paint.vp. Bound as it is
	;first called, not as this is imported, as a (const (ffi)) would be: a
	;program that never paints loads none of the paints
	((ffi "gui/canvas/lisp_paint") canvas kind data))

(defun canvas-floor (n d)
	; (canvas-floor n d) -> (q r)
	;the floor quotient of n by d, more than 0, and what is over, 0 to d
	(defq q (/ n d) r (- n (* q d)))
	(if (< r 0) (list (dec q) (+ r d)) (list q r)))

;where each color of a gradient's lut is along it, i by 65536
(defq +paint_lut_at (nums-scale (apply (const nums) (map! (const !) '() 0 +paint_lut_size)) 65536))

(defun canvas-gradient-lut (stops)
	; (canvas-gradient-lut stops) -> nums
	;the 256 colors of a gradient's stops, ((offset argb) ...), step i at t
	;i over 255, each channel mixed straight between the stops either side,
	;not premultiplied, to the nearest. An offset is kept to 0 to 1 and to no
	;less than the one before, as SVG has it, and before the first and after
	;the last is their color. All in integers, an offset in 255 by 65536ths.
	;The colors between two stops are a line, so they are made together, a
	;vector a channel, not one at a time
	(defq at 0 stops (map (lambda ((o c))
			(setq at (max at (min 16711680 (max 0 (n2i (floor (+ (* (n2q o) 16711680.0) 0.5)))))))
			(list at c)) stops)
		parts (list) lo 0 o0 0 c0 :nil)
	(each (lambda ((o1 c1))
			;the colors before this stop that are not before the last
			(defq hi (min +paint_lut_size (/ (+ o1 65535) 65536)) n (- hi lo))
			(when (> n 0)
				(cond
					((not c0) (push parts (fill (nums) n c1)))
					(:t (defq w (- o1 o0) ones (fill (nums) n 1) by (nums-scale ones (* 2 w))
							f (nums-add-scaled (slice +paint_lut_at lo hi) ones (neg o0)))
						(push parts (reduce (lambda (argb shift)
								(defq a (logand (>> c0 shift) 0xff) b (logand (>> c1 shift) 0xff))
								(nums-add-scaled argb (nums-div (nums-add-scaled (nums-scale f (* 2 (- b a)))
									ones (+ (* 2 a w) w)) by) (<< 1 shift)))
							'(24 16 8 0) (fill (nums) n 0)))))
				(setq lo hi))
			(setq o0 o1 c0 c1))
		stops)
	(if (< lo +paint_lut_size) (push parts (fill (nums) (- +paint_lut_size lo) c0)))
	(apply (const cat) parts))

(defun canvas-gradient (canvas kind stops head)
	; (canvas-gradient canvas kind stops head) -> canvas
	;a gradient's +paint, and the canvas told to draw with it, (canvas-paint):
	;its head, a nums, (spread a0 ax ay b0 bx by focus), its lut, each written
	;whole by the one call, and after them a row as wide as the canvas's
	;pixmap, for the span to work in, of whatever is there
	(defq ss (string-stream (cat "")))
	(write-blk ss (obj-set (str-alloc +paint_lut) +str_data +paint_lut head))
	(write-char ss (apply (const list) (canvas-gradient-lut stops)) +int_size)
	(write-blk ss (str-alloc (* +int_size (getf (getf canvas +canvas_pixmap 0) +pixmap_width 0))))
	(canvas-paint canvas kind (str ss)))

(defun canvas-gradient-matrix (canvas kind spread stops g m focus)
	; (canvas-gradient-matrix canvas kind spread stops g m focus) -> :nil | canvas
	;a gradient whose own space, u along its line or out from its centre, v
	;across, g takes to the space m takes to the pixels, both (a b tx c d
	;ty), m none or a mat3x2. Of the two together the one back from a pixel
	;to u and v, in doubles, then two rows of 32 bits of fraction of the
	;pixel's centre, the head of its +paint, (canvas-gradient), with a
	;radial's focus after them, a double, 0 of one about its centre. :nil if
	;it flattens all to a line
	(bind '(g0 g1 g2 g3 g4 g5) g)
	(bind '(m0 m1 m2 m3 m4 m5) (map (const n2d) (ifn m '(1.0 0.0 0.0 0.0 1.0 0.0))))
	(defq c0 (+ (* m0 g0) (* m1 g3)) c1 (+ (* m0 g1) (* m1 g4)) c2 (+ (* m0 g2) (* m1 g5) m2)
		c3 (+ (* m3 g0) (* m4 g3)) c4 (+ (* m3 g1) (* m4 g4)) c5 (+ (* m3 g2) (* m4 g5) m5)
		det (- (* c0 c4) (* c1 c3)))
	(unless (< (abs det) (const (/ (n2d 1) (n2d 10000000000))))
		(defq i0 (/ c4 det) i1 (/ (neg c1) det) i2 (/ (- (* c1 c5) (* c4 c2)) det)
			i3 (/ (neg c3) det) i4 (/ c0 det) i5 (/ (- (* c3 c2) (* c0 c5)) det)
			h (const (n2d 0.5)) k (const (n2d (<< 1 32))))
		;straight into the nums that is its head, after its spread
		(canvas-gradient canvas kind stops
			(push (reduce (# (push %0 (n2i (* %1 k))))
					(list (+ (* i0 h) (* i1 h) i2) i0 i1 (+ (* i3 h) (* i4 h) i5) i3 i4)
					(nums (ifn spread +spread_pad)))
				(n2i (* focus (const (n2d 65536))))))))

(defclass Canvas-base () (View)
	(def this :paper 0)

	(defmethod :draw ()
		; (. canvas :draw) -> canvas
		(when (defq texture (getf this +canvas_texture 0))
			(bind '(tid tw th) (texture-sized texture))
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
		;to the pixmap. On a node with no GUI there is no texture, the swap
		;is :nil and there is nothing on the screen to be dirty
		(if ((const (ffi "gui/canvas/lisp_swap")) this flags) (. this :dirty) this))

	(defmethod :free ()
		; (. canvas :free) -> canvas
		;let go of the pixmap, the texture stays as it is. For a canvas
		;that is only ever shown, it can not be drawn on after this.
		((const (ffi "gui/canvas/lisp_swap")) this (const (+ +swap_flag_free +swap_flag_keep))))

	(defmethod :exchange (that)
		; (. canvas :exchange that) -> canvas
		;the two canvases exchange what they show, their textures. So one
		;can be off screen, drawn into by the GPU, (canvas-target), and put
		;on show when the frame is whole. They must be the same size.
		(defq tex (getf this +canvas_texture 0) flags (getf this +canvas_flags 0)
			that_flags (getf that +canvas_flags 0)
			mask (const +canvas_flag_shader) keep (const (logxor +canvas_flag_shader -1)))
		(setf this +canvas_texture (getf that +canvas_texture 0) 0)
		(setf that +canvas_texture tex 0)
		(setf this +canvas_flags (logior (logand flags keep) (logand that_flags mask)) 0)
		(setf that +canvas_flags (logior (logand that_flags keep) (logand flags mask)) 0)
		(. this :dirty))

	(defmethod :resize (that)
		; (. canvas :resize canvas) -> canvas
		((const (ffi "gui/canvas/lisp_resize")) this that))

	(defmethod :flip_x (that)
		; (. canvas :flip_x canvas) -> canvas
		((const (ffi "gui/canvas/lisp_flip_x")) this that))

	(defmethod :fill (argb)
		; (. canvas :fill argb) -> canvas
		((const (ffi "gui/canvas/lisp_fill")) this argb))

	(defmethod :clear (argb x y width height)
		; (. canvas :clear argb x y width height) -> canvas
		;a box of the pixmap, in its pixels, is made the color. It is set,
		;not laid over what is there: with 0 the box is made clear, to be
		;drawn in again, which nothing that draws can do. (:fill) is this
		;of all of it. The function is looked for when this is called, a
		;system from before it was written has none, and can still load this
		((ffi "gui/canvas/lisp_clear") this argb x y width height))

	(defmethod :tile (data x1 y1 x2 y2)
		; (. canvas :tile data x1 y1 x2 y2) -> area
		((const (ffi "gui/canvas/lisp_tile")) this data x1 y1 x2 y2))

	(defmethod :set_canvas_flags (flags)
		; (. canvas :set_canvas_flags flags) -> canvas
		(setf this +canvas_flags flags 0))

	(defmethod :set_color (argb)
		; (. canvas :set_color argb) -> canvas
		;what is drawn from here is this color, a paint let go of
		(if (getf this +canvas_span_data 0) (canvas-paint this 0 :nil))
		(setf this +canvas_color argb 0))

	(defmethod :set_linear (x y x1 y1 stops &optional spread m)
		; (. canvas :set_linear x y x1 y1 stops [spread m]) -> canvas
		;what is drawn from here is a linear gradient, t 0 at x y and 1 at x1
		;y1, its bands across that line; m, default none, the matrix from the
		;space they are in to the pixels :fpoly draws in, as an SVG's is,
		;under which the bands may lie at a slant to the line. stops are
		;((offset argb) ...), offsets 0 to 1, spread +spread_pad, the
		;default, +spread_reflect or +spread_repeat. Of no length it is the
		;color of the last stop
		(bind '(x y x1 y1) (map (const n2d) (list x y x1 y1)))
		(defq dx (- x1 x) dy (- y1 y))
		(if (canvas-gradient-matrix this 1 spread stops (list dx (neg dy) x dy dx y) m (const (n2d 0))) this
			(. this :set_color (second (last stops)))))

	(defmethod :set_radial (cx cy r stops &optional spread m fx fy)
		; (. canvas :set_radial cx cy r stops [spread m fx fy]) -> canvas
		;what is drawn from here is a radial gradient, t 0 at cx cy and 1 at r
		;from it, as (:set_linear): under m it may be an ellipse. Of no
		;radius it is the color of the last stop. With a focus, fx fy, t is 0
		;there and 1 on the circle, the colors go out from it, nearer on the
		;side it is to: one outside the circle is brought to just inside it.
		;From the focus, f from the centre of a circle of radius 1, t is f
		;times u, along the way to the centre, and the length of u and v, in
		;a space turned to that way and made narrower by 1 less f squared
		;along it and by its root across: so the span has the one number, f
		(bind '(cx cy r) (map (const n2d) (list cx cy r)))
		(defq zero (const (n2d 0)) one (const (n2d 1)) most (const (n2d 0.99))
			ux (if (and fx (/= r zero)) (/ (- (n2d fx) cx) r) zero)
			uy (if (and fy (/= r zero)) (/ (- (n2d fy) cy) r) zero)
			f (sqrt (+ (* ux ux) (* uy uy))) g (list r zero cx zero r cy))
		(cond
			((< f (const (/ (n2d 1) (n2d 100000)))) (setq f zero))
			(:t (if (> f most) (setq ux (/ (* ux most) f) uy (/ (* uy most) f) f most))
				(defq a (- one (* f f)) ra (sqrt a) c (/ ux f) s (/ uy f))
				(setq g (list (* r c a) (neg (* r s ra)) (+ cx (* r ux))
					(* r s a) (* r c ra) (+ cy (* r uy))))))
		(if (canvas-gradient-matrix this 2 spread stops g m f) this
			(. this :set_color (second (last stops)))))

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
		this))

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

(defun canvas-attach (held width height scale key &optional flags)
	; (canvas-attach held width height scale key [flags]) -> :nil | canvas
	;the canvas of the shared pixels of a key, (canvas-shared), given the
	;canvas flags, kept in held as (pixmap-attach) keeps a pixmap, and
	;found again only when the key or the size is not the one it was
	;found by. :nil if the key is 0 or the pixels can not be reached
	(unless (eql (first held) (defq want (list key width height scale)))
		(clear held)
		(push held want (and (/= key 0) (defq canvas (canvas-shared width height scale key))
				(if flags (. canvas :set_canvas_flags flags) canvas))))
	(second held))

(defun canvas-info (file)
	; (canvas-info file) -> (width height type) | (-1 -1 -1)
	(or (and (defq kind (canvas-loader-of file)) (defq stream (file-stream file))
			((second kind) stream))
		(list -1 -1 -1)))

(defun canvas-save (canvas file type &rest optionals)
	; (canvas-save canvas file type [optionals...]) -> :nil | canvas
	(and (defq kind (canvas-loader-of file)) (defq save (elem-get kind 3))
		(defq stream (file-stream file +file_open_write))
		(apply save (cat (list canvas stream type) optionals))))

(defun canvas-load (file flags &optional swap_mode stream)
	; (canvas-load file flags [swap_mode stream]) -> :nil | canvas
	;the picture of a file, its kind by its name, read from the stream if
	;given, as for an object of a store, lib/files/names.inc
	(defq this :nil)
	(setd swap_mode +pixmap_mode_normal)
	(when (bits? flags +load_flag_shared)
		;check cache first
		(when (defq this (. *pixmap_cache* :find file))
			(defq this (Canvas-pixmap this))))
	(when (and (not this) (setq stream (ifn stream (file-stream file))))
		;load and maybe add to cache
		(defq this (if (defq kind (canvas-loader-of file)) ((third kind) stream)))
		(when this
			(defq pixmap (getf this +canvas_pixmap 0))
			(if (bits? flags +load_flag_shared)
				(. *pixmap_cache* :insert file pixmap))
			(ifn (bits? flags +load_flag_noswap)
				(. this :swap (if (bits? flags +load_flag_free)
						(+ swap_mode +swap_flag_free) swap_mode)))))
	this)

(defun canvas-target (canvas)
	; (canvas-target canvas) -> 0 | handle
	;the texture of a canvas as the GUI of this node has it, made one the
	;GPU can draw triangles into if it is not: what a frame names for the
	;GPU's child of lib/gpu/tris.inc to draw straight into. What it holds
	;is not set till it is drawn into, so it is of a canvas that is not on
	;show, which gives one that is what it has when a frame is whole,
	;(:exchange). 0 on a node with no GUI, or whose GUI has none. Asked
	;each frame, an (:exchange) or a (:swap) gives a canvas another texture
	((const (ffi "gui/canvas/lisp_target")) canvas))

(defun canvas-flush ()
	; (canvas-flush)
	;flush any shared pixmaps that have no users.
	;4 refs are held by the cache Lmap and this loop !
	(defq new_cache (Lmap))
	(. *pixmap_cache* :each (lambda (key val)
			(if (> (getf val +obj_count 0) 4)
				(. new_cache :insert key val))))
	(def *root_env* '*pixmap_cache* new_cache))

;the loaders, see *canvas_loaders* above. Last, all of this file is there
;for them
(import "lib/image/svg.inc")
(import "lib/image/cwb.inc")
(import "lib/image/tga.inc")
(import "lib/image/cpm.inc")
(import "lib/image/png.inc")
(import "lib/image/jpg.inc")
(import "lib/media/flm.inc")
(import "lib/media/mp4.inc")
(import "lib/media/mp3.inc")
(import "lib/media/flac.inc")
