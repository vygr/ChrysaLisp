;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shader language, kernels on the GPU.
; A kernel made for the GPU the node has, MSL or SPIR-V as
; (gpu-format) says, and run over a range of indices. The CPU runs the
; same kernel with (shader-vp-kernel), lib/gpu/vp.inc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././text/searching.inc")
(import "./msl.inc")
(import "./spirv.inc")
(import "./compute.inc")

(defun shader-gpu-kernel (program)
	; (shader-gpu-kernel program) -> :nil | kernel
	;the kernel for this node's GPU, :nil if it has none. It is built on
	;a thread of the host, (shader-gpu-run) waits for that
	(defq bufs (shader-buffers program) reads (length (filter (# (eql (third %0) :read)) bufs))
		layout (list reads (- (length bufs) reads) 1 +sh_kernel_threads 1 1))
	(case (gpu-format)
		(1 (gpu-kernel (shader-msl-kernel program) layout))
		(2 (gpu-kernel (shader-spirv-kernel program) layout))))

(defun shader-gpu-dispatch (program kernel block from to bufs)
	; (shader-gpu-dispatch program kernel block from to (buffer ...)) -> :nil | ticket
	;the kernel given to the GPU for each index from to to, not waited for:
	;what is given after it goes after it on the GPU's one queue, so a chain
	;of kernels needs a wait only at its end, (gpu-wait), or a read of what
	;they wrote. Each ticket is let go of by (gpu-take) once it is done. :nil
	;for no indices, or a kernel that did not build. block, bufs as
	;(shader-gpu-run) has them
	(unless (>= from to)
		(defq names (map (const first) (shader-buffers program)))
		(gpu-dispatch kernel
			(map (lambda ((name &ignore)) (elem-get bufs (find name names))) (sh-kernel-order program))
			(shader-range program block from to)
			(list (/ (+ (- to from) (dec +sh_kernel_threads)) +sh_kernel_threads) 1 1))))

(defun shader-gpu-run (program kernel block from to bufs)
	; (shader-gpu-run program kernel block from to (buffer ...)) -> :t | :nil
	;the kernel run for each index from to to, and waited for, the other
	;tasks of the node have their turns meanwhile. block is the inputs, as
	;(shader-pack) makes them, and bufs a GPU buffer for each buffer, in
	;the order the program has them. Nothing is bounds checked, so each
	;must be long enough, 4 bytes to an element
	(cond
		((>= from to))
		((defq ticket (shader-gpu-dispatch program kernel block from to bufs))
			(gpu-take (gpu-wait ticket))
			:t)))

;how long a GPU is to take over what it is given at once, in us, and the
;least it is given, a few groups of its threads. A GPU can not be stopped
;part way through what it has been given, and the GUI is drawn by the same
;one: a kernel that takes a slow GPU half a second holds the mouse as long
(defq +sh_piece_time 12000 +sh_piece_least 1024)

(defun shader-gpu-run-paced (program kernel block from to bufs pace)
	; (shader-gpu-run-paced program kernel block from to (buffer ...) pace) -> :t | :nil
	;as (shader-gpu-run), the indices given to the GPU a piece at a time,
	;each waited for, so the GUI has its turn between. pace is a list of
	;the one number, how many indices a piece is, which the caller keeps
	;from run to run: each piece is timed, and the next sized to what the
	;GPU does in about a tick of the GUI. A fast GPU is soon given all of
	;them as the one piece
	(defq ok :t)
	(while (and ok (< from to))
		(defq n (first pace) next (min to (+ from n)) began (pii-time))
		(cond
			((shader-gpu-run program kernel block from next bufs)
				(defq took (max 500 (- (pii-time) began)))
				;whole groups of threads
				(elem-set pace 0 (max +sh_piece_least
					(logand (+ (min (* n 4) (/ (* (- next from) +sh_piece_time) took)) 255) -256)))
				(setq from next))
			((setq ok :nil))))
	ok)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a pixel shader as a kernel
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;A pixel shader gives the colour of a pixel, a kernel is run for each of a
;range of indices and writes buffers. So a pixel shader is made a kernel
;in the language itself, and the back ends take it as any other: its main
;is kept as a function, and the main of the kernel takes the index of a
;pixel of a tile, row by row from its top, works out the frag coord of it,
;the centre of the bottom left pixel of the frame 0.5 0.5 and y going up,
;has the function give its colour, and writes that to a buffer as a 32 bit
;argb, clamped, its alpha full on, as (shader-vp-argb) gives one. The tile
;is four inputs after the shader's own, so a block of the pixel shader's
;inputs is the start of the kernel's
(defq +sh_pixel_main "(defun main " +sh_pixel_func "(defun tile_pixel "
	+sh_pixel_kernel (cat "\n"
		"(definput tile_x :int 0)\n(definput tile_y :int 0)\n"
		"(definput tile_w :int 1)\n(definput tile_top :int 1)\n"
		"(defbuffer tile_out :int :write)\n"
		"(defun main :void ((i :int))\n"
		"	(defq row (/ i tile_w)\n"
		"		c (tile_pixel (vec2 (+ (float (+ tile_x (- i (* row tile_w)))) 0.5)\n"
		"			(- (float (- tile_top (+ tile_y row))) 0.5))))\n"
		"	(elem-set tile_out i (logior 0xff000000\n"
		"		(<< (int (+ (* (clamp (:x c) 0.0 1.0) 255.0) 0.5)) 16)\n"
		"		(<< (int (+ (* (clamp (:y c) 0.0 1.0) 255.0) 0.5)) 8)\n"
		"		(int (+ (* (clamp (:z c) 0.0 1.0) 255.0) 0.5)))))\n"))

(defun shader-pixel-kernel (file)
	; (shader-pixel-kernel file) -> :nil | program
	;the kernel of a pixel shader's file, :nil if it has no main of a pixel
	;shader to make one of. Its inputs are the shader's own, then tile_x
	;and tile_y, the top left of the tile in the frame, y down as a canvas
	;has it, tile_w its width, and tile_top the height of the frame. Its
	;one buffer is the tile's pixels, an int each. It is run over as many
	;indices as the tile has pixels
	(when (found? (defq text (shader-text file)) +sh_pixel_main)
		(shader-from-text (cat (replace-str text +sh_pixel_main +sh_pixel_func) +sh_pixel_kernel))))
