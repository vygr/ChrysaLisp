;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; compute on the GPU, the host's GPU table, src/host/gpu_sdl3.cpp
;
; Buffers on the GPU, kernels that read and write them, and what they
; hold read back. Work goes in order on the one queue of the GPU: a write,
; a dispatch and a read of what it wrote need no wait between them. A
; dispatch and a read give a ticket, waited for by (gpu-wait), which looks
; each half a millisecond and gives way to the node's other tasks between.
; A node with no GPU, its host on no SDL3, has a (gpu-format) of 0.
;
; And triangles: a pair of shaders draws meshes into a target, a frame of
; them, with pictures to read from, (gpu-draw). The target is one of this
; node's GPU, and the frame is read back from it; or it is a texture of
; the GUI, on the node that has the GUI, drawn into where it is shown.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "class/buf/lisp.inc")

;module
(env-push)

(ffi "sys/gpu/lisp_call" gpu-call)

(enums +gpu 0
	(enum format buffer_create buffer_destroy buffer_write buffer_read done take
		kernel_create kernel_destroy dispatch
		pair_create pair_destroy mesh_create mesh_destroy picture_create picture_destroy
		target_create target_destroy draw))

(defun gpu-format ()
	; (gpu-format) -> 0 | 1 | 2
	;the language a kernel is given in, 1 MSL, 2 SPIR-V, 0 if no GPU
	(max 0 (gpu-call +gpu_format 0 0 0 0 0 0 0)))

(defun gpu-buffer (size)
	; (gpu-buffer size) -> :nil | buffer
	;a buffer of size bytes on the GPU
	(if (> (defq b (gpu-call +gpu_buffer_create size 0 0 0 0 0 0)) 0) b))

(defun gpu-buffer-free (b)
	; (gpu-buffer-free buffer) -> :nil
	(gpu-call +gpu_buffer_destroy b 0 0 0 0 0 0) :nil)

(defun gpu-bytes (data)
	; (gpu-bytes str | nums) -> bytes
	;how many bytes data has, a str's, or a vector's elements, a floats 4
	;each and the other kinds 8
	(cond
		((str? data) (length data))
		((floats? data) (* (length data) +int_size))
		((* (length data) +long_size))))

(defun gpu-write (b offset data)
	; (gpu-write buffer offset str | nums) -> :t | :nil
	;the bytes of data into the buffer at offset, a str's or a vector's
	;elements, in order after what was given before, not waited for
	(= (gpu-call +gpu_buffer_write b offset data (gpu-bytes data) 0 0 0) 1))

(defun gpu-wait (ticket)
	; (gpu-wait ticket) -> ticket
	;once the work of the ticket is over, giving way to other tasks till then
	(until (= (gpu-call +gpu_done ticket 0 0 0 0 0 0) 1) (task-sleep 500))
	ticket)

(defun gpu-take (ticket)
	; (gpu-take ticket) -> ticket
	;a ticket let go of, once its work is over, (gpu-wait)
	(gpu-call +gpu_take ticket 0 0 0 0 0 0)
	ticket)

(defun gpu-read (b offset into &optional block at)
	; (gpu-read buffer offset len | nums [buf at]) -> :nil | str | nums | buf
	;the buffer at offset, once what was given before is done: len bytes as
	;a new str, or into all of a vector of numbers given, as many bytes as
	;its elements have. Given a buf, len bytes go into its block from at
	;on, with no copy between: the pixels of a frame straight onto a
	;pixmap that is shared. They must fit, or it throws
	(defq len (if (num? into) into (gpu-bytes into)) at (ifn at 0))
	(and block (not (<= 0 at (+ at len) (buf-size block)))
		(throw "A read past the end of a buf !" (list at len)))
	(when (> (defq ticket (gpu-call +gpu_buffer_read b offset len 0 0 0 0)) 0)
		(gpu-wait ticket)
		(defq data (cond (block) ((num? into) (str-alloc len)) (into)))
		(gpu-call +gpu_take ticket data len at 0 0 0)
		data))

(defun gpu-kernel (code layout)
	; (gpu-kernel code (reads writes uniforms tx ty tz)) -> :nil | kernel
	;a kernel of code, MSL text or a SPIR-V module, of the language
	;(gpu-format) says, its function kernel_main. It reads buffers, reads
	;and writes buffers, has a block of uniforms if uniforms is 1, and runs
	;in groups of tx ty tz threads. It is built on a thread of the host,
	;(gpu-dispatch) waits for that
	(defq l (str-alloc 24))
	(each (# (set-int l (* (!) 4) %0)) layout)
	(if (> (defq k (gpu-call +gpu_kernel_create code (length code) l 0 0 0 0)) 0) k))

(defun gpu-kernel-free (k)
	; (gpu-kernel-free kernel) -> :nil
	(gpu-call +gpu_kernel_destroy k 0 0 0 0 0 0) :nil)

(defun gpu-dispatch (k buffers uniforms groups)
	; (gpu-dispatch kernel (buffer ...) uniforms (x y z)) -> :nil | ticket
	;the kernel run over groups of its threads, the buffers it reads then
	;those it writes, uniforms a str of its block, a vector, or :nil. Waits for the
	;kernel to be built, :nil if it did not build
	(defq bs (str-alloc (* 8 (max 1 (length buffers)))) gs (str-alloc 12))
	(each (# (set-long bs (* (!) 8) %0)) buffers)
	(each (# (set-int gs (* (!) 4) %0)) groups)
	(while (= 0 (defq ticket (gpu-call +gpu_dispatch k bs (ifn uniforms 0) (if uniforms (gpu-bytes uniforms) 0) gs 0 0)))
		(task-sleep 500))
	(if (> ticket 0) ticket))

(defun gpu-run (k buffers uniforms groups)
	; (gpu-run kernel (buffer ...) uniforms (x y z)) -> :t | :nil
	;(gpu-dispatch) and wait for it to be over
	(when (defq ticket (gpu-dispatch k buffers uniforms groups))
		(gpu-take (gpu-wait ticket))
		:t))

;;;;;;;;;;;
; triangles
;;;;;;;;;;;

(defun gpu-pair (vertex fragment layout)
	; (gpu-pair vertex fragment layout) -> :nil | pair
	;a vertex shader and a pixel shader that draw triangles, MSL text or
	;SPIR-V modules, of the language (gpu-format) says, their functions
	;vertex_main and fragment_main. layout is a str, a byte each: how many
	;attrs a vertex has, the cull, 0 none, 1 those that face away, 2
	;those that face us, how many floats each attr is, and how many
	;pictures the pixel shader reads from. It is built on a thread of the
	;host, a (gpu-draw) with it gives 0 till it is
	(if (> (defq p (gpu-call +gpu_pair_create vertex (length vertex) fragment (length fragment) layout 0 0)) 0) p))

(defun gpu-pair-free (p)
	; (gpu-pair-free pair) -> :nil
	(gpu-call +gpu_pair_destroy p 0 0 0 0 0 0) :nil)

(defun gpu-mesh (verts)
	; (gpu-mesh verts) -> :nil | mesh
	;the vertices of a mesh, kept on the GPU as floats. verts is their
	;bytes, a double a number, the attrs of a vertex one after another
	(if (> (defq m (gpu-call +gpu_mesh_create verts (gpu-bytes verts) 0 0 0 0 0)) 0) m))

(defun gpu-mesh-free (m)
	; (gpu-mesh-free mesh) -> :nil
	(gpu-call +gpu_mesh_destroy m 0 0 0 0 0 0) :nil)

(defun gpu-picture (pixels width height)
	; (gpu-picture pixels width height) -> :nil | picture
	;a picture a pixel shader reads from, a copy on the GPU of width by
	;height pixels of 32 bits, as a pixmap has them, the top row first.
	;pixels is a str of them, or a buf they are the block of
	(if (> (defq p (gpu-call +gpu_picture_create pixels width height 0 0 0 0)) 0) p))

(defun gpu-picture-free (p)
	; (gpu-picture-free picture) -> :nil
	(gpu-call +gpu_picture_destroy p 0 0 0 0 0 0) :nil)

(defun gpu-target (width height)
	; (gpu-target width height) -> :nil | target
	;a target of this node's GPU to draw triangles into, with its depth
	;buffer, and what is drawn is read back from it
	(if (> (defq target (gpu-call +gpu_target_create width height 0 0 0 0 0)) 0) target))

(defun gpu-target-free (target)
	; (gpu-target-free target) -> :nil
	(gpu-call +gpu_target_destroy target 0 0 0 0 0 0) :nil)

(defun gpu-pad8 (block)
	;a block made up to a whole 8 bytes
	(cat block (slice (const (char 0 8)) 0 (- (logand (+ (length block) 7) -8) (length block)))))

(defun gpu-draw-frame (draws)
	; (gpu-draw-frame draws) -> str
	;a frame of triangles as (gpu-draw) takes it. draws is a list of (pair
	;mesh vblock pblock [pictures]): a pair, a mesh, the blocks of the
	;inputs of the pair's two shaders, and a list of the pictures its
	;pixel shader reads from, in the order it has them, as many as it has
	(apply (const cat) (cat (list (char (length draws) 8))
			(map (lambda ((pair mesh vblock pblock &optional pictures))
					;a shader with no inputs still has a block, of nothing
					(if (empty? vblock) (setq vblock (const (char 0 8))))
					(if (empty? pblock) (setq pblock (const (char 0 8))))
					(apply (const cat) (cat (list (char pair 8) (char mesh 8) (char (length vblock) 8) (char (length pblock) 8)
								(gpu-pad8 vblock) (gpu-pad8 pblock)) (map (# (char %0 8)) (ifn pictures '()))))) draws))))

(defun gpu-draw (target frame &optional gui y y1)
	; (gpu-draw target frame [gui y y1]) -> 0 | :nil | ticket
	;a frame of triangles, of (gpu-draw-frame), drawn into a target, cleared
	;first, what is nearest is what is seen. The target is one of
	;(gpu-target), and a ticket is given: (gpu-take-pixels) has what was
	;drawn. With gui the target is the handle of a texture of the GUI, on
	;the node that has it, the frame is drawn into it where it is, and its
	;ticket has no pixels: (gpu-wait) says when the GPU is done with it,
	;(gpu-take) lets it go. 0 is nothing drawn, a pair is still
	;being built, or the GUI's texture is not to be drawn into yet: try
	;again. :nil is a frame that can not be drawn, a pair that did not
	;build, a target that is not one, a node with no GPU. A frame of flat
	;pairs, (shader-gpu-pair), can be drawn the rows y to y1 of it only,
	;the rest of the target left as it was: a slow GPU is given a long
	;frame some rows at a time, and the GUI, drawn by the same GPU, has its
	;turn between
	(defq out (gpu-call +gpu_draw target frame (length frame) (if gui 1 0) (ifn y 0) (ifn y1 0) 0))
	(cond
		((= out 0) 0)
		((< out 0) :nil)
		(out)))

(defun gpu-take-pixels (ticket len &optional block at)
	; (gpu-take-pixels ticket len [buf at]) -> str | buf
	;what a (gpu-draw) into a target of this node drew, once it is over:
	;len bytes of it, the pixels of 32 bits, the top row first, as a new
	;str, or into the block of a buf from at on, with no copy between.
	;The ticket is let go of. They must fit, or it throws
	(defq at (ifn at 0))
	(and block (not (<= 0 at (+ at len) (buf-size block)))
		(throw "A read past the end of a buf !" (list at len)))
	(gpu-wait ticket)
	(defq data (ifn block (str-alloc len)))
	(gpu-call +gpu_take ticket data len at 0 0 0)
	data)

;module
(export-symbols '(gpu-call gpu-format gpu-buffer gpu-buffer-free gpu-bytes gpu-write gpu-read gpu-wait gpu-take
		gpu-kernel gpu-kernel-free gpu-dispatch gpu-run
		gpu-pair gpu-pair-free gpu-mesh gpu-mesh-free gpu-picture gpu-picture-free
		gpu-target gpu-target-free gpu-draw-frame gpu-draw gpu-take-pixels))
(env-pop)
