;the child of lib/gpu/shade.inc, it has the GPU shade tiles of a shader
;onto the shared pixels of its app's canvas

(import "gui/pixmap/lisp.inc")
(import "./kernel.inc")
(import "./render.inc")
(import "./shade.inc")

;the shader last asked for: the pixel shader, for the layout of its
;inputs, the kernel made of it, and that on the GPU. A buffer of the
;GPU's for the pixels of a tile, as big as the biggest so far. And the
;pixels of the app's canvas, in shared memory, (pixmap-attach). And how
;many pixels the GPU is given at a time, (shader-gpu-run-paced)
(defq shader_file "" pixel :nil program :nil kernel :nil out_buf :nil out_size 0 held (list)
	pace (list 4096))

;for a frame drawn straight into a texture of the GUI: the pixel shader
;as a flat pair with the corners of a frame, made the first time it is
;wanted, :none if it could not be, and the two triangles that cover a
;frame. And how many rows of a frame the GPU is given at a time
(defq pair :nil corners :nil rows 16
	;what the GPU is to take over the rows it is given, and the least
	;rows: it can not be stopped part way through a draw, and the GUI is
	;drawn by the same GPU, so a draw that takes it long stops the desktop
	+rows_time 12000 +rows_least 4)

(defun direct (handle block height main)
	; (direct handle block height main) -> :nil | :gone | :t
	;the frame drawn by the GPU straight into a texture of the GUI's, block
	;the inputs of the pixel shader. Some rows at a time, each waited for,
	;so the GUI has its turn between, and as many rows as the GPU does in
	;about a tick of the GUI: a fast one takes all of a frame as the one
	;draw. :nil if it can not be, :gone if the child is told to go while it
	;waits for the GPU
	(unless pair
		(setq pair (ifn (shader-gpu-pair (shader-load +shade_vertex_file) pixel :nil :t) :none)
			corners (gpu-mesh (apply (const doubles) (map (const n2d) '(-1 -1 1 -1 1 1 -1 -1 1 1 -1 1))))))
	(when (and (nql pair :none) corners)
		(defq frame (gpu-draw-frame (list (list pair corners "" block))) y 0 out :t)
		(while (and (eql out :t) (< y height))
			(defq y1 (min height (+ y rows)) ticket 0)
			(while (eql 0 (setq ticket (gpu-draw handle frame :t y y1)))
				(if (mail-poll (list main)) (setq ticket :gone) (task-sleep 500)))
			(cond
				((not ticket) (setq out :nil))
				((eql ticket :gone) (setq out :gone))
				(:t ;said when the GPU is done with them, so an app that gives
					;its next frame on the answer gives the GPU no more than it
					;can do, and how long it took sets how many rows are next
					(defq began (pii-time))
					(gpu-take (gpu-wait ticket))
					(defq took (max 500 (- (pii-time) began)))
					(setq rows (max +rows_least (min height (* rows 4) (/ (* (- y1 y) +rows_time) took)))
						y y1))))
		out))

(defun tile (key mbox x y x1 y1 height width file_length canvas_key handle main tail)
	;the tile worked out on the GPU, and read back onto the app's pixels
	;where it belongs. tail is the path of the shader file, then the inputs
	;block of the pixel shader, which the tile's four are put after
	(defq file (slice tail 0 file_length) drawn +shade_none w (- x1 x) n (* w (- y1 y))
		pixmap (pixmap-attach held width height canvas_key) gone :nil)
	(unless (eql file shader_file)
		(if kernel (gpu-kernel-free kernel))
		(and pair (nql pair :none) (gpu-pair-free pair))
		(setq shader_file file pixel (shader-load file) program (shader-pixel-kernel file) pair :nil rows 16
			kernel (and program (> (gpu-format) 0) (shader-gpu-kernel program))))
	;straight into the texture of the app's canvas, a tile that is all of
	;the frame, if this is the node that has it: the GPU of this node is
	;then the GUI's own
	(when (and (/= handle 0) (= w width) (= n (* width height)) (> (gpu-format) 0)
			(eql (slice mbox +long_size -1) (task-nodeid)))
		(defq out (direct handle (slice tail file_length -1) height main))
		(cond
			((eql out :gone) (setq gone :t))
			(out (setq drawn +shade_texture))))
	(when (and (= drawn +shade_none) (not gone) kernel pixmap (> n 0))
		(when (> (* n +int_size) out_size)
			(if out_buf (gpu-buffer-free out_buf))
			(setq out_size (* n +int_size) out_buf (gpu-buffer out_size)))
		(and out_buf
			(shader-gpu-run-paced program kernel
				(shader-pack program (cat (shader-unpack pixel (slice tail file_length -1))
					(list (list 'tile_x x) (list 'tile_y y) (list 'tile_w w) (list 'tile_top height))))
				0 n (list out_buf) pace)
			(defq block (pixmap-buf pixmap) row (* w +int_size))
			;a tile as wide as the pixels is read straight onto them, no copy
			;between; one not so wide is read, then put there a row at a time
			(if (= w width) (gpu-read out_buf 0 (* n +int_size) block (* y row))
				(when (defq pixels (gpu-read out_buf 0 (* n +int_size)))
					(each! (lambda ()
							(buf-set block (* (+ (* (+ y (!)) width) x) +int_size) row
								(slice pixels (* (!) row) (* (inc (!)) row))))
						'() 0 (- y1 y))
					:t))
			(setq drawn +shade_pixels)))
	(unless gone
		(mail-send mbox (setf-> (str-alloc +shade_reply_size)
				(+job_reply_key key) (+shade_reply_drawn drawn)))))

(defun main ()
	(job-child 5000000 (lambda (msg mbox)
			(apply tile (push (getf-> msg +job_key +job_reply
						+tile_x +tile_y +tile_x1 +tile_y1 +tile_height +tile_width
						+tile_file_length +tile_shared +tile_target)
					mbox (slice msg +tile_file -1)))))
	(and pair (nql pair :none) (gpu-pair-free pair))
	(if corners (gpu-mesh-free corners))
	(if kernel (gpu-kernel-free kernel))
	(if out_buf (gpu-buffer-free out_buf)))
