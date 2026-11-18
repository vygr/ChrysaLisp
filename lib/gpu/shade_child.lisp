;the child of lib/gpu/shade.inc, it has the GPU shade tiles of a shader
;onto the shared pixels of its app's canvas

(import "gui/pixmap/lisp.inc")
(import "./kernel.inc")
(import "./shade.inc")

;the shader last asked for: the pixel shader, for the layout of its
;inputs, the kernel made of it, and that on the GPU. A buffer of the
;GPU's for the pixels of a tile, as big as the biggest so far. And the
;pixels of the app's canvas, in shared memory, (pixmap-attach)
(defq shader_file "" pixel :nil program :nil kernel :nil out_buf :nil out_size 0 held (list))

(defun tile (key mbox x y x1 y1 height width file_length canvas_key tail)
	;the tile worked out on the GPU, and read back onto the app's pixels
	;where it belongs. tail is the path of the shader file, then the inputs
	;block of the pixel shader, which the tile's four are put after
	(defq file (slice tail 0 file_length) drawn 0 w (- x1 x) n (* w (- y1 y))
		pixmap (pixmap-attach held width height canvas_key))
	(unless (eql file shader_file)
		(if kernel (gpu-kernel-free kernel))
		(setq shader_file file pixel (shader-load file) program (shader-pixel-kernel file)
			kernel (and program (> (gpu-format) 0) (shader-gpu-kernel program))))
	(when (and kernel pixmap (> n 0))
		(when (> (* n +int_size) out_size)
			(if out_buf (gpu-buffer-free out_buf))
			(setq out_size (* n +int_size) out_buf (gpu-buffer out_size)))
		(and out_buf
			(shader-gpu-run program kernel
				(shader-pack program (cat (shader-unpack pixel (slice tail file_length -1))
					(list (list 'tile_x x) (list 'tile_y y) (list 'tile_w w) (list 'tile_top height))))
				0 n (list out_buf))
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
			(setq drawn 1)))
	(mail-send mbox (setf-> (str-alloc +shade_reply_size)
			(+job_reply_key key) (+shade_reply_drawn drawn))))

(defun main ()
	(job-child 5000000 (lambda (msg mbox)
			(apply tile (push (getf-> msg +job_key +job_reply
						+tile_x +tile_y +tile_x1 +tile_y1 +tile_height +tile_width
						+tile_file_length +tile_shared)
					(slice msg +tile_file -1)))))
	(if kernel (gpu-kernel-free kernel))
	(if out_buf (gpu-buffer-free out_buf)))
