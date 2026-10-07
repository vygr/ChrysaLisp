;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A shader drawn with no GPU, a tile at a time, by the nodes.
;
; The app has a Jobs object, lib/task/jobs.inc, of the child here, and
; gives it a tile of the frame as a job, (shader-tile). The child has
; the shader as native code, the VP back end, and shades the tile
; straight into the app's pixels if they are in shared memory that it
; can reach, (canvas-shared), and answers with a +tile_reply. If it can
; not reach them the pixels follow the answer, and the app puts them on
; its canvas, (shader-tile-show).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././task/jobs.inc")

(defq +shader_tile_child "lib/gpu/tile_child.lisp")

;a tile for a child to shade. The height and width of the frame, the key
;of the shared pixels it is drawn on, 0 if there are none, then the path
;of the shader file, and then the inputs block
(structure +tile +job_size
	(uint x y x1 y1 height width file_length)
	(long shared)
	(offset file))

;its answer, the tile, and then its pixels if the child could not reach
;the shared ones
(structure +tile_reply +job_reply_size
	(uint x y x1 y1)
	(offset pixels))

(defun shader-tile (file inputs x y x1 y1 width height shared)
	; (shader-tile file inputs x y x1 y1 width height shared) -> job
	;the job for a tile of a frame of that width and height, inputs is the
	;block from (shader-pack), shared the key of the pixels, (canvas-key)
	(cat (setf-> (str-alloc +tile_size)
		(+tile_x x) (+tile_y y) (+tile_x1 x1) (+tile_y1 y1)
		(+tile_height height) (+tile_width width)
		(+tile_file_length (length file))
		(+tile_shared shared)) file inputs))

(defun shader-tile-show (canvas msg)
	; (shader-tile-show canvas msg) -> canvas
	;the answer to a tile. If the child could not reach the pixels of the
	;canvas they came with the answer, and are put there
	(when (> (length msg) +tile_reply_size)
		(bind '(x y x1 y1) (getf-> msg +tile_reply_x +tile_reply_y
			+tile_reply_x1 +tile_reply_y1))
		(. canvas :tile (slice msg +tile_reply_pixels -1) x y x1 y1))
	canvas)
