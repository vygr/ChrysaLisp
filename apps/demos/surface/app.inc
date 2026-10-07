;the shader, and the message for a tile of it

(defq +shader_file "lib/gpu/shaders/raymarch.shader")

(import "lib/task/jobs.inc")

;a tile for a child to shade, the height and width of the frame, the key
;of the shared pixels it is drawn on, 0 if there are none, then the
;inputs block
(structure +tile +job_size
	(uint x y x1 y1 height width)
	(long shared)
	(offset inputs))

;its answer, the tile, and then its pixels if the child could not reach
;the shared ones
(structure +tile_reply +job_reply_size
	(uint x y x1 y1)
	(offset pixels))
