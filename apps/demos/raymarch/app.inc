;the message for a tile of a frame, and its answer

(import "lib/task/jobs.inc")

;a tile for a child to march, the key of the shared pixels it is drawn
;on, 0 if there are none, and where the camera and the light are
(structure +tile +job_size
	(uint x y x1 y1)
	(long shared)
	(real w h cam_z light_x light_z))

;its answer, the tile, and then its pixels if the child could not reach
;the shared ones
(structure +tile_reply +job_reply_size
	(uint x y x1 y1)
	(offset pixels))
