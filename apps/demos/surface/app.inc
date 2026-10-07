;the shader, and the message for a tile of it

(defq +shader_file "lib/gpu/shaders/raymarch.shader")

(structure +job 0
	(long key)
	(uint x y x1 y1)
	(netid reply)
	(uint height width)
	;the name of the shared pixmap the tile is drawn on, zeros after it
	(struct shared 32)
	(offset inputs))
