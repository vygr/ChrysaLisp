(import "././stream/struct.inc")

(structure +lstream +stream_size
	(ptr lisp fill seek write fault)
	(long buf_len))
