;a picture, of any kind there is a loader for, small. A film is a picture
;too, its first frame, but is left to film.inc, which says how long it is
(file-previewer
	(lambda (file) (and (canvas-loader-of file) (notany (# (ends-with (first %0) file)) *reel_kinds*)))
	(lambda (file most_w most_h) (file-picture file most_w most_h)))
