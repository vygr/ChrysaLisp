;;;;;;;;;;;;;
; tga loader
;;;;;;;;;;;;;

(import "gui/canvas/lisp.inc")

;module
(env-push)

(structure +tga 0
	(ubyte imagedata)   ;id field offset
	(ubyte colmap)      ;color map
	(ubyte type)        ;targa type
	(ubyte a b)         ;nint16 colmap_fei  ;color map origin (non aligned !)
	(ubyte c d)         ;nint16 colmap_num  ;color map number (non aligned !)
	(ubyte colmap_size) ;color map size
	(struct pad 4)      ;padding ?
	(ushort width)
	(ushort height)
	(ubyte depth)
	(ubyte origin))

(defun TGA-info (stream)
	; (TGA-info stream) -> (width height type) | (-1 -1 -1)
	(defq out '(-1 -1 -1))
	(when stream
		(defq header (read-blk stream +tga_size))
		(when (and (= (length header) +tga_size)
				(= (getf header +tga_type) 2)
				(or (= (getf header +tga_depth) 24)
					(= (getf header +tga_depth) 32)))
			; load the tuple directly
			(setq out (getf-> header +tga_width +tga_height +tga_depth))))
	out)

(defun TGA-load (stream)
	; (TGA-load stream) -> :nil | canvas
	(defq canvas :nil)
	(when stream
		(defq header (read-blk stream +tga_size))
		(when (and (= (length header) +tga_size)
				(= (getf header +tga_type) 2)
				(or (= (getf header +tga_depth) 24)
					(= (getf header +tga_depth) 32)))
			(bind '(w h type) (getf-> header +tga_width +tga_height +tga_depth))
			(defq canvas (Canvas w h 1) pixmap (getf canvas +canvas_pixmap 0))
			(setf pixmap +pixmap_type 32 0)
			; loop and copy direct to the pixmap's pixel buffer
			(ifn (pixmap-read pixmap stream type 0)
				(setq canvas :nil))))
	canvas)

;module
(export-symbols '(TGA-load TGA-info))
(env-pop)
