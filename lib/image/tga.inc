;;;;;;;;;;;;;
; tga loader
;;;;;;;;;;;;;

(import "gui/canvas/lisp.inc")

;module
(env-push)

(structure +tga 0
	(ubyte imagedata)	;id field offset
	(ubyte colmap)		;color map
	(ubyte type)		;targa type
	(ubyte a b)			;nint16 colmap_fei	;color map origin (non aligned !)
	(ubyte c d)			;nint16 colmap_num	;color map number (non aligned !)
	(ubyte colmap_size) ;color map size
	(struct pad 4)		;padding ?
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
			(ifn (pixmap-read pixmap stream type)
				(setq canvas :nil))))
	canvas)

(defun TGA-save (canvas stream type)
	; (TGA-save canvas stream type) -> canvas
	;a canvas as a .tga, not packed, 32 bits a pixel whatever type is,
	;the top row first. It is the pixels as they are with a header of 18
	;bytes before them, so it is quick, and most things that show
	;pictures show it
	(defq pixmap (getf canvas +canvas_pixmap 0))
	;the header, every byte of it said: no id, no colour map, type 2, the
	;size, 32 bits, and 0x28 is 8 bits of alpha and the top row first.
	;(str-alloc) is not empty, what is not set in it is whatever was there
	(write-blk stream (cat (char 0) (char 0) (char 2) (char 0 8) (char 0)
		(char (getf pixmap +pixmap_width 0) 2) (char (getf pixmap +pixmap_height 0) 2)
		(char 32) (char 0x28)))
	(pixmap-write (pixmap-as-argb pixmap) stream 32)
	canvas)

;module
(export-symbols '(TGA-load TGA-info TGA-save))
(env-pop)
