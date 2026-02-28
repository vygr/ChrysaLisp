;;;;;;;;;;;;
; tga loader
;;;;;;;;;;;;

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

(defun TGA-Canvas (stream &optional scale)
	; (TGA-Canvas stream [scale]) -> :nil | canvas
	(setd scale 1)
	(defq canvas :nil)
	(when stream
		(defq header (read-blk stream +tga_size))
		(when (and (= (length header) +tga_size)
				   (= (getf header +tga_type) 2)
				   (or (= (getf header +tga_depth) 24)
					   (= (getf header +tga_depth) 32)))
			(bind '(w h depth) (getf-> header +tga_width +tga_height +tga_depth))
			(setq canvas (Canvas w h scale))
			(. canvas :set_canvas_flags +canvas_flag_antialias)
			(defq bit_pool (array 0 0) y 0)
			(while (< y h)
				(defq x 0)
				(while (< x w)
					(defq col (read-bits stream bit_pool depth))
					(if (= col -1)
						; premature EOF
						(setq x w y h canvas :nil)
						(progn
							; opaque alpha for 24-bit TGA
							(if (= depth 24) (setq col (+ col +argb_black)))
							(.-> canvas (:set_color col) (:plot x y))
							(++ x))))
				(++ y)
				(task-slice))
			(when canvas
				; resize to final output
				(bind '(w h) (. canvas :pref_size))
				(setq canvas (. (Canvas w h 1) :resize canvas)))))
	canvas)

;module
(export-symbols '(TGA-Canvas TGA-info))
(env-pop)