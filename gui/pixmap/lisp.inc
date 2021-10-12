; Lisp (getf ...)

(import "class/obj/lisp.inc")
(import "lib/xml/svg.inc")

(structure +pixmap +obj_size
	(ptr texture stream bufp)
	(uint stream_type width height stride type)
	(align)
	(offset data))

;module
(env-push)

(structure +cpm 0
	(uint ident)		;ident .CPM or .FLM
	(uint bytes)		;0 if not known, else total size
	(uint version)		;version number
	(uint type)			;image type
	(uint width)		;image w
	(uint height)		;image h
	(uint trans))		;image transparent colour

(structure +tga 0
	(ubyte imagedata)	;id field offset
	(ubyte colmap)		;color map
	(ubyte type)		;targa type
	(ubyte a b)			;nint16 colmap_fei	;color map origin (non aligned !)
	(ubyte c d)			;nint16 colmap_num	;color map number (non aligned !)
	(ubyte colmap_size)	;color map size
	(struct pad 4)		;padding ?
	(ushort width)
	(ushort height)
	(ubyte depth)
	(ubyte origin))

(defun pixmap-cpm-info (stream)
	; (pixmap-cpm-info stream) -> (width height type) | (-1 -1 -1)
	(if (= (length (defq header (read-data stream +cpm_size))) +cpm_size)
		(case (char (getf header +cpm_ident) +int_size)
			(("MPC." "MLF.")
				(list (getf header +cpm_width)
					(getf header +cpm_height)
					(getf header +cpm_type)))
			(t (list -1 -1 -1)))
		(list -1 -1 -1)))

(defun pixmap-tga-info (stream)
	; (pixmap-tga-info stream) -> (width height type) | (-1 -1 -1)
	(if (= (length (defq header (read-data stream +tga_size))) +tga_size)
		(case (getf header +tga_type)
			(2 (list (getf header +tga_width)
					(getf header +tga_height)
					(getf header +tga_depth)))
			(t (list -1 -1 -1)))
		(list -1 -1 -1)))

(defun pixmap-info (file)
	; (pixmap-info file) -> (width height type) | (-1 -1 -1)
	(if (defq stream (file-stream file))
		(cond
			((ends-with ".cpm" file)
				(pixmap-cpm-info stream))
			((ends-with ".flm" file)
				(pixmap-cpm-info stream))
			((ends-with ".tga" file)
				(pixmap-tga-info stream))
			((ends-with ".svg" file)
				(SVG-info stream))
			(t (list -1 -1 -1)))
		(list -1 -1 -1)))

(defun pixmap-save (pixmap file type)
	; (pixmap-save pixmap file type) -> nil | pixmap
	(if (defq stream (file-stream file +file_open_write))
		(cond
			((ends-with ".cpm" file)
				((const (ffi _ "gui/pixmap/lisp_save_cpm" 0)) pixmap stream type)))))

(defun pixmap-load (file)
	; (pixmap-load file) -> nil | pixmap
	(if (defq stream (file-stream file))
		(cond
			((ends-with ".cpm" file)
				((const (ffi _ "gui/pixmap/lisp_load_cpm" 0)) stream))
			((ends-with ".tga" file)
				((const (ffi _ "gui/pixmap/lisp_load_tga" 0)) stream)))))

;module
(export-symbols
	pixmap-info pixmap-tga-info pixmap-cpm-info
	pixmap-save pixmap-load)
(env-pop)
