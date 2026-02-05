(import "class/obj/lisp.inc")
(import "./struct.inc")
(import "lib/xml/svg.inc")

(defun pixmap-cpm-info (stream)
	; (pixmap-cpm-info stream) -> (width height type) | (-1 -1 -1)
	(if (= (length (defq header (read-blk stream +cpm_size))) +cpm_size)
		(case (char (getf header +cpm_ident) +int_size)
			(("MPC." "MLF.")
				(getf-> header +cpm_width +cpm_height +cpm_type))
			(:t (list -1 -1 -1)))
		(list -1 -1 -1)))

(defun pixmap-tga-info (stream)
	; (pixmap-tga-info stream) -> (width height type) | (-1 -1 -1)
	(if (= (length (defq header (read-blk stream +tga_size))) +tga_size)
		(case (getf header +tga_type)
			(2 (getf-> header +tga_width +tga_height +tga_depth))
			(:t (list -1 -1 -1)))
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
			(:t (list -1 -1 -1)))
		(list -1 -1 -1)))

(defun pixmap-save (pixmap file type)
	; (pixmap-save pixmap file type) -> :nil | pixmap
	(if (defq stream (file-stream file +file_open_write))
		(cond
			((ends-with ".cpm" file)
				((const (ffi "gui/pixmap/lisp_save_cpm")) pixmap stream type)))))

(defun pixmap-load (file)
	; (pixmap-load file) -> :nil | pixmap
	(if (defq stream (file-stream file))
		(cond
			((ends-with ".cpm" file)
				((const (ffi "gui/pixmap/lisp_load_cpm")) stream))
			((ends-with ".tga" file)
				((const (ffi "gui/pixmap/lisp_load_tga")) stream)))))
