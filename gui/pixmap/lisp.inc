(import "class/obj/lisp.inc")
(import "./struct.inc")
(import "lib/image/svg.inc")
(import "lib/image/cwb.inc")
(import "lib/image/tga.inc")

(defun pixmap-cpm-info (stream)
	; (pixmap-cpm-info stream) -> (width height type) | (-1 -1 -1)
	(if (= (length (defq header (read-blk stream +cpm_size))) +cpm_size)
		(case (char (getf header +cpm_ident) +int_size)
			(("MPC." "MLF.")
				(getf-> header +cpm_width +cpm_height +cpm_type))
			(:t (list -1 -1 -1)))
		(list -1 -1 -1)))

(defun pixmap-info (file)
	; (pixmap-info file) -> (width height type) | (-1 -1 -1)
	(or (if (defq stream (file-stream file))
			(some (# (if (ends-with %0 file) (%1 stream)))
				'(".cpm" ".flm" ".tga" ".svg" ".cwb")
				(const `'(,pixmap-cpm-info ,pixmap-cpm-info ,TGA-info ,SVG-info ,CWB-info))))
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
				((const (ffi "gui/pixmap/lisp_load_cpm")) stream)))))
