;;;;;;;;;;;;
; cpm loader
;;;;;;;;;;;;

(import "gui/canvas/lisp.inc")
(import "././streams/rle.inc")

;module
(env-push)

(structure +cpm 0
	(uint ident)        ;ident .CPM or .FLM
	(uint bytes)        ;0 if not known, else total size
	(uint version)      ;version number
	(int type)          ;image type
	(uint width)        ;image w
	(uint height)       ;image h
	(uint trans))       ;image transparent colour

(defun CPM-info (stream)
	; (CPM-info stream) -> (width height type) | (-1 -1 -1)
	(defq out '(-1 -1 -1))
	(when stream
		(defq header (read-blk stream +cpm_size))
		(when (= (length header) +cpm_size)
			(case (char (getf header +cpm_ident) +int_size)
				(("MPC." "MLF.")
					(setq out (getf-> header +cpm_width +cpm_height +cpm_type))))))
	out)

(defun CPM-load (stream)
	; (CPM-load stream) -> :nil | canvas
	(defq canvas :nil)
	(when stream
		(defq header (read-blk stream +cpm_size))
		(when (= (length header) +cpm_size)
			(case (char (getf header +cpm_ident) +int_size)
				(("MPC." "MLF.")
					(bind '(w h type) (getf-> header +cpm_width +cpm_height +cpm_type))
					(defq canvas (Canvas w h 1) num_bits type
						pixmap (getf canvas +canvas_pixmap 0))
					; 12 and 15 bit images are padded to 16 num_bits in the stream
					(if (or (= type 12) (= type 15)) (setq num_bits 16))
					; save stream type for any film players !
					(setf-> pixmap (+pixmap_stream_type type 0) (+pixmap_type 32 0))
					; decompress exactly one frame straight into our memory stream
					; max_tokens cap ensures .flm files are respected!
					(rle-decompress stream (defq pixel_stream (memory-stream)) num_bits 8 (* w h))
					(stream-seek pixel_stream 0 0)
					; loop and copy direct to the pixmap's pixel buffer
					(ifn (pixmap-read pixmap pixel_stream type (getf header +cpm_trans))
						(setq canvas :nil))))))
	canvas)

(defun CPM-save (canvas stream type &optional ident)
	; (CPM-save canvas stream type [ident]) -> canvas
	(setd ident "MPC.")
	(defq pixmap (getf canvas +canvas_pixmap 0) num_bits type)
	(if (or (= type 12) (= type 15)) (setq num_bits 16))
	;header
	(write-blk stream (setf-> (str-alloc +cpm_size)
		(+cpm_ident (code ident 4))
		(+cpm_bytes 0)
		(+cpm_version 1)
		(+cpm_type type)
		(+cpm_width (getf pixmap +pixmap_width 0))
		(+cpm_height (getf pixmap +pixmap_height 0))
		(+cpm_trans 0)))
	;data
	(pixmap-write (pixmap-as-argb pixmap) (defq pixel_stream (memory-stream)) type)
	(stream-seek pixel_stream 0 0)
	(rle-compress pixel_stream stream num_bits 8)
	canvas)

;module
(export-symbols '(CPM-load CPM-info CPM-save))
(env-pop)