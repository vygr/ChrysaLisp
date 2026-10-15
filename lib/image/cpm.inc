;;;;;;;;;;;;
; cpm loader
;;;;;;;;;;;;

(import "gui/canvas/lisp.inc")
(import "lib/streams/stages.inc")

;module
(env-push)

(bits +cpm_flag 0
	(bit rle lz4))

(structure +cpm 0
	(uint ident)		;ident .CPM or .FLM
	(uint flags)		;flags
	(uint version)		;version number
	(int type)			;image type
	(uint width)		;image w
	(uint height)		;image h
	(uint reserved_1))	;reserved 1

(defun CPM-info (stream)
	; (CPM-info stream) -> (width height type) | (-1 -1 -1)
	(defq out '(-1 -1 -1))
	(when stream
		(defq header (read-blk stream +cpm_size))
		(and (= (length header) +cpm_size)
				(find (char (getf header +cpm_ident) +int_size) '("MPC." "MLF."))
			(setq out (getf-> header +cpm_width +cpm_height +cpm_type))))
	out)

(defun CPM-load (stream)
	; (CPM-load stream) -> :nil | canvas
	(defq canvas :nil)
	(when stream
		(defq header (read-blk stream +cpm_size))
		(when (and (= (length header) +cpm_size)
				(find (char (getf header +cpm_ident) +int_size) '("MPC." "MLF.")))
			(bind '(flags ver w h type) (getf-> header +cpm_flags +cpm_version +cpm_width +cpm_height +cpm_type))
			(defq rle :nil lz4 :nil)
			(if (= ver 1)
				(setq rle :t)
				(setq rle (bits? flags +cpm_flag_rle)
					lz4 (bits? flags +cpm_flag_lz4)))
			(defq canvas (Canvas w h 1) num_bits type
				pixmap (getf canvas +canvas_pixmap 0))
			; 12 and 15 bit images are padded to 16 num_bits in the stream
			(if (or (= type 12) (= type 15)) (setq num_bits 16))
			; save stream type for any film players !
			(setf-> pixmap (+pixmap_stream_type type 0) (+pixmap_type 32 0))
			;the stages in the order the data goes, stream to pixmap
			(defq stages (list))
			(if lz4 (push stages (stage "lib/streams/lz4.inc" 'lz4-decompress)))
			(if rle (push stages (stage "lib/streams/rle.inc" 'rle-decompress num_bits 8 (* w h))))
			(unless (if (empty? stages) (pixmap-read pixmap stream type) (stages-load stream pixmap type stages))
				(setq canvas :nil))))
	canvas)

(defun CPM-save (canvas stream type &optional rle lz4 ident)
	; (CPM-save canvas stream type [rle lz4 ident]) -> canvas
	(setd ident "MPC.")
	(defq pixmap (getf canvas +canvas_pixmap 0) num_bits type
		flags (+ (if rle +cpm_flag_rle 0) (if lz4 +cpm_flag_lz4 0)))
	(if (or (= type 12) (= type 15)) (setq num_bits 16))
	;header
	(write-blk stream (setf-> (str-alloc +cpm_size)
		(+cpm_ident (code ident 4))
		(+cpm_flags flags)
		(+cpm_version 2)
		(+cpm_type type)
		(+cpm_width (getf pixmap +pixmap_width 0))
		(+cpm_height (getf pixmap +pixmap_height 0))
		(+cpm_reserved_1 0)))
	;the stages in the order the data goes, pixmap to stream
	(defq stages (list))
	(if rle (push stages (stage "lib/streams/rle.inc" 'rle-compress num_bits 8)))
	(if lz4 (push stages (stage "lib/streams/lz4.inc" 'lz4-compress)))
	(if (empty? stages)
		(pixmap-write (pixmap-as-argb pixmap) stream type)
		(stages-save pixmap type stream stages))
	canvas)

;the canvas is told what this reads
(canvas-loader ".cpm" CPM-info CPM-load CPM-save)
(canvas-loader ".flm" CPM-info CPM-load)

;module
(export-symbols '(CPM-load CPM-info CPM-save))
(env-pop)