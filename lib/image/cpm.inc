;;;;;;;;;;;;
; cpm loader
;;;;;;;;;;;;

(import "gui/canvas/lisp.inc")
(import "././streams/rle.inc")
(import "././streams/lz4.inc")

;module
(env-push)

(bits +cpm_flag 0
	(bit rle lz4))

(structure +cpm 0
	(uint ident)        ;ident .CPM or .FLM
	(uint flags)        ;flags (formerly reserved 0)
	(uint version)      ;version number
	(int type)          ;image type
	(uint width)        ;image w
	(uint height)       ;image h
	(uint reserved_1))  ;reserved 1

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
					(defq source_stream stream)
					; LZ4 Layer
					(when lz4
						(defq lz4_stream (memory-stream))
						(lz4-decompress source_stream lz4_stream)
						(stream-seek lz4_stream 0 0)
						(setq source_stream lz4_stream))
					; RLE Layer
					(when rle
						(defq rle_stream (memory-stream))
						; decompress exactly one frame straight into our memory stream
						; max_tokens cap ensures .flm files are respected!
						(rle-decompress source_stream rle_stream num_bits 8 (* w h))
						(stream-seek rle_stream 0 0)
						(setq source_stream rle_stream))
					; loop and copy direct to the pixmap's pixel buffer
					(ifn (pixmap-read pixmap source_stream type)
						(setq canvas :nil))))))
	canvas)

(defun CPM-save (canvas stream type &optional ident rle lz4)
	; (CPM-save canvas stream type [ident rle lz4]) -> canvas
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
	;data
	(defq pixel_stream (memory-stream))
	(pixmap-write (pixmap-as-argb pixmap) pixel_stream type)
	(stream-seek pixel_stream 0 0)
	(defq source_stream pixel_stream)
	; RLE Layer
	(when rle
		(defq rle_stream (memory-stream))
		(rle-compress source_stream rle_stream num_bits 8)
		(stream-seek rle_stream 0 0)
		(setq source_stream rle_stream))
	; LZ4 Layer
	(if lz4
		(lz4-compress source_stream stream)
		(while (defq c (read-blk source_stream 1024))
			(write-blk stream c)))
	canvas)

;module
(export-symbols '(CPM-load CPM-info CPM-save))
(env-pop)