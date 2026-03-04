;;;;;;;;;;;;
; cpm loader
;;;;;;;;;;;;

(import "gui/canvas/lisp.inc")
(import "lib/streams/rle.inc")

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
					(defq trans (getf header +cpm_trans)
						canvas (Canvas w h 1) num_bits type
						pixmap (getf canvas +canvas_pixmap 0))
					; 12 and 15 bit images are padded to 16 num_bits in the stream
					(if (or (= num_bits 12) (= num_bits 15)) (setq num_bits 16))
					; save stream type for any film players !
					(setf pixmap +pixmap_stream_type num_bits 0)
					(setf pixmap +pixmap_type 32 0)

					; decompress exactly one frame straight into our memory stream
					; max_tokens cap ensures .flm files are respected!
					(defq mem_stream (memory-stream)
						read_pool_state (array 0 0)
						pixel_data +pixmap_data
						pixel_data_end (+ +pixmap_data (* w h +int_size)))
					(rle-decompress stream mem_stream num_bits 8 (* w h))
					(stream-seek mem_stream 0 0)
					
					; loop and copy direct to the pixmap's pixel buffer
					(while (< pixel_data pixel_data_end)
						(task-slice)
						(defq col (read-bits mem_stream read_pool_state num_bits))
						(if (= col -1)
							; premature EOF
							(setq pixel_data pixel_data_end canvas :nil)
							(progn
								(if (/= (setq col (canvas-to-argb32 col type)) trans)
									(obj-set pixmap pixel_data +type_uint col))
								(setq pixel_data (+ pixel_data +int_size)))))))))
	canvas)

(defun CPM-save (canvas stream format)
	; (CPM-save canvas stream format) -> canvas
	(defq pixmap (getf canvas +canvas_pixmap 0)
		w (getf pixmap +pixmap_width 0) h (getf pixmap +pixmap_height 0)
		num_bits format bit_pool_state (array 0 0)
		pixel_data +pixmap_data
		pixel_data_end (+ +pixmap_data (* w h +int_size)))
	(if (or (= num_bits 12) (= num_bits 15)) (setq num_bits 16))
	;header
	(write-blk stream (setf-> (str-alloc +cpm_size)
		(+cpm_ident (code "MPC." 4))
		(+cpm_bytes 0)
		(+cpm_version 1)
		(+cpm_type format)
		(+cpm_width w)
		(+cpm_height h)
		(+cpm_trans 0)))
	;data
	(pixmap-as-argb pixmap)
	(defq mem_stream (memory-stream))
	(until (= pixel_data pixel_data_end)
		(write-bits mem_stream bit_pool_state (canvas-from-argb32 (obj-get pixmap pixel_data +type_uint) format) num_bits)
		(setq pixel_data (+ pixel_data +int_size)))
	(flush-bits mem_stream bit_pool_state)
	(stream-seek mem_stream 0 0)
	(rle-compress mem_stream stream num_bits 8)
	canvas)

;module
(export-symbols '(CPM-load CPM-info CPM-save))
(env-pop)