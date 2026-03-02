;;;;;;;;;;;;
; cpm loader
;;;;;;;;;;;;

(import "gui/canvas/lisp.inc")

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

(defun CPM-Canvas (stream)
	; (CPM-Canvas stream) -> :nil | canvas
	(defq canvas :nil)
	(when stream
		(defq header (read-blk stream +cpm_size))
		(when (= (length header) +cpm_size)
			(case (char (getf header +cpm_ident) +int_size)
				(("MPC." "MLF.")
					(bind '(w h type) (getf-> header +cpm_width +cpm_height +cpm_type))
					(defq trans (getf header +cpm_trans)
						canvas (Canvas w h 1)
						num_bits type bit_pool (array 0 0)
						total (* w h) i 0)
					; 12 and 15 bit images are padded to 16 num_bits in the stream
					(if (or (= num_bits 12) (= num_bits 15)) (setq num_bits 16))
					; save stream type for any film players !
					(setf (getf canvas +canvas_pixmap 0) +pixmap_stream_type num_bits 0)
					(defq line_str (str-alloc (* w +int_size))
						  line_buf (string-stream line_str)
						  x 0 y 0)
					(while (< i total)
						(defq len (read-bits stream bit_pool 8))
						(if (= len -1)
							; premature EOF
							(setq i total canvas :nil)
							(cond
								((>= len 128)
									; run of a single pixel
									(setq len (- len 127))
									(defq col (read-bits stream bit_pool num_bits))
									(if (= col -1)
										(setq i total canvas :nil)
										(progn
											(setq col (canvas-to-argb32 col type))
											(if (= col trans) (setq col 0))
											(setq i (+ i len))
											(while (> len 0)
												(defq chunk (min len (- w x)))
												(times chunk (write-int line_buf col))
												(setq len (- len chunk) x (+ x chunk))
												(when (= x w)
													(if canvas (canvas-tile canvas line_str 0 y w (inc y)))
													(stream-seek line_buf 0 0)
													(++ y)
													(setq x 0))))))
								(:t ; block of raw pixels
									(setq len (inc len))
									(setq i (+ i len))
									(while (> len 0)
										(defq chunk (min len (- w x)))
										(times chunk
											(defq col (read-bits stream bit_pool num_bits))
											(if (= col -1)
												(setq len 0 chunk 0 i total canvas :nil)
												(progn
													(setq col (canvas-to-argb32 col type))
													(write-int line_buf (if (= col trans) 0 col)))))
										(when canvas
											(setq len (- len chunk) x (+ x chunk))
											(when (= x w)
												(canvas-tile canvas line_str 0 y w (inc y))
												(stream-seek line_buf 0 0)
												(++ y)
												(setq x 0)))))))
						(task-slice))))))
	canvas)

;module
(export-symbols '(CPM-Canvas CPM-info))
(env-pop)