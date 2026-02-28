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

(defun CPM-Canvas (stream &optional scale)
	; (CPM-Canvas stream [scale]) -> :nil | canvas
	(setd scale 1)
	(defq canvas :nil)
	(when stream
		(defq header (read-blk stream +cpm_size))
		(when (= (length header) +cpm_size)
			(case (char (getf header +cpm_ident) +int_size)
				(("MPC." "MLF.")
					(bind '(w h type) (getf-> header +cpm_width +cpm_height +cpm_type))
					(defq trans (getf header +cpm_trans)
						canvas (Canvas w h scale)
						num_bits type bit_pool (array 0 0)
						total (* w h) i 0)
					; 12 and 15 bit images are padded to 16 num_bits in the stream
					(if (or (= num_bits 12) (= num_bits 15)) (setq num_bits 16))
					(. canvas :set_canvas_flags +canvas_flag_antialias)
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
											(when (/= col trans)
												(. canvas :set_color col)
												(times len
													(. canvas :plot (% i w) (/ i w))
													(++ i)))
											(when (= col trans)
												(setq i (+ i len))))))
								(:t ; block of raw pixels
									(setq len (inc len))
									(times len
										(defq col (read-bits stream bit_pool num_bits))
										(if (= col -1)
											(setq i total canvas :nil)
											(progn
												(setq col (canvas-to-argb32 col type))
												(when (/= col trans)
													(.-> canvas (:set_color col) (:plot (% i w) (/ i w))))
												(++ i)))))))
						(task-slice))
					(when canvas
						; resize to final output
						(bind '(fw fh) (. canvas :pref_size))
						(setq canvas (. (Canvas fw fh 1) :resize canvas)))))))
	canvas)

;module
(export-symbols '(CPM-Canvas CPM-info))
(env-pop)