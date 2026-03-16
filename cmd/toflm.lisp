;;;;;;;;;;;;;;;;
; cmd/toflm.lisp
;;;;;;;;;;;;;;;;
(import "gui/lisp.inc")
(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/streams/rle.inc")
(import "lib/image/cpm.inc")

(defq usage `(
(("-h" "--help")
"Usage: toflm [options] [path] ...

	options:
		-h --help: this help info.
		-f --format 1|8|12|15|16|24|32: pixel format, default 32.
		-n --name path: output film filename, default \"film.flm\".

	Convert images to a .flm animation.

	If no paths given on command line
	then paths are read from stdin.")
(("-f" "--format") ,(opt-num 'opt_f))
(("-n" "--name") ,(opt-str 'opt_n))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_f 32 opt_n "film" args (options stdio usage)))
		(if (empty? (defq jobs (rest args)))
			;no, so from stdin
			(lines! (# (push jobs %0)) (io-stream 'stdin)))
		(unless (ends-with ".flm" opt_n)
			(setq opt_n (cat opt_n ".flm")))
		(when (nempty? jobs)
			(defq out_stream (file-stream opt_n +file_open_write)
			      num_bits (if (or (= opt_f 12) (= opt_f 15)) 16 opt_f)
			      p_stream (memory-stream)
			      c_stream (memory-stream)
			      total_pixels 0
			      first_frame :t)
			(each (lambda (file)
				(when (and file (some (# (ends-with %0 file)) '("cpm" "tga" "svg" "cwb")))
					(defq canvas (canvas-load file +load_flag_noswap))
					(when canvas
						(defq pixmap (getf canvas +canvas_pixmap 0))

						(if first_frame
							(progn
								; First frame - use existing CPM-save logic to write full frame
								(CPM-save canvas out_stream opt_f "MLF.")
								
								(setq total_pixels (* (getf pixmap +pixmap_width 0) (getf pixmap +pixmap_height 0)))
								(pixmap-write (pixmap-as-argb pixmap) p_stream opt_f)
								(setq first_frame :nil))
							(progn
								; Subsequent frames - RLE diff encoding
								(stream-seek p_stream 0 0)
								(stream-seek c_stream 0 0)
								(pixmap-write (pixmap-as-argb pixmap) c_stream opt_f)
								(stream-seek c_stream 0 0)

								(defq p_state (array 0 0) c_state (array 0 0) w_state (array 0 0)
								      mode :none count 0 draw_buf (list))

								(times total_pixels
									(defq p (read-bits p_stream p_state num_bits)
									      c (read-bits c_stream c_state num_bits))
									(if (= p -1) (setq p 0))
									(if (= c -1) (setq c 0))
									(cond
										((= p c)
											(if (eql mode :draw)
												(progn
													(write-char out_stream count)
													(each (# (write-bits out_stream w_state %0 num_bits)) draw_buf)
													(flush-bits out_stream w_state)
													(clear draw_buf)
													(setq count 0 mode :skip)))
											(setq mode :skip)
											(++ count)
											(when (= count 128)
												(write-char out_stream (- 256 128))
												(setq count 0)))
										(:t
											(if (eql mode :skip)
												(progn
													(if (> count 0)
														(write-char out_stream (- 256 count)))
													(setq count 0 mode :draw)))
											(setq mode :draw)
											(push draw_buf c)
											(++ count)
											(when (= count 127)
												(write-char out_stream 127)
												(each (# (write-bits out_stream w_state %0 num_bits)) draw_buf)
												(flush-bits out_stream w_state)
												(clear draw_buf)
												(setq count 0)))))

								; Flush remaining tokens
								(if (eql mode :draw)
									(when (> count 0)
										(write-char out_stream count)
										(each (# (write-bits out_stream w_state %0 num_bits)) draw_buf)
										(flush-bits out_stream w_state)))
								(if (eql mode :skip)
									(when (> count 0)
										(write-char out_stream (- 256 count))))

								; swap streams for next iteration
								(defq temp p_stream)
								(setq p_stream c_stream c_stream temp)))
						(print file " -> " opt_n)))) jobs)
			(stream-flush out_stream))))