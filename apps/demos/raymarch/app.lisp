(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/gpu/shader.inc")
(import "lib/gpu/gui.inc")
(import "lib/gpu/tile.inc")
(import "lib/task/pipe.inc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Raymarch film. A shader is drawn for each frame of a flight into
; a lattice of balls, and the frame is saved, for the Films app to play.
;
; The GPU draws it if the host has one, and the frame is then read back
; from the texture to be saved. With no GPU the nodes of the machine
; draw it between them, as native code, straight onto the canvas.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enums +event 0
	(enum close))

(enums +select 0
	(enum main task reply timer))

(defq +shader_file (cat *app_root* "film.shader")
	+film_path "apps/media/films/data/" +film_name "raymarch"
	;24 bits a pixel, at 16 the shading of the balls shows bands
	+film_bits 24
	+width 600 +height 600 +line_batch 8
	+timer_rate (/ 1000000 60) +slow_ticks 30 ticks 0 +retry_timeout (task-timeout 5)
	+num_frames 40 frame_idx 0 z_start (n2r -3.0) z_dist (n2r 2.0)
	program (shader-load +shader_file) select :nil jobs :nil lst_stream :nil
	film_time 0 frame_time 0 frame_us 0
	;how the frames are being drawn. :wait, the driver is building the
	;shader. :gpu, the GPU. :cpu, the nodes. :done, the film is made
	mode :wait gpu_shader :nil
	;a GPU frame is drawn as strips, each of so many lines, so that a GPU
	;that is slow at it leaves room for the GUI to be drawn in between
	gpu_inputs :nil gpu_y 0 gpu_strip +height gpu_wait 0
	;the pixels of the canvas are in shared memory if the host has it, the
	;nodes shade their tiles straight into them
	shared_canvas (canvas-shared +width +height 1))

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Raymarch" (0xea19) +event_close)
	(ui-label *status* (:text "..." :font *env_body_font*))
	(ui-element *canvas* (ifn shared_canvas (Canvas +width +height 1)) (:color 0)))

(defun set-label (label text)
	;a label lays its text out once, so lay it out again for the new text
	(unless (eql (get :text label) text)
		(def label :text text)
		(.-> label :layout :dirty)))

(defun frame-inputs ()
	;the inputs block for the frame, where the camera and the light are
	(defq fraction (/ (n2r frame_idx) (n2r +num_frames)))
	(shader-pack program (list
		(list 'resolution (list +width +height))
		(list 'cam_z (+ z_start (* z_dist fraction)))
		(list 'light_x (+ (n2r -0.1) (* (sin (* fraction +real_2pi)) (n2r 0.15)))))))

(defun start-frame ()
	;the next frame of the film, or the end of it
	(setq frame_time (pii-time) gpu_inputs :nil gpu_y 0)
	(cond
		((>= frame_idx +num_frames)
			;the list ends where it began, the film loops
			(write-line lst_stream (cat +film_path +film_name "_0.cpm"))
			(stream-flush lst_stream)
			(setq lst_stream :nil mode :done)
			(defq frames_ms (/ (- (pii-time) film_time) 1000) now (pii-time))
			;the frames are made into the one file the Films app plays
			(pipe-run (cat "cat " +film_path +film_name ".lst | toflm -f " (str +film_bits) " -n "
				+film_path +film_name ".flm") (lambda (_)))
			(set-label *status* (cat "The film is made, " (str +num_frames) " frames in "
				(str frames_ms) "ms, by the " (if gpu_shader "GPU" "nodes")
				", and " (str (/ (- (pii-time) now) 1000)) "ms to make them the .flm")))
		((eql mode :cpu)
			;the nodes shade it, a tile each
			(defq inputs (frame-inputs) key (canvas-key *canvas*))
			(. jobs :add (map (# (shader-tile +shader_file inputs
				0 %0 +width (min +height (+ %0 +line_batch)) +width +height key))
				(range 0 +height +line_batch))))))

(defun frame-done ()
	;the frame is whole, and is in the pixmap of the canvas. Save it
	(defq cpm_path (cat +film_path +film_name "_" (str frame_idx) ".cpm"))
	(canvas-save *canvas* cpm_path +film_bits :t :t)
	;the save left the pixmap as argb. Every pixel is full on, so that is
	;the same as premultiplied, and the type is all that has to change
	(setf (getf *canvas* +canvas_pixmap 0) +pixmap_type -32 0)
	(write-line lst_stream cpm_path)
	(stream-flush lst_stream)
	(setq frame_us (- (pii-time) frame_time) frame_idx (inc frame_idx))
	(set-label *status* (cat (if (eql mode :gpu) "GPU" (cat (str (length (lisp-nodes))) " nodes"))
		", frame " (str frame_idx) " of " (str +num_frames) ", "
		(str (/ frame_us 1000)) "ms to draw and save it"))
	(start-frame))

(defun to-cpu (why)
	;the nodes draw the film, there is no GPU for it
	(if gpu_shader (canvas-shader-destroy gpu_shader))
	(setq gpu_shader :nil mode :cpu)
	(set-label *status* why)
	(. jobs :restart)
	(start-frame))

(defun gpu-tick ()
	;the next strip of the frame, if the GPU has done with the last. The
	;strip is sized to what the GPU does in about a tick. A fast GPU takes
	;the whole frame as one strip, every tick
	(unless gpu_inputs (setq gpu_inputs (frame-inputs) gpu_y 0))
	(cond
		((eql (defq drawn (. *canvas* :shade gpu_shader gpu_inputs 0 gpu_y +width
				(defq y1 (min +height (+ gpu_y gpu_strip))))) :error)
			(to-cpu "No GPU, the driver could not build the shader, the nodes draw it"))
		(drawn
			(when (eql mode :wait)
				;the shader is built, the film starts here
				(setq mode :gpu film_time (pii-time) frame_time film_time gpu_wait 1))
			;a strip should take the GPU more than one tick and less than two,
			;so the GPU is not left idle, and the GUI does not wait long
			(setq gpu_strip (max 4 (min +height (case gpu_wait
				(0 (inc (/ (* gpu_strip 5) 4)))
				(1 gpu_strip)
				(2 (/ (* gpu_strip 4) 5))
				(:t (/ (* gpu_strip 2) (inc gpu_wait)))))))
			(setq gpu_wait 0 gpu_y y1)
			(when (>= gpu_y +height)
				;the frame is in the texture, read it back to be saved
				(. *canvas* :swap +swap_read)
				(frame-done)))
		(:t (++ gpu_wait))))

(defun main ()
	(setq select (task-mboxes +select_size))
	(.-> *canvas* (:fill +argb_black) (:swap +swap_write))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(setq lst_stream (file-stream (cat +film_path +film_name ".lst") +file_open_write)
		jobs (Jobs +shader_tile_child
			(elem-get select +select_task) (elem-get select +select_reply))
		gpu_shader (shader-gui program) film_time (pii-time))
	(if gpu_shader
		(set-label *status* "GPU, the driver is building the shader")
		(to-cpu "No GPU, the nodes draw it"))
	(mail-timeout (elem-get select +select_timer) +timer_rate 0)
	(defq id :t)
	(while id
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_main
				;main mailbox
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						;close button
						(setq id :nil))
					((. *window* :event msg))))
			(+select_task
				;a child has started
				(. jobs :launched msg))
			(+select_reply
				;a tile is shaded
				(when (and (defq out (. jobs :answered msg)) (eql mode :cpu))
					;a child that could not reach the canvas sends the pixels
					(shader-tile-show *canvas* msg)
					(when (= out 0)
						(. *canvas* :swap +swap_write)
						(frame-done))))
			(:t ;timer event, a strip of the GPU frame every tick, or all of it
				(mail-timeout (elem-get select +select_timer) +timer_rate 0)
				(if (or (eql mode :wait) (eql mode :gpu)) (gpu-tick))
				(when (= (setq ticks (% (inc ticks) +slow_ticks)) 0)
					(when (eql mode :cpu)
						;show what there is of the frame so far
						(. *canvas* :swap +swap_write)
						(. jobs :refresh +retry_timeout))))))
	;close window and children
	(if gpu_shader (canvas-shader-destroy gpu_shader))
	(. jobs :close)
	(gui-sub-rpc *window*))
