(defq *app_root* (path-to-file))
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/gpu/render_farm.inc")
(import "lib/media/flm.inc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Raymarch film. A shader is drawn for each frame of a flight into
; a lattice of balls, and the frame goes straight into the film file,
; for the Films app to play.
;
; The frames are drawn by a (Render-shade), lib/gpu/render_farm.inc: by
; the GPU if the host has one, by the nodes of the CPU if not, a tile of
; the frame each. Every frame's pixels are wanted, to go in the film, so
; the GPU's are read back onto the canvas's, never drawn straight into
; its texture, and the film is the same to the bit whichever drew it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enums +select 0
	(enum main timer))

(defq +shader_file (cat *app_root* "film.shader")
	+film_path "apps/media/films/data/" +film_name "raymarch"
	;24 bits a pixel, at 16 the shading of the balls shows bands
	+film_bits 24
	+width 600 +height 600 +line_batch 8
	+timer_rate (/ 1000000 60)
	+num_frames 40 frame_idx 0 z_start (n2d -3.0) z_dist (n2d 2.0)
	select :nil render :nil film :nil
	film_time 0 frame_time 0 frame_us 0 save_us 0 first_frame :nil
	;the film is made
	done :nil
	;the pixels of the canvas are in shared memory, the children draw
	;their tiles straight onto them
	shared_canvas (canvas-shared +width +height 1))

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Raymarch" (+sym_close) +event_close)
	(ui-label *status* (:text "..." :font :font_body))
	(ui-element *canvas* (ifn shared_canvas (Canvas +width +height 1))))

;how far the light swings across
(defq +light_swing (n2d 0.15))

(defun frame-values ()
	;the inputs of the shader for the frame, where the camera and the light are
	(defq fraction (/ (n2d frame_idx) (const (n2d +num_frames))))
	(list (list 'resolution (list +width +height))
		(list 'cam_z (+ z_start (* z_dist fraction)))
		(list 'light_x (+ +double_-1/10 (* (sin (* fraction +double_2pi)) +light_swing)))))

;the film is made in a file of its own, and is the film only once it is
;whole: one stopped part way does not leave half a film in the tree,
;item 124. There is no rename, it is copied over and let go of. The part
;is made beside the film as a .tmp, which .gitignore leaves out, and so
;the herd's sync, which would copy it to the other machines and not take
;it away again
(defq +film_file (cat +film_path +film_name ".flm") +film_part (cat +film_file ".tmp"))

(defun film-keep ()
	;the whole film is the film
	(with-write-lock +film_file
		(when (and (defq src (file-stream +film_part)) (defq dst (file-stream +film_file +file_open_write)))
			(while (defq c (read-blk src 65536)) (write-blk dst c) (task-slice))
			(stream-flush dst))
		(setq src :nil dst :nil))
	(pii-remove +film_part))

(defun film-drop ()
	;a film not finished is let go of, the one there was is as it was
	(when film
		(flm-close film)
		(setq film :nil))
	(pii-remove +film_part))

(defun start-frame ()
	;the next frame of the film, or the end of it
	(setq frame_time (pii-time))
	(cond
		((>= frame_idx +num_frames)
			;the film ends where it began, so that it loops, and is whole
			(defq again (Canvas +width +height 1) pixmap (getf again +canvas_pixmap 0)
				by (if (eql (. render :mode) :gpu) "GPU" "nodes"))
			(setf pixmap +pixmap_type 32 0)
			(stream-seek first_frame 0 0)
			(pixmap-read pixmap first_frame 32)
			(flm-add film again)
			(flm-close film)
			(film-keep)
			;there is no more for the render's farm
			(. render :close)
			(setq film :nil first_frame :nil done :t)
			(. *status* :set_text (cat "The film is made, " (str +num_frames) " frames in "
					(str (/ (- (pii-time) film_time) 1000)) "ms, by the "
					by ", " (str (/ save_us 1000)) "ms of it writing the film")))
		(:t ;the GPU shades it, the one tile, or the nodes, a tile each
			(. render :frame (frame-values)))))

(defun frame-done ()
	;the frame is whole, and is in the pixmap of the canvas. It goes
	;straight into the film, there is no file for a frame
	(defq now (pii-time) pixmap (getf *canvas* +canvas_pixmap 0))
	(flm-add film *canvas*)
	(when (= frame_idx 0)
		;the pixels of the first frame are kept, to be the last as well
		(setq first_frame (memory-stream))
		(pixmap-write pixmap first_frame 32))
	;that left the pixmap as argb. Every pixel is full on, so that is the
	;same as premultiplied, and the type is all that has to change
	(setf pixmap +pixmap_type -32 0)
	(setq save_us (+ save_us (- (pii-time) now))
		frame_us (- (pii-time) frame_time) frame_idx (inc frame_idx))
	(. *status* :set_text (cat (if (eql (. render :mode) :gpu) "GPU" (cat (str (length (lisp-nodes :t))) " nodes"))
			", frame " (str frame_idx) " of " (str +num_frames) ", "
			(str (/ frame_us 1000)) "ms to draw and save it"))
	(start-frame))

(defun main ()
	(setq select (task-mboxes +select_size) render (Render-shade *canvas* +shader_file +line_batch))
	(def render :direct :nil :partial :t)
	(.-> *canvas* (:fill +argb_black) (:swap +swap_write))
	(. *window* :open)
	(setq film (flm-open (file-stream +film_part +file_open_write) +film_bits)
		film_time (pii-time))
	(. *status* :set_text (if (eql (. render :mode) :gpu) "GPU, its kernel is being built" "No GPU, the nodes draw it"))
	(start-frame)
	(mail-timeout (elem-get select +select_timer) +timer_rate 0)
	(defq id :t)
	(while id
		;the render's mail is read with the app's, before the timer, and
		;dealt with
		(bind '(idx msg) (. render :read select +select_timer))
		(cond
			((not idx)
				;a frame that was out is whole, and the next is started
				(and (not done) (not (. render :busy)) (frame-done)))
			((= idx +select_main)
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						;close button
						(setq id :nil))
					((. *window* :event msg))))
			(:t ;timer event, the patience of the render's farm, and what
				;there is of a frame of tiles so far shown
				(mail-timeout (elem-get select +select_timer) +timer_rate 0)
				(. render :tick))))
	;a film not finished is let go of, and the farm and the window closed
	(film-drop)
	(. render :close)
	(. *window* :close))
