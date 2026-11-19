(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/lock/app.inc")
(import "lib/math/vector.inc")
(import "lib/gpu/render_farm.inc")

;;;;;;;;;;;;;;;
; configuration
;;;;;;;;;;;;;;;

(defq +min_width 256 +min_height 128 +max_width 512 +max_height 256
	*canvas* :nil *config* :nil +config_version 2
	+config_file (cat (get :home *env*) "eyes.tre")
	;the eyes are a shader, both of them in one go, drawn by a
	;(Render-shade), lib/gpu/render_farm.inc: by the GPU, straight into the
	;texture of the canvas, or with none by native code in this task. A
	;frame is drawn only when the mouse has moved, and never while the last
	;is still out, so the eyes take no more of the GPU than a look needs
	+eyes_file (cat (path-to-file) "eyes.shader") render :nil
	;the most an eye turns, as how far from its middle the middle of the
	;iris gets, of the eye. Further and the iris is seen so side on that
	;it is flat
	+look_most 0.66)

(defun config-default ()
	(scatter (Emap)
		:version +config_version :x 0 :y 0 :width +min_width :height +min_height
		:iris_color +argb_green :iris_scale 0.7 :pupil_scale 0.4))

(defun config-load ()
	(setq *config* (with-read-lock +config_file
			(tree-load (file-stream +config_file))))
	(if (or (not *config*) (nql (. *config* :find :version) +config_version))
		(setq *config* (config-default))))

(defun config-save ()
	(bind '(x y) (. *window* :get_pos))
	(bind '(w h) (. *canvas* :get_size))
	(scatter *config*
		:x x :y y :width w :height h
		:iris_color iris_color :iris_scale iris_scale :pupil_scale pupil_scale)
	(with-write-lock +config_file
		(tree-save (file-stream +config_file +file_open_write) *config*)))

;;;;;;;;;;;;;;
; UI and State
;;;;;;;;;;;;;;

(enums +select 0
	(enum main timer))

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Eyes" (+sym_close +sym_max +sym_min) +event_close)
	(ui-backdrop *backdrop* (:style :grid :paper +argb_black :ink +argb_grey6)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Drawing and Window Logic
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun look (cx cy r mx my)
	; (look cx cy r mx my) -> gaze
	;the way an eye at cx cy, r across to its edge, looks to see the
	;mouse, a unit vector, z out of the screen. With the mouse on the eye
	;the iris is under it, and as it goes off the eye turns less and less
	;more, up to the most it turns, there is no stop that it hits
	(defq off (Vec2-q (/ (- mx cx) r) (/ (- my cy) r))
		dist (/ (vector-length off) +look_most)
		off (vector-scale off (/ 1.0 (sqrt (+ 1.0 (* dist dist))))))
	(map (const n2d) (list (first off) (second off)
			(neg (sqrt (- 1.0 (vector-dot off off)))))))

(defun eyes-values (mx my)
	; (eyes-values mx my) -> vals
	;the inputs of the shader, the eyes looking at the mouse where it is
	(bind '(w h) (. *canvas* :pref_size))
	; Relative mouse position within the canvas, which is not in the
	; window yet the first time
	(bind '(canvas_x canvas_y &ignore) (if (penv *canvas*)
			(. (penv *window*) :get_relative *canvas*) '(0 0)))
	(defq fw (n2q w) fh (n2q h) rel_mx (n2q (- mx canvas_x)) rel_my (n2q (- my canvas_y))
		eye_r (* fh 0.48) eye_cy (* fh 0.5))
	(list (list 'resolution (list w h))
		(list 'look_left (look (* fw 0.25) eye_cy eye_r rel_mx rel_my))
		(list 'look_right (look (* fw 0.75) eye_cy eye_r rel_mx rel_my))
		(list 'iris_color (map (# (/ (n2q (logand (>> iris_color %0) 0xff)) 255.0)) '(16 8 0)))
		;the iris is so much of the eye as it is seen flat, on the
		;ball it is that far round
		(list 'iris_size (* iris_scale 0.7))
		(list 'pupil_size pupil_scale)))

(defun resize-window (pw ph)
	;a canvas of the new size. It is drawn here, in this task, before it is
	;put on show, so nothing is seen that is not the eyes, and the frames
	;after are the render's
	(def *backdrop* :min_width pw :min_height ph)
	(defq old *canvas*)
	(setq *canvas* (ifn (canvas-shared pw ph 1) (Canvas pw ph 1)))
	(. render :canvas *canvas*)
	(bind '(mx my &ignore) (gui-info))
	(. render :here (eyes-values mx my))
	(if old (. old :sub))
	(. *backdrop* :add_child *canvas*)
	; Get current position then fit window to screen
	(. *window* :fit :t)
	(setq last_mx -1 last_my -1))

;;;;;;;;;;;
; Main Loop
;;;;;;;;;;;

(defun main ()
	(defq select (task-mboxes +select_size) last_mx -1 last_my -1
		poll_rate (/ 1000000 30) *running* :t
		;when the last frame was given, is it still out, and how long it took
		given 0 out :nil took 0)
	(config-load)
	; Apply initial dimensions and settings from config
	(bind '(x y w h iris_color iris_scale pupil_scale)
		(gather *config* :x :y :width :height :iris_color :iris_scale :pupil_scale))
	;no farm of the nodes for so small a picture: what the GPU does not
	;draw is drawn here
	(setq render (Render-shade (Canvas 1 1 1) +eyes_file))
	(def render :farm :nil)
	; Position and display the window
	(. *window* :set_pos x y)
	(resize-window w h)
	(. *window* :open :here)
	(mail-timeout (elem-get select +select_timer) 1 0)
	(while *running*
		;the render's mail is read with the app's, before the timer, and
		;dealt with
		(bind '(idx msg) (. render :read select +select_timer))
		(cond
			((not idx)
				;a frame that was out is whole, and how long it took: the GPU
				;was done with it when its child said so
				(and out (not (. render :busy))
					(setq out :nil took (- (pii-time) given))))
			((= idx +select_timer)
				(bind '(mx my &ignore) (gui-info))
				;a look only when the mouse has moved, and the last is drawn
				(when (and (not (. render :busy)) (or (/= mx last_mx) (/= my last_my)))
					(setq last_mx mx last_my my given (pii-time))
					(if (. render :frame (eyes-values mx my))
						(setq took (- (pii-time) given))
						(setq out :t)))
				(. render :tick)
				;the next look is a poll on, or four times as long as the last
				;frame took if that is more: on a slow machine the eyes are
				;drawn less often, and they never have more than a quarter of
				;the GPU, or of their node, the apps beside them have the rest
				(mail-timeout (elem-get select +select_timer)
					(max poll_rate (* took 4)) 0))
			;must be for +select_main !
			((= (defq id (getf msg +ev_msg_target_id)) +event_close)
				(setq *running* :nil))
			((= id +event_min)
				(resize-window +min_width +min_height))
			((= id +event_max)
				(resize-window +max_width +max_height))
			((. *window* :event msg))))
	(config-save)
	(. render :close)
	(. *window* :close))
