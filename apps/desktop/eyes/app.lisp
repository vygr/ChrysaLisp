(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/lock/app.inc")
(import "lib/math/vector.inc")
(import "lib/gpu/vp.inc")

;;;;;;;;;;;;;;;
; configuration
;;;;;;;;;;;;;;;

(defq +min_width 256 +min_height 128 +max_width 512 +max_height 256
	*canvas* :nil *config* :nil +config_version 2
	+config_file (cat *env_home* "eyes.tre")
	;the eyes are a shader, as native code, both of them in one go, drawn
	;straight onto the pixels of the canvas
	eyes_program (shader-load (cat (path-to-file) "eyes.shader"))
	eyes_native (shader-vp eyes_program)
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
	(if (or (not *config*) (/= (. *config* :find :version) +config_version))
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
	(ui-backdrop *backdrop* (:style :grid :color +argb_black :ink_color +argb_grey6)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Drawing and Window Logic
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resize-window (pw ph)
	(def *backdrop* :min_width pw :min_height ph)
	(if *canvas* (. *canvas* :sub))
	(setq *canvas* (Canvas pw ph 1))
	(. *backdrop* :add_child *canvas*)
	; Get current position then fit window to screen
	(bind '(x y) (. *window* :get_pos))
	(bind '(fw fh) (. *window* :pref_size))
	(bind '(x y fw fh) (view-fit x y fw fh))
	(. *window* :change_dirty x y fw fh :t)
	(setq last_mx -1 last_my -1))

(defun look (cx cy r mx my)
	; (look cx cy r mx my) -> gaze
	;the way an eye at cx cy, r across to its edge, looks to see the
	;mouse, a unit vector, z out of the screen. With the mouse on the eye
	;the iris is under it, and as it goes off the eye turns less and less
	;more, up to the most it turns, there is no stop that it hits
	(defq off (Vec2-f (/ (- mx cx) r) (/ (- my cy) r))
		dist (/ (vector-length off) +look_most)
		off (vector-scale off (/ 1.0 (sqrt (+ 1.0 (* dist dist))))))
	(map (const n2r) (list (first off) (second off)
		(neg (sqrt (- 1.0 (vector-dot off off)))))))

(defun redraw (mx my)
	(bind '(w h) (. *canvas* :pref_size))
	; Relative mouse position within the canvas
	(bind '(canvas_x canvas_y & &) (. (penv *window*) :get_relative *canvas*))
	(defq fw (n2f w) fh (n2f h) rel_mx (n2f (- mx canvas_x)) rel_my (n2f (- my canvas_y))
		eye_r (* fh 0.48) eye_cy (* fh 0.5))
	(shader-vp-draw eyes_native
		(shader-vp-frame eyes_program eyes_native (list
			(list 'resolution (list w h))
			(list 'look_left (look (* fw 0.25) eye_cy eye_r rel_mx rel_my))
			(list 'look_right (look (* fw 0.75) eye_cy eye_r rel_mx rel_my))
			(list 'iris_color (map (# (/ (n2f (logand (>> iris_color %0) 0xff)) 255.0)) '(16 8 0)))
			;the iris is so much of the eye as it is seen flat, on the
			;ball it is that far round
			(list 'iris_size (* iris_scale 0.7))
			(list 'pupil_size pupil_scale)))
		(getf *canvas* +canvas_pixmap 0) 0 0 w h h :t)
	(. *canvas* :swap +swap_write))

;;;;;;;;;;;
; Main Loop
;;;;;;;;;;;

(defun main ()
	(defq select (task-mboxes +select_size) last_mx -1 last_my -1
		poll_rate (/ 1000000 30) *running* :t)
	(config-load)
	; Apply initial dimensions and settings from config
	(bind '(x y w h iris_color iris_scale pupil_scale)
		(gather *config* :x :y :width :height :iris_color :iris_scale :pupil_scale))
	; Position and display the window
	(. *window* :set_pos x y)
	(resize-window w h)
	(gui-add-front-rpc *window*)
	(mail-timeout (elem-get select +select_timer) 1 0)
	(while *running*
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_timer)
				(mail-timeout (elem-get select +select_timer) poll_rate 0)
				(bind '(mx my & &) (gui-info))
				(when (or (/= mx last_mx) (/= my last_my))
					(setq last_mx mx last_my my)
					(redraw mx my)))
			;must be for +select_main !
			((= (defq id (getf msg +ev_msg_target_id)) +event_close)
				(setq *running* :nil))
			((= id +event_min)
				(resize-window +min_width +min_height))
			((= id +event_max)
				(resize-window +max_width +max_height))
			((. *window* :event msg))))
	(config-save)
	(gui-sub-rpc *window*))
