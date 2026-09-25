(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/lock/app.inc")
(import "lib/math/vector.inc")

;;;;;;;;;;;;;;;
; configuration
;;;;;;;;;;;;;;;

(defq +min_width 256 +min_height 128 +max_width 512 +max_height 256
	*canvas* :nil *config* :nil +config_version 2
	+config_file (cat *env_home* "eyes.tre"))

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

(enums +event 0
	(enum close max min))

(enums +select 0
	(enum main timer))

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Eyes" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-backdrop *backdrop* (:style :grid :color +argb_black :ink_color +argb_grey6)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Drawing and Window Logic
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resize-window (pw ph)
	(def *backdrop* :min_width pw :min_height ph)
	(if *canvas* (. *canvas* :sub))
	(setq *canvas* (Canvas pw ph 1))
	(. *canvas* :set_canvas_flags +canvas_flag_antialias)
	(. *backdrop* :add_child *canvas*)
	; Get current position then fit window to screen
	(bind '(x y) (. *window* :get_pos))
	(bind '(fw fh) (. *window* :pref_size))
	(bind '(x y fw fh) (view-fit x y fw fh))
	(. *window* :change_dirty x y fw fh :t)
	(setq last_mx -1 last_my -1))

(defun circle (r)
	; Cached circle path generation
	(memoize r (list (path-gen-arc 0.0 0.0 0.0 +fp_2pi r (path))) 3))

(defun draw-eye (cx rel_mx rel_my eye_cy eye_r iris_r pupil_r hr max_dist)
	; Iris position clamped to the eyeball boundary
	(defq vec (Vec2-f (- rel_mx cx) (- rel_my eye_cy))
		dist (vector-length vec)
		off (if (> dist 0.0)
			(vector-scale (vector-norm vec) (min dist max_dist))
			(Vec2-f 0.0 0.0)))
	(bind '(ipx ipy) (vector-add (Vec2-f cx eye_cy) off))
	; Highlight offset in the direction opposite to gaze
	(bind '(nx ny) (if (> dist 0.0) (vector-norm vec) (Vec2-f -0.707 -0.707)))
	(defq hx (+ ipx (* (- 0.0 nx) (* hr 1.33))) hy (+ ipy (* (- 0.0 ny) (* hr 1.33))))
	(.-> *canvas*
		(:set_color +argb_white) (:fpoly cx eye_cy +winding_odd_even (circle eye_r))
		(:set_color iris_color) (:fpoly ipx ipy +winding_odd_even (circle iris_r))
		(:set_color +argb_black) (:fpoly ipx ipy +winding_odd_even (circle pupil_r))
		(:set_color +argb_white) (:fpoly hx hy +winding_odd_even (circle hr))))

(defun redraw (mx my)
	(bind '(w h) (map (const n2f) (. *canvas* :pref_size)))
	(. *canvas* :fill 0)
	; Relative mouse position within the canvas
	(bind '(canvas_x canvas_y & &)
		(map (const n2f) (. (penv *window*) :get_relative *canvas*)))
	(defq rel_mx (- (n2f mx) canvas_x)
		rel_my (- (n2f my) canvas_y))
	; Shared eye geometry
	(defq eye_r (* h 0.48) iris_r (* eye_r iris_scale) pupil_r (* iris_r pupil_scale)
		hr (* pupil_r 0.3) max_d (- eye_r iris_r) eye_cy (* h 0.5))
	(draw-eye (* w 0.25) rel_mx rel_my eye_cy eye_r iris_r pupil_r hr max_d)
	(draw-eye (* w 0.75) rel_mx rel_my eye_cy eye_r iris_r pupil_r hr max_d)
	(. *canvas* :swap +pixmap_mode_normal))

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
