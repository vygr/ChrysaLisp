(import "././login/env.inc")
(import "gui/lisp.inc")
(import "lib/math/vector.inc")

;;;
;;; Configuration
;;;

(defq *config* :nil *config_version* 1
	*config_file* (cat *env_home* "eyes.tre"))

(defun get-default-config ()
	(scatter (Emap)
		:version *config_version*
		:width min_width
		:height min_height
		:iris_color +argb_green
		:iris_scale 0.7
		:pupil_scale 0.4))

(defun load-config ()
	(if (defq stream (file-stream *config_file*))
		(setq *config* (tree-load stream)))
	(if (or (not *config*) (/= (. *config* :find :version) *config_version*))
		(setq *config* (get-default-config))))

(defun save-config ()
	(scatter *config*
		:iris_color *iris_color*
		:iris_scale *iris_scale*
		:pupil_scale *pupil_scale*)
	(when (defq stream (file-stream *config_file* +file_open_write))
		(tree-save stream *config*)))

;;;
;;; UI and State
;;;

(enums +event 0
	(enum close max min))

(enums +select 0
	(enum main timer))

(defq +rate (/ 1000000 30) ; 30 FPS
	min_width 256 min_height 128
	max_width 512 max_height 256
	*running* :t
	*iris_color* +argb_green
	*iris_scale* 0.7
	*pupil_scale* 0.4)

(ui-window *window* ()
	(ui-title-bar _ "Eyes" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-backdrop _ (:style :plain :color +argb_black
			:min_width 0 :min_height 0)
		; This initial canvas is a placeholder that will be replaced in main.
		(ui-canvas *canvas* 1 1 1)))

;;;
;;; Drawing and Window Logic
;;;

(defun resize-window (w h)
	(defq parent (penv *canvas*))
	(def parent :min_width w :min_height h)
	(. *canvas* :sub)
	(setq *canvas* (Canvas w h 1))
	(. parent :add_child *canvas*)
	(. *canvas* :set_canvas_flags +canvas_flag_antialias)

	; Get current position and new preferred size
	(bind '(x y) (. *window* :get_pos))
	(bind '(pw ph) (. *window* :pref_size))

	; Fit and apply change
	(bind '(x y w h) (view-fit x y pw ph))
	(. *window* :change_dirty x y w h :t)
	(setq *last_mx* -1 *last_my* -1))

(defun circle (r)
	; Cached circle generation
	(memoize r (list (path-gen-arc 0.0 0.0 0.0 +fp_2pi r (path))) 11))

(defun redraw (mx my)
	(bind '(w h) (map (const n2f) (. *canvas* :pref_size)))
	(. *canvas* :fill +argb_black)

	; Get absolute canvas position and calculate relative mouse coordinates
	(bind '(canvas_x canvas_y _ _) (map (const n2f) (. (penv *window*) :get_relative *canvas*)))
	(defq rel_mx (- (n2f mx) canvas_x)
		  rel_my (- (n2f my) canvas_y))

	; Calculate eye dimensions and positions using configured scales
	(defq eye_radius (* h 0.48)
		  iris_radius (* eye_radius *iris_scale*)
		  pupil_radius (* iris_radius *pupil_scale*)
		  highlight_radius (* pupil_radius 0.3)
		  max_iris_dist (- eye_radius iris_radius)
		  left_eye_cx (* w 0.25)
		  right_eye_cx (* w 0.75)
		  eye_cy (* h 0.5))

	; --- Left Eye ---
	(defq vec_to_mouse (Vec2-f (- rel_mx left_eye_cx) (- rel_my eye_cy)))
	(defq dist_to_mouse (vec-length vec_to_mouse))
	(defq iris_offset (if (> dist_to_mouse 0.0)
		(vec-scale (vec-norm vec_to_mouse) (min dist_to_mouse max_iris_dist))
		(Vec2-f 0.0 0.0)))
	(bind '(l_iris_px l_iris_py) (vec-add (Vec2-f left_eye_cx eye_cy) iris_offset))

	; Draw left eye
	(.-> *canvas*
		(:set_color +argb_white)
		(:fpoly left_eye_cx eye_cy +winding_odd_even (circle eye_radius))
		(:set_color *iris_color*)
		(:fpoly l_iris_px l_iris_py +winding_odd_even (circle iris_radius))
		(:set_color +argb_black)
		(:fpoly l_iris_px l_iris_py +winding_odd_even (circle pupil_radius))
		(:set_color +argb_white)
		(:fpoly (+ l_iris_px (* pupil_radius -0.4)) (+ l_iris_py (* pupil_radius -0.4)) +winding_odd_even (circle highlight_radius)))

	; --- Right Eye ---
	(defq vec_to_mouse (Vec2-f (- rel_mx right_eye_cx) (- rel_my eye_cy)))
	(defq dist_to_mouse (vec-length vec_to_mouse))
	(defq iris_offset (if (> dist_to_mouse 0.0)
		(vec-scale (vec-norm vec_to_mouse) (min dist_to_mouse max_iris_dist))
		(Vec2-f 0.0 0.0)))
	(bind '(r_iris_px r_iris_py) (vec-add (Vec2-f right_eye_cx eye_cy) iris_offset))

	; Draw right eye
	(.-> *canvas*
		(:set_color +argb_white)
		(:fpoly right_eye_cx eye_cy +winding_odd_even (circle eye_radius))
		(:set_color *iris_color*)
		(:fpoly r_iris_px r_iris_py +winding_odd_even (circle iris_radius))
		(:set_color +argb_black)
		(:fpoly r_iris_px r_iris_py +winding_odd_even (circle pupil_radius))
		(:set_color +argb_white)
		(:fpoly (+ r_iris_px (* pupil_radius -0.4)) (+ r_iris_py (* pupil_radius -0.4)) +winding_odd_even (circle highlight_radius)))

	(. *canvas* :swap 0))

;;;
;;; Main Loop
;;;

(defun main ()
	(defq select (task-mboxes +select_size)
		  *last_mx* -1 *last_my* -1)
	(load-config)

	; Get initial dimensions and settings from config
	(defq w (ifn (. *config* :find :width) min_width)
		  h (ifn (. *config* :find :height) min_height))
	(setq *iris_color* (ifn (. *config* :find :iris_color) +argb_green))
	(setq *iris_scale* (ifn (. *config* :find :iris_scale) 0.7))
	(setq *pupil_scale* (ifn (. *config* :find :pupil_scale) 0.4))

	; Replace the placeholder canvas with the correctly sized one
	(defq parent (penv *canvas*))
	(def parent :min_width w :min_height h)
	(. *canvas* :sub)
	(setq *canvas* (Canvas w h 1))
	(. parent :add_child *canvas*)
	(. *canvas* :set_canvas_flags +canvas_flag_antialias)

	; Position and display the window for the first time
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(mail-timeout (elem-get select +select_timer) +rate 0)
	(redraw 0 0) ; Initial draw

	(while *running*
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_timer)
				(mail-timeout (elem-get select +select_timer) +rate 0)
				(bind '(mx my _ _) (gui-info))
				(when (or (/= mx *last_mx*) (/= my *last_my*))
					(setq *last_mx* mx *last_my* my)
					(redraw mx my)))

			((= (defq id (getf msg +ev_msg_target_id)) +event_close)
				(setq *running* :nil))

			((= id +event_min)
				(resize-window min_width min_height)
				(. *config* :insert :width min_width)
				(. *config* :insert :height min_height))

			((= id +event_max)
				(resize-window max_width max_height)
				(. *config* :insert :width max_width)
				(. *config* :insert :height max_height))

			(:t (. *window* :event msg))))

	(save-config)
	(gui-sub-rpc *window*))
