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

(ui-window *window* ()
	(ui-title-bar _ "Eyes" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-backdrop *backdrop* (:style :plain :color +argb_black)))

;;;
;;; Drawing and Window Logic
;;;

(defun resize-window (w h)
	(. *config* :insert :width w)
	(. *config* :insert :height h)

	(def *backdrop* :min_width w :min_height h)
	(if *canvas* (. *canvas* :sub))
	(setq *canvas* (Canvas w h 1))
	(. *canvas* :set_canvas_flags +canvas_flag_antialias)
	(. *backdrop* :add_child *canvas*)

	; Get current position and new preferred size
	(bind '(x y) (. *window* :get_pos))
	(bind '(pw ph) (. *window* :pref_size))

	; Fit and apply change
	(bind '(x y w h) (view-fit x y pw ph))
	(. *window* :change_dirty x y w h :t)
	(setq *last_mx* -1 *last_my* -1))

(defun circle (r)
	; Cached circle generation
	(memoize r (list (path-gen-arc 0.0 0.0 0.0 +fp_2pi r (path))) 3))

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
		*last_mx* -1 *last_my* -1
		+rate (/ 1000000 30) ; 30 FPS
		min_width 256 min_height 128
		max_width 512 max_height 256
		*canvas* :nil *running* :t)
	(load-config)

	; Get initial dimensions and settings from config
	(bind '(w h *iris_color* *iris_scale* *pupil_scale*)
		(gather *config* :width :height :iris_color
			:iris_scale :pupil_scale))

	; Position and display the window for the first time
	(resize-window w h)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))

	(mail-timeout (elem-get select +select_timer) +rate 0)
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
				(resize-window min_width min_height))

			((= id +event_max)
				(resize-window max_width max_height))

			(:t (. *window* :event msg))))

	(save-config)
	(gui-sub-rpc *window*))
