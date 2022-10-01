;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-canvas (size)
	(defq parent (penv main_widget))
	(. main_widget :sub)
	(setq main_widget (Canvas size size canvas_scale) canvas_size size *dirty* :t)
	(bind '(w h) (. parent :get_size))
	(. parent :add_child main_widget)
	(.-> main_widget (:set_canvas_flags +canvas_mode) (:change 0 0 w h))
	(. *window* :layout))

(defun set-size (size)
	(def (defq parent (penv main_widget)) :min_width size :min_height size)
	(bind '(x y w h) (apply view-fit
		(cat (. *window* :get_pos) (. *window* :pref_size))))
	(. *window* :change_dirty x y w h)
	(def parent :min_width +min_size :min_height +min_size)
	(set-canvas size))

(defun action-minimise ()
	(set-size +min_size))

(defun action-maximise ()
	(set-size +max_size))

(defun action-layout ()
	(bind '(w h) (. (penv main_widget) :get_size))
	(set-canvas (min w h)))

(defun action-close ()
	(setq *running* :nil))

(defun action-mode ()
	(defq button (elem-get 0 (. main_toolbar :children)))
	(undef (. button :dirty) :color)
	(if (setq *dirty* :t *render_mode* (not *render_mode*))
		(def button :color *env_radio_col*)))

(defun action-auto ()
	(defq button (elem-get 1 (. main_toolbar :children)))
	(undef (. button :dirty) :color)
	(if (setq *auto_mode* (not *auto_mode*))
		(def button :color *env_radio_col*)))

(defun action-xrot ()
	(if *auto_mode* (action-auto))
	(setq *rotx* (get-rot xrot_slider) *dirty* :t))

(defun action-yrot ()
	(if *auto_mode* (action-auto))
	(setq *roty* (get-rot yrot_slider) *dirty* :t))

(defun action-zrot ()
	(if *auto_mode* (action-auto))
	(setq *rotz* (get-rot zrot_slider) *dirty* :t))

(defun action-style-plain ()
	(def (. main_backdrop :dirty) :style :plain)
	(radio-select style_toolbar 0))

(defun action-style-grid ()
	(def (. main_backdrop :dirty) :style :grid)
	(radio-select style_toolbar 1))

(defun action-style-axis ()
	(def (. main_backdrop :dirty) :style :axis)
	(radio-select style_toolbar 2))
