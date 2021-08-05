;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-prev ()
	(setq *mol_index* (% (+ *mol_index* -1 (length mol_files)) (length mol_files))
		*dirty* t)
	(ball-file *mol_index*))

(defun action-next ()
	(setq *mol_index* (% (+ *mol_index* 1 (length mol_files)) (length mol_files))
		*dirty* t)
	(ball-file *mol_index*))

(defun action-minimise ()
	(bind '(x y w h) (apply view-fit
		(cat (. *window* :get_pos) (. *window* :pref_size))))
	(. *window* :change_dirty x y w h))

(defun action-maximise ()
	(bind '(x y w h) (apply view-fit
		(cat (. *window* :get_pos) (. *window* :pref_size))))
	(. *window* :change_dirty x y w h))

(defun action-close ()
	(setq *running* nil))

(defun get-rot (slider)
	(/ (* (i2r (get :value slider)) +real_2pi) (const (i2r 1000))))

(defun action-auto ()
	(defq button (elem 2 (. main_toolbar :children)))
	(undef (. button :dirty) :color)
	(cond
		((setq *auto_mode* (not *auto_mode*))
			(def button :color *env_radio_col*))
		(t  (setq *rotx* (get-rot xrot_slider)
				*roty* (get-rot yrot_slider)
				*rotz* (get-rot zrot_slider)
				*dirty* t))))

(defun action-xrot ()
	(if *auto_mode* (action-auto))
	(setq *rotx* (get-rot xrot_slider) *dirty* t))

(defun action-yrot ()
	(if *auto_mode* (action-auto))
	(setq *roty* (get-rot yrot_slider) *dirty* t))

(defun action-zrot ()
	(if *auto_mode* (action-auto))
	(setq *rotz* (get-rot zrot_slider) *dirty* t))
