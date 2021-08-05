;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-prev ()
	(reset))

(defun action-next ()
	(reset))

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

(defun action-xrot ()
	(setq *rotx* (/ (* (i2r (get :value xrot_slider)) +real_2pi) (const (i2r 1000)))
		*dirty* t))

(defun action-yrot ()
	(setq *roty* (/ (* (i2r (get :value yrot_slider)) +real_2pi) (const (i2r 1000)))
		*dirty* t))

(defun action-zrot ()
	(setq *rotz* (/ (* (i2r (get :value zrot_slider)) +real_2pi) (const (i2r 1000)))
		*dirty* t))
