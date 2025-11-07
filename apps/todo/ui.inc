;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-minimise ()
	(layout-items *min_width* *min_height*)
	(bind '(x y) (. *window* :get_pos))
	(bind '(w h) (. *window* :pref_size))
	(bind '(x y w h) (view-fit x y w h))
	(. *window* :change_dirty x y w h))

(defun action-maximise ()
	(layout-items *max_width* *max_height*)
	(bind '(x y) (. *window* :get_pos))
	(bind '(w h) (. *window* :pref_size))
	(bind '(x y w h) (view-fit x y w h))
	(. *window* :change_dirty x y w h))

(defun action-close ()
	(setq *running* :nil))
