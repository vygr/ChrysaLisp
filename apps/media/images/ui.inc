;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-close ()
	(setq *running* :nil))

(defun action-minimise ()
	(when *canvas*
		(bind '(w h) (. *canvas* :pref_size))
		(def *image_scroll* :min_width (/ w 2) :min_height (/ h 2))
		(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
		(def *image_scroll* :min_width 128 :min_height 128)
		(. *window* :change_dirty x y w h)))

(defun action-maximise ()
	(when *canvas*
		(bind '(w h) (. *canvas* :pref_size))
		(def *image_scroll* :min_width w :min_height h)
		(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
		(def *image_scroll* :min_width 128 :min_height 128)
		(. *window* :change_dirty x y w h)))

(defun action-file-leaf-action ()
	;load the file selected
	(win-refresh (. *file_selector* :get_route
		(. *window* :find_id (getf *msg* +ev_msg_action_source_id)))))