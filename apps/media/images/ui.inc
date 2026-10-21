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

(defun action-open ()
	;the picker, at the picture shown, its places those pictures were taken
	;from, the one open told to go first
	(if *picker* (mail-send *picker* ""))
	(setq *picker* (file-pick (elem-get select +select_picker) "Open image..." (ifn *file* ".")
		(join +file_types " ") :nil "images" :t)))
