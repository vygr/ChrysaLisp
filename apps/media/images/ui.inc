;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-close ()
	(setq *running* :nil))

(defun action-minimise ()
	(when *canvas*
		(bind '(w h) (. *canvas* :pref_size))
		(def *image_scroll* :want_width (/ w 2) :want_height (/ h 2))
		(. *window* :fit)))

(defun action-maximise ()
	(when *canvas*
		(bind '(w h) (. *canvas* :pref_size))
		(def *image_scroll* :want_width w :want_height h)
		(. *window* :fit)))

(defun action-open ()
	;the picker, at the picture shown, its places those pictures were taken
	;from, the one open told to go first
	(if *picker* (mail-send *picker* ""))
	(setq *picker* (file-pick (elem-get select +select_picker) "Open image..." (ifn *file* ".")
			(join +file_types " ") :nil "images" :t '("apps/media/images/data"))))
