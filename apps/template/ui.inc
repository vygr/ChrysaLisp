;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-minimise ()
	(bind '(x y w h) (apply view-fit
		(cat (. *window* :get_pos) (. *window* :pref_size))))
	(. *window* :change_dirty x y w h))

(defun action-maximise ()
	(bind '(x y) (. *window* :get_pos))
	(bind '(w h) (. *window* :pref_size))
	(bind '(x y w h) (view-fit x y (* w 2) (* h 2)))
	(. *window* :change_dirty x y w h))

(defun action-close ()
	(setq *running* :nil))

(defun action-button-1 ()
	(def (. *b1* :dirty) :paper (random-color)))

(defun action-button-2 ()
	(def (. *b2* :dirty) :paper (random-color)))

(defun action-button-3 ()
	(def (. *b3* :dirty) :paper (random-color)))

(defun action-button-4 ()
	(def (. *b4* :dirty) :paper (random-color)))
