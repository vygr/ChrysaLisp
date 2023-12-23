;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-minimise ()
	(debug-brk "minimize" :t)
	(bind '(x y w h) (apply view-fit
		(cat (. *window* :get_pos) (. *window* :pref_size))))
	(. *window* :change_dirty x y w h))

(defun action-maximise ()
	(debug-brk "maximize" :t)
	(bind '(x y) (. *window* :get_pos))
	(bind '(w h) (. *window* :pref_size))
	(bind '(x y w h) (view-fit x y (* w 2) (* h 2)))
	(. *window* :change_dirty x y w h))

(defun action-close ()
	(debug-brk "close" :t)
	(setq *running* :nil))

(defun action-button-1 ()
	(debug-brk "button1" :t)
	(def (. *b1* :dirty) :color (random-color)))

(defun action-button-2 ()
	(debug-brk "button2" :t)
	(def (. *b2* :dirty) :color (random-color)))

(defun action-button-3 ()
	(debug-brk "button3" :t)
	(def (. *b3* :dirty) :color (random-color)))

(defun action-button-4 ()
	(debug-brk "button4" :t)
	(def (. *b4* :dirty) :color (random-color)))
