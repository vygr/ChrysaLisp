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

(defun action-main-tab ()
	(debug-brk "main" :t)
	(select-panel *main_widget*))

(defun action-settings-tab ()
	(debug-brk "settings" :t)
	(select-panel *settings_widget*))

(defun action-status-tab ()
	(debug-brk "status" :t)
	(select-panel *status_widget*))

(defun action-info-tab ()
	(debug-brk "info" :t)
	(select-panel *info_widget*))

(defun action-button-1 ()
	(debug-brk "button1" :t))

(defun action-button-2 ()
	(debug-brk "button2" :t))

(defun action-button-3 ()
	(debug-brk "button3" :t))

(defun action-button-4 ()
	(debug-brk "button4" :t))
