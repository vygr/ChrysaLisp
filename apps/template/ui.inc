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


(defun select-panel (panel)
	(. panel :to_front))

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
