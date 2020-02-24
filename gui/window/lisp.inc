;lisp bindings
(ffi create-window "gui/window/lisp_create" 0)

(defun-bind window-set-title (window txt)
	;(window-set-title window txt) -> window
	(defq title (get-field window (const window_title) 0))
	(def title 'text txt)
	(view-layout title)
	window)

(defun-bind window-set-status (window txt)
	;(window-set-status window txt) -> window
	(defq status (get-field window (const window_status) 0))
	(def status 'text txt)
	(view-layout status)
	window)

(defun-bind window-connect-close (window id)
	;(window-connect-close window id) -> window
	(component-connect (get-field window (const window_close) 0) id)
	window)

(defun-bind window-connect-min (window id)
	;(window-connect-min window id) -> window
	(component-connect (get-field window (const window_min) 0) id)
	window)

(defun-bind window-connect-max (window id)
	;(window-connect-max window id) -> window
	(component-connect (get-field window (const window_max) 0) id)
	window)
