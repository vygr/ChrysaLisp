;lisp bindings
(ffi create-window "gui/window/lisp_create" 0)

(defun window-set-title (window str)
	(defq title (get-field window window_title 0))
	(def title 'text str)
	(view-layout title)
	window)

(defun window-set-status (window str)
	(defq status (get-field window window_status 0))
	(def status 'text str)
	(view-layout status)
	window)

(defun window-connect-close (window id)
	(component-connect (get-field window window_close 0) id)
	window)

(defun window-connect-min (window id)
	(component-connect (get-field window window_min 0) id)
	window)

(defun window-connect-max (window id)
	(component-connect (get-field window window_max 0) id)
	window)
