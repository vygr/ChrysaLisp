;lisp bindings
(ffi create-window "gui/window/lisp_create" 0)

(defun window-set-title (window str)
	(defq title (get window 'window_title))
	(def title 'text str)
	(view-layout title)
	window)

(defun window-set-status (window str)
	(defq status (get window 'window_status))
	(when status
		(def status 'text str)
		(view-layout status))
	window)

(defun window-connect-close (window id)
	(component-connect (get window 'window_close) id)
	window)

(defun window-connect-min (window id)
	(component-connect (get window 'window_min) id)
	window)

(defun window-connect-max (window id)
	(component-connect (get window 'window_max) id)
	window)
