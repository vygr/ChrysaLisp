;common imports and tuples
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

;quick debug switch
(when nil	;t for debug
	(import "lib/debug/debug.inc")
	(defmacro defun-bind (&rest _) `(defun-debug ~_)))

;quick profiling switch
(if t	;t for profiling
	(import "lib/debug/profile.inc")
	(defun profile-print (&rest _)))

(structure '+dlist 0
	(byte 'mask+ 'rate+ 'flatten+ 'commited_canvas+ 'overlay_canvas+ 'commited_polygons+ 'overlay_paths+))

(structure '+path 0
	(byte 'mode+ 'color+ 'radius+ 'path+))
