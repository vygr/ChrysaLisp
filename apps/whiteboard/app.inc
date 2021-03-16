;common imports
(import "sys/lisp.inc")
(import "class/lisp.inc")

;quick profiling switch
(if t	;t for profiling
	(import "lib/debug/profile.inc")
	(defun profile-print (&rest _)))

(import "gui/lisp.inc")

;quick debug switch
(when nil	;t for debug
	(import "lib/debug/debug.inc")
	(defmacro defun (&rest _) `(defun-debug ~_)))

(structure +dlist 0
	(byte mask commited_canvas overlay_canvas commited_polygons overlay_paths))

(structure +path 0
	(byte mode color radius path))

(structure +event 0
	(byte close max min)
	(byte save load clear undo redo)
	(byte grid plain axis lines)
	(byte radius1 radius2 radius3)
	(byte pen line arrow1 arrow2 box circle fbox fcircle)
	(byte black white red green blue cyan yellow magenta
		tblack twhite tred tgreen tblue tcyan tyellow tmagenta))
