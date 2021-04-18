;common imports
(import "sys/lisp.inc")
(import "class/lisp.inc")

;quick profiling switch
(if t	;t for profiling
	(import "lib/debug/profile.inc")
	(defun profile-report (&rest _)))

(import "gui/lisp.inc")

;quick debug switch
(when nil	;t for debug
	(import "lib/debug/debug.inc"))

(enums +dlist 0
	(enum mask commited_canvas overlay_canvas commited_polygons overlay_paths))

(enums +path 0
	(enum mode color radius path))

(enums +event 0
	(enum close max min)
	(enum save load clear undo redo)
	(enum grid plain axis lines)
	(enum radius1 radius2 radius3)
	(enum pen line arrow1 arrow2 box circle fbox fcircle)
	(enum black white red green blue cyan yellow magenta
		tblack twhite tred tgreen tblue tcyan tyellow tmagenta))
