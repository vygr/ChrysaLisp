(import 'class/view/view.inc)

(def-class 'grid 'view)
(def-method 'create 'class/grid/create)
(def-method 'new 'class/grid/new)
(def-method 'init 'class/grid/init)
(def-method 'set_grid 'class/grid/set_grid 'static '(r0 r10 r11))

(def-method 'pref_size 'class/grid/pref_size 'final)
(def-method 'layout 'class/grid/layout 'final)

(def-struct 'grid 'view)
	(uint 'grid_width)
	(uint 'grid_height)
(def-struct-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun class/grid/set_grid ()
	;inputs
	;r0 = view object
	;r10 = width
	;r11 = height
	(assign '(r10 r11) '((r0 grid_width i) (r0 grid_height i))))
