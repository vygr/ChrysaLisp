(import 'class/view/view.inc)

(def-class 'grid 'view)
(def-method 'create 'class/grid/create 'static nil '(r0))
(def-method 'new 'class/grid/new 'static nil '(r0))
(def-method 'init 'class/grid/init)
(def-method 'set_grid 'class/grid/set_grid 'static '(r0 r10 r11) '(r0))

(def-method 'pref_size 'class/grid/pref_size 'final)
(def-method 'layout 'class/grid/layout 'final)

(def-struct 'grid 'view)
	(uint 'width)
	(uint 'height)
(def-struct-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun class/grid/init ()
	;inputs
	;r0 = grid object
	;r1 = vtable pointer
	;outputs
	;r0 = grid object
	;r1 = 0 if error, else ok
	(vp-cpy-cr 1 r2)
	(assign '(r2 r2) '((r0 grid_width i) (r0 grid_height i)))
	(s-call 'grid 'init '(r0 r1) '(r0 r1)))

(defcfun class/grid/set_grid ()
	;inputs
	;r0 = view object
	;r10 = width
	;r11 = height
	(assign '(r10 r11) '((r0 grid_width i) (r0 grid_height i))))
