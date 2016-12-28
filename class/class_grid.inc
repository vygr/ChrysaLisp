(import 'class/class_view.inc)

(def-class 'grid 'view)
(def-method 'create 'class/grid/create)
(def-method 'new 'class/grid/new)
(def-method 'init 'class/grid/init)
(def-method 'set_grid 'class/grid/set_grid 'static '(r0 r10 r11))

(def-method 'pref_size 'class/grid/pref_size 'override)
(def-method 'layout 'class/grid/layout 'override)

(def-struct 'grid 'view)
	(uint 'grid_width)
	(uint 'grid_height)
(def-struct-end)
