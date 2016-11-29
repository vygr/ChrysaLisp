(import 'class/class_view.inc)

(defcvar 'progress_border_size 1)

(def-class 'progress 'view)
(def-method 'create 'class/progress/create)
(def-method 'new 'class/progress/new)
(def-method 'set_max 'class/progress/set_max 'static '(r0 r1))
(def-method 'set_val 'class/progress/set_val 'static '(r0 r1))

(def-method 'pref_size 'class/progress/pref_size 'override)
(def-method 'draw 'class/progress/draw 'override)
(def-method 'layout 'class/progress/layout 'override)

(def-struct 'progress 'view)
	(long 'progress_max)
	(long 'progress_val)
(def-struct-end)
