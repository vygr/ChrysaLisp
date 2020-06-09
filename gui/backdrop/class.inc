(include 'gui/view/class.inc)

(def-class 'backdrop 'view)
(dec-method 'vtable 'gui/backdrop/vtable)
(dec-method 'create 'gui/backdrop/create :static '() '(r0))

(dec-method 'pref_size 'gui/backdrop/pref_size :final)
(dec-method 'draw 'gui/backdrop/draw :final)
(dec-method 'layout 'gui/backdrop/layout :override)

(dec-method 'lisp_create 'gui/backdrop/lisp_create :static '(r0 r1) '(r0 r1))

(def-struct 'backdrop 'view)
(def-struct-end)
