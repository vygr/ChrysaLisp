(import 'gui/view/class.inc)

(def-class 'backdrop 'view)
(dec-method 'vtable 'apps/boing/class_backdrop)
(dec-method 'create 'apps/boing/backdrop/create 'static '() '(r0))
(dec-method 'new 'apps/boing/backdrop/new 'static nil '(r0))

(dec-method 'pref_size 'apps/boing/backdrop/pref_size 'override)
(dec-method 'draw 'apps/boing/backdrop/draw 'override)
(dec-method 'layout 'apps/boing/backdrop/layout 'override)

(dec-method 'lisp_create 'apps/boing/backdrop/lisp_create 'static '(r0 r1) '(r0 r1))

(def-struct 'backdrop 'view)
(def-struct-end)
