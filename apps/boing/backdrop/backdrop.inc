(import 'class/view/view.inc)

(def-class 'backdrop 'view)
(def-method 'create 'apps/boing/backdrop/create 'static '() '(r0))
(def-method 'new 'apps/boing/backdrop/new 'static nil '(r0))

(def-method 'pref_size 'apps/boing/backdrop/pref_size 'override)
(def-method 'draw 'apps/boing/backdrop/draw 'override)
(def-method 'layout 'apps/boing/backdrop/layout 'override)

(def-method 'lisp_create 'apps/boing/backdrop/lisp_create 'static '(r0 r1) '(r0 r1))

(def-struct 'backdrop 'view)
(def-struct-end)
