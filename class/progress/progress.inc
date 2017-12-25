(import 'class/view/view.inc)

(def-class 'progress 'view)
(def-method 'create 'class/progress/create 'static '() '(r0))
(def-method 'new 'class/progress/new 'static nil '(r0))

(def-method 'pref_size 'class/progress/pref_size 'final)
(def-method 'draw 'class/progress/draw 'final)
(def-method 'layout 'class/progress/layout 'final)

(def-struct 'progress 'view)
(def-struct-end)
