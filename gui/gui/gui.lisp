(import "class/lisp.inc")
(import "sys/lisp.inc")
(import "gui/lisp.inc")

;screen widget
(def (defq screen (Backdrop)) :style 0 :color +argb_grey2+ :ink_color +argb_grey1+)
(.-> screen (:change 0 0 1280 960) :dirty_all)

;fire up the login app
(open-child "apps/login/app.lisp" kn_call_open)
(open-child "apps/clipboard/app.lisp" kn_call_open)

;jump to gui compositor
((ffi _ "gui/gui/gui" 0) screen)
