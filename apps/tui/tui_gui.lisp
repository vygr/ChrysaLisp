(open-child "service/gui/app.lisp" +kn_call_pin)

;wait for GUI service to declare itself so *root_env* has all GUI classes ready
(mail-await (# (mail-here "@Gui")))

(import "./tui.lisp")
