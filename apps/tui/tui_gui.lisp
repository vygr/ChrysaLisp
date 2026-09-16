(open-child "service/gui/app.lisp" +kn_call_pin)

;wait for GUI service to declare itself so *root_env* has all GUI classes ready
(while (empty? (mail-enquire "Gui,"))
	(task-sleep 10000))

(import "./tui.lisp")
