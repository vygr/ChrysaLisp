;debug options
(case :nil
(0 (import "lib/debug/frames.inc")))

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/clipboard/app.inc")

;our UI widgets
(import "./widgets.inc")

(enums +select 0
	(enum main tip timer))

(defq +rate (/ 1000000 1))

;import actions and bindings
(import "./actions.inc")

(defun main ()
	(defq select (task-mboxes +select_size) *running* :t)
	(def *window* :tip_mbox (elem-get select +select_tip))
	(. *window* :show)
	(mail-timeout (elem-get select +select_timer) +rate 0)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				;tip event
				(. *window* :tip *msg*))
			((= idx +select_timer)
				;timer event
				(mail-timeout (elem-get select +select_timer) +rate 0))
			;must be +select_main
			((. *window* :dispatch *msg*))
			((. *window* :event *msg*))))
	(gui-sub-rpc *window*))
