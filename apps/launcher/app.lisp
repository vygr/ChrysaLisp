;imports
(import 'sys/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_button))

(defq app_list '(
	"apps/terminal/app.lisp"
	"apps/debug/app.lisp"
	"apps/help/app.lisp"
	"apps/netmon/app.lisp"
	"apps/boing/app.lisp"
	"apps/images/app.lisp"
	"apps/films/app.lisp"
	"apps/canvas/app.lisp"
	"apps/raymarch/app.lisp"
	"apps/pcb/app.lisp"
	"apps/calculator/app.lisp"
	"apps/clock/app.lisp"))

(defq auto_app_list '(
	"apps/clock/app.lisp"))

;auto start
(each (lambda (_)
	(open-child _ kn_call_open)) auto_app_list)

(ui-tree window (create-window 0) nil
	(ui-element _ (create-flow) ('flow_flags (bit-or flow_flag_down flow_flag_fillw) 'color argb_yellow)
		(each (lambda (path)
			(button-connect-click (ui-element _ (create-button) ('text path)) event_win_button)) app_list)))

(window-set-title window "Launcher")
(bind '(w h) (view-pref-size window))
(gui-add (view-change window 16 16 (add w 32) h))

(while t
	(cond
		((eq (get-long (defq msg (mail-mymail)) ev_msg_target_id) event_win_button)
			(open-child (get (view-find-id window (get-long msg ev_msg_action_source_id)) 'text) kn_call_open))
		(t (view-event window msg))))
