;import settings
(run 'sys/lisp.inc)
(run 'gui/lisp.inc)
(run 'class/flow/lisp.inc)
(run 'class/window/lisp.inc)
(run 'class/button/lisp.inc)
(run 'class/view/lisp.inc)

(structure 'event 0
	(byte 'win_button))

(defq app_list '(
	"apps/terminal/app"
	"apps/netmon/app.lisp"
	"apps/boing/app.lisp"
	"apps/canvas/app.lisp"
	"apps/raymarch/app.lisp"
	"apps/calculator/app.lisp"
	"tests/farm.lisp"
	"tests/pipe.lisp"
	"tests/global.lisp"
	"tests/migrate.lisp"))

(ui-tree window (create-window 0) nil
	(ui-element _ (create-flow) ('flow_flags (bit-or flow_flag_down flow_flag_fillw) 'color 0xffffff00)
		(each (lambda (path)
			(button-connect-click (ui-element _ (create-button) ('text path)) event_win_button)) app_list)))

(window-set-title window "Launcher")
(bind '(w h) (view-pref-size window))
(view-change window 32 32 (add w 32) h)
(gui-add window)

(while t
	(cond
		((eq (read-long ev_msg_target_id (defq msg (mail-mymail))) event_win_button)
			(open-child (get (view-find-id window (read-long ev_msg_action_source_id msg)) 'text) kn_call_open))
		(t (view-event window msg))))
