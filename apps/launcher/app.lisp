;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_button))

(defq app_list '(
	"terminal"
	"debug"
	"help"
	"docs"
	"netmon"
	"stats"
	"boing"
	"freeball"
	"images"
	"films"
	"canvas"
	"raymarch"
	"pcb"
	"calculator"
	"chess"
	"clock"))

(defq auto_app_list '(
	"clock"
	"terminal"))

; TODO - change above into default lists in a seperate file
; with a personalized pupa file for users.

(each (lambda (_)
	(open-child (cat "apps/" _ "/app.lisp") kn_call_open)) auto_app_list)

(ui-tree window (create-window 0) nil
	(ui-element _ (create-flow) ('flow_flags (logior flow_flag_down flow_flag_fillw) 'color toolbar_col)
		(each (lambda (path)
			(component-connect (ui-element _ (create-button) ('text path)) event_win_button)) app_list)))

(window-set-title window "Launcher")
(bind '(w h) (view-pref-size window))
(gui-add (view-change window 16 16 (+ w 32) h))

(while t
	(cond
		((= (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id) event_win_button)
			(open-child (cat "apps/" (get (view-find-id window (get-long msg ev_msg_action_source_id)) 'text) "/app.lisp") kn_call_open))
		(t (view-event window msg))))

(view-hide window)
