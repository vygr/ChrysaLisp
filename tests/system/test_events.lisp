(report-header "Events: the ids every app has, and an app's own after them")

(import "usr/env.inc")
(import "gui/lisp.inc")

;what is not an app's own widgets sends these, so they are the same for all
(assert-list-eq "the events of the system are the first ids" '(0 1 2 3 4 5 6)
	(list +event_close +event_max +event_min +event_theme +event_layout +event_zoom_in +event_zoom_out))
(assert-eq "and an app's own start after 16 of them" 16 +event_user)
(enums +event +event_user
	(enum mine yours))
(assert-list-eq "an app's enum goes on from there" '(16 17) (list +event_mine +event_yours))

;a title bar's three buttons are close, max, min, as they always were
(ui-window ev_window ()
	(ui-title-bar ev_title "Events" (+sym_close +sym_max +sym_min) +event_close))
(assert-list-eq "the buttons of a title bar are close, max and min" (list +event_close +event_max +event_min)
	(map (# (first (get :targets %0))) (filter (# (Button? %0)) (. ev_window :flatten))))

;the Logout app has a row of four, and they went on from close, 0 1 2 3,
;when the three after it were its own. Its window as the app makes it
(defq ev_said (test-output (cat
	;the file ends with its main, which the task this is run in has one
	;of, the window is made by then
	"(catch (import {apps/system/logout/app.lisp}) :t)"
	" (print (map (# (first (get :targets %0))) (filter (# (Button? %0)) (. *window* :flatten)))"
	" { } (list +event_close +event_logout +event_quit +event_shutdown))")))
(assert-eq "Logout's buttons are cancel, which is close, and its own three" "(0 16 17 18) (0 16 17 18)\n" ev_said)

;a new theme comes as an action, with the name after it, and a window that
;is passed it does its part
(defq ev_event (cat (setf-> (str-alloc +ev_msg_theme_size)
	(+ev_msg_type +ev_type_action) (+ev_msg_target_id +event_theme)
	(+ev_msg_action_source_id (. ev_window :get_id))) "Sharp"))
(assert-eq "the name of the theme is after the action" "Sharp" (slice ev_event +ev_msg_theme_name -1))
(. ev_window :theme "Regular")
(. ev_window :event ev_event)
(assert-true "a window passed the event has the theme's symbols"
	(eql (get :font (penv ev_title)) (create-font (theme-file "Sharp") (second (font-info (get :font (penv ev_title)))))))
