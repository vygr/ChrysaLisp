;import into the shared root env of this node !
;comment <- lines out if need to...
(defq e (env))
(while (penv e) (setq e (penv e))) ;<-
(eval '(env 307) e) ;<-
(import "sys/lisp.inc"e)
(import "class/lisp.inc" e)
(import "gui/lisp.inc" e)

;can this node host a GUI ?
(when (gui-info)
	;profiling callbacks on the GUI thread from :draw method !!!
	(defq *profile* (env -1) *profile_ret* (list))

	;single instance only on this node
	(defq gui_services (mail-enquire "GUI_SERVICE")
		node (slice +long_size -1 (task-mailbox)))
	(when (notany (# (eql node (slice +long_size -1
			(to-net-id (elem 1 (split %0 ",")))))) gui_services)
		(mail-declare (task-mailbox) "GUI_SERVICE" "GUI Service 0.2")

		;screen widget
		(def (defq screen (Backdrop)) :style :grid :color +argb_grey2
			:ink_color +argb_grey1)
		(.-> screen (:change 0 0 1280 960) :dirty_all)

		;fire up the login app
		(open-child "apps/login/app.lisp" +kn_call_open)
		(open-child "apps/clipboard/app.lisp" +kn_call_open)

		;jump to gui compositor
		((ffi _ "gui/gui/gui" 0) screen))
)
