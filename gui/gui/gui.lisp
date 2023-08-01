;can this node host a GUI ?
(env-push)
(import "sys/lisp.inc")
(import "./lisp.inc")
(if (gui-info)
	;single instance per node only
	(when (= 0 (length (mail-enquire "Gui,")))
		(env-pop)
		(import "./gui_impl.lisp")
		(env-push)))
(env-pop)
