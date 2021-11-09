;can this node host a GUI ?
(env-push)
(import "sys/lisp.inc")
(import "gui/gui/lisp.inc")
(if (gui-info)
	;single instance per node only
	(when (= 0 (length
			(filter (# (eql (slice +long_size -1 (task-mailbox)) (slice +long_size -1 %0)))
			(map (# (to-net-id (elem-get 1 (split %0 ",")))) (mail-enquire "GUI_SERVICE")))))
		(env-pop)
		(import "gui/gui/gui_impl.lisp")
		(env-push)))
(env-pop)
