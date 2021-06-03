(import "sys/lisp.inc")

;can this node host a GUI ?
(if ((ffi gui-info "gui/gui/lisp_info" 0))
	;single instance per node only
	(if (= 0 (length
			(filter (# (eql (slice +long_size -1 (task-mailbox)) (slice +long_size -1 %0)))
			(map (# (to-net-id (elem 1 (split %0 ",")))) (mail-enquire "GUI_SERVICE")))))
		(import "gui/gui/gui_impl.lisp")))
