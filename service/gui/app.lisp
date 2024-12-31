;can this node host a GUI ?
(and ((ffi _ "service/gui/lisp_info" 0))
	;single instance per node only !
	(empty? (mail-enquire "Gui,"))
	(import "./app_impl.lisp"))
