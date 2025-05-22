;can this node host a GUI ?
(and ((ffi "service/gui/lisp_info"))
	;single instance per node only !
	(empty? (mail-enquire "Gui,"))
	(import "./app_impl.lisp"))
