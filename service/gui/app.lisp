;can this node host a GUI ?
(and ((ffi "service/gui/lisp_info"))
	;single instance per node only !
	(not (mail-here "@Gui"))
	(import "./app_impl.lisp"))
