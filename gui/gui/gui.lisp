;can this node host a GUI ?
(and ((ffi _ "gui/gui/lisp_info" 0))
	;single instance per node only !
	(= 0 (length (mail-enquire "Gui,")))
	(import "./gui_impl.lisp"))
