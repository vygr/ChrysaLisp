(include "gui/view/class.inc")

(def-class vdu view
	(dec-method :vtable gui/vdu/vtable)
	(dec-method :create gui/vdu/create :static () (:r0))
	(dec-method :init gui/vdu/init)
	(dec-method :deinit gui/vdu/deinit :final)
	(dec-method :draw gui/vdu/draw :final)
	(dec-method :lisp_create gui/vdu/lisp_create :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_load gui/vdu/lisp_load :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_configure gui/vdu/lisp_configure :static (:r0 :r1) (:r0 :r1)))

(def-struct vdu view_size
	(ptr font textures)
	(plong chars)
	(uint width height)
	(ushort char_width char_height))
