(include 'gui/view/class.inc)

(def-class 'scroll 'view)
(dec-method :vtable 'gui/scroll/vtable)
(dec-method :create 'gui/scroll/create :static '(r0) '(r0))
(dec-method :init 'gui/scroll/init :static '(r0 r1 r2))

(dec-method :add_child 'gui/scroll/add_child :final)
(dec-method :layout 'gui/scroll/layout :final)
(dec-method :pref_size 'gui/scroll/pref_size :final)
(dec-method :action 'gui/scroll/action :final)

(dec-method :lisp_create 'gui/scroll/lisp_create :static '(r0 r1) '(r0 r1))

(def-bit 'scroll_flag)
	(bit 'vertical 'horizontal)
(def-bit-end)

(def-struct 'scroll 'view)
	(ptr 'vslider)
	(ptr 'hslider)
	(ptr 'child)
(def-struct-end)
