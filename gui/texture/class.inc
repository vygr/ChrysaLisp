(import 'class/obj/class.inc)

(def-class 'texture 'obj)
(dec-method 'vtable 'gui/class_texture)
(dec-method 'create 'gui/texture/create 'static '(r0 r1 r2) '(r0))
(dec-method 'new 'gui/texture/new 'static nil '(r0))
(dec-method 'init 'gui/texture/init 'static '(r0 r1 r2 r3 r4))
(dec-method 'get_metrics 'gui/texture/get_metrics 'static '(r0) '(r0 r1 r2 r3))

(dec-method 'deinit 'gui/texture/deinit 'final)

(def-struct 'texture 'obj)
	(ulong 'handle)
	(uint 'width)
	(uint 'height)
(def-struct-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun gui/texture/init ()
	;inputs
	;r0 = texture object (ptr)
	;r1 = vtable (pptr)
	;r2 = texture handle (ulong)
	;r3 = texture width (pixels)
	;r4 = texture height (pixels)
	;outputs
	;r0 = texture object (ptr)
	;r1 = 0 if error, else ok
	(f-entry 'texture 'init '(r0 r1 (r0 texture_handle) (r0 texture_width i) (r0 texture_height i)))
	(s-call 'texture 'init '(r0 r1) '(r0 r1))
	(f-exit 'texture 'init '(r0 r1))
	(vp-ret))

(defcfun gui/texture/get_metrics ()
	;inputs
	;r0 = texture object (ptr)
	;outputs
	;r0 = texture object (ptr)
	;r1 = texture handle (ulong)
	;r2 = width (pixels)
	;r3 = height (pixels)
	(vp-cpy-ir r0 texture_handle r1)
	(vp-cpy-ir-ui r0 texture_width r2)
	(vp-cpy-ir-ui r0 texture_height r3))
