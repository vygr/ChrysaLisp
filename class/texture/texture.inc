(import 'class/obj/obj.inc)

(def-class 'texture 'obj)
(def-method 'create 'class/texture/create 'static '(r0 r1 r2) '(r0))
(def-method 'new 'class/texture/new 'static nil '(r0))
(def-method 'init 'class/texture/init 'static '(r0 r1 r2 r3 r4))
(def-method 'get_metrics 'class/texture/get_metrics 'static '(r0) '(r0 r1 r2 r3))

(def-method 'deinit 'class/texture/deinit 'final)

(def-struct 'texture 'obj)
	(ulong 'handle)
	(uint 'width)
	(uint 'height)
(def-struct-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun class/texture/init ()
	;inputs
	;r0 = texture object (ptr)
	;r1 = vtable (pptr)
	;r2 = texture handle
	;r3 = texture width
	;r4 = texture height
	;outputs
	;r0 = texture object (ptr)
	;r1 = 0 if error, else ok
	(f-entry 'texture 'init '(r0 r1 (r0 texture_handle) (r0 texture_width i) (r0 texture_height i)))
	(s-call 'texture 'init '(r0 r1) '(r0 r1))
	(f-exit 'texture 'init '(r0 r1))
	(vp-ret))

(defcfun class/texture/get_metrics ()
	;inputs
	;r0 = texture object (ptr)
	;outputs
	;r0 = texture object (ptr)
	;r1 = handle
	;r2 = width (pixels)
	;r3 = height (pixels)
	(vp-cpy-ir r0 texture_handle r1)
	(vp-cpy-ir-ui r0 texture_width r2)
	(vp-cpy-ir-ui r0 texture_height r3))
