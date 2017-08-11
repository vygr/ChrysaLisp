(import 'class/view/view.inc)

(def-class 'flow 'view)
(def-method 'create 'class/flow/create 'static nil '(r0))
(def-method 'new 'class/flow/new 'static nil '(r0))
(def-method 'init 'class/flow/init)
(def-method 'set_flow_flags 'class/flow/set_flow_flags 'static '(r0 r1) '(r0))

(def-method 'pref_size 'class/flow/pref_size 'final)
(def-method 'layout 'class/flow/layout 'final)

(def-bit 'flow_flag)
	(bit 'left 'right 'up 'down
		'fillw 'fillh 'lastw 'lasth
		'align_hcenter 'align_hleft 'align_hright
		'align_vcenter 'align_vtop 'align_vbottom)
(def-bit-end)

(def-struct 'flow 'view)
	(ulong 'flags)
(def-struct-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun class/flow/init ()
	;inputs
	;r0 = flow object
	;r1 = vtable pointer
	;outputs
	;r0 = flow object
	;r1 = 0 if error, else ok
	(vp-xor-rr r2 r2)
	(vp-cpy-ri r2 r0 flow_flags)
	(s-call 'flow 'init '(r0 r1) '(r0 r1)))

(defcfun class/flow/set_flow_flags ()
	;inputs
	;r0 = view object
	;r1 = flags
	(vp-cpy-ri r1 r0 flow_flags))
