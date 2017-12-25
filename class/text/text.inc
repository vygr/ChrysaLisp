(import 'class/view/view.inc)

(def-class 'text 'view)
(def-method 'create 'class/text/create 'static '() '(r0))
(def-method 'new 'class/text/new 'static nil '(r0))
(def-method 'init 'class/text/init)
(def-method 'set_text 'class/text/set_text 'static '(r0 r1) '(r0))
(def-method 'get_text 'class/text/get_text 'static '(r0) '(r0 r1))
(def-method 'ref_text 'class/text/ref_text 'static '(r0) '(r0 r1))

(def-method 'deinit 'class/text/deinit 'final)
(def-method 'pref_size 'class/text/pref_size 'final)
(def-method 'draw 'class/text/draw 'final)

(def-struct 'text 'view)
	(ptr 'string)
	(ptr 'words)
(def-struct-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun class/text/get_text ()
	;inputs
	;r0 = text object
	;outputs
	;r0 = text object
	;r1 = 0 else, string object
	(vp-cpy-ir r0 text_string r1))
