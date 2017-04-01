(import 'class/view/view.inc)

(defcvar 'progress_border_size 1)

(def-class 'progress 'view)
(def-method 'create 'class/progress/create 'static nil '(r0))
(def-method 'new 'class/progress/new 'static nil '(r0))
(def-method 'set_max 'class/progress/set_max 'static '(r0 r1) '(r0))
(def-method 'set_val 'class/progress/set_val 'static '(r0 r1) '(r0))

(def-method 'pref_size 'class/progress/pref_size 'final)
(def-method 'draw 'class/progress/draw 'final)
(def-method 'layout 'class/progress/layout 'final)

(def-struct 'progress 'view)
	(long 'progress_max)
	(long 'progress_val)
(def-struct-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun class/progress/set_max ()
	;inputs
	;r0 = progress object
	;r1 = maximum value
	(vp-cpy-ri r1 r0 progress_max))

(defcfun class/progress/set_val ()
	;inputs
	;r0 = progress object
	;r1 = value
	;outputs
	;r1 = value, clipped to max
	;trashes
	;r2
	(vp-cpy-ri r1 r0 progress_val))
