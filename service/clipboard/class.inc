(include "lib/asm/class.inc")

;;;;;;;;;;;;;;;;;
; clipboard class
;;;;;;;;;;;;;;;;;

(def-class host_clipboard :nil
	(dec-method :lisp_put service/clipboard/lisp_put :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_get service/clipboard/lisp_get :static (:r0 :r1) (:r0 :r1)))
