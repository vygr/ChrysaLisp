(include "lib/asm/class.inc")

;;;;;;;;;;;;;;;;;;
; audio static class
;;;;;;;;;;;;;;;;;;

(def-class host_audio :nil
	(dec-method :init :nil :virtual (abi-args 0) (:r0))
	(dec-method :deinit :nil :virtual (abi-args 0))
	(dec-method :add :nil :virtual (abi-args 1) (:r0))
	(dec-method :play :nil :virtual (abi-args 1) (:r0))
	(dec-method :change :nil :virtual (abi-args 2) (:r0))
	(dec-method :remove :nil :virtual (abi-args 1) (:r0))
	(dec-method :lisp_init service/audio/lisp_init :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_deinit service/audio/lisp_deinit :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_add service/audio/lisp_add :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_play service/audio/lisp_play :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_change service/audio/lisp_change :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_remove service/audio/lisp_remove :static (:r0 :r1) (:r0 :r1)))
