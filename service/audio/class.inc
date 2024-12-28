(include "lib/asm/class.inc")

;;;;;;;;;;;;;;;;;;
; audio static class
;;;;;;;;;;;;;;;;;;

(def-class host_audio :nil
	(dec-method :init :nil :virtual (abi-args 0) (:r0))
	(dec-method :deinit :nil :virtual (abi-args 0))
	(dec-method :add_sfx :nil :virtual (abi-args 1) (:r0))
	(dec-method :play_sfx :nil :virtual (abi-args 1) (:r0))
	(dec-method :change_sfx :nil :virtual (abi-args 2) (:r0))
	(dec-method :remove_sfx :nil :virtual (abi-args 1) (:r0))
	(dec-method :lisp_init service/audio/lisp_init :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_deinit service/audio/lisp_deinit :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_add_sfx service/audio/lisp_add_sfx :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_play_sfx service/audio/lisp_play_sfx :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_change_sfx service/audio/lisp_change_sfx :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_remove_sfx service/audio/lisp_remove_sfx :static (:r0 :r1) (:r0 :r1)))
