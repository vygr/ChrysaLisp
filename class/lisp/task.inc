;;;;;;;;;;
; Per Task 
;;;;;;;;;;

(defq *debug_state* :nil
	*stack_frame* :nil *stack_rvals* :nil
	*profile_map* :nil *profile_rvals* :nil)

(import "sys/lisp.inc")
(import "class/lisp.inc")
