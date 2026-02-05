(import "class/stream/lisp.inc")
(import "./struct.inc")

(defun out-set-state (out state)
	; (out-set-state out num) -> out
	(setf out +out_state state 0))
