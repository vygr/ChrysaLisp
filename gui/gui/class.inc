(include "sys/heap/class.inc")
(include "sys/list/class.inc")
(include "sys/mail/class.inc")

;;;;;;;;;;;;;;;;;;
; gui static class
;;;;;;;;;;;;;;;;;;

(def-class gui :nil
	(dec-method :update gui/gui/update :static (:r0 :r1 :r2))
	(dec-method :composit gui/gui/composit :static (:r0))
	(dec-method :lisp_init gui/gui/lisp_init :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_deinit gui/gui/lisp_deinit :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_update gui/gui/lisp_update :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_event gui/gui/lisp_event :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_info gui/gui/lisp_info :static (:r0 :r1) (:r0 :r1)))

;;;;;;;;;;;;;;;;
; gui structures
;;;;;;;;;;;;;;;;

;0 for render to texture, assumes previous rendered frames are destroyed
;1 for double buffered, 2 for triple buffered etc, assumes previous rendered frames are intact !
;when running in buffered modes there may still be issues if SDL does not send EVENT_WINDOW_RESTORED etc
(defcvar 'num_old_regions 0)

(def-enum ev_type 0
	(enum mouse key action gui wheel enter exit))

(def-struct ev_msg 0
	(ulong target_id type))

(def-struct ev_msg_mouse ev_msg_size
	(uint buttons count)
	(int x y rx ry))

(def-struct ev_msg_wheel ev_msg_size
	(uint direction)
	(int x y))

(def-struct ev_msg_key ev_msg_size
	(uint keycode key mod))

(def-struct ev_msg_action ev_msg_size
	(ulong source_id))

(def-struct ev_msg_gui ev_msg_size)

(def-struct ev_msg_enter ev_msg_size)

(def-struct ev_msg_exit ev_msg_size)
