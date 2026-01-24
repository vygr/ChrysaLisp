(include "lib/asm/class.inc")

;;;;;;;;;;;;;;;;;;
; gui static class
;;;;;;;;;;;;;;;;;;

(def-class :host_gui :nil
	(dec-method :init :nil :virtual (abi-args 2) (:r0))
	(dec-method :deinit :nil :virtual (abi-args 0) (:r0))
	(dec-method :box :nil :virtual (abi-args 1) (:r0))
	(dec-method :filled_box :nil :virtual (abi-args 1) (:r0))
	(dec-method :blit :nil :virtual (abi-args 3) (:r0))
	(dec-method :set_clip :nil :virtual (abi-args 1) (:r0))
	(dec-method :set_color :nil :virtual (abi-args 4) (:r0))
	(dec-method :set_texture_color :nil :virtual (abi-args 4) (:r0))
	(dec-method :destroy_texture :nil :virtual (abi-args 1) (:r0))
	(dec-method :create_texture :nil :virtual (abi-args 5) (:r0))
	(dec-method :begin_composite :nil :virtual (abi-args 0) (:r0))
	(dec-method :end_composite :nil :virtual (abi-args 0) (:r0))
	(dec-method :flush :nil :virtual (abi-args 1) (:r0))
	(dec-method :resize :nil :virtual (abi-args 2) (:r0))
	(dec-method :poll_event :nil :virtual (abi-args 1) (:r0))
	(dec-method :clip_put :nil :virtual (abi-args 1) (:r0))
	(dec-method :clip_get :nil :virtual (abi-args 0) (:r0))
	(dec-method :clip_free :nil :virtual (abi-args 1) (:r0))
	(dec-method :update service/gui/update :static (:r0 :r1 :r2))
	(dec-method :composite service/gui/composite :static (:r0) (:r7 :r8 :r9 :r10))
	(dec-method :lisp_init service/gui/lisp_init :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_deinit service/gui/lisp_deinit :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_update service/gui/lisp_update :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_event service/gui/lisp_event :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_info service/gui/lisp_info :static (:r0 :r1) (:r0 :r1)))

(defun host-gui-call (%0 %1 &optional %2 %3 %4 %5)
	;%0 = class name
	;%1 = member name
	;%2 = in parameters
	;%3 = out parameters
	;%4 = dispatch reg
	;%5 = temp reg
	(setd %4 :r14 %5 :r13)
	(fn-bind 'sys/statics/statics %4)
	(vp-cpy-ir %4 statics_sys_load_host_gui_funcs %4)
	(abi-call-table %0 %1 %4 %2 %3 %5))

;;;;;;;;;;;;;;;;
; gui structures
;;;;;;;;;;;;;;;;

(def-struct host_gui_rect 0
	(int x y w h))

(def-enum ev_type 0
	(enum mouse key action gui wheel enter exit))

(def-struct ev_msg 0
	(long target_id type))

(def-struct ev_msg_mouse ev_msg_size
	(uint buttons count)
	(int x y rx ry))

(def-struct ev_msg_wheel ev_msg_size
	(uint direction)
	(int x y))

(def-struct ev_msg_key ev_msg_size
	(uint keycode key mod))

(def-struct ev_msg_action ev_msg_size
	(long source_id))

(def-struct ev_msg_gui ev_msg_size)

(def-struct ev_msg_enter ev_msg_size)

(def-struct ev_msg_exit ev_msg_size)
