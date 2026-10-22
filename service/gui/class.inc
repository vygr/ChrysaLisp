(include "lib/asm/class.inc")

;;;;;;;;;;;;;;;;;;
; gui static class
;;;;;;;;;;;;;;;;;;

(def-class :host_gui :nil
	(dec-method :init :nil :virtual (:r0 :r1) (:r0))
	(dec-method :deinit :nil :virtual () (:r0))
	(dec-method :box :nil :virtual (:r0) (:r0))
	(dec-method :filled_box :nil :virtual (:r0) (:r0))
	(dec-method :blit :nil :virtual (:r0 :r1 :r2) (:r0))
	(dec-method :set_clip :nil :virtual (:r0) (:r0))
	(dec-method :set_color :nil :virtual (:r0 :r1 :r2 :r3) (:r0))
	(dec-method :set_texture_color :nil :virtual (:r0 :r1 :r2 :r3) (:r0))
	(dec-method :destroy_texture :nil :virtual (:r0) (:r0))
	(dec-method :create_texture :nil :virtual (:r0 :r1 :r2 :r3 :r4) (:r0))
	(dec-method :begin_composite :nil :virtual () (:r0))
	(dec-method :end_composite :nil :virtual () (:r0))
	(dec-method :flush :nil :virtual (:r0) (:r0))
	(dec-method :resize :nil :virtual (:r0 :r1) (:r0))
	(dec-method :poll_event :nil :virtual (:r0) (:r0))
	(dec-method :clip_put :nil :virtual (:r0) (:r0))
	(dec-method :clip_get :nil :virtual () (:r0))
	(dec-method :clip_free :nil :virtual (:r0) (:r0))
	(dec-method :shader_format :nil :virtual () (:r0))
	(dec-method :shader_create :nil :virtual (:r0 :r1 :r2 :r3) (:r0))
	(dec-method :shader_destroy :nil :virtual (:r0) (:r0))
	(dec-method :shader_texture :nil :virtual (:r0 :r1) (:r0))
	(dec-method :shader_draw :nil :virtual (:r0 :r1 :r2 :r3 :r4) (:r0))
	(dec-method :read_texture :nil :virtual (:r0 :r1 :r2 :r3 :r4) (:r0))
	(dec-method :mesh_create :nil :virtual (:r0 :r1) (:r0))
	(dec-method :mesh_destroy :nil :virtual (:r0) (:r0))
	(dec-method :pair_create :nil :virtual (:r0 :r1 :r2 :r3 :r4) (:r0))
	(dec-method :tris_draw :nil :virtual (:r0 :r1 :r2) (:r0))
	(dec-method :read_screen :nil :virtual (:r0 :r1 :r2 :r3 :r4 :r5) (:r0))
	(dec-method :video_open :nil :virtual (:r0 :r1 :r2) (:r0))
	(dec-method :video_decode :nil :virtual (:r0 :r1 :r2 :r3) (:r0))
	(dec-method :video_frame :nil :virtual (:r0 :r1 :r2 :r3 :r4 :r5) (:r0))
	(dec-method :video_close :nil :virtual (:r0) (:r0))
	(dec-method :update service/gui/update :static (:r0 :r1 :r2) :nil (:r0-:r14 :f0-:f15))
	(dec-method :composite service/gui/composite :static (:r0) (:r7 :r8 :r9 :r10) (:r0-:r14 :f0-:f15))
	(dec-method :frame_view service/gui/frame_view :static (:r0) (:r0) (:r1-:r3 :r7-:r11 :f0-:f15))
	(dec-method :replay service/gui/replay :static (:r0) () (:r0-:r4 :r7-:r14 :f0-:f15))
	(dec-method :frame_free service/gui/frame_free :static () () (:r0-:r5 :f0-:f15))
	(dec-method :lisp_init service/gui/lisp_init :static (:r0 :r1) (:r0 :r1) (:r1-:r6 :f0-:f15))
	(dec-method :lisp_deinit service/gui/lisp_deinit :static (:r0 :r1) (:r0 :r1) (:r1-:r14 :f0-:f15))
	(dec-method :lisp_update service/gui/lisp_update :static (:r0 :r1) (:r0 :r1) (:r1-:r14 :f0-:f15))
	(dec-method :lisp_event service/gui/lisp_event :static (:r0 :r1) (:r0 :r1) (:r1-:r14 :f0-:f15))
	(dec-method :lisp_info service/gui/lisp_info :static (:r0 :r1) (:r0 :r1) (:r1-:r4 :r7-:r11 :r14 :f0-:f15))
	(dec-method :lisp_read_screen service/gui/lisp_read_screen :static (:r0 :r1) (:r0 :r1) (:r1-:r14 :f0-:f15))
	(dec-method :lisp_video service/gui/lisp_video :static (:r0 :r1) (:r0 :r1) (:r1-:r14 :f0-:f15)))

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

(structure host_gui_rect 0
	(int x y w h))

(enums ev_type 0
	(enum mouse key action gui wheel enter exit))

(structure ev_msg 0
	(long target_id type))

(structure ev_msg_mouse ev_msg_size
	(uint buttons count)
	(int x y rx ry))

(structure ev_msg_wheel ev_msg_size
	(uint direction)
	(int x y))

(structure ev_msg_key ev_msg_size
	(uint keycode key mod))

(structure ev_msg_action ev_msg_size
	(long source_id))

(structure ev_msg_gui ev_msg_size)

(structure ev_msg_enter ev_msg_size)

(structure ev_msg_exit ev_msg_size)
