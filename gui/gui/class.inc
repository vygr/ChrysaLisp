(include 'sys/heap/class.inc)
(include 'sys/list/class.inc)
(include 'sys/mail/class.inc)

(defcvar 'SCREEN_WIDTH 1280)
(defcvar 'SCREEN_HEIGHT 960)

;;;;;;;;;;;;;;;;;;
; gui static class
;;;;;;;;;;;;;;;;;;

(def-class 'gui)
(dec-method :statics 'gui/gui/statics)
(dec-method :statics_init 'gui/gui/statics_init)
(dec-method :update 'gui/gui/update :static '(r0))
(dec-method :gui 'gui/gui/gui)

(dec-method :lisp_add 'gui/gui/lisp_add :static '(r0 r1) '(r0 r1))
(dec-method :lisp_add_back 'gui/gui/lisp_add_back :static '(r0 r1) '(r0 r1))
(dec-method :lisp_info 'gui/gui/lisp_info :static '(r0 r1) '(r0 r1))

;;;;;;;;;;;;;;;;
; gui structures
;;;;;;;;;;;;;;;;

;0 for render to texture, assumes previous rendered frames are destroyed
;1 for double buffered, 2 for triple buffered etc, assumes previous rendered frames are intact !
;when running in buffered modes there may still be issues if SDL does not send EVENT_WINDOW_RESTORED etc
(defcvar 'num_old_regions 0)

(def-struct 'gui_statics)
	(struct 'rect_heap 'hp_heap)
	(ptr 'window 'screen 'temps)
	(ulong 'renderer 'texture 'next_id)
	(uint 'dirty_flag 'old_region_index)
	(int 'mouse_x 'mouse_y)
	(struct 'old_regions (* num_old_regions ptr_size))
(def-struct-end)

;;;;;;;;;;;;;;;;;;;;
; gui event msg data
;;;;;;;;;;;;;;;;;;;;

(def-enum 'ev_type)
	(enum 'mouse 'key 'action 'gui)
(def-enum-end)

(def-struct 'ev_msg)
	(ulong 'target_id 'type)
(def-struct-end)

(def-struct 'ev_msg_mouse 'ev_msg)
	(uint 'buttons)
	(int 'x 'y 'rx 'ry)
(def-struct-end)

(def-struct 'ev_msg_key 'ev_msg)
	(uint 'keycode 'key)
(def-struct-end)

(def-struct 'ev_msg_action 'ev_msg)
	(ulong 'source_id)
(def-struct-end)

(def-struct 'ev_msg_gui 'ev_msg)
(def-struct-end)
