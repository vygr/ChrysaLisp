(import 'sys/class.inc)
(import 'sys/code.inc)
(import 'sys/heap/heap.inc)
(import 'sys/list/list.inc)
(import 'sys/mail/mail.inc)

(defcvar 'SCREEN_WIDTH 1280)
(defcvar 'SCREEN_HEIGHT 960)

;;;;;;;;;;;;;;;;;;
; gui static class
;;;;;;;;;;;;;;;;;;

(def-class 'gui_gui)
(def-method 'statics 'gui/gui_statics)
(def-method 'init 'gui/gui_init 'static '(r0))
(def-method 'update 'gui/gui_update 'static '(r0))
(def-method 'add 'gui/gui_add 'static '(r0))
(def-method 'gui 'gui/gui)

;;;;;;;;;;;;;;;;
; gui structures
;;;;;;;;;;;;;;;;

(def-struct 'gui_statics)
	(ptr 'sdl_funcs)
	(struct 'rect_heap 'hp_heap)
	(ptr 'window)
	(ptr 'screen)
	(ulong 'renderer)
	(ptr 'old_region)
	(ulong 'next_id)
	(ubyte 'dirty_flag)
(def-struct-end)

;;;;;;;;;;;;;;;;;;;;
; gui event msg data
;;;;;;;;;;;;;;;;;;;;

(def-enum 'ev_type)
	(enum 'mouse 'key 'action)
(def-enum-end)

(def-struct 'ev_msg)
	(ulong 'target_id)
	(ulong 'type)
(def-struct-end)

(def-struct 'ev_msg_mouse)
	(struct 'base 'ev_msg)
	(uint 'buttons)
	(int 'x)
	(int 'y)
	(int 'rx)
	(int 'ry)
(def-struct-end)

(def-struct 'ev_msg_key)
	(struct 'base 'ev_msg)
	(uint 'keycode)
	(uint 'key)
(def-struct-end)

(def-struct 'ev_msg_action)
	(struct 'base 'ev_msg)
	(ulong 'source_id)
(def-struct-end)
