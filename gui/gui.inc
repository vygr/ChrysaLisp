(import 'sys/class.inc)
(import 'sys/code.inc)
(import 'sys/heap/heap.inc)
(import 'sys/list/list.inc)
(import 'sys/mail/mail.inc)

(defcvar 'SCREEN_WIDTH 1280)
(defcvar 'SCREEN_HEIGHT 960)

;;;;;;;;;;;;;;;;;;;;;
; gui statics classes
;;;;;;;;;;;;;;;;;;;;;

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
	(ubyte 'dirty_flag)
(def-struct-end)

(def-struct 'gui_sigslot)
	(struct 'sig_node 'ln_node)
	(struct 'slot_node 'ln_node)
	(ptr 'inst)
	(ptr 'addr)
(def-struct-end)

;;;;;;;;;;;;;;;
; gui mail data
;;;;;;;;;;;;;;;

(def-enum 'ev_type)
	(enum 'mouse 'key)
(def-enum-end)

(def-struct 'ev_msg)
	(ulong 'type)
	(ptr 'view)
	(int 'x)
	(int 'y)
	(int 'rx)
	(int 'ry)
	(ulong 'buttons)
	(ulong 'keycode)
	(ulong 'key)
(def-struct-end)
