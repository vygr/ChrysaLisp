(import 'inc/class.inc)
(import 'inc/code.inc)
(import 'inc/heap.inc)
(import 'inc/list.inc)
(import 'inc/mail.inc)

(defcvar 'SCREEN_WIDTH 1280)
(defcvar 'SCREEN_HEIGHT 960)

;;;;;;;;;;;;;;;;;;;;;
; gui statics classes
;;;;;;;;;;;;;;;;;;;;;

(def-class 'gui_gui)
(def-method 'statics 'gui/gui_statics)
(def-method 'init 'gui/gui_init 'static '(r0))
(def-method 'deinit 'gui/gui_deinit)
(def-method 'update 'gui/gui_update 'static '(r0))
(def-method 'add 'gui/gui_add 'static '(r0))
(def-method 'gui 'gui/gui)

(def-class 'gui_region)
(def-method 'translate 'gui/region_translate 'static '(r1 r8 r9))
(def-method 'bounds 'gui/region_bounds 'static '(r1) '(r8 r9 r10 r11))
(def-method 'clip_rect 'gui/region_clip_rect 'static '(r0 r1 r8 r9 r10 r11) '(r0))
(def-method 'remove_rect 'gui/region_remove_rect 'static '(r0 r1 r8 r9 r10 r11) '(r0))
(def-method 'cut_rect 'gui/region_cut_rect 'static '(r0 r1 r2 r8 r9 r10 r11) '(r0))
(def-method 'copy_rect 'gui/region_copy_rect 'static '(r0 r1 r2 r8 r9 r10 r11) '(r0))
(def-method 'paste_rect 'gui/region_paste_rect 'static '(r0 r1 r8 r9 r10 r11) '(r0))
(def-method 'free 'gui/region_free 'static '(r0 r1) '(r0))
(def-method 'copy_region 'gui/region_copy_region 'static '(r0 r1 r2 r3 r8 r9) '(r0))
(def-method 'paste_region 'gui/region_paste_region 'static '(r0 r1 r2 r8 r9) '(r0))
(def-method 'remove_region 'gui/region_remove_region 'static '(r0 r1 r2 r8 r9) '(r0))

(def-class 'gui_ctx)
(def-method 'box 'gui/ctx_box 'static '(r0 r8 r9 r10 r11))
(def-method 'filled_box 'gui/ctx_filled_box 'static '(r0 r8 r9 r10 r11))
(def-method 'blit 'gui/ctx_blit 'static '(r0 r1 r2 r8 r9 r10 r11))
(def-method 'set_color 'gui/ctx_set_color 'static '(r0 r1))
(def-method 'panel 'gui/ctx_panel 'static '(r0 r1 r2 r3 r8 r9 r10 r11))
(def-method 'brighter 'gui/ctx_brighter 'static '(r1) '(r1))
(def-method 'darker 'gui/ctx_darker 'static '(r1) '(r1))

;;;;;;;;;;;;;;;;
; gui structures
;;;;;;;;;;;;;;;;

(def-struct 'gui_statics)
	(ptr 'sdl_funcs)
	(struct 'rect_heap 'hp_heap)
	(struct 'sigslot_heap 'hp_heap)
	(ptr 'window)
	(ptr 'screen)
	(ulong 'renderer)
	(ptr 'old_region)
(def-struct-end)

(def-struct 'gui_rect 'ln_fnode)
	(int 'x)
	(int 'y)
	(int 'x1)
	(int 'y1)
(def-struct-end)

(def-struct 'gui_ctx)
	(ptr 'sdl_ctx)
	(ptr 'dirty_region)
	(int 'x)
	(int 'y)
;	(int 'bounds_x)
;	(int 'bounds_y)
;	(int 'bounds_x1)
;	(int 'bounds_y1)
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

(def-struct 'ev_msg 'msg_header)
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
