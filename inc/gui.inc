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
(def-method 'clip_rect 'gui/region_clip_rect 'static '(r0 r1 r8 r9 r10 r11))
(def-method 'remove_rect 'gui/region_remove_rect 'static '(r0 r1 r8 r9 r10 r11))
(def-method 'cut_rect 'gui/region_cut_rect 'static '(r0 r1 r2 r8 r9 r10 r11))
(def-method 'copy_rect 'gui/region_copy_rect 'static '(r0 r1 r2 r8 r9 r10 r11))
(def-method 'paste_rect 'gui/region_paste_rect 'static '(r0 r1 r8 r9 r10 r11))
(def-method 'free 'gui/region_free 'static '(r0 r1))
(def-method 'copy_region 'gui/region_copy_region 'static '(r0 r1 r2 r3 r8 r9))
(def-method 'paste_region 'gui/region_paste_region 'static '(r0 r1 r2 r8 r9))
(def-method 'remove_region 'gui/region_remove_region 'static '(r0 r1 r2 r8 r9))

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
	(ptr 'gui_statics_sdl_funcs)
	(struct 'gui_statics_rect_heap 'hp_heap)
	(struct 'gui_statics_sigslot_heap 'hp_heap)
	(ptr 'gui_statics_window)
	(ptr 'gui_statics_screen)
	(ulong 'gui_statics_renderer)
	(ptr 'gui_statics_old_region)
(def-struct-end)

(def-struct 'gui_rect 'ln_fnode)
	(int 'gui_rect_x)
	(int 'gui_rect_y)
	(int 'gui_rect_x1)
	(int 'gui_rect_y1)
(def-struct-end)

(def-struct 'gui_ctx)
	(ptr 'gui_ctx_sdl_ctx)
	(ptr 'gui_ctx_dirty_region)
	(uint 'gui_ctx_x)
	(uint 'gui_ctx_y)
(def-struct-end)

(def-struct 'gui_sigslot)
	(struct 'gui_sigslot_sig_node 'ln_node)
	(struct 'gui_sigslot_slot_node 'ln_node)
	(ptr 'gui_sigslot_inst)
	(ptr 'gui_sigslot_addr)
(def-struct-end)

;;;;;;;;;;;;;;;
; gui mail data
;;;;;;;;;;;;;;;

(def-enum)
	(enum 'ev_type_mouse)
	(enum 'ev_type_key)
(def-enum-end)

(def-struct 'ev_msg 'msg_header)
	(ulong 'ev_msg_type)
	(ptr 'ev_msg_view)
	(int 'ev_msg_x)
	(int 'ev_msg_y)
	(int 'ev_msg_rx)
	(int 'ev_msg_ry)
	(ulong 'ev_msg_buttons)
	(ulong 'ev_msg_keycode)
	(ulong 'ev_msg_key)
(def-struct-end)
