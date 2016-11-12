%ifndef FONT_1234
%define FONT_1234

(import 'inc/class.inc)
(import 'inc/list.inc)

;;;;;;;;;;;;;;;;;;;
; font static class
;;;;;;;;;;;;;;;;;;;

(def-class 'gui_font)
(def-method 'statics 'gui/font_statics)
(def-method 'init 'gui/font_init)
(def-method 'deinit 'gui/font_deinit)
(def-method 'open 'gui/font_open 'static '(r0 r1) '(r0))
(def-method 'text 'gui/font_text 'static '(r0 r1) '(r0))
(def-method 'bounds 'gui/font_bounds 'static '(r0 r1) '(r10 r11))

;;;;;;;;;;;;;;;;;
; font structures
;;;;;;;;;;;;;;;;;

(def-struct 'ft_font)
	(ptr 'ft_font_next)
	(ulong 'ft_font_handle)
	(ulong 'ft_font_points)
	(ulong 'ft_font_ascent)
	(ulong 'ft_font_descent)
	(ulong 'ft_font_height)
	(offset 'ft_font_name)
(def-struct-end)

(def-struct 'ft_text)
	(ptr 'ft_text_next)
	(ulong 'ft_text_font)
	(ulong 'ft_text_texture)
	(ulong 'ft_text_width)
	(ulong 'ft_text_height)
	(offset 'ft_text_name)
(def-struct-end)

ft_num_buckets equ 61
ft_buckets_size equ ptr_size * ft_num_buckets

(def-struct 'ft_statics)
	(ptr 'ft_statics_font_flist)
	(struct 'ft_statics_text_flists 'ft_buckets)
(def-struct-end)

%endif
