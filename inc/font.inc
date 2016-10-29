%ifndef FONT_1234
%define FONT_1234

%include 'inc/class.inc'
%include 'inc/list.inc'

;;;;;;;;;;;;;;;;;;;
; font static class
;;;;;;;;;;;;;;;;;;;

def_class gui_font
def_method statics, gui/font_statics
def_method init, gui/font_init
def_method deinit, gui/font_deinit
def_method open, gui/font_open, static, {r0, r1}, {r0}
def_method text, gui/font_text, static, {r0, r1}, {r0}
def_method bounds, gui/font_bounds, static, {r0, r1}, {r10, r11}

;;;;;;;;;;;;;;;;;
; font structures
;;;;;;;;;;;;;;;;;

def_struct ft_font
	ptr ft_font_next
	ulong ft_font_handle
	ulong ft_font_points
	ulong ft_font_ascent
	ulong ft_font_descent
	ulong ft_font_height
	offset ft_font_name
def_struct_end

def_struct ft_text
	ptr ft_text_next
	ulong ft_text_font
	ulong ft_text_texture
	ulong ft_text_width
	ulong ft_text_height
	offset ft_text_name
def_struct_end

ft_num_buckets equ 61
ft_buckets_size equ ptr_size * ft_num_buckets

def_struct ft_statics
	ptr ft_statics_font_flist
	struct ft_statics_text_flists, ft_buckets
def_struct_end

%endif
