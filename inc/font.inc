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

	def_structure	ft_font
		def_struct	ft_font_node, ln_node
		def_long	ft_font_handle
		def_long	ft_font_points
		def_long	ft_font_ascent
		def_long	ft_font_descent
		def_long	ft_font_height
		def_struct	ft_font_name, null
	def_structure_end

	def_structure	ft_text
		def_struct	ft_text_node, ln_node
		def_long	ft_text_font
		def_long	ft_text_texture
		def_long	ft_text_width
		def_long	ft_text_height
		def_struct	ft_text_name, null
	def_structure_end

	def_structure	ft_statics
		def_long	ft_statics_font_list
		def_long	ft_statics_text_list
	def_structure_end

%endif
