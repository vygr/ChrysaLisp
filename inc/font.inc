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

	def_structure ft_font
		struct ft_font_node, ln_node
		long ft_font_handle
		long ft_font_points
		long ft_font_ascent
		long ft_font_descent
		long ft_font_height
		struct ft_font_name, null
	def_structure_end

	def_structure ft_text
		struct ft_text_node, ln_node
		long ft_text_font
		long ft_text_texture
		long ft_text_width
		long ft_text_height
		struct ft_text_name, null
	def_structure_end

	def_structure ft_statics
		long ft_statics_font_list
		long ft_statics_text_list
	def_structure_end

%endif
