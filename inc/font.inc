%ifndef FONT_1234
%define FONT_1234

%include 'inc/class.inc'
%include 'inc/list.inc'

;;;;;;;;;;;;;;;;;;;
; font static class
;;;;;;;;;;;;;;;;;;;

	def_class font
	def_method statics, sys/font_statics
	def_method init, sys/font_init
	def_method deinit, sys/font_deinit
	def_method open, sys/font_open

;;;;;;;;;;;;;;;;;
; font structures
;;;;;;;;;;;;;;;;;

	def_structure	ft_font
		def_struct	ft_font_node, ln_node
		def_long	ft_font_handle
		def_long	ft_font_point_size
		def_struct	ft_font_name, null
	def_structure_end

	def_structure	ft_word
		def_struct	ft_word_node, ln_node
		def_long	ft_word_texture
		def_long	ft_word_font
		def_struct	ft_word_name, null
	def_structure_end

	def_structure	ft_statics
		def_struct	ft_statics_font_list, lh_list
		def_struct	ft_statics_word_list, lh_list
	def_structure_end

%endif
