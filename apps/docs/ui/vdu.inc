(ui-window *ui_vdu*
	(:min_width 128
	:min_height 128)
	(ui-flow _
		(:flow_flags +flow_stack_fill)
		(ui-vdu view
			(:vdu_width 16
			:vdu_height 4
			:ink_color +argb_green
			:font *env_editor_font*))
		(ui-backdrop _
			(:color +argb_grey1
			:style :plain))))

(bind '(w h) (. view :pref_size))
(. view :change 0 0 w h)
(. view :load
	'({This is line 1.}
	{This is line 2.}
	{This is line 3.}
	{This is line 4.})
	0 0 0 0)
