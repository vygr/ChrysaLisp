(enums +event +event_user
	(enum xscroll yscroll)
	(enum copy paste paragraph))

(defq +vdu_min_width 60 +vdu_min_height 40
	+vdu_max_width 120 +vdu_max_height 40)

(ui-window *window* (:color 0xc0000000)
	(ui-title-bar *title* "Terminal" (+sym_close +sym_max +sym_min) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (+sym_copy +sym_paste +sym_select_paragraph) +event_copy))
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-flow _ (:flow_flags +flow_left_fill)
		(. (ui-slider *yslider*) :connect +event_yscroll)
		(ui-flow *edit_flow* (:flow_flags +flow_up_fill)
			(. (ui-slider *xslider*) :connect +event_xscroll)
			(ui-backdrop _ (:style :lines :ink_color +argb_white)))))

(ui-tool-tips *main_toolbar*
	'("copy" "paste" "select paragraph"))
