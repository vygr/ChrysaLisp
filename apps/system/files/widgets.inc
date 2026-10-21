(enums +event +event_user
	(enum up path ok kinds filter)
	(enum place))

;the places down the side, and past them the rows of the drawer
(defq +places_max 24 +event_item (+ +event_place +places_max))

;a drawer: places to go to down the side, the folder it is open at above,
;what is in it below, a row a thing, folders first, and the name of the
;one taken at the foot
(ui-window *window* :nil
	(ui-flow _ (:flow_flags +flow_down_fill)
		(ui-title-bar *window_title* "" (+sym_close) +event_close)
		(ui-flow _ (:flow_flags +flow_right_fill)
			(ui-flow *places* (:flow_flags +flow_down_fill :min_width 150))
			(ui-flow _ (:flow_flags +flow_down_fill)
				(ui-flow _ (:flow_flags +flow_right_fill)
					(ui-buttons (+sym_up) +event_up (:font :font_symbol))
					(. (ui-textfield *path* (:clear_text "" :hint_text "folder" :paper +argb_white))
						:connect +event_path))
				(ui-scroll *scroll* +scroll_flag_vertical (:min_width 420 :min_height 360)
					(ui-flow *list* (:flow_flags +flow_down_fill)))
				(ui-flow _ (:flow_flags +flow_right_fill)
					(ui-label _ (:text "Name:"))
					(. (ui-textfield *name* (:clear_text "" :hint_text "name" :paper +argb_white))
						:connect +event_ok))
				(ui-flow _ (:flow_flags +flow_right_fill)
					(ui-label _ (:text "Kinds:"))
					(. (ui-textfield *kinds* (:clear_text "" :hint_text "all" :paper +argb_white))
						:connect +event_kinds))
				(ui-flow _ (:flow_flags +flow_right_fill)
					(ui-label _ (:text "Filter:"))
					(. (ui-textfield *filter* (:clear_text "" :hint_text "regexp" :paper +argb_white))
						:connect +event_filter))
				(ui-flow _ (:flow_flags +flow_right_fill)
					(ui-buttons (+sym_done) +event_ok (:font :font_symbol))
					(ui-label *note* (:text "")))))))
