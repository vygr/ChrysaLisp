(enums +event +event_user
	(enum record stop play delete)
	(enum note))

(ui-window *window* ()
	(ui-title-bar *title* "Voice Notes" (+sym_close) +event_close)
	(ui-tool-bar *main_toolbar* ()
		(ui-buttons (+sym_media_record +sym_media_stop +sym_media_play +sym_delete) +event_record))
	(ui-label *status* (:text "Ready" :font :font_button))
	(ui-progress *level* (:maximum 100 :value 0 :min_height 8 :paper +argb_green))
	(ui-scroll *notes_scroll* +scroll_flag_vertical (:min_width 320 :min_height 240)
		(ui-flow *notes* (:flow_flags +flow_down_fill))))

(ui-tool-tips *main_toolbar*
	'("record" "stop" "play" "delete"))
