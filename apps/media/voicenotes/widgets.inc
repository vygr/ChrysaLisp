(enums +event +event_user
	(enum record stop play delete)
	(enum call join leave mute)
	;each note a button from here, and each call after the notes, +event_pick
	(enum note))

(ui-window *window* ()
	(ui-title-bar *title* "Voice" (+sym_close) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (+sym_media_record +sym_media_stop +sym_media_play +sym_delete) +event_record))
		(ui-tool-bar *call_toolbar* ()
			(ui-buttons (+sym_mic +sym_media_join +sym_hangup +sym_mute) +event_call)))
	(ui-label *status* (:text "Ready" :font :font_button))
	(ui-progress *level* (:maximum 100 :value 0 :min_height 8 :paper +argb_green))
	;the call this is in, and the calls of the mesh to join
	(ui-label *call_status* (:text "In no call" :font :font_button))
	(ui-scroll *calls_scroll* +scroll_flag_vertical (:min_width 320 :min_height 80)
		(ui-flow *calls* (:flow_flags +flow_down_fill)))
	(ui-scroll *notes_scroll* +scroll_flag_vertical (:min_width 320 :min_height 200)
		(ui-flow *notes* (:flow_flags +flow_down_fill))))

(ui-tool-tips *main_toolbar*
	'("record" "stop" "play" "delete"))

(ui-tool-tips *call_toolbar*
	'("call" "join" "leave" "mute"))
