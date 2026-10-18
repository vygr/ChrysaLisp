(enums +event +event_user
	(enum record stop play delete)
	(enum start mute headphones leave)
	;each note a button from here, and each call's Join after the notes,
	;+event_join
	(enum note))

;as a phone is: the call at the top, who is in it and what they are doing,
;or the calls there are to join; the notes below
(ui-window *window* ()
	(ui-title-bar *title* "Voice" (+sym_close) +event_close)
	(ui-label _ (:text "Call" :font :font_sub_title))
	;made again as the call changes, (call-show)
	(ui-flow *call_area* (:flow_flags +flow_down_fill :min_width 340))
	(ui-label _ (:text "Notes" :font :font_sub_title))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (+sym_media_record +sym_media_stop +sym_media_play +sym_delete) +event_record))
		(ui-label *status* (:text "Ready" :font :font_button)))
	(ui-progress *level* (:maximum 100 :value 0 :min_height 8 :paper +argb_grey12))
	(ui-scroll *notes_scroll* +scroll_flag_vertical (:min_width 340 :min_height 200)
		(ui-flow *notes* (:flow_flags +flow_down_fill))))

(ui-tool-tips *main_toolbar*
	'("record" "stop" "play" "delete"))
