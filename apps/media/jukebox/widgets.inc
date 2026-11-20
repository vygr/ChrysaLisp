(enums +event +event_user
	(enum prev play pause stop next)
	(enum pin open)
	(enum mode shuffle tab up filter seek)
	;the rows of the list, an event each from here on
	(enum item))

(defq +bars 32 +bars_width 512 +bars_height 96 +rows_shown 25 +list_max 300)

(ui-window *window* (:resizable :nil)
	(ui-title-bar *title* "Jukebox" (+sym_close) +event_close)
	(ui-flow _ (:flow_flags +flow_down_fill)
		;the controls, a favourite or not, a file, Player or Jukebox and shuffle
		(ui-flow _ (:flow_flags +flow_right_fill)
			(ui-tool-bar *controls* ()
				(ui-buttons (+sym_media_start +sym_media_play +sym_media_pause +sym_media_stop +sym_media_end) +event_prev))
			(ui-tool-bar *favourite* ()
				(ui-buttons (+sym_pin +sym_open) +event_pin))
			(. (ui-radio-bar *mode* ("Player" "Jukebox") (:font :font_body)) :connect +event_mode)
			(. (ui-toggle-bar *shuffle* ("Shuffle") (:font :font_body)) :connect +event_shuffle)
			(ui-backdrop _ (:paper :paper_toolbar)))
		;where the song is, dragged to seek
		(. (ui-slider *seek* (:value 0 :maximum 0 :portion 1 :min_height 14)) :connect +event_seek)
		(ui-label *status* (:text "Nothing playing" :font :font_body))
		;the song's samples, lit as they play, and its rows as a tracker shows
		;them, the row heard in the middle
		;a Vdu draws its letters only, black is behind them
		(ui-flow _ (:flow_flags +flow_stack_fill)
			(ui-flow _ (:flow_flags +flow_right_fill)
				(ui-vdu *samples* (:vdu_width 26 :vdu_height +rows_shown :font :font_small_terminal
					:ink +argb_cyan))
				(ui-backdrop _ (:paper +argb_grey6 :min_width 2))
				(ui-vdu *rows* (:vdu_width 48 :vdu_height +rows_shown :font :font_small_terminal
					:ink +argb_green)))
			(ui-backdrop _ (:style :plain :paper +argb_black)))
		;the bars of what is heard, in the middle of black the window wide
		(ui-flow _ (:flow_flags +flow_stack_fill)
			(ui-flow _ (:flow_flags (bit-mask +flow_flag_down +flow_flag_align_hcenter))
				(ui-canvas *bars* +bars_width +bars_height 1))
			(ui-backdrop _ (:style :plain :paper +argb_black)))
		;the favourites, or the modules of Modland, folder by folder
		(ui-flow _ (:flow_flags +flow_right_fill)
			(. (ui-radio-bar *tab* ("Favourites" "Modland") (:font :font_body)) :connect +event_tab)
			(ui-tool-bar *places* ()
				(ui-buttons (+sym_up) +event_up))
			(. (ui-textfield *filter* (:paper +argb_white :clear_text "" :hint_text "filter" :min_width 160))
				:connect +event_filter))
		(ui-label *where* (:text "" :font :font_small))
		;the rows as wide as the view, however long their names, on black
		;that fills it however few they are
		(ui-scroll *list_scroll* +scroll_flag_vertical (:min_height 200 :child_fill +scroll_flag_both)
			(ui-flow _ (:flow_flags +flow_stack_fill)
				(ui-flow *list* (:flow_flags +flow_down))
				(ui-backdrop _ (:style :plain :paper +argb_black))))))

(ui-tool-tips *controls* '("prev" "play" "pause" "stop" "next"))
(ui-tool-tips *favourite* '("a favourite, kept to play offline, or not" "open a module"))
(ui-tool-tips *places* '("up a folder"))
