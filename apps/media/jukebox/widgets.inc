(enums +event +event_user
	(enum prev play pause stop next)
	(enum pin open)
	(enum mode shuffle tab up filter search seek)
	;a row of the list
	(enum item))

(defq +bars_width 512 +bars_height 96 +rows_shown 25 +list_rows 14)

(defclass Filter () (Textfield)
	; (Filter) -> filter
	;a textfield that says each change as it is typed, not only at enter
	(defmethod :key_down (event)
		; (. filter :key_down event) -> filter
		(defq text (get :clear_text this))
		(.super this :key_down event)
		(unless (eql text (get :clear_text this)) (. this :emit))
		this))

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
		;where the song is, dragged to seek, or in front of it while a module
		;or a folder comes, how much of it has
		(ui-flow _ (:flow_flags +flow_stack_fill)
			(. (ui-slider *seek* (:value 0 :maximum 0 :portion 1 :min_height 14)) :connect +event_seek)
			(ui-progress *loading* (:value 0 :maximum 1000 :min_height 14)))
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
		;the favourites, or Modland's charts, or all its modules played here,
		;folder by folder
		(ui-flow _ (:flow_flags +flow_right_fill)
			(. (ui-radio-bar *tab* ("Favourites" "Charts" "All" "Search") (:font :font_body)) :connect +event_tab)
			(ui-tool-bar *places* ()
				(ui-buttons (+sym_up) +event_up))
			;the search of all of Modland, by the start of a name, ./index.lisp,
			;and the filter, side by side, the last of the row so it has the
			;rest of the width, each the same share of it
			(ui-grid _ (:grid_width 0 :grid_height 1)
				(. (ui-element *search* (Filter) (:flow_flags (bit-mask +flow_flag_right +flow_flag_align_vcenter)
						:border :border_textfield :paper +argb_white :clear_text "" :hint_text "search" :min_width 160))
					:connect +event_search)
				(. (ui-element *filter* (Filter) (:flow_flags (bit-mask +flow_flag_right +flow_flag_align_vcenter)
						:border :border_textfield :paper +argb_white :clear_text "" :hint_text "filter" :min_width 160))
					:connect +event_filter)))
		(ui-label *where* (:text "" :font :font_small))
		;the list, on black, the rows only of what is seen
		(ui-scroll *list_scroll* +scroll_flag_vertical (:child_fill +scroll_flag_both)
			(ui-items *list* (:paper +argb_black)))))

(ui-tool-tips *controls* '("prev" "play" "pause" "stop" "next"))
(ui-tool-tips *favourite* '("favourite" "open"))
(ui-tool-tips *places* '("up a folder"))
