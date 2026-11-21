(enums +event +event_user
	(enum prev play pause stop next)
	(enum pin open)
	(enum mode shuffle tab up filter seek list)
	;the rows of the list, an event each from here on
	(enum item))

(defq +bars 32 +bars_width 512 +bars_height 96 +rows_shown 25 +list_rows 14)

(defclass Rows () (Flow)
	; (Rows) -> rows
	;the rows of the list seen, as many as fit, however long the list: the
	;wheel over them moves the slider along all of it, :slider
	(defmethod :mouse_wheel (event)
		; (. rows :mouse_wheel event) -> rows
		(defq slider (get :slider this))
		(def slider :value (max 0 (min (get :maximum slider)
			(- (get :value slider) (getf event +ev_msg_wheel_y)))))
		(.-> slider :emit :dirty)
		this))

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
			(. (ui-radio-bar *tab* ("Favourites" "Charts" "All") (:font :font_body)) :connect +event_tab)
			(ui-tool-bar *places* ()
				(ui-buttons (+sym_up) +event_up))
			(. (ui-element *filter* (Filter) (:flow_flags (bit-mask +flow_flag_right +flow_flag_align_vcenter)
					:border :border_textfield :paper +argb_white :clear_text "" :hint_text "filter" :min_width 160))
				:connect +event_filter))
		(ui-label *where* (:text "" :font :font_small))
		;the rows seen, on black, and a slider along all of the list
		(ui-flow _ (:flow_flags +flow_left_fill)
			(. (ui-slider *list_slider* (:value 0 :maximum 0 :portion +list_rows :min_width 12))
				:connect +event_list)
			(ui-flow _ (:flow_flags +flow_stack_fill)
				(ui-element *list* (Rows) (:flow_flags +flow_down))
				(ui-backdrop _ (:style :plain :paper +argb_black))))))

(ui-tool-tips *controls* '("prev" "play" "pause" "stop" "next"))
(ui-tool-tips *favourite* '("a favourite, kept to play offline, or not" "open a module"))
(ui-tool-tips *places* '("up a folder"))
