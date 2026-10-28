(enums +event 0
	(enum close max min)
	(enum to_right to_left delete refresh)
	(enum skip replace both)
	(enum copies cancel)
	(enum source_0 source_1 up_0 up_1 path_0 path_1 all_0 all_1))

;the rows of the two sides past the rest, a name and an open button
;each, so many shown at most
(defq +rows_max 2000 +event_rows 1000)

(defmacro stores-side (n)
	;one side: what it shows, the folder it is at, what is in it, a row a
	;thing, folders first, and what of it is taken at the foot
	(defq side (str n) at (# (sym (cat "*" %0 "_" side "*"))) event (# (sym (cat "+event_" %0 "_" side))))
	(static-qq (ui-flow _ (:flow_flags +flow_down_fill)
		(ui-flow _ (:flow_flags +flow_right_fill :font :font_button)
			(ui-buttons (+sym_up) ,(event "up") (:font :font_symbol))
			(. (ui-button ,(at "source") (:text "")) :connect ,(event "source")))
		(ui-flow _ (:flow_flags +flow_right_fill)
			(ui-buttons ("All") ,(event "all") (:font :font_button))
			(. (ui-textfield ,(at "path") (:clear_text "" :hint_text "folder" :paper +argb_white))
				:connect ,(event "path")))
		(ui-flow _ (:flow_flags +flow_up_fill)
			(ui-label ,(at "note") (:text "" :font :font_small))
			(ui-element ,(at "scroll") (Sheet) (:paper :paper_slider :min_width 440 :min_height 420)
				(ui-backdrop ,(at "ruled") (:style :lines :paper +argb_white :ink +argb_grey14)
					(ui-flow ,(at "list") (:flow_flags +flow_down_fill))))))))

(defclass Sheet () (Scroll +scroll_flag_vertical)
	; (Sheet) -> sheet
	;a scroll up and down that, each time it is laid out, a new size of
	;the window say, first has (fit sheet) make what it scrolls fit it
	(defmethod :layout ()
		(if (defq fit_fn (get :fit this)) (fit_fn this))
		(.super this :layout)))

;two sides, each the files of the host or a store, and between them what
;is taken copied from one to the other, by the rule for what is there
(ui-window *window* ()
	(ui-title-bar *title* "Stores" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill :paper :paper_toolbar)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (+sym_next +sym_prev +sym_delete +sym_reset) +event_to_right))
		(ui-label _ (:text " If there: " :font :font_button :paper :paper_toolbar))
		(ui-tool-bar *rule_toolbar* (:font :font_button)
			(ui-buttons ("Skip" "Replace" "Keep both") +event_skip))
		(ui-label _ (:text " Copies: " :font :font_button :paper :paper_toolbar))
		(ui-flow _ (:flow_flags +flow_right_fill :font :font_button)
			(. (ui-textfield *copies* (:clear_text "" :hint_text "0 store's" :min_width 80 :paper +argb_white))
				:connect +event_copies)
			(ui-label _ (:text "" :paper :paper_toolbar))))
	(ui-flow _ (:flow_flags +flow_up_fill)
		(ui-flow _ (:flow_flags +flow_right_fill :font :font_button)
			(ui-buttons ("Cancel") +event_cancel)
			(ui-flow _ (:flow_flags +flow_down_fill)
				(ui-label *status* (:text ""))
				(ui-progress *progress* (:value 0 :maximum 1000 :min_height 8))))
		(ui-grid _ (:grid_width 2 :grid_height 1)
			(stores-side 0)
			(stores-side 1))))

(ui-tool-tips *main_toolbar*
	'("copy left to right" "copy right to left" "delete from the store" "look again"))
