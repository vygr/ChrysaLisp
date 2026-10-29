(enums +event 0
	(enum close max min)
	(enum to_right to_left delete refresh)
	(enum skip replace both)
	(enum copies cancel)
	(enum source_0 source_1 path_0 path_1 up_0 all_0 up_1 all_1))

;the rows of the two sides past the rest, a name and an open button
;each, so many shown at most
(defq +rows_max 2000 +event_rows 1000)

(defmacro stores-side (n)
	;one side: what it shows, the folder it is at, what is in it, a row a
	;thing, folders first, and what of it is taken at the foot
	(defq side (str n) at (# (sym (cat "*" %0 "_" side "*"))) event (# (sym (cat "+event_" %0 "_" side))))
	(static-qq (ui-flow _ (:flow_flags +flow_down_fill)
		(ui-flow _ (:flow_flags +flow_right_fill :font :font_button)
			(ui-tool-bar ,(at "tools") ()
				(ui-buttons (+sym_up +sym_select_region) ,(event "up")))
			(. (ui-button ,(at "source") (:text "")) :connect ,(event "source")))
		(. (ui-textfield ,(at "path") (:clear_text "" :hint_text "folder" :paper +argb_white))
			:connect ,(event "path"))
		(ui-flow _ (:flow_flags +flow_up_fill)
			(ui-label ,(at "note") (:text "" :font :font_small))
			(ui-element ,(at "scroll") (Sheet) (:paper :paper_slider :min_width 440 :min_height 420)
				(ui-backdrop ,(at "ruled") (:style :lines :paper +argb_white :ink +argb_grey14)
					(ui-flow ,(at "list") (:flow_flags +flow_down_fill))))))))

;two sides, each the files of the host or a store, and between them what
;is taken copied from one to the other, by the rule for what is there
(ui-window *window* ()
	(ui-title-bar *title* "Stores" (+sym_close +sym_max +sym_min) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill :paper :paper_toolbar)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (+sym_go_forward +sym_go_back +sym_delete +sym_reset) +event_to_right))
		(ui-tool-bar *rule_toolbar* ()
			(ui-buttons (+sym_step +sym_replace +sym_duplicate) +event_skip))
		(ui-flow _ (:flow_flags +flow_right_fill :font :font_button)
			(. (ui-textfield *copies* (:clear_text "" :hint_text "copies" :min_width 80 :paper +argb_white))
				:connect +event_copies)
			(ui-label _ (:text "" :paper :paper_toolbar))))
	(ui-flow _ (:flow_flags +flow_up_fill)
		(ui-flow _ (:flow_flags +flow_right_fill :font :font_button)
			(ui-tool-bar *cancel_toolbar* ()
				(ui-buttons (+sym_media_stop) +event_cancel))
			(ui-flow _ (:flow_flags +flow_down_fill)
				(ui-label *status* (:text ""))
				(ui-progress *progress* (:value 0 :maximum 1000 :min_height 8))))
		(ui-grid _ (:grid_width 2 :grid_height 1)
			(stores-side 0)
			(stores-side 1))))

(ui-tool-tips *main_toolbar* '("right" "left" "delete" "refresh"))
(ui-tool-tips *rule_toolbar* '("skip" "replace" "keep both"))
(ui-tool-tips *cancel_toolbar* '("cancel"))
(ui-tool-tips *tools_0* '("up" "all"))
(ui-tool-tips *tools_1* '("up" "all"))
