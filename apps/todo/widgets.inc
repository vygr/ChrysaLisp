(enums +event 0
	(enum close max min)
	(enum layout)
	(enum todo_done todo_del)
	(enum done_redo done_del)
	(enum del_redo del_del)
	(enum new_item))

(ui-window *window* ()
	(ui-title-bar *title* "Todo" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-tool-bar *main_toolbar* ()
		(ui-buttons (0xe94e) +event_new_item))
	(ui-stack *stack_flow* '("todo" "done" "deleted") :nil
		(ui-scroll *todo_scroll* +scroll_flag_vertical :nil
			(ui-flow *todo_scroll_child* (:flow_flags +flow_stack_fill)
				(ui-flow *todo_flow* (:flow_flags +flow_down))
				(ui-backdrop *todo_back* (:color +argb_grey2 :ink_color +argb_grey8 :style :lines))))
		(ui-scroll *done_scroll* +scroll_flag_vertical :nil
			(ui-flow *done_scroll_child* (:flow_flags +flow_stack_fill)
				(ui-flow *done_flow* (:flow_flags +flow_down))
				(ui-backdrop *done_back* (:color +argb_grey2 :ink_color +argb_grey8 :style :lines))))
		(ui-scroll *deleted_scroll* +scroll_flag_vertical :nil
			(ui-flow *deleted_scroll_child* (:flow_flags +flow_stack_fill)
				(ui-flow *deleted_flow* (:flow_flags +flow_down))
				(ui-backdrop *deleted_back* (:color +argb_grey2 :ink_color +argb_grey8 :style :lines))))))

(ui-tool-tips *stack_flow*
	'("yet to do" "have done" "to be deleted"))

(ui-tool-tips *main_toolbar*
	'("new todo"))
