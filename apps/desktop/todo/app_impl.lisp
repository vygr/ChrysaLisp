(import "usr/env.inc")
(import "gui/lisp.inc")

;our config UI widgets
(import "./config.inc")
(import "./widgets.inc")

(enums +select 0
	(enum main tip))

(defun create-item (flow &optional text)
	(defq column (find flow (list *todo_flow* *done_flow* *deleted_flow*)))
	(ui-root entry (Flow) (:flow_flags +flow_right_fill)
		(ui-tool-bar toolbar (:font :font_small_toolbar)
			(case column
				(0 (ui-buttons (+sym_done +sym_delete) +event_todo_done))
				(1 (ui-buttons (+sym_reset +sym_delete) +event_done_redo))
				(2 (ui-buttons (+sym_reset +sym_delete) +event_del_redo))))
		(ui-textfield _ (:clear_text (setd text "")
			:hint_text "todo item" :paper +argb_white)))
	(. flow :add_child entry)
	(ui-tool-tips toolbar
		(case column
			(0 '("done it" "delete"))
			(1 '("redo it" "delete"))
			(2 '("redo it" "erase")))))

(defun layout-items (min_width min_height)
	(defq item_flows (list *todo_flow* *done_flow* *deleted_flow*)
		scrolls (list *todo_scroll* *done_scroll* *deleted_scroll*))
	(each (# (def %0 :want_width min_width :want_height min_height)) scrolls)
	(defq first_item (some (# (first (. %0 :children))) item_flows)
		spacing (if first_item (second (. first_item :pref_size)) (. *config* :find :spacing)))
	(. *config* :insert :spacing spacing)
	(def *stack_flow* :spacing spacing)
	;each what it scrolls fills it, (. scroll :fit)
	(each (# (.-> %0 :layout :dirty_all)) scrolls))

(defun populate-items ()
	(defq item_lists (gather *config* :todo :done :deleted)
		item_flows (list *todo_flow* *done_flow* *deleted_flow*))
	(each (lambda (item_flow item_list)
			(each (# (. %0 :sub)) (. item_flow :children))
			(each (lambda (text)
				(create-item item_flow text)) item_list))
		item_flows item_lists))

;import actions and bindings
(import "./actions.inc")

(defun main ()
	(defq select (task-mboxes +select_size) *running* :t
		*min_width* 512 *min_height* 512
		*max_width* 1024 *max_height* 512
		todo_service (mail-declare (task-mbox) "Todo" "Todo Service 0.1"))
	(config-load)
	(populate-items)
	(layout-items *min_width* *min_height*)
	(. *stack_flow* :show_tab 0)
	(def *window* :tip_mbox (elem-get select +select_tip))
	(.-> *window* (:connect +event_layout) :open)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				;tip event
				(. *window* :tip *msg*))
			((. *window* :dispatch *msg*))
			((. *window* :event *msg*))))
	(config-save)
	(. *window* :close)
	(mail-forget todo_service))
