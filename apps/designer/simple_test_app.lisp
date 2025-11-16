; Simple Test App - Just UI, no event loop
(import "gui/lisp.inc")

; Create a simple UI
(ui-window test_window (:min_width 400 :min_height 300)
	(ui-flow main_content (:flow_flags +flow_down)
		(ui-label title (:text "Simple Test App"))
		(ui-flow button_row (:flow_flags +flow_right)
			(ui-button ok_btn (:text "OK" :min_width 80))
			(ui-button cancel_btn (:text "Cancel" :min_width 80)))))

; Don't start event loop - just define the UI
