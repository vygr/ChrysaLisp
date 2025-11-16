;Test app for designer mode
;This demonstrates how the designer tracking works

(import "././login/env.inc")
(import "gui_designer/lisp.inc")

(enums +event 0
	(enum close)
	(enum button_click))

;Define a simple UI with designer tracking enabled
(ui-window *test_window* (:min_width 400 :min_height 300)
	(ui-title-bar _ "Test App" (0xea19) +event_close)

	(ui-flow main_flow (:flow_flags +flow_down_fill)
		(ui-label title_label (:text "Test Application"
			:font *env_title_font*))

		(ui-grid button_grid (:grid_width 2 :grid_height 2)
			(ui-button btn1 (:text "Button 1"))
			(ui-button btn2 (:text "Button 2"))
			(ui-button btn3 (:text "Button 3"))
			(ui-button btn4 (:text "Button 4")))

		(ui-flow input_row (:flow_flags +flow_right)
			(ui-label input_label (:text "Input:"))
			(ui-textfield input_field (:min_width 200)))

		(ui-slider test_slider (:min_width 300))
		(ui-progress test_progress (:min_width 300))))

;After UI is created, the designer tree is available
(defun show-designer-tree ()
	; (show-designer-tree) -> :nil
	(defq tree (designer-get-tree))
	(when tree
		(print "Designer UI Tree:")
		(print (str tree))
		(print "")
		(print "Type: " (get :type tree))
		(print "Name: " (get :name tree))
		(print "Children: " (str (length (get :children tree))))))

;Export for testing
(defun get-ui-tree ()
	; (get-ui-tree) -> tree
	(designer-get-tree))

;Show the tree
(show-designer-tree)
