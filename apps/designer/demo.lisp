;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Designer Mode Demo
; Demonstrates how apps can be designed visually
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././login/env.inc")
(import "gui_designer/lisp.inc")
(import "lib/consts/chars.inc")

(enums +event 0
	(enum close)
	(enum click_me)
	(enum submit))

;This is a simple demo app that shows the designer in action
;The exact same code runs in both normal mode and designer mode

(ui-window *demo_window* (:min_width 400 :min_height 300)
	;Title bar with close button
	(ui-title-bar _ "Designer Demo" (0xea19) +event_close)

	;Main content area
	(ui-flow content_flow (:flow_flags +flow_down_fill)

		;Title
		(ui-label title_label (:text "Designer Mode Demo"
			:font *env_title_font*
			:min_height 40))

		;Description
		(ui-label desc_label (:text "This app is built using designer-tracked UI macros"
			:min_width 380))

		;Button grid
		(ui-grid button_grid (:grid_width 3 :grid_height 2
				:min_width 380 :min_height 120)
			(ui-button btn1 (:text "Button 1"))
			(ui-button btn2 (:text "Button 2"))
			(ui-button btn3 (:text "Click Me"))
			(ui-button btn4 (:text "Button 4"))
			(ui-button btn5 (:text "Button 5"))
			(ui-button btn6 (:text "Button 6")))

		;Input section
		(ui-flow input_section (:flow_flags +flow_down_fill)
			(ui-label input_title (:text "Input Section"
				:min_width 380))

			;Text input row
			(ui-flow input_row (:flow_flags +flow_right
					:min_width 380)
				(ui-label name_label (:text "Name:"
					:min_width 80))
				(ui-textfield name_field (:min_width 280
					:text "Enter name")))

			;Slider
			(ui-slider demo_slider (:min_width 360))

			;Progress bar
			(ui-progress demo_progress (:min_width 360)))

		;Submit button
		(ui-button submit_btn (:text "Submit"
			:min_width 100))))

;Connect events
(. btn3 :connect +event_click_me)
(. submit_btn :connect +event_submit)

;Show designer tree info
(defun show-tree-info ()
	; (show-tree-info) -> :nil
	(defq tree (designer-get-tree))
	(when tree
		(print "╔════════════════════════════════════════╗")
		(print "║  Designer Mode - UI Tree Generated     ║")
		(print "╚════════════════════════════════════════╝")
		(print "")
		(print "Root Element:")
		(print "  Type: " (get :type tree))
		(print "  Name: " (get :name tree))
		(print "  Children: " (str (length (get :children tree))))
		(print "")
		(defun count-all (elem)
			(defq count 1)
			(each (lambda (child) (setq count (+ count (count-all child))))
				(get :children elem))
			count)
		(print "Total Elements: " (str (count-all tree)))
		(print "")
		(print "This UI tree can be:")
		(print "  • Edited visually in the Designer")
		(print "  • Serialized back to Lisp source")
		(print "  • Loaded and modified")
		(print "  • Merged with imperative code")
		(print "")
		(print "Try: (designer-serialize-tree (designer-get-tree))")
		(print "")))

;Export function for external access
(defun get-demo-tree ()
	; (get-demo-tree) -> tree
	(designer-get-tree))

;Show info on load
(show-tree-info)

;Main function (if run as app)
(defun main ()
	(bind '(x y w h) (apply view-locate (. *demo_window* :pref_size)))
	(gui-add-front-rpc (. *demo_window* :change x y w h))

	(defq select (task-mboxes +select_size)
		running :t)

	(while running
		(defq msg (mail-read (elem-get select (defq idx (mail-select select))))
			id (getf msg +ev_msg_target_id))

		(cond
			((= id +event_close)
				(setq running :nil))

			((= id +event_click_me)
				(print "Click Me button pressed!"))

			((= id +event_submit)
				(print "Submit button pressed!"))

			(:t (. *demo_window* :event msg))))

	(gui-sub-rpc *demo_window*))
