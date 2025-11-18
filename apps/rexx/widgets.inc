;REXX UI Widgets

(enums +select 0
	(enum main tip timer))

(defq +rate (/ 1000000 10))  ; 10Hz update rate

;create main window
(defun create-main-window ()
	(defq *event_map* (Fmap)
		toolbar_height 32
		button_width 100
		button_height 28
		margin 5)

	;create main window
	(defq window (Window)
		vbox (Vbox))

	;toolbar with buttons
	(defq toolbar (Flow)
		execute_btn (Button "Execute" button_width button_height)
		clear_btn (Button "Clear" button_width button_height)
		ports_btn (Button "Ports" button_width button_height)
		help_btn (Button "Help" button_width button_height))

	(. toolbar :flow_flags +flow_left_fill)
	(. toolbar :add_child execute_btn)
	(. toolbar :add_child clear_btn)
	(. toolbar :add_child ports_btn)
	(. toolbar :add_child help_btn)

	;REXX input area (text editor)
	(defq input_label (Label "REXX Code:")
		input_text (Textview 600 200))
	(. input_text :border 1)
	(. input_text :set_text
		";Sample REXX code demonstrating ADDRESS/PORTS
SAY \"Hello from REXX!\"
ADDRESS SYSTEM
\"echo ChrysaLisp REXX is running\"
SAY \"Command executed via SYSTEM port\"")

	;output area
	(defq output_label (Label "Output:")
		output_text (Textview 600 150))
	(. output_text :border 1)
	(. output_text :readonly :t)

	;status line showing current ADDRESS
	(defq status_label (Label "ADDRESS: COMMAND"))
	(. status_label :border 1)

	;assemble UI
	(. vbox :add_child toolbar)
	(. vbox :add_child input_label)
	(. vbox :add_child input_text)
	(. vbox :add_child output_label)
	(. vbox :add_child output_text)
	(. vbox :add_child status_label)

	(. window :add_child vbox)

	;store widget references
	(def window
		:input_text input_text
		:output_text output_text
		:status_label status_label
		:execute_btn execute_btn
		:clear_btn clear_btn
		:ports_btn ports_btn
		:help_btn help_btn)

	;bind events
	(. *event_map* :insert (. execute_btn :get_id) '(action-execute))
	(. *event_map* :insert (. clear_btn :get_id) '(action-clear))
	(. *event_map* :insert (. ports_btn :get_id) '(action-show-ports))
	(. *event_map* :insert (. help_btn :get_id) '(action-show-help))

	window)

;Helper to get text from textview
(defun get-text (textview)
	(. textview :get_text))

;Helper to set text in textview
(defun set-text (textview text)
	(. textview :set_text text))

;Helper to append text to textview
(defun append-text (textview text)
	(defq current (get-text textview))
	(set-text textview (cat current (if (nempty? current) (ascii-char 10) "") text)))
