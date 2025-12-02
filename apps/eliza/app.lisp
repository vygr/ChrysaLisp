;ELIZA - Classic chatbot demonstrating pattern matching and symbolic processing
;Based on Joseph Weizenbaum's 1966 ELIZA program
;Simulates a Rogerian psychotherapist using pattern-response transformations

(import "././login/env.inc")
(import "gui/lisp.inc")
(import "./engine.inc")

(enums +event 0
	(enum close)
	(enum send)
	(enum clear)
	(enum save)
	(enum toggle_delay)
	(enum toggle_debug))

(enums +select 0
	(enum main tip))

(defq vdu_width 70 vdu_height 25)

;Build the GUI
(ui-window *window* (:color +argb_grey4)
	(ui-title-bar _ "ELIZA - Classic Pattern Matching Chatbot" (0xea19) +event_close)
	(ui-tool-bar *main_toolbar* ()
		(ui-buttons (0xe95d 0xe800 0xe9f3 0xe9e2) +event_clear))
	(ui-vdu vdu (:vdu_width vdu_width :vdu_height vdu_height
				 :ink_color +argb_green :color +argb_black))
	(ui-flow _ (:flow_flags +flow_right_fill :color +argb_grey4)
		(ui-label _ (:text "You: " :color +argb_grey4 :ink_color +argb_white))
		(. (ui-textfield chat_text (:clear_text "" :hint_text "Type your message and press Enter..."
									 :color +argb_white :ink_color +argb_black))
		   :connect +event_send)))

;Print text to VDU with word wrapping
(defun vdu-print (vdu buf prefix text)
	(defq line (cat prefix text)
		  words (split line " ")
		  current_line ""
		  lines (list))
	;Word wrap
	(each! (lambda (_)
		(defq test_line (if (eql current_line "")
						   %0
						   (cat current_line " " %0)))
		(if (> (length test_line) (const vdu_width))
			(progn
				(push lines current_line)
				(setq current_line %0))
			(setq current_line test_line))) words)
	;Add remaining line
	(when (> (length current_line) 0)
		(push lines current_line))
	;Add to buffer and trim if needed
	(each! (# (push buf %0)) lines)
	(while (> (length buf) (const vdu_height))
		(setq buf (slice buf 1 -1)))
	;Display
	(. vdu :load buf 0 0 0 (length buf))
	buf)

;Save conversation to file
(defun save-conversation (buf)
	(defq timestamp (pii-time)
		  filename (cat "eliza_" (str timestamp) ".txt")
		  filepath (cat (pii-home) "/" filename)
		  stream (file-stream filepath file_open_write))
	(when stream
		(each! (# (write-line stream %0)) buf)
		(close stream)
		filename))

;Add tooltips
(defun tooltips (select)
	(def *window* :tip_mbox (elem-get select +select_tip))
	(ui-tool-tips *main_toolbar*
		'("Clear conversation" "Save conversation" "Toggle typing delay" "Toggle debug mode")))

;Main event loop
(defun main ()
	(defq text_buf (list) select (task-mboxes +select_size)
		  typing_delay :t ;Toggle for typing delay
		  debug_mode :nil ;Toggle for debug visualization
		  delay_ms 500) ;Delay in milliseconds
	(tooltips select)

	;Print initial greeting
	(setq text_buf (vdu-print vdu text_buf "ELIZA: " *initial*))
	(setq text_buf (vdu-print vdu text_buf "" ""))

	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))

	(defq *running* :t)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				;Tooltip event
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= (setq id (getf *msg* +ev_msg_target_id)) +event_close)
				;Close window
				(setq *running* :nil))
			((= id +event_clear)
				;Clear conversation
				(setq text_buf (list))
				(setq text_buf (vdu-print vdu text_buf "ELIZA: " *initial*))
				(setq text_buf (vdu-print vdu text_buf "" "")))
			((= id +event_save)
				;Save conversation
				(when (defq filename (save-conversation text_buf))
					(setq text_buf (vdu-print vdu text_buf "[Saved to " (cat filename "]")))
					(setq text_buf (vdu-print vdu text_buf "" ""))))
			((= id +event_toggle_delay)
				;Toggle typing delay
				(setq typing_delay (not typing_delay))
				(setq text_buf (vdu-print vdu text_buf "[Typing delay: "
					(cat (if typing_delay "ON" "OFF") "]")))
				(setq text_buf (vdu-print vdu text_buf "" "")))
			((= id +event_toggle_debug)
				;Toggle debug mode
				(setq debug_mode (not debug_mode))
				(setq text_buf (vdu-print vdu text_buf "[Debug mode: "
					(cat (if debug_mode "ON" "OFF") "]")))
				(setq text_buf (vdu-print vdu text_buf "" "")))
			((= id +event_send)
				;User sent a message
				(defq user_input (get :clear_text chat_text))
				(unless (eql user_input "")
					;Display user input
					(setq text_buf (vdu-print vdu text_buf "You: " user_input))

					;Apply typing delay if enabled
					(when typing_delay
						(task-sleep (* delay_ms 1000)))

					;Generate ELIZA response with optional debug info
					(defq response (eliza-response user_input))
					(if response
						(progn
							;Show debug info if enabled
							(when debug_mode
								(defq debug_info (eliza-debug-info user_input))
								(setq text_buf (vdu-print vdu text_buf "[Debug: "
									(cat "Pattern='" (first debug_info) "' Rank=" (str (second debug_info)) "]")))
								(when (third debug_info)
									(setq text_buf (vdu-print vdu text_buf "[Matches: "
										(cat (third debug_info) "]")))))
							;Display ELIZA response
							(setq text_buf (vdu-print vdu text_buf "ELIZA: " response))
							(setq text_buf (vdu-print vdu text_buf "" "")))
						;User quit
						(progn
							(setq text_buf (vdu-print vdu text_buf "ELIZA: "
								"Goodbye. It was nice talking to you."))
							(setq text_buf (vdu-print vdu text_buf "" ""))))

					;Clear input field
					(set chat_text :clear_text "" :cursor 0 :anchor 0)
					(.-> chat_text :layout :dirty)))
			(:t ;Other GUI events
				(. *window* :event *msg*))))

	(gui-sub-rpc *window*))
