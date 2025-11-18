;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Example 3: ColorPicker Widget Integration
; Shows how to integrate ColorPicker into apps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././login/env.inc")
(import "gui/lisp.inc")
(import "lib/colors/colors.inc")
(import "lib/colors/picker.inc")

(enums +event 0
	(enum close)
	(enum picker)
	(enum apply_to_bg apply_to_text))

(enums +select 0
	(enum main))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Simple Paint App with ColorPicker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ui-window *window* (:title "ColorPicker Integration Example")
	(ui-title-bar _ "Paint Tool Example" (0xea19) +event_close)

	(ui-flow _ (:flow_flags +flow_right_fill)
		; Left: ColorPicker
		(ui-flow left_panel (:flow_flags +flow_down_fill :min_width 350)
			(ui-label _ (:text "Choose Color:" :font *env_title_font*))
			(defq *picker* (ColorPicker))
			(def *picker* :action_event +event_picker)
			(. left_panel :add_child *picker*))

		; Right: Preview and apply
		(ui-flow right_panel (:flow_flags +flow_down_fill :min_width 300)
			(ui-label _ (:text "Preview:" :font *env_title_font*))

			; Current color display
			(defq *color_display* (ui-view _ (:min_width 200 :min_height 100
											  :color 0xffff0000)))
			(. right_panel :add_child *color_display*)

			; Sample text
			(ui-label _ (:text "Sample Applications:"))
			(defq *sample_text* (ui-label _ (:text "Sample Text Here"
													:min_height 60
													:font *env_title_font*
													:ink_color 0xffff0000)))
			(. right_panel :add_child *sample_text*)

			; Sample background
			(defq *sample_bg* (ui-label _ (:text "Background Color"
												  :min_height 60
												  :font *env_title_font*
												  :color 0xffff0000)))
			(. right_panel :add_child *sample_bg*)

			; Apply buttons
			(ui-label _ (:text "Apply Color To:"))
			(ui-flow _ (:flow_flags +flow_right)
				(ui-button _ (:text "Text Color" :action_event +event_apply_to_text))
				(ui-button _ (:text "Background" :action_event +event_apply_to_bg)))

			; Info display
			(ui-label _ (:text "Current Color Info:"))
			(defq *info_label* (ui-label _ (:text "" :min_height 80)))
			(. right_panel :add_child *info_label*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun update-preview ()
	; Update all preview elements with current color
	(defq current_color (. *picker* :get_color))

	; Update color display
	(def (. *color_display* :dirty) :color current_color)

	; Update info
	(bind '(r g b) (rgb-unpack current_color))
	(bind '(h s v) (rgb-to-hsv r g b))
	(defq info_text (cat
		"Hex: " (argb-to-hex current_color) (ascii-char 10)
		"RGB: " r ", " g ", " b (ascii-char 10)
		"HSV: " h "°, " s "%, " v "%"))
	(set *info_label* :text info_text)
	(.-> *info_label* :layout :dirty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main Event Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
	; Position and show window
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))

	(defq select (task-mboxes +select_size)
		  running :t)

	; Initial update
	(update-preview)

	; Main event loop
	(while running
		(defq msg (mail-read (elem-get select (defq idx (mail-select select))))
			  id (getf msg +ev_msg_target_id))

		(case idx
			(+select_main
				(cond
					; Window close
					((= id +event_close)
						(setq running :nil))

					; Color changed
					((>= id +event_picker)
						(update-preview))

					; Apply to text color
					((= id +event_apply_to_text)
						(defq color (. *picker* :get_color))
						(def (. *sample_text* :dirty) :ink_color color))

					; Apply to background
					((= id +event_apply_to_bg)
						(defq color (. *picker* :get_color))
						(def (. *sample_bg* :dirty) :color color))

					; Default: pass to window
					(:t
						(. *window* :event msg))))))

	; Cleanup
	(gui-sub-rpc *window*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Integration Notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "")
(print "═══════════════════════════════════════")
(print "  ColorPicker Integration Example")
(print "═══════════════════════════════════════")
(print "")
(print "This example demonstrates:")
(print "")
(print "1. Creating a ColorPicker widget:")
(print "   (defq picker (ColorPicker))")
(print "   (def picker :action_event +my_event)")
(print "")
(print "2. Getting the current color:")
(print "   (. picker :get_color) → ARGB integer")
(print "")
(print "3. Setting a color programmatically:")
(print "   (. picker :set_color 0xffff0000)")
(print "")
(print "4. Handling color change events:")
(print "   ((>= id +event_picker)")
(print "    (update-preview))")
(print "")
(print "5. Integrating with other UI elements:")
(print "   - Apply to text color")
(print "   - Apply to background color")
(print "   - Update preview displays")
(print "")
(print "═══════════════════════════════════════")
(print "")

; Run the app
(main)
