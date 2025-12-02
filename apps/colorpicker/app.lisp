;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Color Picker Demo Application
; Demonstrates the full-featured color picker library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././login/env.inc")
(import "gui/lisp.inc")
(import "lib/colors/colors.inc")
(import "lib/colors/picker.inc")

(enums +event 0
	(enum close max min)
	(enum picker)
	(enum copy_hex copy_rgb copy_hsv)
	(enum lighten darken saturate desaturate)
	(enum find_name))

(enums +select 0
	(enum main tip))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; UI Construction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ui-window *window* (:title "Color Picker Demo")
	(ui-title-bar _ "Color Picker" (0xea19 0xea1b 0xea1a) +event_close)

	(ui-flow _ (:flow_flags +flow_right_fill)
		; Left side: Color Picker
		(ui-flow left_panel (:flow_flags +flow_down_fill :min_width 360)
			(defq *picker* (ColorPicker))
			(def *picker* :action_event +event_picker)
			(. left_panel :add_child *picker*))

		; Right side: Information and Actions
		(ui-flow right_panel (:flow_flags +flow_down_fill :min_width 280)
			; Current Color Info
			(. right_panel :add_child (ui-label _ (:text "Current Color:" :font *env_title_font*)))

			(defq *info_text* (ui-label _ (:text "" :min_height 100
										   :flow_flags +flow_down_fill)))
			(. right_panel :add_child *info_text*)

			; Copy buttons
			(. right_panel :add_child (ui-label _ (:text "Copy to Clipboard:")))
			(ui-flow _ (:flow_flags +flow_right)
				(ui-button _ (:text "Hex" :action_event +event_copy_hex))
				(ui-button _ (:text "RGB" :action_event +event_copy_rgb))
				(ui-button _ (:text "HSV" :action_event +event_copy_hsv)))

			; Color manipulation buttons
			(. right_panel :add_child (ui-label _ (:text "Adjust Color:")))
			(ui-grid _ (:grid_width 2 :grid_height 2)
				(ui-button _ (:text "Lighten" :action_event +event_lighten :tip_text "Lighten by 10%"))
				(ui-button _ (:text "Darken" :action_event +event_darken :tip_text "Darken by 10%"))
				(ui-button _ (:text "Saturate" :action_event +event_saturate :tip_text "Increase saturation"))
				(ui-button _ (:text "Desaturate" :action_event +event_desaturate :tip_text "Decrease saturation")))

			; Find color name
			(. right_panel :add_child (ui-label _ (:text "Color Name:")))
			(defq *color_name* (ui-label _ (:text "red" :min_height 30)))
			(. right_panel :add_child *color_name*)

			; Sample uses
			(. right_panel :add_child (ui-label _ (:text "Sample:" :font *env_title_font*)))
			(ui-flow _ (:flow_flags +flow_down_fill)
				(defq *sample_text* (ui-label _ (:text "Sample Text" :min_height 40
												 :ink_color 0xffff0000
												 :font *env_title_font*)))
				(defq *sample_bg* (ui-label _ (:text "Background Sample" :min_height 40
											   :color 0xffff0000
											   :font *env_title_font*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun update-info ()
	; Update the info display with current color information
	(defq current_color (. *picker* :get_color))
	(bind '(r g b) (rgb-unpack current_color))
	(bind '(h s v) (rgb-to-hsv r g b))

	(defq info_lines (list
		(cat "Hex: " (argb-to-hex current_color))
		(cat "RGB: " r ", " g ", " b)
		(cat "HSV: " h ", " s ", " v)))

	(set *info_text* :text (apply cat (map (lambda (s) (cat s (ascii-char 10))) info_lines)))

	; Update color name
	(defq name (color-find-name current_color 50))
	(set *color_name* :text (if name (rest (str name)) "Unknown"))

	; Update samples
	(def (. *sample_text* :dirty) :ink_color current_color)
	(def (. *sample_bg* :dirty) :color current_color))

(defun copy-to-clipboard (text)
	; Copy text to clipboard via clipboard service
	(mail-send (elem-get (mail-enquire "Clipboard") 0)
		(list +select_main text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main Event Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
	; Position and show window
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))

	(defq select (task-mboxes +select_size)
		  running :t)

	; Setup tooltips
	(def *window* :tip_mbox (elem-get select +select_tip))

	; Initial update
	(update-info)

	; Main event loop
	(while running
		(defq msg (mail-read (elem-get select (defq idx (mail-select select))))
			  id (getf msg +ev_msg_target_id))

		(case idx
			(+select_tip
				(when (defq view (. *window* :find_id (getf msg +mail_timeout_id)))
					(. view :show_tip)))

			(+select_main
				(cond
					; Window controls
					((= id +event_close)
						(setq running :nil))

					((= id +event_min)
						(bind '(x y w h) (apply view-fit
							(cat (. *window* :get_pos) (. *window* :pref_size))))
						(. *window* :change_dirty x y w h))

					((= id +event_max)
						(bind '(x y w h) (apply view-fit
							(cat (. *window* :get_pos) '(800 600))))
						(. *window* :change_dirty x y w h))

					; Color picker changed
					((>= id +event_picker)
						(update-info))

					; Copy buttons
					((= id +event_copy_hex)
						(copy-to-clipboard (argb-to-hex (. *picker* :get_color))))

					((= id +event_copy_rgb)
						(bind '(r g b) (rgb-unpack (. *picker* :get_color)))
						(copy-to-clipboard (cat "rgb(" r ", " g ", " b ")")))

					((= id +event_copy_hsv)
						(bind '(r g b) (rgb-unpack (. *picker* :get_color)))
						(bind '(h s v) (rgb-to-hsv r g b))
						(copy-to-clipboard (cat "hsv(" h ", " s ", " v ")")))

					; Color adjustments
					((= id +event_lighten)
						(. *picker* :set_color
							(color-lighten (. *picker* :get_color) 10)))

					((= id +event_darken)
						(. *picker* :set_color
							(color-darken (. *picker* :get_color) 10)))

					((= id +event_saturate)
						(. *picker* :set_color
							(color-saturate (. *picker* :get_color) 10)))

					((= id +event_desaturate)
						(. *picker* :set_color
							(color-desaturate (. *picker* :get_color) 10)))

					; Default: pass to window
					(:t
						(. *window* :event msg))))))

	; Cleanup
	(gui-sub-rpc *window*))

; Run the app
(main)
