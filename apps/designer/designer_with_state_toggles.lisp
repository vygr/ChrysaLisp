(import "././login/env.inc")
(import "gui/lisp.inc")
(import "gui_designer/state_toggles.inc")
(import "lib/consts/chars.inc")

(enums +event 0
	(enum close max min)
	(enum new open save)
	(enum toggle_changed))

(enums +select 0
	(enum main tip))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; State Toggle UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Example toggles for demonstration
(designer-register-toggle "error_state" :nil "Show error messages")
(designer-register-toggle "logged_in" :t "User logged in")
(designer-register-toggle "dark_mode" :nil "Dark theme")
(designer-register-toggle "debug_mode" :nil "Debug info")
(designer-register-toggle "empty_data" :nil "Empty list/no data")

;UI Definition
(ui-window *window* (:min_width 900 :min_height 600)
	;Title bar
	(ui-title-bar _ "Designer with State Toggles" (0xea19 0xea1b 0xea1a) +event_close)

	;Main layout
	(ui-flow main_area (:flow_flags +flow_down_fill)

		;State Toggle Ribbon at top
		(ui-flow toggle_ribbon (:flow_flags +flow_down
				:color *env_toolbar2_col*
				:min_height 120)
			(ui-title ribbon_title (:text "Design-Time State Toggles"))

			;Explanation
			(ui-label info_label (:text "Toggle UI states without running the app"
				:font *env_toolbar_font*
				:min_width 880))

			;Toggle buttons grid
			(ui-grid toggle_grid (:grid_width 5 :grid_height 1
					:min_width 880)

				;error_state toggle
				(ui-flow error_toggle (:flow_flags +flow_down)
					(ui-button error_btn (:text "[error_state = false]"
						:min_width 170 :min_height 40
						:color *env_toolbar_col*))
					(ui-label error_desc (:text "Error messages"
						:font *env_toolbar_font*
						:min_width 170)))

				;logged_in toggle
				(ui-flow login_toggle (:flow_flags +flow_down)
					(ui-button login_btn (:text "[logged_in = true]"
						:min_width 170 :min_height 40
						:color +argb_green))
					(ui-label login_desc (:text "User logged in"
						:font *env_toolbar_font*
						:min_width 170)))

				;dark_mode toggle
				(ui-flow dark_toggle (:flow_flags +flow_down)
					(ui-button dark_btn (:text "[dark_mode = false]"
						:min_width 170 :min_height 40
						:color *env_toolbar_col*))
					(ui-label dark_desc (:text "Dark theme"
						:font *env_toolbar_font*
						:min_width 170)))

				;debug_mode toggle
				(ui-flow debug_toggle (:flow_flags +flow_down)
					(ui-button debug_btn (:text "[debug_mode = false]"
						:min_width 170 :min_height 40
						:color *env_toolbar_col*))
					(ui-label debug_desc (:text "Debug info"
						:font *env_toolbar_font*
						:min_width 170)))

				;empty_data toggle
				(ui-flow empty_toggle (:flow_flags +flow_down)
					(ui-button empty_btn (:text "[empty_data = false]"
						:min_width 170 :min_height 40
						:color *env_toolbar_col*))
					(ui-label empty_desc (:text "Empty list/no data"
						:font *env_toolbar_font*
						:min_width 170)))))

		;Design surface
		(ui-flow design_surface (:flow_flags +flow_down_fill
				:color +argb_grey10)
			(ui-title surface_title (:text "Design Surface (Preview)"))

			;Preview area showing conditional elements
			(ui-flow preview_area (:flow_flags +flow_down_fill
					:color +argb_white
					:min_height 300)

				;Always visible
				(ui-label title_label (:text "My Application"
					:font *env_title_font*
					:min_width 880))

				;Conditional: error state
				(ui-label error_msg (:text "ERROR: Something went wrong!"
					:color +argb_red
					:min_width 880))

				;Conditional: logged in (showing logout)
				(ui-button logout_btn (:text "Logout"
					:min_width 100))

				;Conditional: NOT logged in (showing login) - HIDDEN initially
				;(ui-button login_btn_surface (:text "Login" :min_width 100))

				;Conditional: debug mode
				(ui-vdu debug_vdu (:text "Debug: state = OK"
					:min_width 880
					:min_height 60
					:color +argb_black
					:ink_color +argb_green))

				;Conditional: empty data
				(ui-label empty_msg (:text "No data to display"
					:color +argb_grey12
					:min_width 880))))

		;Info panel at bottom
		(ui-flow info_panel (:flow_flags +flow_down
				:min_height 100
				:color *env_toolbar2_col*)
			(ui-title info_title (:text "Toggle History"))
			(ui-vdu history_vdu (:min_width 880 :min_height 60
				:text "Ready. Toggle states above to see different UI conditions.")))))

;Connect toggle buttons
(. error_btn :connect +event_toggle_changed)
(. login_btn :connect +event_toggle_changed)
(. dark_btn :connect +event_toggle_changed)
(. debug_btn :connect +event_toggle_changed)
(. empty_btn :connect +event_toggle_changed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Toggle State Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun update-toggle-button (btn toggle-name)
	; (update-toggle-button btn toggle-name) -> :nil
	(defq value (designer-get-toggle toggle-name))
	(defq text (cat "[" toggle-name " = " (if value "true" "false") "]"))
	(defq color (if value +argb_green *env_toolbar_col*))
	(def (. btn :dirty) :text text :color color)
	(.-> btn :layout :dirty))

(defun update-preview ()
	; (update-preview) -> :nil
	; Updates preview area based on toggle states

	;Show/hide error message
	(defq show-error (designer-get-toggle "error_state"))
	(. error_msg :set_flags
		(if show-error 0 +view_flag_hidden)
		+view_flag_hidden)

	;Show/hide logout button (shown when logged_in)
	(defq logged-in (designer-get-toggle "logged_in"))
	(. logout_btn :set_flags
		(if logged-in 0 +view_flag_hidden)
		+view_flag_hidden)

	;Show/hide debug VDU
	(defq show-debug (designer-get-toggle "debug_mode"))
	(. debug_vdu :set_flags
		(if show-debug 0 +view_flag_hidden)
		+view_flag_hidden)

	;Show/hide empty message
	(defq show-empty (designer-get-toggle "empty_data"))
	(. empty_msg :set_flags
		(if show-empty 0 +view_flag_hidden)
		+view_flag_hidden)

	;Relayout
	(.-> preview_area :constrain :t :dirty_all))

(defun add-history-entry (toggle-name old-value new-value)
	; (add-history-entry toggle-name old-value new-value) -> :nil
	(defq msg (cat toggle-name ": " (if old-value "true" "false")
		" → " (if new-value "true" "false")))
	(. history_vdu :write msg)
	(. history_vdu :write (ascii-char 10))
	(.-> history_vdu :layout :dirty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
	;Initialize window
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))

	(defq select (task-mboxes +select_size)
		running :t)

	(def *window* :tip_mbox (elem-get select +select_tip))

	;Set initial visibility
	(update-preview)

	;Main event loop
	(while running
		(defq msg (mail-read (elem-get select (defq idx (mail-select select))))
			id (getf msg +ev_msg_target_id))

		(case idx
			(+select_tip
				(if (defq view (. *window* :find_id (getf msg +mail_timeout_id)))
					(. view :show_tip)))

			(+select_main
				(cond
					;Window events
					((= id +event_close)
						(setq running :nil))

					((= id +event_min)
						(bind '(x y w h) (apply view-fit
							(cat (. *window* :get_pos) (. *window* :pref_size))))
						(. *window* :change_dirty x y w h))

					((= id +event_max)
						(bind '(x y w h) (apply view-fit
							(cat (. *window* :get_pos) '(900 600))))
						(. *window* :change_dirty x y w h))

					;Toggle button clicked
					((= id +event_toggle_changed)
						(defq source-id (getf msg +ev_msg_action_source_id))
						(defq button (. *window* :find_id source-id))
						(cond
							((eql button error_btn)
								(defq old (designer-get-toggle "error_state"))
								(designer-toggle-flip "error_state")
								(update-toggle-button error_btn "error_state")
								(add-history-entry "error_state" old (designer-get-toggle "error_state")))

							((eql button login_btn)
								(defq old (designer-get-toggle "logged_in"))
								(designer-toggle-flip "logged_in")
								(update-toggle-button login_btn "logged_in")
								(add-history-entry "logged_in" old (designer-get-toggle "logged_in")))

							((eql button dark_btn)
								(defq old (designer-get-toggle "dark_mode"))
								(designer-toggle-flip "dark_mode")
								(update-toggle-button dark_btn "dark_mode")
								(add-history-entry "dark_mode" old (designer-get-toggle "dark_mode")))

							((eql button debug_btn)
								(defq old (designer-get-toggle "debug_mode"))
								(designer-toggle-flip "debug_mode")
								(update-toggle-button debug_btn "debug_mode")
								(add-history-entry "debug_mode" old (designer-get-toggle "debug_mode")))

							((eql button empty_btn)
								(defq old (designer-get-toggle "empty_data"))
								(designer-toggle-flip "empty_data")
								(update-toggle-button empty_btn "empty_data")
								(add-history-entry "empty_data" old (designer-get-toggle "empty_data"))))

						;Update preview
						(update-preview))

					;Default event handling
					(:t (. *window* :event msg))))))

	;Cleanup
	(gui-sub-rpc *window*))
