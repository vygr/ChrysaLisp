(import "././login/env.inc")
(import "gui/lisp.inc")
(import "gui_designer/loader_enhanced.inc")
(import "gui_designer/runtime.inc")
(import "gui_designer/property_editor.inc")
(import "gui_designer/state_toggles.inc")
(import "gui_designer/serialize.inc")
(import "lib/consts/chars.inc")

(enums +event 0
	(enum close max min)
	(enum new open save save_as)
	(enum select_element)
	(enum toggle_state)
	(enum apply_props)
	(enum load_file))

(enums +select 0
	(enum main tip))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Global State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *designer-tree* :nil)
(defq *designer-original-source* :nil)
(defq *designer-file* :nil)
(defq *designer-modified* :nil)
(defq *selected-element* :nil)

;Register design-time toggles
(designer-register-toggle "error_state" :nil "Error messages")
(designer-register-toggle "logged_in" :t "User logged in")
(designer-register-toggle "debug_mode" :nil "Debug panel")
(designer-register-toggle "empty_data" :nil "Empty state")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; UI Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ui-window *window* (:min_width 1200 :min_height 800)
	;Title bar
	(ui-title-bar _ "ChrysaLisp Designer - Complete Edition"
		(0xea19 0xea1b 0xea1a) +event_close)

	;Main layout
	(ui-flow main_area (:flow_flags +flow_down_fill)

		;Top: File toolbar
		(ui-tool-bar file_toolbar ()
			(ui-button new_btn (:text "New" :min_width 80))
			(ui-button open_btn (:text "Open" :min_width 80))
			(ui-button save_btn (:text "Save" :min_width 80))
			(ui-button save_as_btn (:text "Save As" :min_width 80)))

		;File path input row
		(ui-flow file_path_row (:flow_flags +flow_right
				:color *env_toolbar2_col*
				:min_height 50)
			(ui-label path_label (:text "File Path:"
				:min_width 80
				:font *env_toolbar_font*))
			(ui-textfield file_path_input (:text ""
				:min_width 900))
			(ui-button load_path_btn (:text "Load"
				:min_width 100)))

		;State Toggle Ribbon
		(ui-flow toggle_ribbon (:flow_flags +flow_down
				:color *env_toolbar2_col*
				:min_height 100)
			(ui-title ribbon_title (:text "Design-Time State Toggles"))
			(ui-grid toggle_grid (:grid_width 4 :grid_height 1
					:min_width 1180)
				;error_state
				(ui-flow error_toggle (:flow_flags +flow_down)
					(ui-button error_btn (:text "[error_state = false]"
						:min_width 280 :min_height 35
						:color *env_toolbar_col*))
					(ui-label _ (:text "Error messages"
						:font *env_toolbar_font*
						:min_width 280)))
				;logged_in
				(ui-flow login_toggle (:flow_flags +flow_down)
					(ui-button login_btn (:text "[logged_in = true]"
						:min_width 280 :min_height 35
						:color +argb_green))
					(ui-label _ (:text "User logged in"
						:font *env_toolbar_font*
						:min_width 280)))
				;debug_mode
				(ui-flow debug_toggle (:flow_flags +flow_down)
					(ui-button debug_btn (:text "[debug_mode = false]"
						:min_width 280 :min_height 35
						:color *env_toolbar_col*))
					(ui-label _ (:text "Debug panel"
						:font *env_toolbar_font*
						:min_width 280)))
				;empty_data
				(ui-flow empty_toggle (:flow_flags +flow_down)
					(ui-button empty_btn (:text "[empty_data = false]"
						:min_width 280 :min_height 35
						:color *env_toolbar_col*))
					(ui-label _ (:text "Empty state"
						:font *env_toolbar_font*
						:min_width 280)))))

		;Main workspace: Tree | Preview | Properties
		(ui-flow workspace (:flow_flags +flow_right_fill)

			;Left: Element Tree
			(ui-flow tree_panel (:flow_flags +flow_down
					:min_width 300
					:color *env_toolbar2_col*)
				(ui-title tree_title (:text "UI Tree"))
				(ui-scroll tree_scroll (+scroll_flag_vertical)
					(:min_width 300 :min_height 400)
					(ui-vdu tree_vdu (:min_width 280 :min_height 380
						:text "No file loaded"))))

			;Center: Preview
			(ui-flow preview_panel (:flow_flags +flow_down_fill
					:color +argb_grey10)
				(ui-title preview_title (:text "Preview"))
				(ui-backdrop preview_area (:min_width 500 :min_height 400
						:color +argb_white)
					(ui-label preview_info (:text "Load an app to preview"
						:font *env_title_font*
						:min_width 480))))

			;Right: Property Editor
			(ui-flow props_panel (:flow_flags +flow_down
					:min_width 320
					:color *env_toolbar2_col*)
				(ui-title props_title (:text "Properties"))
				(ui-scroll props_scroll (+scroll_flag_vertical)
					(:min_width 320 :min_height 400)
					(ui-flow props_flow (:flow_flags +flow_down_fill)
						;Element info
						(ui-label elem_type (:text "Type: (none)"
							:font *env_toolbar_font*
							:min_width 300))
						(ui-label elem_name (:text "Name: (none)"
							:font *env_toolbar_font*
							:min_width 300))
						(ui-stroke _ (:color +argb_grey8 :min_width 300 :min_height 2))

						;Properties
						(ui-label _ (:text ":text" :font *env_toolbar_font* :min_width 300))
						(ui-textfield prop_text (:text "" :min_width 300))

						(ui-label _ (:text ":min_width" :font *env_toolbar_font* :min_width 300))
						(ui-textfield prop_width (:text "" :min_width 300))

						(ui-label _ (:text ":min_height" :font *env_toolbar_font* :min_width 300))
						(ui-textfield prop_height (:text "" :min_width 300))

						(ui-label _ (:text ":color" :font *env_toolbar_font* :min_width 300))
						(ui-textfield prop_color (:text "" :min_width 300))

						(ui-stroke _ (:color +argb_grey8 :min_width 300 :min_height 2))
						(ui-button apply_props_btn (:text "Apply Changes"
							:min_width 300 :color +argb_green)))))))

		;Bottom: Output/Console
		(ui-flow output_panel (:flow_flags +flow_down
				:min_height 150
				:color *env_toolbar2_col*)
			(ui-title output_title (:text "Output"))
			(ui-vdu output_vdu (:min_width 1180 :min_height 120
				:text "Ready. Load an app or create a new design."))))

;Connect toolbar buttons
(. new_btn :connect +event_new)
(. open_btn :connect +event_open)
(. save_btn :connect +event_save)
(. save_as_btn :connect +event_save_as)
(. load_path_btn :connect +event_load_file)

;Connect toggle buttons
(. error_btn :connect +event_toggle_state)
(. login_btn :connect +event_toggle_state)
(. debug_btn :connect +event_toggle_state)
(. empty_btn :connect +event_toggle_state)

;Connect property button
(. apply_props_btn :connect +event_apply_props)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output (msg)
	; (output msg) -> :nil
	(. output_vdu :write msg)
	(. output_vdu :write (ascii-char 10))
	(.-> output_vdu :layout :dirty))

(defun show-tree (elem indent vdu)
	(defq spaces "")
	(times indent (setq spaces (cat spaces "  ")))
	(. vdu :write spaces)
	(. vdu :write "├─ ")
	(. vdu :write (get :type elem))
	(. vdu :write " \"")
	(. vdu :write (get :name elem))
	(. vdu :write "\"")
	(. vdu :write (ascii-char 10))
	(each (lambda (child) (show-tree child (inc indent) vdu))
		(get :children elem)))

(defun display-tree (tree)
	(.-> tree_vdu :clear)
	(when tree
		(show-tree tree 0 tree_vdu))
	(.-> tree_vdu :layout :dirty))

(defun update-toggle-button (btn name)
	(defq value (designer-get-toggle name))
	(defq text (cat "[" name " = " (if value "true" "false") "]"))
	(defq color (if value +argb_green *env_toolbar_col*))
	(def (. btn :dirty) :text text :color color)
	(.-> btn :layout :dirty))

(defun update-property-panel (element)
	(if element
		(progn
			(def (. elem_type :dirty) :text (cat "Type: " (get :type element)))
			(def (. elem_name :dirty) :text (cat "Name: " (get :name element)))

			(defq props (get-element-properties element))
			(defq props-map (scatter (Lmap)))
			(each (lambda ((k v)) (. props-map :insert k v)) props)

			(def (. prop_text :dirty) :text (ifn (. props-map :find :text) ""))
			(def (. prop_width :dirty) :text (ifn (. props-map :find :min_width) ""))
			(def (. prop_height :dirty) :text (ifn (. props-map :find :min_height) ""))
			(def (. prop_color :dirty) :text (ifn (. props-map :find :color) ""))

			(.-> props_flow :layout :dirty_all))

		(progn
			(def (. elem_type :dirty) :text "Type: (none)")
			(def (. elem_name :dirty) :text "Name: (none)")
			(def (. prop_text :dirty) :text "")
			(def (. prop_width :dirty) :text "")
			(def (. prop_height :dirty) :text "")
			(def (. prop_color :dirty) :text "")
			(.-> props_flow :layout :dirty_all))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))

	(defq select (task-mboxes +select_size)
		running :t)

	(def *window* :tip_mbox (elem-get select +select_tip))

	(output "╔══════════════════════════════════════════════════════════╗")
	(output "║  ChrysaLisp Designer - Complete Edition                 ║")
	(output "╚══════════════════════════════════════════════════════════╝")
	(output "")
	(output "Features:")
	(output "  • State toggles - Toggle design-time states")
	(output "  • Property editor - Visual property editing")
	(output "  • File path input - Type or paste file paths")
	(output "  • UI tree view - See structure")
	(output "  • Preview area - See layout")
	(output "")
	(output "Try:")
	(output "  1. Enter path: apps/calculator/app.lisp")
	(output "  2. Click 'Load'")
	(output "  3. See UI tree and structure")
	(output "  4. Toggle states to see different views")
	(output "")

	(while running
		(defq msg (mail-read (elem-get select (defq idx (mail-select select))))
			id (getf msg +ev_msg_target_id))

		(case idx
			(+select_tip
				(if (defq view (. *window* :find_id (getf msg +mail_timeout_id)))
					(. view :show_tip)))

			(+select_main
				(cond
					((= id +event_close) (setq running :nil))

					((= id +event_min)
						(bind '(x y w h) (apply view-fit
							(cat (. *window* :get_pos) (. *window* :pref_size))))
						(. *window* :change_dirty x y w h))

					((= id +event_max)
						(bind '(x y w h) (apply view-fit
							(cat (. *window* :get_pos) '(1200 800))))
						(. *window* :change_dirty x y w h))

					;File operations
					((= id +event_new)
						(output "Creating new design...")
						(output "Feature not yet implemented"))

					((= id +event_load_file)
						(defq path (get :text file_path_input))
						(when (nempty? path)
							(output (cat "Loading: " path))
							(defq result (load-app-for-designer path))
							(if result
								(progn
									(bind '(tree original swapped) result)
									(setq *designer-tree* tree)
									(setq *designer-original-source* original)
									(setq *designer-file* path)
									(display-tree tree)
									(output "✓ Loaded successfully!")
									(output (cat "  Elements: " (str (count-tree tree)))))
								(output "✗ Failed to load"))))

					((= id +event_save)
						(if *designer-tree*
							(progn
								(output "Saving...")
								(defq code (designer-serialize-tree *designer-tree*))
								(output "Serialized code:")
								(output "══════════════════════════════════════════")
								(output code)
								(output "══════════════════════════════════════════"))
							(output "No design to save")))

					;Toggle state
					((= id +event_toggle_state)
						(defq source-id (getf msg +ev_msg_action_source_id))
						(defq button (. *window* :find_id source-id))
						(cond
							((eql button error_btn)
								(designer-toggle-flip "error_state")
								(update-toggle-button error_btn "error_state")
								(output "Toggled: error_state"))
							((eql button login_btn)
								(designer-toggle-flip "logged_in")
								(update-toggle-button login_btn "logged_in")
								(output "Toggled: logged_in"))
							((eql button debug_btn)
								(designer-toggle-flip "debug_mode")
								(update-toggle-button debug_btn "debug_mode")
								(output "Toggled: debug_mode"))
							((eql button empty_btn)
								(designer-toggle-flip "empty_data")
								(update-toggle-button empty_btn "empty_data")
								(output "Toggled: empty_data"))))

					;Apply properties
					((= id +event_apply_props)
						(when *selected-element*
							(output "Applying properties...")
							;TODO: Get values and apply
							(output "Feature coming soon!")))

					(:t (. *window* :event msg))))))

	(gui-sub-rpc *window*))

(defun count-tree (elem)
	(defq count 1)
	(each (lambda (child) (setq count (+ count (count-tree child))))
		(get :children elem))
	count)
