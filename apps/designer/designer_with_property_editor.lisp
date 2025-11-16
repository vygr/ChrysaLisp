(import "././login/env.inc")
(import "gui/lisp.inc")
(import "gui_designer/runtime.inc")
(import "gui_designer/property_editor.inc")
(import "lib/consts/chars.inc")

(enums +event 0
	(enum close max min)
	(enum select_element)
	(enum prop_text_changed)
	(enum prop_number_changed)
	(enum prop_color_changed)
	(enum prop_font_changed)
	(enum prop_bool_changed))

(enums +select 0
	(enum main tip))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sample UI Tree for Editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-sample-tree ()
	; (create-sample-tree) -> tree
	(defq root (scatter (Lmap)
		:id 1
		:type "ui-window"
		:name "*window*"
		:meta (scatter (Lmap))
		:props (list (list))
		:children (list)))

	(defq flow (scatter (Lmap)
		:id 2
		:type "ui-flow"
		:name "main_flow"
		:meta (scatter (Lmap))
		:props (list (list (:flow_flags "+flow_down_fill" :color "+argb_white")))
		:children (list)))
	(push (get :children root) flow)

	(defq title (scatter (Lmap)
		:id 3
		:type "ui-label"
		:name "title_label"
		:meta (scatter (Lmap))
		:props (list (list (:text "My Application" :font "*env_title_font*"
			:min_width "300")))
		:children (list)))
	(push (get :children flow) title)

	(defq btn1 (scatter (Lmap)
		:id 4
		:type "ui-button"
		:name "submit_btn"
		:meta (scatter (Lmap))
		:props (list (list (:text "Submit" :min_width "100" :color "+argb_green")))
		:children (list)))
	(push (get :children flow) btn1)

	(defq btn2 (scatter (Lmap)
		:id 5
		:type "ui-button"
		:name "cancel_btn"
		:meta (scatter (Lmap))
		:props (list (list (:text "Cancel" :min_width "100")))
		:children (list)))
	(push (get :children flow) btn2)

	root)

;Global state
(defq *design-tree* (create-sample-tree))
(defq *selected-element* :nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; UI Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ui-window *window* (:min_width 1000 :min_height 700)
	;Title bar
	(ui-title-bar _ "Designer with Property Editor" (0xea19 0xea1b 0xea1a) +event_close)

	;Main layout
	(ui-flow main_area (:flow_flags +flow_right_fill)

		;Left: Element Tree
		(ui-flow tree_panel (:flow_flags +flow_down
				:min_width 250
				:color *env_toolbar2_col*)
			(ui-title tree_title (:text "UI Tree"))

			;Element list (clickable)
			(ui-scroll tree_scroll (+scroll_flag_vertical)
				(:min_width 250 :min_height 400)
				(ui-flow tree_flow (:flow_flags +flow_down_fill)
					;Buttons for each element
					(ui-button elem_window (:text "ui-window *window*"
						:min_width 230))
					(ui-button elem_flow (:text "  ui-flow main_flow"
						:min_width 230))
					(ui-button elem_label (:text "    ui-label title_label"
						:min_width 230))
					(ui-button elem_btn1 (:text "    ui-button submit_btn"
						:min_width 230))
					(ui-button elem_btn2 (:text "    ui-button cancel_btn"
						:min_width 230)))))

		;Center: Preview
		(ui-flow preview_panel (:flow_flags +flow_down_fill
				:color +argb_grey10)
			(ui-title preview_title (:text "Preview"))
			(ui-backdrop preview_area (:min_width 400 :min_height 500
					:color +argb_white)
				;Preview content would go here
				(ui-label preview_info (:text "Select an element to edit"
					:min_width 380))))

		;Right: Property Editor
		(ui-flow props_panel (:flow_flags +flow_down
				:min_width 300
				:color *env_toolbar2_col*)
			(ui-title props_title (:text "Properties"))

			;Property editor scroll area
			(ui-scroll props_scroll (+scroll_flag_vertical)
				(:min_width 300 :min_height 500)
				(ui-flow props_flow (:flow_flags +flow_down_fill)

					;Element info
					(ui-label elem_type (:text "Type: (none)"
						:font *env_toolbar_font*
						:min_width 280))
					(ui-label elem_name (:text "Name: (none)"
						:font *env_toolbar_font*
						:min_width 280))

					;Separator
					(ui-stroke sep1 (:color +argb_grey8 :min_width 280 :min_height 2))

					;:text property (most common)
					(ui-label text_label (:text ":text"
						:font *env_toolbar_font*
						:min_width 280))
					(ui-textfield text_input (:text ""
						:min_width 280))

					;:min_width property
					(ui-label width_label (:text ":min_width"
						:font *env_toolbar_font*
						:min_width 280))
					(ui-textfield width_input (:text ""
						:min_width 280))

					;:min_height property
					(ui-label height_label (:text ":min_height"
						:font *env_toolbar_font*
						:min_width 280))
					(ui-textfield height_input (:text ""
						:min_width 280))

					;:color property (dropdown)
					(ui-label color_label (:text ":color"
						:font *env_toolbar_font*
						:min_width 280))
					(ui-flow color_row (:flow_flags +flow_right)
						(ui-textfield color_input (:text ""
							:min_width 200))
						(ui-backdrop color_preview (:min_width 60 :min_height 30
							:color +argb_white)))

					;:font property
					(ui-label font_label (:text ":font"
						:font *env_toolbar_font*
						:min_width 280))
					(ui-textfield font_input (:text ""
						:min_width 280))

					;:border property
					(ui-label border_label (:text ":border"
						:font *env_toolbar_font*
						:min_width 280))
					(ui-textfield border_input (:text ""
						:min_width 280))

					;Apply button
					(ui-stroke sep2 (:color +argb_grey8 :min_width 280 :min_height 2))
					(ui-button apply_btn (:text "Apply Changes"
						:min_width 280
						:color +argb_green)))))))

;Connect element selection buttons
(. elem_window :connect +event_select_element)
(. elem_flow :connect +event_select_element)
(. elem_label :connect +event_select_element)
(. elem_btn1 :connect +event_select_element)
(. elem_btn2 :connect +event_select_element)

;Connect property input fields
(. text_input :connect +event_prop_text_changed)
(. width_input :connect +event_prop_number_changed)
(. height_input :connect +event_prop_number_changed)
(. color_input :connect +event_prop_color_changed)
(. font_input :connect +event_prop_font_changed)
(. border_input :connect +event_prop_number_changed)
(. apply_btn :connect +event_prop_text_changed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Element Selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun select-element-by-name (tree name)
	; (select-element-by-name tree name) -> element | :nil
	(defq result :nil)
	(designer-walk-tree tree (lambda (elem)
		(when (eql (get :name elem) name)
			(setq result elem))))
	result)

(defun update-property-panel (element)
	; (update-property-panel element) -> :nil
	(if element
		(progn
			;Update element info
			(def (. elem_type :dirty) :text (cat "Type: " (get :type element)))
			(def (. elem_name :dirty) :text (cat "Name: " (get :name element)))

			;Get properties
			(defq props (get-element-properties element))
			(defq props-map (scatter (Lmap)))
			(each (lambda ((k v)) (. props-map :insert k v)) props)

			;Update text field
			(defq text-val (. props-map :find :text))
			(def (. text_input :dirty) :text (ifn text-val ""))

			;Update width
			(defq width-val (. props-map :find :min_width))
			(def (. width_input :dirty) :text (ifn width-val ""))

			;Update height
			(defq height-val (. props-map :find :min_height))
			(def (. height_input :dirty) :text (ifn height-val ""))

			;Update color
			(defq color-val (. props-map :find :color))
			(def (. color_input :dirty) :text (ifn color-val ""))
			;Update color preview
			(when color-val
				(defq color-num (if (str? color-val)
					;Try to parse color constant
					(cond
						((eql color-val "+argb_white") +argb_white)
						((eql color-val "+argb_black") +argb_black)
						((eql color-val "+argb_red") +argb_red)
						((eql color-val "+argb_green") +argb_green)
						((eql color-val "+argb_blue") +argb_blue)
						((eql color-val "+argb_grey10") +argb_grey10)
						((eql color-val "+argb_grey20") +argb_grey20)
						(:t +argb_white))
					color-val))
				(def (. color_preview :dirty) :color color-num))

			;Update font
			(defq font-val (. props-map :find :font))
			(def (. font_input :dirty) :text (ifn font-val ""))

			;Update border
			(defq border-val (. props-map :find :border))
			(def (. border_input :dirty) :text (ifn border-val ""))

			;Relayout
			(.-> props_flow :layout :dirty_all))

		;No selection
		(progn
			(def (. elem_type :dirty) :text "Type: (none)")
			(def (. elem_name :dirty) :text "Name: (none)")
			(def (. text_input :dirty) :text "")
			(def (. width_input :dirty) :text "")
			(def (. height_input :dirty) :text "")
			(def (. color_input :dirty) :text "")
			(def (. font_input :dirty) :text "")
			(def (. border_input :dirty) :text "")
			(.-> props_flow :layout :dirty_all))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Property Application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun apply-property-changes ()
	; (apply-property-changes) -> :nil
	(when *selected-element*
		(print "Applying property changes to: " (get :name *selected-element*))

		;Get values from inputs
		(defq text-val (get :text text_input))
		(defq width-val (get :text width_input))
		(defq height-val (get :text height_input))
		(defq color-val (get :text color_input))
		(defq font-val (get :text font_input))
		(defq border-val (get :text border_input))

		;Apply non-empty values
		(when (nempty? text-val)
			(designer-set-property *selected-element* :text text-val)
			(print "  Set :text = " text-val))

		(when (nempty? width-val)
			(designer-set-property *selected-element* :min_width width-val)
			(print "  Set :min_width = " width-val))

		(when (nempty? height-val)
			(designer-set-property *selected-element* :min_height height-val)
			(print "  Set :min_height = " height-val))

		(when (nempty? color-val)
			(designer-set-property *selected-element* :color color-val)
			(print "  Set :color = " color-val))

		(when (nempty? font-val)
			(designer-set-property *selected-element* :font font-val)
			(print "  Set :font = " font-val))

		(when (nempty? border-val)
			(designer-set-property *selected-element* :border border-val)
			(print "  Set :border = " border-val))

		(print "Properties updated in tree model!")
		(print "")

		;Show updated tree
		(import "gui_designer/serialize.inc")
		(print "Updated serialized code:")
		(print "══════════════════════════════════════════════════════════")
		(print (designer-serialize-tree *design-tree*))
		(print "══════════════════════════════════════════════════════════")
		(print "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
	;Initialize
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))

	(defq select (task-mboxes +select_size)
		running :t)

	(def *window* :tip_mbox (elem-get select +select_tip))

	(print "")
	(print "╔══════════════════════════════════════════════════════════╗")
	(print "║  Property Editor Demo                                    ║")
	(print "╚══════════════════════════════════════════════════════════╝")
	(print "")
	(print "Click elements in the tree to select them")
	(print "Edit properties in the right panel")
	(print "Click 'Apply Changes' to update the model")
	(print "")

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
							(cat (. *window* :get_pos) '(1000 700))))
						(. *window* :change_dirty x y w h))

					;Element selection
					((= id +event_select_element)
						(defq source-id (getf msg +ev_msg_action_source_id))
						(defq button (. *window* :find_id source-id))
						(cond
							((eql button elem_window)
								(setq *selected-element*
									(select-element-by-name *design-tree* "*window*")))
							((eql button elem_flow)
								(setq *selected-element*
									(select-element-by-name *design-tree* "main_flow")))
							((eql button elem_label)
								(setq *selected-element*
									(select-element-by-name *design-tree* "title_label")))
							((eql button elem_btn1)
								(setq *selected-element*
									(select-element-by-name *design-tree* "submit_btn")))
							((eql button elem_btn2)
								(setq *selected-element*
									(select-element-by-name *design-tree* "cancel_btn"))))

						(when *selected-element*
							(print "Selected: " (get :type *selected-element*)
								" \"" (get :name *selected-element*) "\"")
							(update-property-panel *selected-element*)))

					;Property changes
					((or (= id +event_prop_text_changed)
						(= id +event_prop_number_changed)
						(= id +event_prop_color_changed)
						(= id +event_prop_font_changed)
						(= id +event_prop_bool_changed))
						;Apply button clicked
						(apply-property-changes))

					;Default event handling
					(:t (. *window* :event msg))))))

	;Cleanup
	(gui-sub-rpc *window*))
