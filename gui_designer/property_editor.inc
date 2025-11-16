;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Visual Property Editor
; Displays and edits properties of selected element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui_designer/runtime.inc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Property Metadata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +property-types (scatter (Lmap)
	;Text properties
	:text "text"
	:tip_text "text"
	:name "text"

	;Number properties
	:min_width "number"
	:min_height "number"
	:border "number"
	:grid_width "number"
	:grid_height "number"

	;Color properties
	:color "color"
	:ink_color "color"
	:hint_color "color"

	;Font properties
	:font "font"

	;Flag properties
	:flow_flags "flags"
	:disabled "boolean"))

(defq +common-colors (list
	(list "+argb_white" +argb_white "White")
	(list "+argb_black" +argb_black "Black")
	(list "+argb_red" +argb_red "Red")
	(list "+argb_green" +argb_green "Green")
	(list "+argb_blue" +argb_blue "Blue")
	(list "+argb_yellow" +argb_yellow "Yellow")
	(list "+argb_grey4" +argb_grey4 "Grey4")
	(list "+argb_grey8" +argb_grey8 "Grey8")
	(list "+argb_grey12" +argb_grey12 "Grey12")
	(list "+argb_grey20" +argb_grey20 "Grey20")))

(defq +common-fonts (list
	"*env_window_font*"
	"*env_title_font*"
	"*env_toolbar_font*"
	"*env_terminal_font*"))

(defq +flow-flags (list
	(list "+flow_down" "+flow_down" "Down")
	(list "+flow_up" "+flow_up" "Up")
	(list "+flow_right" "+flow_right" "Right")
	(list "+flow_left" "+flow_left" "Left")
	(list "+flow_down_fill" "+flow_down_fill" "Down Fill")
	(list "+flow_up_fill" "+flow_up_fill" "Up Fill")
	(list "+flow_right_fill" "+flow_right_fill" "Right Fill")
	(list "+flow_left_fill" "+flow_left_fill" "Left Fill")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Property Extraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-element-properties (element)
	; (get-element-properties element) -> ((prop-name prop-value) ...)
	; Extracts all properties from element
	(defq props (first (get :props element)))
	(if props
		(map (lambda (prop)
			(if (lst? prop)
				(if (>= (length prop) 2)
					(list (elem-get prop 0) (elem-get prop 1))
					(list (elem-get prop 0) :nil))
				(list prop :nil)))
			props)
		(list)))

(defun get-property-type (prop-name)
	; (get-property-type prop-name) -> "text" | "number" | "color" | ...
	(defq type (. +property-types :find prop-name))
	(ifn type "text"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Property Value Formatting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-property-value (value type)
	; (format-property-value value type) -> str
	(case type
		("text" (if (str? value) value (str value)))
		("number" (str value))
		("color"
			;Find color name if it's a known constant
			(defq found :nil)
			(some (lambda ((name val label))
				(when (= value val)
					(setq found name))
				found)
				+common-colors)
			(ifn found (str value)))
		("font" (str value))
		("flags" (str value))
		("boolean" (if value "true" "false"))
		(:t (str value))))

(defun parse-property-value (str-value type)
	; (parse-property-value str-value type) -> value
	(case type
		("text" str-value)
		("number" (str-as-num str-value))
		("color"
			;Check if it's a constant name
			(defq found :nil)
			(some (lambda ((name val label))
				(when (eql str-value name)
					(setq found val))
				found)
				+common-colors)
			(ifn found (str-as-num str-value)))
		("font" str-value)
		("flags" str-value)
		("boolean" (or (eql str-value "true") (eql str-value ":t")))
		(:t str-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Property Editor UI Builder
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-property-editor-row (prop-name prop-value prop-type)
	; (create-property-editor-row prop-name prop-value prop-type) -> (label-view input-view)
	; Creates a row for the property editor
	; Returns list of (label input) views for later reference

	;Format the property name for display
	(defq display-name (slice (str prop-name) 1))  ;Remove leading :

	;Create label
	(defq label-text (cat display-name ":"))

	;Create appropriate input widget based on type
	(defq input-widget
		(case prop-type
			("text"
				;Text field
				(cat "textfield for " display-name))

			("number"
				;Number spinner (or text field)
				(cat "number for " display-name))

			("color"
				;Color dropdown
				(cat "color picker for " display-name))

			("font"
				;Font dropdown
				(cat "font picker for " display-name))

			("boolean"
				;Checkbox
				(cat "checkbox for " display-name))

			("flags"
				;Flags dropdown
				(cat "flags picker for " display-name))

			(:t
				;Default text field
				(cat "text for " display-name))))

	(list label-text input-widget))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Property Editor State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *property-editor-state* (scatter (Lmap)
	:element :nil           ;Currently edited element
	:property-widgets (scatter (Lmap))  ;prop-name -> widget
	:updating :nil))        ;Flag to prevent feedback loops

(defun property-editor-set-element (element)
	; (property-editor-set-element element) -> :nil
	; Sets the element being edited
	(. *property-editor-state* :insert :element element)
	:nil)

(defun property-editor-get-element ()
	; (property-editor-get-element) -> element | :nil
	(. *property-editor-state* :find :element))

(defun property-editor-register-widget (prop-name widget)
	; (property-editor-register-widget prop-name widget) -> :nil
	; Registers a widget for property editing
	(defq widgets (. *property-editor-state* :find :property-widgets))
	(. widgets :insert prop-name widget)
	:nil)

(defun property-editor-get-widget (prop-name)
	; (property-editor-get-widget prop-name) -> widget | :nil
	(defq widgets (. *property-editor-state* :find :property-widgets))
	(. widgets :find prop-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Property Change Handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun property-editor-handle-change (prop-name new-value-str)
	; (property-editor-handle-change prop-name new-value-str) -> :t | :nil
	; Called when user changes a property value

	(when (. *property-editor-state* :find :updating)
		;Prevent feedback loop
		(return :nil))

	(defq element (property-editor-get-element))
	(unless element
		(return :nil))

	;Parse value based on type
	(defq prop-type (get-property-type prop-name))
	(defq new-value (parse-property-value new-value-str prop-type))

	(print "Property change: " prop-name " = " (str new-value))

	;Update the element in the tree
	(designer-set-property element prop-name new-value)

	:t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Property Editor Refresh
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun property-editor-refresh ()
	; (property-editor-refresh) -> :nil
	; Refreshes property editor to show current element's properties

	;Set updating flag to prevent feedback
	(. *property-editor-state* :insert :updating :t)

	(defq element (property-editor-get-element))

	(if element
		(progn
			;Get all properties
			(defq props (get-element-properties element))

			;Update each widget
			(each (lambda ((prop-name prop-value))
				(when (defq widget (property-editor-get-widget prop-name))
					;Update widget to show current value
					(defq prop-type (get-property-type prop-name))
					(defq display-value (format-property-value prop-value prop-type))
					;TODO: Actually update widget text/value
					:nil))
				props))

		;No element selected
		(print "No element selected"))

	;Clear updating flag
	(. *property-editor-state* :insert :updating :nil)
	:nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Common Properties List
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-common-properties-for-type (element-type)
	; (get-common-properties-for-type element-type) -> (prop-name ...)
	; Returns list of common properties for this element type

	(defq base-props '(:min_width :min_height))

	(cond
		;Text-containing widgets
		((or (find "button" element-type)
			(find "label" element-type)
			(find "text" element-type))
			(cat base-props '(:text :font :ink_color)))

		;Containers
		((or (find "flow" element-type)
			(find "grid" element-type)
			(find "window" element-type))
			(cat base-props '(:color :border)))

		;Grid specific
		((find "grid" element-type)
			(cat base-props '(:grid_width :grid_height :color)))

		;Flow specific
		((find "flow" element-type)
			(cat base-props '(:flow_flags :color)))

		;Default
		(:t base-props)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Property Editor Display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun display-property-info (element)
	; (display-property-info element) -> :nil
	; Displays property information (for text-based preview)

	(print "")
	(print "╔══════════════════════════════════════════════════════════╗")
	(print "║  Property Editor                                         ║")
	(print "╚══════════════════════════════════════════════════════════╝")
	(print "")

	(if element
		(progn
			(print "Element: " (get :type element) " \"" (get :name element) "\"")
			(print "")
			(print "Properties:")
			(print "──────────────────────────────────────────────────────────")

			(defq props (get-element-properties element))

			(each (lambda ((prop-name prop-value))
				(defq prop-type (get-property-type prop-name))
				(defq display-value (format-property-value prop-value prop-type))
				(print "  " prop-name)
				(print "    Type: " prop-type)
				(print "    Value: " display-value)
				(print ""))
				props)

			;Show available properties
			(print "Available properties for this type:")
			(defq available (get-common-properties-for-type (get :type element)))
			(each (lambda (prop)
				(print "  " prop " (" (get-property-type prop) ")"))
				available)
			(print ""))

		(print "No element selected")
		(print ""))

	:nil)
