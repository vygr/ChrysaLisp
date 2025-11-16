;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Property Editor - List-Based
; Visual property editing with type-specific inputs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Property Type System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +property-types (list
	; (keyword-string type description)
	(list "text" "string" "Text content")
	(list "min_width" "number" "Minimum width")
	(list "min_height" "number" "Minimum height")
	(list "max_width" "number" "Maximum width")
	(list "max_height" "number" "Maximum height")
	(list "color" "color" "Color value")
	(list "ink_color" "color" "Ink color")
	(list "hint_color" "color" "Hint color")
	(list "border" "number" "Border size")
	(list "shadow" "number" "Shadow size")
	(list "flow_flags" "flags" "Flow flags")
	(list "font" "symbol" "Font reference")
	(list "disabled" "boolean" "Disabled state")))

(defun get-property-type (prop-sym)
	; Get the type for a property keyword
	; Convert keyword to string by removing the colon
	(defq prop-str (str prop-sym))
	(defq prop-str (slice prop-str 1 (length prop-str)))  ; Remove leading :

	(defq result "unknown")
	(defq i 0)
	(defq len (length +property-types))
	(while (< i len)
		(defq entry (elem-get +property-types i))
		(when (eql (elem-get entry 0) prop-str)
			(setq result (elem-get entry 1)))
		(setq i (+ i 1)))
	result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Property Access Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-element-properties (elem)
	; Get properties from element as list of (keyword value) pairs
	(defq props (designer-get-props elem))
	(if (and props (nempty? props))
		; Convert flat list (:key1 val1 :key2 val2) to pairs ((key1 val1) (key2 val2))
		(progn
			(defq pairs (list))
			(defq i 0)
			(while (< i (length props))
				(push pairs (list (elem-get props i) (elem-get props (+ i 1))))
				(setq i (+ i 2)))
			pairs)
		(list)))

(defun set-element-property (elem keyword value)
	; Set a property value in the element
	(defq props (designer-get-props elem))
	(defq new-props (list))

	; If props exist, update or add the property
	(if (and props (nempty? props))
		(progn
			(defq found :nil)
			(defq i 0)
			(while (< i (length props))
				(defq key (elem-get props i))
				(defq val (elem-get props (+ i 1)))
				(if (eql key keyword)
					(progn
						(push new-props keyword)
						(push new-props value)
						(setq found :t))
					(progn
						(push new-props key)
						(push new-props val)))
				(setq i (+ i 2)))
			; If not found, add it
			(when (not found)
				(push new-props keyword)
				(push new-props value)))
		; No props exist, create new list
		(progn
			(push new-props keyword)
			(push new-props value)))

	; Update element
	(elem-set elem 4 new-props))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Property Display Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-property-value (value)
	; Format a property value for display
	(cond
		((str? value) (cat "\"" value "\""))
		((num? value) (str value))
		(:t (str value))))

(defun parse-property-value (value-str type)
	; Parse a string back to proper type
	; For now, just return as-is - proper parsing would need eval
	value-str)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Property Editor UI Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun show-element-properties (elem)
	; Display properties for an element
	(print "Properties for: " (designer-get-name elem))
	(print "Type: " (designer-get-type elem))
	(print "Constructor: " (designer-get-constructor elem))
	(print "")

	(defq props (get-element-properties elem))
	(if (nempty? props)
		(progn
			(print "Properties:")
			(defq i 0)
			(while (< i (length props))
				(defq pair (elem-get props i))
				(defq key (elem-get pair 0))
				(defq val (elem-get pair 1))
				(defq type (get-property-type key))
				(print "  " key " = " (format-property-value val) " (" type ")")
				(setq i (+ i 1))))
		(print "  (no properties)")))

(defun edit-property-interactive (elem)
	; Interactive property editing (for testing)
	(show-element-properties elem)
	(print "")
	(print "To edit: (set-element-property elem :keyword value)")
	elem)

(print "Property editor (list-based) loaded")
