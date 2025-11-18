;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; State Toggles - List-Based
; Design-time state preview (disabled, selected, hidden, etc.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; State Storage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Global state tracking: list of (elem-id state-keyword value) entries
; Example: (123 :disabled :t) (123 :selected :nil) (456 :disabled :t)
(defq +design-states (list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; State Access Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-element-state (elem state-key)
	; Get the value of a design-time state for an element
	; Returns :t, :nil, or :nil if not found
	(defq elem-id (designer-get-id elem))
	(defq result :nil)
	(defq i 0)
	(while (< i (length +design-states))
		(defq entry (elem-get +design-states i))
		(defq entry-id (elem-get entry 0))
		(defq entry-key (elem-get entry 1))
		(defq entry-val (elem-get entry 2))
		(when (and (= entry-id elem-id) (eql entry-key state-key))
			(setq result entry-val))
		(setq i (+ i 1)))
	result)

(defun set-element-state (elem state-key value)
	; Set a design-time state for an element
	; Creates new entry if not found, updates if found
	(defq elem-id (designer-get-id elem))
	(defq found :nil)
	(defq new-states (list))

	; Scan existing states
	(defq i 0)
	(while (< i (length +design-states))
		(defq entry (elem-get +design-states i))
		(defq entry-id (elem-get entry 0))
		(defq entry-key (elem-get entry 1))
		(defq entry-val (elem-get entry 2))

		; If this is the entry we want to update
		(if (and (= entry-id elem-id) (eql entry-key state-key))
			(progn
				(push new-states (list elem-id state-key value))
				(setq found :t))
			; Keep other entries unchanged
			(push new-states entry))
		(setq i (+ i 1)))

	; If not found, add new entry
	(when (not found)
		(push new-states (list elem-id state-key value)))

	; Update global state list
	(setq +design-states new-states))

(defun toggle-element-state (elem state-key)
	; Toggle a boolean state on/off
	(defq current (get-element-state elem state-key))
	(defq new-val (if (eql current :t) :nil :t))
	(set-element-state elem state-key new-val)
	new-val)

(defun clear-element-states (elem)
	; Remove all design-time states for an element
	(defq elem-id (designer-get-id elem))
	(defq new-states (list))

	(defq i 0)
	(while (< i (length +design-states))
		(defq entry (elem-get +design-states i))
		(defq entry-id (elem-get entry 0))
		; Keep entries for other elements
		(when (not (= entry-id elem-id))
			(push new-states entry))
		(setq i (+ i 1)))

	(setq +design-states new-states))

(defun clear-all-states ()
	; Clear all design-time states
	(setq +design-states (list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; State Query Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-all-element-states (elem)
	; Get all design-time states for an element as list of (state-key value) pairs
	(defq elem-id (designer-get-id elem))
	(defq result (list))

	(defq i 0)
	(while (< i (length +design-states))
		(defq entry (elem-get +design-states i))
		(defq entry-id (elem-get entry 0))
		(defq entry-key (elem-get entry 1))
		(defq entry-val (elem-get entry 2))

		(when (= entry-id elem-id)
			(push result (list entry-key entry-val)))
		(setq i (+ i 1)))

	result)

(defun is-state-enabled? (elem state-key)
	; Check if a state is enabled (returns :t or :nil)
	(eql (get-element-state elem state-key) :t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Common State Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun toggle-disabled (elem)
	; Toggle disabled state
	(toggle-element-state elem :disabled))

(defun toggle-selected (elem)
	; Toggle selected state
	(toggle-element-state elem :selected))

(defun toggle-hidden (elem)
	; Toggle hidden state
	(toggle-element-state elem :hidden))

(defun is-disabled? (elem)
	; Check if element is in disabled state
	(is-state-enabled? elem :disabled))

(defun is-selected? (elem)
	; Check if element is in selected state
	(is-state-enabled? elem :selected))

(defun is-hidden? (elem)
	; Check if element is in hidden state
	(is-state-enabled? elem :hidden))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; State Display Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun show-element-states (elem)
	; Display all design-time states for an element
	(print "Design States for: " (designer-get-name elem))
	(defq states (get-all-element-states elem))

	(if (nempty? states)
		(progn
			(defq i 0)
			(while (< i (length states))
				(defq pair (elem-get states i))
				(defq key (elem-get pair 0))
				(defq val (elem-get pair 1))
				(print "  " key " = " val)
				(setq i (+ i 1))))
		(print "  (no design states)")))

(defun show-all-states ()
	; Display all design-time states in the system
	(print "All Design States (" (length +design-states) " entries):")
	(if (nempty? +design-states)
		(progn
			(defq i 0)
			(while (< i (length +design-states))
				(defq entry (elem-get +design-states i))
				(defq elem-id (elem-get entry 0))
				(defq state-key (elem-get entry 1))
				(defq state-val (elem-get entry 2))
				(print "  Element " elem-id ": " state-key " = " state-val)
				(setq i (+ i 1))))
		(print "  (no states)")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; State Visual Preview Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-state-visual-hint (elem)
	; Get a visual hint string for element's current design states
	; Returns string like "[D][S]" for disabled+selected
	(defq hints "")

	(when (is-disabled? elem)
		(setq hints (cat hints "[D]")))
	(when (is-selected? elem)
		(setq hints (cat hints "[S]")))
	(when (is-hidden? elem)
		(setq hints (cat hints "[H]")))

	hints)

(defun format-element-with-state (elem)
	; Format element name with state visual hints
	(defq name (designer-get-name elem))
	(defq hint (get-state-visual-hint elem))
	(if (nempty? hint)
		(cat name " " hint)
		name))

(print "State toggles (list-based) loaded")
