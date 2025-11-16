;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Designer State Toggles
; Allows viewing different UI states in design mode without running the app
; Inspired by Paul Hammant's AngularJS StoryNavigator showcase
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui_designer/runtime.inc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; State Toggle System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Global state toggles (like design-time variables)
(defq *designer-state-toggles* (scatter (Lmap)))
(defq *designer-state-history* (list))

(defun designer-register-toggle (name default-value description)
	; (designer-register-toggle name default-value description) -> :nil
	; Registers a state toggle for design-time viewing
	(. *designer-state-toggles* :insert name (scatter (Lmap)
		:value default-value
		:description description
		:original default-value))
	:nil)

(defun designer-set-toggle (name value)
	; (designer-set-toggle name value) -> :nil
	; Sets a toggle value and records in history
	(when (defq toggle (. *designer-state-toggles* :find name))
		(push *designer-state-history* (list name (get :value toggle) value))
		(. toggle :insert :value value))
	:nil)

(defun designer-get-toggle (name)
	; (designer-get-toggle name) -> value | :nil
	(when (defq toggle (. *designer-state-toggles* :find name))
		(get :value toggle)))

(defun designer-toggle-flip (name)
	; (designer-toggle-flip name) -> new-value
	; Flips a boolean toggle
	(when (defq toggle (. *designer-state-toggles* :find name))
		(defq current (get :value toggle))
		(defq new-value (not current))
		(designer-set-toggle name new-value)
		new-value))

(defun designer-list-toggles ()
	; (designer-list-toggles) -> :nil
	; Lists all registered toggles
	(print "")
	(print "Design-Time State Toggles:")
	(print "══════════════════════════════════════════════════════")
	(each (lambda ((name toggle))
		(print "  [" (if (get :value toggle) "✓" " ") "] "
			name " = " (str (get :value toggle)))
		(print "      " (get :description toggle)))
		(tolist *designer-state-toggles*))
	(print ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Conditional Element Visibility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun designer-mark-conditional (element condition-expr description)
	; (designer-mark-conditional element condition-expr description) -> :nil
	; Marks an element as conditionally visible based on state toggles
	; condition-expr is a string like "error_state" or "!logged_in"
	(when element
		(. (get :meta element) :insert :conditional :t)
		(. (get :meta element) :insert :condition condition-expr)
		(. (get :meta element) :insert :condition_desc description))
	:nil)

(defun designer-evaluate-condition (condition-expr)
	; (designer-evaluate-condition condition-expr) -> :t | :nil
	; Evaluates a condition expression against current toggle state
	(defq negated :nil)
	(when (starts-with "!" condition-expr)
		(setq negated :t
			condition-expr (slice condition-expr 1)))
	(defq value (designer-get-toggle condition-expr))
	(if negated (not value) value))

(defun designer-filter-by-state (tree)
	; (designer-filter-by-state tree) -> filtered-tree
	; Filters the tree based on current toggle states
	; Elements marked as conditional are shown/hidden based on toggles
	(defq filtered (scatter (Lmap)))
	(each (lambda ((key val))
		(. filtered :insert key val))
		(tolist tree))

	;Check if this element should be visible
	(defq meta (get :meta tree))
	(when (and meta (get :conditional meta))
		(defq condition (get :condition meta))
		(unless (designer-evaluate-condition condition)
			;Element should be hidden - mark it
			(. filtered :insert :designer_hidden :t)))

	;Recursively filter children
	(defq children (get :children tree))
	(defq filtered-children (list))
	(each (lambda (child)
		(defq filtered-child (designer-filter-by-state child))
		(unless (get :designer_hidden filtered-child)
			(push filtered-children filtered-child)))
		children)
	(. filtered :insert :children filtered-children)

	filtered)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Designer Comments (Inline Annotations)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun designer-add-comment (element comment)
	; (designer-add-comment element comment) -> :nil
	; Adds a designer comment to an element (visible only in designer)
	(when element
		(. (get :meta element) :insert :designer_comment comment))
	:nil)

(defun designer-get-comments (tree)
	; (designer-get-comments tree) -> ((element comment) ...)
	; Extracts all designer comments from tree
	(defq comments (list))
	(designer-walk-tree tree (lambda (elem)
		(when (defq comment (get :designer_comment (get :meta elem)))
			(push comments (list elem comment)))))
	comments)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Serialization with State Info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun serialize-with-state-comments (tree)
	; (serialize-with-state-comments tree) -> str
	; Serializes tree with designer comments as actual code comments
	(import "gui_designer/serialize.inc")

	(defun serialize-elem (elem indent)
		(defq result "")

		;Add designer comment if present
		(when (defq comment (get :designer_comment (get :meta elem)))
			(defq spaces "")
			(times indent (setq spaces (cat spaces (ascii-char 9))))
			(setq result (cat result spaces ";Designer: " comment (ascii-char 10))))

		;Add conditional info if present
		(when (and (get :conditional (get :meta elem))
				(get :condition (get :meta elem)))
			(defq spaces "")
			(times indent (setq spaces (cat spaces (ascii-char 9))))
			(setq result (cat result spaces ";Condition: " (get :condition (get :meta elem))
				" - " (get :condition_desc (get :meta elem)) (ascii-char 10))))

		;Serialize the element itself
		(setq result (cat result (designer-serialize-element elem indent)))

		result)

	(serialize-elem tree 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Example Usage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun demo-state-toggles ()
	; (demo-state-toggles) -> :nil
	(print "")
	(print "╔══════════════════════════════════════════════════════════╗")
	(print "║  Designer State Toggles Demo                            ║")
	(print "╚══════════════════════════════════════════════════════════╝")
	(print "")

	;Register some toggles (like design-time variables)
	(designer-register-toggle "error_state" :nil "Show error messages")
	(designer-register-toggle "logged_in" :t "User is logged in")
	(designer-register-toggle "dark_mode" :nil "Dark theme enabled")
	(designer-register-toggle "debug_mode" :nil "Show debug info")

	(print "Registered toggles:")
	(designer-list-toggles)

	;Create a mock UI tree with conditional elements
	(print "Creating UI with conditional elements...")
	(print "")

	(defq root (scatter (Lmap)
		:type "ui-window"
		:name "*window*"
		:meta (scatter (Lmap))
		:children (list)))

	;Main content
	(defq main-flow (scatter (Lmap)
		:type "ui-flow"
		:name "main_flow"
		:meta (scatter (Lmap))
		:children (list)))
	(push (get :children root) main-flow)

	;Normal label (always visible)
	(defq normal-label (scatter (Lmap)
		:type "ui-label"
		:name "title"
		:meta (scatter (Lmap))
		:props (list (list (:text "My App")))
		:children (list)))
	(designer-add-comment normal-label "Main title - always visible")
	(push (get :children main-flow) normal-label)

	;Error message (conditional on error_state)
	(defq error-label (scatter (Lmap)
		:type "ui-label"
		:name "error_msg"
		:meta (scatter (Lmap))
		:props (list (list (:text "ERROR!" :color "+argb_red")))
		:children (list)))
	(designer-mark-conditional error-label "error_state" "Shown when error occurs")
	(designer-add-comment error-label "Error state indicator")
	(push (get :children main-flow) error-label)

	;Login button (conditional on NOT logged_in)
	(defq login-btn (scatter (Lmap)
		:type "ui-button"
		:name "login_btn"
		:meta (scatter (Lmap))
		:props (list (list (:text "Login")))
		:children (list)))
	(designer-mark-conditional login-btn "!logged_in" "Shown when not logged in")
	(push (get :children main-flow) login-btn)

	;Logout button (conditional on logged_in)
	(defq logout-btn (scatter (Lmap)
		:type "ui-button"
		:name "logout_btn"
		:meta (scatter (Lmap))
		:props (list (list (:text "Logout")))
		:children (list)))
	(designer-mark-conditional logout-btn "logged_in" "Shown when logged in")
	(push (get :children main-flow) logout-btn)

	;Debug panel (conditional on debug_mode)
	(defq debug-panel (scatter (Lmap)
		:type "ui-vdu"
		:name "debug_panel"
		:meta (scatter (Lmap))
		:props (list (list (:text "Debug: ...")))
		:children (list)))
	(designer-mark-conditional debug-panel "debug_mode" "Debug information panel")
	(push (get :children main-flow) debug-panel)

	;Show current state
	(print "Current toggle states:")
	(designer-list-toggles)

	(print "Filtering tree by current state...")
	(defq filtered (designer-filter-by-state root))

	(defun count-visible (tree)
		(defq count 1)
		(each (lambda (child) (setq count (+ count (count-visible child))))
			(get :children tree))
		count)

	(print "  Total elements: 6")
	(print "  Visible elements: " (str (count-visible filtered)))
	(print "")

	;Toggle error state on
	(print "Toggling error_state ON...")
	(designer-set-toggle "error_state" :t)
	(defq filtered (designer-filter-by-state root))
	(print "  Visible elements: " (str (count-visible filtered)))
	(print "")

	;Toggle logged_in off
	(print "Toggling logged_in OFF...")
	(designer-set-toggle "logged_in" :nil)
	(defq filtered (designer-filter-by-state root))
	(print "  Visible elements: " (str (count-visible filtered)))
	(print "")

	;Show all toggles
	(print "Final toggle states:")
	(designer-list-toggles)

	;Show designer comments
	(print "Designer comments:")
	(defq comments (designer-get-comments root))
	(each (lambda ((elem comment))
		(print "  " (get :name elem) ": " comment))
		comments)
	(print "")

	;Serialize with comments
	(print "Serialized with designer comments:")
	(print "══════════════════════════════════════════════════════")
	(print (serialize-with-state-comments root))
	(print "══════════════════════════════════════════════════════")
	(print ""))
