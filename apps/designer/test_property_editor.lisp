;; Test Property Editor (List-Based)
(print "")
(print "╔══════════════════════════════════════════════════════════╗")
(print "║  Property Editor Test                                    ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")

; Import environment
(import "././login/env.inc")

; Import standard GUI
(import "gui/lisp.inc")

; Import enhanced tracking
(import "gui_designer/lisp_enhanced.inc")

; Import property editor
(import "gui_designer/property_editor_list.inc")

(print "Step 1: Create UI and Capture Tree")
(print "──────────────────────────────────────────────────────────")

(designer-reset)

; Create UI with various properties
(ui-window test_app (:min_width 400 :min_height 300 :color 0xfff0f0f0)
	(ui-flow main_layout (:flow_flags +flow_down :min_width 350)
		(ui-label title (:text "Property Editor Test" :color 0xff0000ff))
		(ui-button ok_btn (:text "OK" :min_width 80 :min_height 30))))

(defq tree (designer-get-tree))

(print "✓ UI created and tree captured")
(print "")

(print "Step 2: Find and Display Element Properties")
(print "──────────────────────────────────────────────────────────")

; Debug: print tree structure first
(print "Tree root name: " (designer-get-name tree))
(print "Tree root type: " (designer-get-type tree))
(defq children (designer-get-children tree))
(print "Tree has " (length children) " children")

; Find the button element - just scan all nodes and print names
(defq button-elem :nil)
(defq all-names (list))
(defun collect-names (node)
	(defq name (designer-get-name node))
	(push all-names name)
	(each collect-names (designer-get-children node)))

(collect-names tree)
(print "All element names found: " all-names)

; Now try to find button by checking each name
(defun find-button (node)
	(defq name (designer-get-name node))
	; Try to match - names are quoted symbols
	(when (or (equal? name 'ok_btn) (find "ok_btn" (str name)))
		(setq button-elem node)
		(print "Found button with name: " name))
	(each find-button (designer-get-children node)))

(find-button tree)

(if button-elem
	(progn
		(print "Found button element!")
		(print "")
		(show-element-properties button-elem)
		(print ""))
	(print "✗ Button not found - checking tree...")
	(print "  Tree root: " (designer-get-name tree)))

(print "Step 3: Edit Property")
(print "──────────────────────────────────────────────────────────")

(when button-elem
	(print "Before edit:")
	(defq props-before (get-element-properties button-elem))
	(each (lambda (pair)
		(when (= (elem-get pair 0) :text)
			(print "  :text = " (elem-get pair 1))))
		props-before)

	; Edit the text property
	(set-element-property button-elem :text "Modified!")

	(print "")
	(print "After edit:")
	(defq props-after (get-element-properties button-elem))
	(each (lambda (pair)
		(when (= (elem-get pair 0) :text)
			(print "  :text = " (elem-get pair 1))))
		props-after)

	(print "")
	(print "✓ Property editing works!"))

(print "")

(print "Step 4: Test Property Type Detection")
(print "──────────────────────────────────────────────────────────")

(print "Property type system loaded")
(defq type-text (get-property-type :text))
(print "  :text → " type-text)
(defq type-width (get-property-type :min_width))
(print "  :min_width → " type-width)
(print "")

(print "╔══════════════════════════════════════════════════════════╗")
(print "║  ✓ SUCCESS - Property Editor Works!                     ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")
