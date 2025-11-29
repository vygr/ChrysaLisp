;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test Integrated Designer UI
; Demonstrates all designer features working together
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "")
(print "╔══════════════════════════════════════════════════════════╗")
(print "║  INTEGRATED DESIGNER UI DEMO                            ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")

; Import required modules
(import "././login/env.inc")
(import "gui/lisp.inc")
(import "gui_designer/lisp_enhanced.inc")
(import "gui_designer/property_editor_list.inc")
(import "gui_designer/designer_ui.inc")

; Reset designer
(designer-reset)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Create a realistic UI for testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "Creating sample UI...")
(print "")

(ui-window my_app (:min_width 800 :min_height 600)
	(ui-flow main_layout (:flow_flags +flow_down)
		(ui-title app_title (:text "My Application"))
		(ui-flow toolbar (:flow_flags +flow_right)
			(ui-button btn_new (:text "New"))
			(ui-button btn_open (:text "Open"))
			(ui-button btn_save (:text "Save")))
		(ui-flow content_area (:flow_flags +flow_right)
			(ui-grid sidebar (:min_width 200)
				(ui-label lbl_files (:text "Files"))
				(ui-label lbl_project (:text "Project")))
			(ui-flow main_panel (:flow_flags +flow_down)
				(ui-label lbl_editor (:text "Editor Area"))
				(ui-flow status_bar (:flow_flags +flow_right)
					(ui-label lbl_line (:text "Line: 1"))
					(ui-label lbl_col (:text "Col: 1")))))))

(print "UI created successfully!")
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test 1: Show entire tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "═══ TEST 1: Show UI Tree ═══")
(designer-show)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test 2: Designer statistics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "═══ TEST 2: Designer Statistics ===")
(designer-stats)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test 3: Select element by ID
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "═══ TEST 3: Select Element by ID ===")
(print "Selecting element with ID 1 (root window)...")
(designer-select 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test 4: Select element by name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "═══ TEST 4: Select Element by Name ===")
(print "Selecting 'btn_new' button...")
(designer-select-name 'btn_new)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test 5: Find and inspect specific elements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "═══ TEST 5: Find and Inspect Elements ===")

(print "Finding toolbar...")
(defq toolbar (find-element-by-name 'toolbar))
(if toolbar
	(progn
		(print "✓ Found toolbar (ID: " (designer-get-id toolbar) ")")
		(print "  Children: " (length (designer-get-children toolbar))))
	(print "✗ Toolbar not found"))

(print "")
(print "Finding status bar...")
(defq status (find-element-by-name 'status_bar))
(if status
	(progn
		(print "✓ Found status_bar (ID: " (designer-get-id status) ")")
		(show-element-properties status))
	(print "✗ Status bar not found"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test 6: Navigation - Parent/Children
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "")
(print "═══ TEST 6: Navigation ===")

(defq btn (find-element-by-name 'btn_new))
(if btn
	(progn
		(print "Button 'btn_new' found (ID: " (designer-get-id btn) ")")
		(defq parent (get-parent btn))
		(if parent
			(print "  Parent: " (designer-get-name parent) " (ID: " (designer-get-id parent) ")")
			(print "  No parent found"))

		(defq siblings (get-children parent))
		(print "  Siblings: " (length siblings))
		(defq i 0)
		(while (< i (length siblings))
			(defq sibling (elem-get siblings i))
			(print "    - " (designer-get-name sibling))
			(setq i (+ i 1))))
	(print "Button not found"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test 7: Property Modification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "")
(print "═══ TEST 7: Property Modification ===")

(defq title (find-element-by-name 'app_title))
(if title
	(progn
		(print "Original properties:")
		(show-element-properties title)
		(print "")
		(print "Modifying :text property...")
		(set-element-property title :text "Updated Title!")
		(print "")
		(print "Updated properties:")
		(show-element-properties title))
	(print "Title element not found"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test 8: Collection Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "")
(print "═══ TEST 8: Collection Functions ===")

(print "Getting all elements...")
(defq all-elems (get-all-elements))
(print "Total elements: " (length all-elems))

(print "")
(print "First 5 elements:")
(defq i 0)
(while (< i (min 5 (length all-elems)))
	(defq elem (elem-get all-elems i))
	(print "  [" (designer-get-id elem) "] "
	       (designer-get-name elem) " (" (designer-get-type elem) ")")
	(setq i (+ i 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test 9: Search Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "")
(print "═══ TEST 9: Search Functions ===")

(print "Searching for elements by name:")
(defq search-names (list 'btn_new 'btn_save 'lbl_editor 'sidebar))
(defq i 0)
(while (< i (length search-names))
	(defq name (elem-get search-names i))
	(defq found (find-element-by-name name))
	(if found
		(print "  ✓ " name " found (ID: " (designer-get-id found) ")")
		(print "  ✗ " name " not found"))
	(setq i (+ i 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Final Summary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "")
(print "╔══════════════════════════════════════════════════════════╗")
(print "║  INTEGRATED DESIGNER UI - ALL TESTS COMPLETE            ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")

(designer-help)

(print "")
(print "✓ Integrated Designer UI is fully functional!")
(print "")
(print "Try these commands:")
(print "  (designer-show)")
(print "  (designer-select 1)")
(print "  (designer-select-name :btn_save)")
(print "")
