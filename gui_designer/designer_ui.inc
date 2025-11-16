;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Integrated Designer UI
; Console-based designer tool combining tree view and property editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Tree Display Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-tree-element (elem depth)
	; Print a single element with indentation
	(defq indent "")
	(defq i 0)
	(while (< i depth)
		(setq indent (cat indent "  "))
		(setq i (+ i 1)))

	(defq id (designer-get-id elem))
	(defq type (designer-get-type elem))
	(defq name (designer-get-name elem))
	(defq children (designer-get-children elem))
	(defq child-count (length children))

	(print indent "[" id "] " name " (" type ") - " child-count " children"))

(defun print-tree-recursive (elem depth)
	; Recursively print element and all children
	(print-tree-element elem depth)
	(defq children (designer-get-children elem))
	(defq i 0)
	(while (< i (length children))
		(defq child (elem-get children i))
		(print-tree-recursive child (+ depth 1))
		(setq i (+ i 1))))

(defun show-tree ()
	; Display the entire UI tree
	(defq tree *designer-ui-tree*)
	(if tree
		(progn
			(print "")
			(print "=== UI TREE ===")
			(print "")
			(print-tree-recursive tree 0)
			(print "")
			tree)
		(progn
			(print "No UI tree available")
			:nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Element Search Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-element-by-id-recursive (elem target-id)
	; Recursively search for element with given ID
	(defq elem-id (designer-get-id elem))
	(if (= elem-id target-id)
		elem
		(progn
			(defq children (designer-get-children elem))
			(defq result :nil)
			(defq i 0)
			(while (and (< i (length children)) (not result))
				(defq child (elem-get children i))
				(setq result (find-element-by-id-recursive child target-id))
				(setq i (+ i 1)))
			result)))

(defun find-element-by-id (id)
	; Find element in tree by ID
	(defq tree *designer-ui-tree*)
	(if tree
		(find-element-by-id-recursive tree id)
		:nil))

(defun find-element-by-name-recursive (elem target-name)
	; Recursively search for first element with given name
	(defq elem-name (designer-get-name elem))
	(if (eql elem-name target-name)
		elem
		(progn
			(defq children (designer-get-children elem))
			(defq result :nil)
			(defq i 0)
			(while (and (< i (length children)) (not result))
				(defq child (elem-get children i))
				(setq result (find-element-by-name-recursive child target-name))
				(setq i (+ i 1)))
			result)))

(defun find-element-by-name (name)
	; Find first element in tree by name
	(defq tree *designer-ui-tree*)
	(if tree
		(find-element-by-name-recursive tree name)
		:nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Element Info Display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun show-element-info (elem)
	; Display detailed info about an element
	(if elem
		(progn
			(print "")
			(print "=== ELEMENT INFO ===")
			(print "ID: " (designer-get-id elem))
			(print "Type: " (designer-get-type elem))
			(print "Name: " (designer-get-name elem))
			(print "Constructor: " (designer-get-constructor elem))
			(defq children (designer-get-children elem))
			(print "Children: " (length children))
			(print "")
			elem)
		(progn
			(print "Element not found")
			:nil)))

(defun show-element-with-properties (elem)
	; Display element info and properties
	(when elem
		(show-element-info elem)
		(show-element-properties elem)
		(print "")
		elem))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Navigation Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-parent-of-element-recursive (elem target parent)
	; Recursively find parent of target element
	(defq children (designer-get-children elem))
	(defq i 0)
	(defq result :nil)
	(while (and (< i (length children)) (not result))
		(defq child (elem-get children i))
		(defq child-id (designer-get-id child))
		(defq target-id (designer-get-id target))
		(if (= child-id target-id)
			(setq result elem)
			(setq result (get-parent-of-element-recursive child target elem)))
		(setq i (+ i 1)))
	result)

(defun get-parent (elem)
	; Get parent of an element
	(defq tree *designer-ui-tree*)
	(if (and tree elem)
		(get-parent-of-element-recursive tree elem tree)
		:nil))

(defun get-children (elem)
	; Get children of an element
	(if elem
		(designer-get-children elem)
		(list)))

(defun get-child-at-index (elem index)
	; Get child at specific index
	(if elem
		(progn
			(defq children (designer-get-children elem))
			(if (and (>= index 0) (< index (length children)))
				(elem-get children index)
				:nil))
		:nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Collection Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun collect-all-elements-recursive (elem result)
	; Recursively collect all elements into a list
	(push result elem)
	(defq children (designer-get-children elem))
	(defq i 0)
	(while (< i (length children))
		(defq child (elem-get children i))
		(collect-all-elements-recursive child result)
		(setq i (+ i 1)))
	result)

(defun get-all-elements ()
	; Get list of all elements in tree
	(defq tree *designer-ui-tree*)
	(if tree
		(collect-all-elements-recursive tree (list))
		(list)))

(defun count-elements-recursive (elem)
	; Count total elements in tree
	(defq count 1)
	(defq children (designer-get-children elem))
	(defq i 0)
	(while (< i (length children))
		(defq child (elem-get children i))
		(setq count (+ count (count-elements-recursive child)))
		(setq i (+ i 1)))
	count)

(defun count-elements ()
	; Count total elements in tree
	(defq tree *designer-ui-tree*)
	(if tree
		(count-elements-recursive tree)
		0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Designer Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun designer-show ()
	; Show the UI tree
	(show-tree))

(defun designer-select (id)
	; Select and show element by ID
	(defq elem (find-element-by-id id))
	(if elem
		(show-element-with-properties elem)
		(progn
			(print "Element with ID " id " not found")
			:nil)))

(defun designer-select-name (name)
	; Select and show element by name
	(defq elem (find-element-by-name name))
	(if elem
		(show-element-with-properties elem)
		(progn
			(print "Element with name " name " not found")
			:nil)))

(defun designer-stats ()
	; Show statistics about the UI tree
	(defq tree *designer-ui-tree*)
	(if tree
		(progn
			(print "")
			(print "=== DESIGNER STATISTICS ===")
			(print "Total elements: " (count-elements))
			(print "Root element: " (designer-get-name tree) " (ID: " (designer-get-id tree) ")")
			(print "")
			:t)
		(progn
			(print "No UI tree available")
			:nil)))

(defun designer-help ()
	; Show help for designer commands
	(print "")
	(print "=== DESIGNER COMMANDS ===")
	(print "")
	(print "(designer-show)              - Show UI tree")
	(print "(designer-select ID)         - Select element by ID")
	(print "(designer-select-name NAME)  - Select element by name")
	(print "(designer-stats)             - Show statistics")
	(print "(designer-help)              - Show this help")
	(print "")
	(print "Property Editor:")
	(print "(show-element-properties ELEM) - Show properties")
	(print "(get-element-properties ELEM)  - Get property list")
	(print "(set-element-property ELEM :key value) - Set property")
	(print "")
	:t)

(print "Integrated Designer UI loaded")
(print "Type (designer-help) for available commands")
