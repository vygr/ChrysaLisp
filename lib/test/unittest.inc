;; ChrysaLisp Unit Testing Framework
;; Provides macros and functions for writing and running unit tests

(defq
	*test-suite-name* :nil
	*test-name* :nil
	*test-passed* 0
	*test-failed* 0
	*test-errors* '()
	*test-verbose* :t)

(defun test-reset ()
	; Reset test counters
	(setq *test-passed* 0
		*test-failed* 0
		*test-errors* '()))

(defun test-value-to-string (val)
	; Convert value to string for test messages
	; Avoids shadowing built-in str function
	(cond
		((str? val) val)
		((num? val) (cat "" val))
		((nil? val) "nil")
		(:t (cat "" val))))

(defun test-report ()
	; Print test results summary
	(print "")
	(print "========================================")
	(print "Test Results for: " *test-suite-name*)
	(print "========================================")
	(print "Passed: " *test-passed*)
	(print "Failed: " *test-failed*)
	(when (> *test-failed* 0)
		(print "")
		(print "Failures:")
		(each! (lambda ((name msg))
			(print "  " name ": " msg)) *test-errors*))
	(print "========================================")
	(print "")
	(if (= *test-failed* 0)
		(print "All tests passed!")
		(print "Some tests failed!"))
	(= *test-failed* 0))

(defmacro deftest-suite (name)
	; Define a test suite
	`(setq *test-suite-name* ,name))

(defmacro deftest (name &rest body)
	; Define a test case
	`(progn
		(setq *test-name* ,name)
		(when *test-verbose*
			(print "Running test: " ,name))
		~body))

(defun test-assert-impl (condition msg)
	; Internal assertion implementation
	(if condition
		(setq *test-passed* (inc *test-passed*))
		(progn
			(setq *test-failed* (inc *test-failed*))
			(push *test-errors* (list *test-name* msg))
			(when *test-verbose*
				(print "  FAILED: " msg)))))

;; Basic assertions

(defmacro assert-true (expr &optional msg)
	; Assert that expression is true
	`(test-assert-impl ,expr ,(if msg msg "Expected true")))

(defmacro assert-false (expr &optional msg)
	; Assert that expression is false
	`(test-assert-impl (not ,expr) ,(if msg msg "Expected false")))

(defmacro assert-eq (expected actual &optional msg)
	; Assert that two values are equal
	`(test-assert-impl (eql ,expected ,actual) ,(if msg msg "assert-eq")))

(defmacro assert-neq (expected actual &optional msg)
	; Assert that two values are not equal
	`(test-assert-impl (not (eql ,expected ,actual)) ,(if msg msg "assert-neq")))

(defmacro assert-nil (expr &optional msg)
	; Assert that expression is nil
	`(test-assert-impl (nil? ,expr) ,(if msg msg "Expected nil")))

(defmacro assert-not-nil (expr &optional msg)
	; Assert that expression is not nil
	`(test-assert-impl (not (nil? ,expr)) ,(if msg msg "Expected not nil")))

;; Numeric comparison assertions

(defmacro assert-greater (actual expected &optional msg)
	; Assert that actual > expected
	`(test-assert-impl (> ,actual ,expected) ,(if msg msg "actual > expected")))

(defmacro assert-less (actual expected &optional msg)
	; Assert that actual < expected
	`(test-assert-impl (< ,actual ,expected) ,(if msg msg "actual < expected")))

(defmacro assert-greater-eq (actual expected &optional msg)
	; Assert that actual >= expected
	`(test-assert-impl (>= ,actual ,expected) ,(if msg msg "actual >= expected")))

(defmacro assert-less-eq (actual expected &optional msg)
	; Assert that actual <= expected
	`(test-assert-impl (<= ,actual ,expected) ,(if msg msg "actual <= expected")))

(defmacro assert-close (actual expected tolerance &optional msg)
	; Assert that actual is within tolerance of expected (for floats)
	`(test-assert-impl (<= (abs (- ,actual ,expected)) ,tolerance) ,(if msg msg "values within tolerance")))

;; Collection assertions

(defmacro assert-contains (collection item &optional msg)
	; Assert that collection contains item
	`(test-assert-impl (some! 0 -1 (lambda (x) (eql x ,item)) ,collection) ,(if msg msg "collection contains item")))

(defmacro assert-not-contains (collection item &optional msg)
	; Assert that collection does not contain item
	`(test-assert-impl (not (some! 0 -1 (lambda (x) (eql x ,item)) ,collection)) ,(if msg msg "collection not contains item")))

(defmacro assert-empty (collection &optional msg)
	; Assert that collection is empty
	`(test-assert-impl (or (nil? ,collection) (= (length ,collection) 0)) ,(if msg msg "collection empty")))

(defmacro assert-not-empty (collection &optional msg)
	; Assert that collection is not empty
	`(test-assert-impl (and (not (nil? ,collection)) (> (length ,collection) 0)) ,(if msg msg "collection not empty")))

(defmacro assert-length (collection expected-length &optional msg)
	; Assert that collection has expected length
	`(test-assert-impl (= (length ,collection) ,expected-length) ,(if msg msg "collection length match")))

;; String assertions

(defmacro assert-starts-with (str prefix &optional msg)
	; Assert that string starts with prefix
	`(test-assert-impl (starts-with ,prefix ,str) ,(if msg msg "string starts with")))

(defmacro assert-ends-with (str suffix &optional msg)
	; Assert that string ends with suffix
	`(test-assert-impl (ends-with ,suffix ,str) ,(if msg msg "string ends with")))

(defmacro assert-contains-str (str substring &optional msg)
	; Assert that string contains substring
	`(test-assert-impl (find ,substring ,str) ,(if msg msg "string contains")))

;; Exception/Error assertions

(defmacro assert-throws (expr &optional msg)
	; Assert that expression throws an error/exception
	`(progn
		(defq threw-error :nil)
		(catch
			(progn ,expr (setq threw-error :nil))
			(setq threw-error :t))
		(test-assert-impl threw-error ,(if msg msg "expected throw"))))

;; Type assertions

(defmacro assert-type (expr expected-type &optional msg)
	; Assert that expression has expected type
	; expected-type can be 'str, 'num, 'list, etc.
	`(test-assert-impl
		(cond
			((= ,expected-type 'str) (str? ,expr))
			((= ,expected-type 'num) (num? ,expr))
			((= ,expected-type 'list) (list? ,expr))
			((= ,expected-type 'env) (env? ,expr))
			(:t :nil))
		,(if msg msg "type check")))

;; DOM/HTML assertions (for HTML parser testing)

(defmacro assert-element-exists (doc selector &optional msg)
	; Assert that an element matching selector exists in document
	`(test-assert-impl (not (nil? ,doc)) ,(if msg msg "element exists")))

(defmacro assert-element-tag (element expected-tag &optional msg)
	; Assert that element has expected tag name
	`(test-assert-impl
		(eql (to-lower (. ,element :get-tag-name)) (to-lower ,expected-tag))
		,(if msg msg "element tag")))

(defmacro assert-element-has-attribute (element attr-name &optional msg)
	; Assert that element has an attribute
	`(test-assert-impl (. ,element :has-attribute ,attr-name) ,(if msg msg "has attribute")))

(defmacro assert-element-attribute-eq (element attr-name expected-value &optional msg)
	; Assert that element attribute equals expected value
	`(test-assert-impl
		(eql (. ,element :get-attribute ,attr-name) ,expected-value)
		,(if msg msg "attribute equals")))

(defmacro assert-element-has-class (element class-name &optional msg)
	; Assert that element has a CSS class
	`(test-assert-impl
		(progn
			(defq classes (split (. ,element :get-attribute "class") " "))
			(some! 0 -1 (lambda (c) (eql c ,class-name)) classes))
		,(if msg msg "has class")))

(defmacro assert-element-child-count (element expected-count &optional msg)
	; Assert that element has expected number of children
	`(test-assert-impl
		(= (length (. ,element 'child_nodes)) ,expected-count)
		,(if msg msg "child count")))

(defmacro assert-element-has-children (element &optional msg)
	; Assert that element has at least one child
	`(test-assert-impl
		(> (length (. ,element 'child_nodes)) 0)
		,(if msg msg "has children")))

(defmacro assert-element-no-children (element &optional msg)
	; Assert that element has no children
	`(test-assert-impl
		(= (length (. ,element 'child_nodes)) 0)
		,(if msg msg "no children")))

(defmacro assert-element-text-contains (element expected-text &optional msg)
	; Assert that element's text content contains expected text
	`(test-assert-impl
		(progn
			(defq text "")
			(defun collect-text (node)
				(cond
					((= (. node 'node_type) 3)  ; NODE_TEXT
						(setq text (cat text (. node :get-text))))
					((= (. node 'node_type) 1)  ; NODE_ELEMENT
						(each! 0 -1 collect-text (. node 'child_nodes)))))
			(collect-text ,element)
			(find ,expected-text text))
		,(if msg msg "text contains")))

(defmacro assert-element-id (element expected-id &optional msg)
	; Assert that element has expected ID
	`(test-assert-impl
		(eql (. ,element :get-attribute "id") ,expected-id)
		,(if msg msg "element id")))

(defmacro assert-elements-count (doc tag-name expected-count &optional msg)
	; Assert that document has expected number of elements with tag name
	`(test-assert-impl
		(= (length (. ,doc :get-elements-by-tag-name ,tag-name)) ,expected-count)
		,(if msg msg "elements count")))

(defmacro assert-element-parent (element expected-parent &optional msg)
	; Assert that element has expected parent
	`(test-assert-impl
		(= (. ,element 'parent_node) ,expected-parent)
		,(if msg msg "element parent")))

(defmacro assert-element-has-parent (element &optional msg)
	; Assert that element has a parent
	`(test-assert-impl
		(not (nil? (. ,element 'parent_node)))
		,(if msg msg "has parent")))

(defmacro assert-document-valid (doc &optional msg)
	; Assert that document is valid (not nil and is document type)
	`(test-assert-impl
		(and (not (nil? ,doc)) (= (. ,doc 'node_type) 9))  ; NODE_DOCUMENT = 9
		,(if msg msg "document valid")))

(defmacro run-test-suite (&rest tests)
	; Run a series of tests
	`(progn
		(test-reset)
		~tests
		(test-report)))
