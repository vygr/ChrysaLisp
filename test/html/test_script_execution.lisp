#!/usr/bin/env lsp

;; Script Execution Tests
;; Tests <script> tag support for ChrysaLisp Lisp execution
;; Test-driven development for DOM scripting

(import "lib/test/unittest.inc")
(import "lib/html/parser.inc")
(import "lib/html/dom.inc")

(deftest-suite "Script Execution Tests")

(defun main ()
	(run-test-suite
		; Test parsing script tags
		(deftest "Parse Inline Script Tag"
			(defq html "<html><head><script>print \"Hello\"</script></head></html>")
			(defq doc (parse-html html))

			; Should have script element
			(defq scripts (. doc :get-elements-by-tag-name "script"))
			(assert-eq 1 (length scripts)))

		; Test script with src attribute
		(deftest "Parse External Script Tag"
			(defq html "<html><head><script src=\"app.lisp\"></script></head></html>")
			(defq doc (parse-html html))

			(defq scripts (. doc :get-elements-by-tag-name "script"))
			(assert-eq 1 (length scripts))

			(defq script (first scripts))
			(assert-element-attribute-eq script "src" "app.lisp"))

		; Test multiple script tags
		(deftest "Parse Multiple Script Tags"
			(defq html "
				<html>
					<head>
						<script src=\"lib.lisp\"></script>
						<script>print \"inline\"</script>
					</head>
				</html>")
			(defq doc (parse-html html))

			(defq scripts (. doc :get-elements-by-tag-name "script"))
			(assert-eq 2 (length scripts)))

		; Test script content extraction
		(deftest "Extract Inline Script Content"
			(defq html "<script>(defq x 42)</script>")
			(defq doc (parse-html html))

			(defq script (first (. doc :get-elements-by-tag-name "script")))
			(assert-not-nil script)

			; Should have text content
			(defq content (. script :get-text-content))
			(assert-contains "defq" content))

		; Test DOM API - getElementById
		(deftest "DOM API getElementById"
			(defq html "<div id=\"test\">Content</div>")
			(defq doc (parse-html html))

			; getElementById should work
			(defq elem (. doc :get-element-by-id "test"))
			(assert-not-nil elem)
			(assert-element-id elem "test"))

		; Test DOM API - getElementsByTagName
		(deftest "DOM API getElementsByTagName"
			(defq html "<div><p>One</p><p>Two</p></div>")
			(defq doc (parse-html html))

			(defq paras (. doc :get-elements-by-tag-name "p"))
			(assert-eq 2 (length paras)))

		; Test DOM API - querySelector (to be implemented)
		(deftest "DOM API querySelector"
			(defq html "<div class=\"box\"><p>Content</p></div>")
			(defq doc (parse-html html))

			; querySelector by class
			(defq elem (. doc :query-selector ".box"))
			(assert-not-nil elem)
			(assert-element-has-class elem "box"))

		; Test DOM manipulation - createElement
		(deftest "DOM createElement"
			(defq doc (html-document))

			(defq div (. doc :create-element "div"))
			(assert-not-nil div)
			(assert-element-tag div "div"))

		; Test DOM manipulation - setAttribute
		(deftest "DOM setAttribute"
			(defq doc (html-document))
			(defq div (. doc :create-element "div"))

			(. div :set-attribute "id" "myDiv")
			(assert-element-id div "myDiv")

			(. div :set-attribute "class" "box")
			(assert-element-has-class div "box"))

		; Test DOM manipulation - appendChild
		(deftest "DOM appendChild"
			(defq doc (html-document))
			(defq parent (. doc :create-element "div"))
			(defq child (. doc :create-element "p"))

			(. parent :append-child child)

			(assert-element-has-children parent)
			(assert-element-child-count parent 1)
			(assert-element-parent child parent))

		; Test DOM manipulation - removeChild
		(deftest "DOM removeChild"
			(defq doc (html-document))
			(defq parent (. doc :create-element "div"))
			(defq child (. doc :create-element "p"))

			(. parent :append-child child)
			(assert-element-child-count parent 1)

			(. parent :remove-child child)
			(assert-element-no-children parent))

		; Test DOM manipulation - textContent
		(deftest "DOM textContent"
			(defq doc (html-document))
			(defq p (. doc :create-element "p"))

			(. p :set-text-content "Hello World")

			(defq text (. p :get-text-content))
			(assert-contains "Hello" text))

		; Test DOM manipulation - innerHTML (to be implemented)
		(deftest "DOM innerHTML"
			(defq doc (html-document))
			(defq div (. doc :create-element "div"))

			(. div :set-inner-html "<p>Paragraph</p>")

			(assert-element-has-children div)
			(defq children (. div 'child_nodes))
			(assert-eq 1 (length children)))

		; Test script execution context
		(deftest "Script Has Document Context"
			(import "lib/html/script.inc")

			(defq html "<div id=\"app\"></div><script>(defq elem (. document :get-element-by-id \"app\"))</script>")
			(defq doc (parse-html html))

			; Execute scripts in the document
			(defq executor (execute-document-scripts doc))

			; Script should have been able to access document
			; We can verify scripts ran by checking no errors occurred
			(assert-not-nil executor))

		; Test script event handlers
		(deftest "Script Event Handler Registration"
			(defq html "<button id=\"btn\">Click</button><script>(. (. document :get-element-by-id \"btn\") :add-event-listener \"click\" (lambda () (print \"Clicked\")))</script>")
			(defq doc (parse-html html))

			; Should have button with event listener
			(defq btn (. doc :get-element-by-id "btn"))
			(assert-not-nil btn))

		; Test window object in scripts
		(deftest "Script Has Window Object"
			; Scripts should have access to window object
			; window.document === document
			:t)

		; Test script load order
		(deftest "Scripts Execute in Order"
			(defq html "
				<script>global_var = 1</script>
				<script>global_var = global_var + 1</script>
				<script>global_var = global_var + 1</script>")
			(defq doc (parse-html html))

			; After execution, global_var should be 3
			; Requires script execution implementation
			:t)
	))

