
;; DevTools Inspector Tests
;; Tests console logging and DOM tree viewing

(import "lib/test/unittest.inc")
(import "lib/html/parser.inc")
(import "lib/html/devtools.inc")

(deftest-suite "DevTools Inspector Tests")

(defun main ()
	(run-test-suite
		; Test console creation
		(deftest "Create Console"
			(defq console (devtools-console :init))
			(assert-not-nil console))

		; Test console logging
		(deftest "Console Log Messages"
			(defq console (devtools-console :init))
			(. console :info "Test message")
			(. console :warn "Warning message")
			(. console :error "Error message")

			(defq entries (. console :get-entries))
			(assert-eq 3 (length entries)))

		; Test console clear
		(deftest "Console Clear"
			(defq console (devtools-console :init))
			(. console :info "Message 1")
			(. console :info "Message 2")
			(assert-eq 2 (length (. console :get-entries)))

			(. console :clear)
			(assert-eq 0 (length (. console :get-entries))))

		; Test console to-string
		(deftest "Console to-string"
			(defq console (devtools-console :init))
			(. console :info "Hello")
			(. console :error "Problem")

			(defq output (. console :to-string))
			(assert-contains "[INFO]" output)
			(assert-contains "[ERROR]" output)
			(assert-contains "Hello" output)
			(assert-contains "Problem" output))

		; Test DOM tree viewer creation
		(deftest "Create DOM Tree Viewer"
			(defq html "<html><body><p>Test</p></body></html>")
			(defq doc (parse-html html))
			(defq viewer (dom-tree-viewer :init doc))
			(assert-not-nil viewer))

		; Test DOM tree rendering
		(deftest "Render DOM Tree"
			(defq html "<html><body><h1>Title</h1><p>Content</p></body></html>")
			(defq doc (parse-html html))
			(defq viewer (dom-tree-viewer :init doc))

			(defq tree (. viewer :render-tree))
			(assert-contains "<html>" tree)
			(assert-contains "<body>" tree)
			(assert-contains "<h1>" tree)
			(assert-contains "<p>" tree))

		; Test tree with attributes
		(deftest "Render Tree with Attributes"
			(defq html "<div id=\"main\" class=\"container\"><p>Text</p></div>")
			(defq doc (parse-html html))
			(defq viewer (dom-tree-viewer :init doc))

			(defq tree (. viewer :render-tree))
			(assert-contains "id=\"main\"" tree)
			(assert-contains "class=\"container\"" tree))

		; Test tree with text content
		(deftest "Render Tree with Text"
			(defq html "<p>Hello World</p>")
			(defq doc (parse-html html))
			(defq viewer (dom-tree-viewer :init doc))

			(defq tree (. viewer :render-tree))
			(assert-contains "Hello World" tree))

		; Test get element info
		(deftest "Get Element Info"
			(defq html "<div id=\"test\" class=\"box\"><p>Content</p></div>")
			(defq doc (parse-html html))
			(defq viewer (dom-tree-viewer :init doc))

			(defq div (. doc :get-element-by-id "test"))
			(defq info (. viewer :get-element-info div))

			(assert-not-nil (get info :tag_name))
			(assert-not-nil (get info :attributes))
			(assert-eq 1 (get info :child_count)))

		; Test devtools inspector creation
		(deftest "Create DevTools Inspector"
			(defq html "<html><body><p>Test</p></body></html>")
			(defq doc (parse-html html))
			(defq inspector (devtools-inspector :init doc))

			(assert-not-nil inspector)
			(assert-not-nil (. inspector :get-console))
			(assert-not-nil (. inspector :get-tree-viewer)))

		; Test inspector render all
		(deftest "Render Complete Inspector"
			(defq html "<html><body><p>Test</p></body></html>")
			(defq doc (parse-html html))
			(defq inspector (devtools-inspector :init doc))

			; Log some messages
			(. (. inspector :get-console) :info "Page loaded")
			(. (. inspector :get-console) :warn "Test warning")

			; Render all
			(defq output (. inspector :render-all))
			(assert-contains "CONSOLE" output)
			(assert-contains "DOM TREE" output)
			(assert-contains "[INFO]" output)
			(assert-contains "Page loaded" output)
			(assert-contains "<html>" output))

		; Test inspector with script errors
		(deftest "Console Error Logging"
			(defq html "<html></html>")
			(defq doc (parse-html html))
			(defq inspector (devtools-inspector :init doc))

			(. (. inspector :get-console) :error "Script error: undefined variable")
			(defq entries (. (. inspector :get-console) :get-entries))

			(assert-eq 1 (length entries))
			(defq entry (first entries))
			(assert-eq :error (get entry :level)))

		; Test network tracker creation
		(deftest "Create Network Tracker"
			(defq html "<html></html>")
			(defq doc (parse-html html))
			(defq inspector (devtools-inspector :init doc))

			(assert-not-nil (. inspector :get-network)))

		; Test network resource tracking
		(deftest "Track Network Resource"
			(defq html "<html></html>")
			(defq doc (parse-html html))
			(defq inspector (devtools-inspector :init doc))

			(. (. inspector :get-network) :track-resource "/test/file.html" "GET" "OK")
			(defq resources (. (. inspector :get-network) :get-resources))

			(assert-eq 1 (length resources))
			(defq res (first resources))
			(assert-eq "/test/file.html" (get res :url))
			(assert-eq "GET" (get res :method))
			(assert-eq "OK" (get res :status)))

		; Test network rendering
		(deftest "Render Network Resources"
			(defq html "<html></html>")
			(defq doc (parse-html html))
			(defq inspector (devtools-inspector :init doc))

			(. (. inspector :get-network) :track-resource "/page1.html" "GET" "OK")
			(. (. inspector :get-network) :track-resource "/page2.html" "GET" "OK")

			(defq output (. inspector :render-network))
			(assert-contains "GET" output)
			(assert-contains "/page1.html" output)
			(assert-contains "/page2.html" output))
	))

