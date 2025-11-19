
;; Browser Driver Tests (WebDriver-compatible automation)
;; TDD approach - Selenium-style browser automation

(import "lib/test/unittest.inc")
(import "lib/html/browser_driver.inc")

(deftest-suite "Browser Driver Tests")

; Test 1: Create browser session
(deftest "Create Browser Session"
	(defq driver (create-browser-driver))
	(assert-not-nil driver))

; Test 2: Navigate to URL (file://)
(deftest "Navigate To URL"
	(defq driver (create-browser-driver))

	; Navigate to a file:// URL
	(. driver :navigate "file://test.html")

	; Verify current URL
	(assert-eq "file://test.html" (. driver :current-url)))

; Test 3: Get page title
(deftest "Get Page Title"
	(defq driver (create-browser-driver))

	; Load HTML with title
	(. driver :load-html "<html><head><title>Test Page</title></head><body></body></html>")

	; Get title
	(assert-eq "Test Page" (. driver :title)))

; Test 4: Find element by ID
(deftest "Find Element By ID"
	(defq driver (create-browser-driver))
	(. driver :load-html "<div id=\"test\">Content</div>")

	; Find element
	(defq element (. driver :find-element-by-id "test"))

	(assert-not-nil element)
	(assert-eq "test" (. element :get-attribute "id")))

; Test 5: Find elements by tag name
(deftest "Find Elements By Tag Name"
	(defq driver (create-browser-driver))
	(. driver :load-html "
		<div>First</div>
		<div>Second</div>
		<div>Third</div>")

	; Find all divs
	(defq elements (. driver :find-elements-by-tag-name "div"))

	(assert-eq 3 (length elements)))

; Test 6: Find element by CSS selector
(deftest "Find Element By CSS Selector"
	(defq driver (create-browser-driver))
	(. driver :load-html "<div class=\"container\"><p class=\"text\">Hello</p></div>")

	; Find by class
	(defq element (. driver :find-element-by-selector ".text"))

	(assert-not-nil element)
	(assert-eq "Hello" (. element :text)))

; Test 7: Click element
(deftest "Click Element"
	(defq driver (create-browser-driver))
	(. driver :load-html "<button id=\"btn\" onclick=\"(defq clicked :t)\">Click</button>")

	; Find and click button
	(defq button (. driver :find-element-by-id "btn"))
	(. button :click)

	; Verify click happened
	(defq ctx (. driver :script-context))
	(assert-eq :t (. ctx :get-global "clicked")))

; Test 8: Send keys to input
(deftest "Send Keys To Input"
	(defq driver (create-browser-driver))
	(. driver :load-html "<input id=\"name\" type=\"text\">")

	; Find input and type
	(defq input (. driver :find-element-by-id "name"))
	(. input :send-keys "Alice")

	; Verify value
	(assert-eq "Alice" (. input :get-attribute "value")))

; Test 9: Clear input field
(deftest "Clear Input Field"
	(defq driver (create-browser-driver))
	(. driver :load-html "<input id=\"name\" value=\"Old Value\">")

	(defq input (. driver :find-element-by-id "name"))

	; Clear the field
	(. input :clear)

	; Verify empty
	(assert-eq "" (. input :get-attribute "value")))

; Test 10: Get element text
(deftest "Get Element Text"
	(defq driver (create-browser-driver))
	(. driver :load-html "<p id=\"para\">Hello World</p>")

	(defq para (. driver :find-element-by-id "para"))

	(assert-eq "Hello World" (. para :text)))

; Test 11: Get element attribute
(deftest "Get Element Attribute"
	(defq driver (create-browser-driver))
	(. driver :load-html "<a id=\"link\" href=\"http://example.com\">Link</a>")

	(defq link (. driver :find-element-by-id "link"))

	(assert-eq "http://example.com" (. link :get-attribute "href")))

; Test 12: Check element displayed
(deftest "Check Element Displayed"
	(defq driver (create-browser-driver))
	(. driver :load-html "<div id=\"visible\">Visible</div>
		<div id=\"hidden\" style=\"display:none\">Hidden</div>")

	(defq visible (. driver :find-element-by-id "visible"))
	(defq hidden (. driver :find-element-by-id "hidden"))

	(assert-eq :t (. visible :is-displayed))
	(assert-eq :nil (. hidden :is-displayed)))

; Test 13: Check element enabled
(deftest "Check Element Enabled"
	(defq driver (create-browser-driver))
	(. driver :load-html "
		<input id=\"enabled\">
		<input id=\"disabled\" disabled>")

	(defq enabled (. driver :find-element-by-id "enabled"))
	(defq disabled (. driver :find-element-by-id "disabled"))

	(assert-eq :t (. enabled :is-enabled))
	(assert-eq :nil (. disabled :is-enabled)))

; Test 14: Execute script
(deftest "Execute Script"
	(defq driver (create-browser-driver))
	(. driver :load-html "<div id=\"target\">Original</div>")

	; Execute script to modify DOM
	(. driver :execute-script "(. (. document :get-element-by-id \"target\") :set-text-content \"Modified\")")

	; Verify change
	(defq target (. driver :find-element-by-id "target"))
	(assert-eq "Modified" (. target :text)))

; Test 15: Execute script with return value
(deftest "Execute Script With Return"
	(defq driver (create-browser-driver))
	(. driver :load-html "<div></div>")

	; Execute script that returns value
	(defq result (. driver :execute-script "(+ 2 3)"))

	(assert-eq 5 result))

; Test 16: Get all cookies (placeholder)
(deftest "Get Cookies"
	(defq driver (create-browser-driver))
	(. driver :load-html "<div></div>")

	; Get cookies (empty for file://)
	(defq cookies (. driver :get-cookies))

	(assert-not-nil cookies))

; Test 17: Take screenshot (placeholder)
(deftest "Take Screenshot"
	(defq driver (create-browser-driver))
	(. driver :load-html "<div>Test</div>")

	; Take screenshot
	(defq screenshot (. driver :screenshot))

	; Should return some data
	(assert-not-nil screenshot))

; Test 18: Get window size
(deftest "Get Window Size"
	(defq driver (create-browser-driver))

	(defq size (. driver :window-size))

	(assert-not-nil size)
	(assert-not-nil (get size :width))
	(assert-not-nil (get size :height)))

; Test 19: Set window size
(deftest "Set Window Size"
	(defq driver (create-browser-driver))

	(. driver :set-window-size 800 600)

	(defq size (. driver :window-size))
	(assert-eq 800 (get size :width))
	(assert-eq 600 (get size :height)))

; Test 20: Wait for element
(deftest "Wait For Element"
	(defq driver (create-browser-driver))
	(. driver :load-html "<div id=\"target\">Found</div>")

	; Wait for element to exist
	(defq element (. driver :wait-for-element-by-id "target" 1000))

	(assert-not-nil element)
	(assert-eq "Found" (. element :text)))

; Test 21: Form submission
(deftest "Submit Form"
	(defq driver (create-browser-driver))
	(. driver :load-html "
		<form id=\"myform\" onsubmit=\"(defq submitted :t)\">
			<input type=\"submit\" id=\"submit-btn\">
		</form>")

	; Submit form via button click
	(defq submit-btn (. driver :find-element-by-id "submit-btn"))
	(. submit-btn :click)

	; Verify submission
	(defq ctx (. driver :script-context))
	(assert-eq :t (. ctx :get-global "submitted")))

; Test 22: Chained element finding
(deftest "Find Element Within Element"
	(defq driver (create-browser-driver))
	(. driver :load-html "
		<div id=\"container\">
			<p id=\"para1\">First</p>
			<p id=\"para2\">Second</p>
		</div>")

	; Find container first
	(defq container (. driver :find-element-by-id "container"))

	; Find para within container
	(defq para2 (. container :find-element-by-id "para2"))

	(assert-eq "Second" (. para2 :text)))

; Test 23: Alert handling
(deftest "Handle Alert"
	(defq driver (create-browser-driver))
	(. driver :load-html "<script>(alert \"Test alert\")</script>")

	; Get alert
	(defq alerts (. driver :get-alerts))

	(assert-eq 1 (length alerts))
	(assert-eq "Test alert" (first alerts)))

; Test 24: Confirm handling
(deftest "Handle Confirm"
	(defq driver (create-browser-driver))
	(. driver :load-html "<script>(defq answer (confirm \"Proceed?\"))</script>")

	; Set confirm result
	(. driver :set-confirm-result :t)

	; Re-execute scripts
	(. driver :refresh)

	; Verify result
	(defq ctx (. driver :script-context))
	(assert-eq :t (. ctx :get-global "answer")))

; Test 25: Multiple elements interaction
(deftest "Interact With Multiple Elements"
	(defq driver (create-browser-driver))
	(. driver :load-html "
		<input id=\"first\" type=\"text\">
		<input id=\"second\" type=\"text\">
		<button id=\"submit\" onclick=\"(defq done :t)\">Submit</button>")

	; Fill both inputs
	(. (. driver :find-element-by-id "first") :send-keys "Alice")
	(. (. driver :find-element-by-id "second") :send-keys "Bob")

	; Click button
	(. (. driver :find-element-by-id "submit") :click)

	; Verify all values
	(assert-eq "Alice" (. (. driver :find-element-by-id "first") :get-attribute "value"))
	(assert-eq "Bob" (. (. driver :find-element-by-id "second") :get-attribute "value"))
	(assert-eq :t (. (. driver :script-context) :get-global "done")))
