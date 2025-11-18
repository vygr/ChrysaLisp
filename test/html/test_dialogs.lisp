#!/usr/bin/env lsp

;; Dialog Function Tests (alert, confirm, prompt)
;; TDD approach - tests first!

(import "lib/test/unittest.inc")
(import "lib/html/dom.inc")
(import "lib/html/parser.inc")
(import "lib/html/script.inc")

(deftest-suite "Dialog Function Tests")

; Test 1: alert() function exists in window
(deftest "alert() Function Exists"
	(defq html "<div></div>")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))
	(defq win (. ctx 'window))

	; window should have alert method
	(assert-not-nil (. win :has-method :alert)))

; Test 2: alert() displays message
(deftest "alert() Displays Message"
	(defq html "<script>(alert \"Test message\")</script>")
	(defq doc (parse-html html))

	; Capture alert calls
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))
	(defq win (. ctx 'window))

	; Check alert was called with message
	(defq alerts (. win :get-alerts))
	(assert-eq 1 (length alerts))
	(assert-eq "Test message" (first alerts)))

; Test 3: Multiple alert() calls
(deftest "Multiple alert() Calls"
	(defq html "
		<script>(alert \"First\")</script>
		<script>(alert \"Second\")</script>
		<script>(alert \"Third\")</script>")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))
	(defq win (. ctx 'window))

	(defq alerts (. win :get-alerts))
	(assert-eq 3 (length alerts))
	(assert-eq "First" (first alerts))
	(assert-eq "Second" (elem 1 alerts))
	(assert-eq "Third" (elem 2 alerts)))

; Test 4: confirm() function exists
(deftest "confirm() Function Exists"
	(defq html "<div></div>")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))
	(defq win (. ctx 'window))

	(assert-not-nil (. win :has-method :confirm)))

; Test 5: confirm() returns boolean
(deftest "confirm() Returns Boolean"
	(defq html "<script>(defq result (confirm \"Continue?\"))</script>")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	; Set mock confirm result to true
	(defq win (. ctx 'window))
	(. win :set-confirm-result :t)

	; Re-execute to get result
	(defq executor2 (execute-document-scripts doc))
	(defq ctx2 (. executor2 :get-context))
	(defq result (. ctx2 :get-global "result"))
	(assert-eq :t result))

; Test 6: confirm() with false result
(deftest "confirm() Returns False"
	(defq html "<script>(defq result (confirm \"Delete?\"))</script>")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))
	(defq win (. ctx 'window))

	; Set mock confirm result to false
	(. win :set-confirm-result :nil)

	; Re-execute
	(defq executor2 (execute-document-scripts doc))
	(defq ctx2 (. executor2 :get-context))
	(defq result (. ctx2 :get-global "result"))
	(assert-eq :nil result))

; Test 7: prompt() function exists
(deftest "prompt() Function Exists"
	(defq html "<div></div>")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))
	(defq win (. ctx 'window))

	(assert-not-nil (. win :has-method :prompt)))

; Test 8: prompt() returns user input
(deftest "prompt() Returns User Input"
	(defq html "<script>(defq name (prompt \"Enter name:\" \"Guest\"))</script>")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))
	(defq win (. ctx 'window))

	; Set mock prompt result
	(. win :set-prompt-result "Alice")

	; Re-execute
	(defq executor2 (execute-document-scripts doc))
	(defq ctx2 (. executor2 :get-context))
	(defq name (. ctx2 :get-global "name"))
	(assert-eq "Alice" name))

; Test 9: prompt() with cancel (returns nil)
(deftest "prompt() Cancel Returns Nil"
	(defq html "<script>(defq name (prompt \"Name?\"))</script>")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))
	(defq win (. ctx 'window))

	; Set mock prompt result to nil (cancelled)
	(. win :set-prompt-result nil)

	; Re-execute
	(defq executor2 (execute-document-scripts doc))
	(defq ctx2 (. executor2 :get-context))
	(defq name (. ctx2 :get-global "name"))
	(assert-eq nil name))

; Test 10: prompt() with default value
(deftest "prompt() Uses Default Value"
	(defq html "<script>(defq age (prompt \"Age?\" \"18\"))</script>")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))
	(defq win (. ctx 'window))

	; Check prompt was called with default
	(defq prompts (. win :get-prompts))
	(assert-eq 1 (length prompts))
	(defq prompt_call (first prompts))
	(assert-eq "Age?" (get prompt_call :message))
	(assert-eq "18" (get prompt_call :default)))

; Test 11: Alert in event handler
(deftest "alert() In Event Handler"
	(defq html "<button id=\"btn\" onclick=\"(alert 'Button clicked!')\">Click</button>")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))

	; Trigger click event
	(defq btn (. doc :get-element-by-id "btn"))
	(. btn :dispatch-event (env :type "click"))

	; Check alert was called
	(defq ctx (. executor :get-context))
	(defq win (. ctx 'window))
	(defq alerts (. win :get-alerts))
	(assert-eq 1 (length alerts))
	(assert-eq "Button clicked!" (first alerts)))

; Test 12: Conditional confirm() in script
(deftest "Conditional confirm() Logic"
	(defq html "
		<script>
			(if (confirm \"Proceed?\")
				(defq action \"proceeded\")
				(defq action \"cancelled\"))
		</script>")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))
	(defq win (. ctx 'window))

	; Test with true
	(. win :set-confirm-result :t)
	(defq executor2 (execute-document-scripts doc))
	(defq ctx2 (. executor2 :get-context))
	(assert-eq "proceeded" (. ctx2 :get-global "action"))

	; Test with false
	(. win :set-confirm-result :nil)
	(defq executor3 (execute-document-scripts doc))
	(defq ctx3 (. executor3 :get-context))
	(assert-eq "cancelled" (. ctx3 :get-global "action")))
