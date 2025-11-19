
;; Form Widget Tests (input, button, textarea)
;; TDD approach - tests first!

(import "lib/test/unittest.inc")
(import "lib/html/dom.inc")
(import "lib/html/parser.inc")
(import "lib/html/script.inc")

(deftest-suite "Form Widget Tests")

; Test 1: Text input element parses correctly
(deftest "Text Input Element"
	(defq html "<input type=\"text\" id=\"name\" value=\"John\">")
	(defq doc (parse-html html))

	(defq input (. doc :get-element-by-id "name"))
	(assert-not-nil input)
	(assert-element-attribute-eq input "type" "text")
	(assert-element-attribute-eq input "value" "John"))

; Test 2: Password input element
(deftest "Password Input Element"
	(defq html "<input type=\"password\" id=\"pass\" value=\"secret\">")
	(defq doc (parse-html html))

	(defq input (. doc :get-element-by-id "pass"))
	(assert-not-nil input)
	(assert-element-attribute-eq input "type" "password")
	(assert-element-attribute-eq input "value" "secret"))

; Test 3: Button element
(deftest "Button Element"
	(defq html "<button id=\"btn\">Click Me</button>")
	(defq doc (parse-html html))

	(defq btn (. doc :get-element-by-id "btn"))
	(assert-not-nil btn)
	(assert-element-text-contains btn "Click Me"))

; Test 4: Input type=button
(deftest "Input Button Element"
	(defq html "<input type=\"button\" id=\"btn\" value=\"Submit\">")
	(defq doc (parse-html html))

	(defq btn (. doc :get-element-by-id "btn"))
	(assert-not-nil btn)
	(assert-element-attribute-eq input "type" "button")
	(assert-element-attribute-eq btn "value" "Submit"))

; Test 5: Input type=submit
(deftest "Submit Button Element"
	(defq html "<input type=\"submit\" id=\"btn\" value=\"Submit Form\">")
	(defq doc (parse-html html))

	(defq btn (. doc :get-element-by-id "btn"))
	(assert-not-nil btn)
	(assert-element-attribute-eq btn "type" "submit")
	(assert-element-attribute-eq btn "value" "Submit Form"))

; Test 6: Textarea element
(deftest "Textarea Element"
	(defq html "<textarea id=\"comment\" rows=\"5\" cols=\"40\">Initial text</textarea>")
	(defq doc (parse-html html))

	(defq textarea (. doc :get-element-by-id "comment"))
	(assert-not-nil textarea)
	(assert-element-attribute-eq textarea "rows" "5")
	(assert-element-attribute-eq textarea "cols" "40")
	(assert-element-text-contains textarea "Initial text"))

; Test 7: Input value can be read via attribute
(deftest "Input Value Read"
	(defq html "<input type=\"text\" id=\"name\" value=\"Alice\">")
	(defq doc (parse-html html))
	(defq input (. doc :get-element-by-id "name"))

	(assert-eq "Alice" (. input :get-attribute "value")))

; Test 8: Input value can be changed via set-attribute
(deftest "Input Value Write"
	(defq html "<input type=\"text\" id=\"name\" value=\"Alice\">")
	(defq doc (parse-html html))
	(defq input (. doc :get-element-by-id "name"))

	; Change value
	(. input :set-attribute "value" "Bob")
	(assert-eq "Bob" (. input :get-attribute "value")))

; Test 9: Button click triggers onclick handler
(deftest "Button Click Handler"
	(defq html "<button id=\"btn\" onclick=\"(defq clicked :t)\">Click</button>")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))

	; Click button
	(defq btn (. doc :get-element-by-id "btn"))
	(. btn :dispatch-event (env :type "click"))

	; Check handler executed
	(defq ctx (. executor :get-context))
	(assert-eq :t (. ctx :get-global "clicked")))

; Test 10: Input onchange handler
(deftest "Input Change Handler"
	(defq html "<input id=\"field\" onchange=\"(defq newval 'changed')\">")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq input (. doc :get-element-by-id "field"))

	; Trigger change
	(. input :dispatch-event (env :type "change"))

	(defq ctx (. executor :get-context))
	(assert-eq "changed" (. ctx :get-global "newval")))

; Test 11: Input onfocus handler
(deftest "Input Focus Handler"
	(defq html "<input id=\"field\" onfocus=\"(defq got_focus :t)\">")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq input (. doc :get-element-by-id "field"))

	; Trigger focus
	(. input :dispatch-event (env :type "focus"))

	(defq ctx (. executor :get-context))
	(assert-eq :t (. ctx :get-global "got_focus")))

; Test 12: Textarea onblur handler
(deftest "Textarea Blur Handler"
	(defq html "<textarea id=\"bio\" onblur=\"(defq lost_focus :t)\"></textarea>")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq textarea (. doc :get-element-by-id "bio"))

	; Trigger blur
	(. textarea :dispatch-event (env :type "blur"))

	(defq ctx (. executor :get-context))
	(assert-eq :t (. ctx :get-global "lost_focus")))

; Test 13: Form element with multiple inputs
(deftest "Form With Multiple Inputs"
	(defq html "
		<form id=\"myform\">
			<input type=\"text\" id=\"name\" value=\"Alice\">
			<input type=\"password\" id=\"pass\" value=\"secret\">
			<textarea id=\"bio\">Developer</textarea>
			<button type=\"submit\">Submit</button>
		</form>")
	(defq doc (parse-html html))

	; All inputs should exist
	(assert-not-nil (. doc :get-element-by-id "myform"))
	(assert-not-nil (. doc :get-element-by-id "name"))
	(assert-not-nil (. doc :get-element-by-id "pass"))
	(assert-not-nil (. doc :get-element-by-id "bio"))

	; Values should be correct
	(assert-eq "Alice" (. (. doc :get-element-by-id "name") :get-attribute "value"))
	(assert-eq "secret" (. (. doc :get-element-by-id "pass") :get-attribute "value")))

; Test 14: Form onsubmit handler
(deftest "Form Submit Handler"
	(defq html "<form id=\"myform\" onsubmit=\"(defq submitted :t)\">
		<input type=\"submit\" value=\"Go\">
		</form>")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq form (. doc :get-element-by-id "myform"))

	; Trigger submit
	(. form :dispatch-event (env :type "submit"))

	(defq ctx (. executor :get-context))
	(assert-eq :t (. ctx :get-global "submitted")))

; Test 15: Checkbox input
(deftest "Checkbox Element"
	(defq html "<input type=\"checkbox\" id=\"agree\" checked>")
	(defq doc (parse-html html))

	(defq checkbox (. doc :get-element-by-id "agree"))
	(assert-not-nil checkbox)
	(assert-element-attribute-eq checkbox "type" "checkbox")
	(assert-eq :t (. checkbox :has-attribute "checked")))

; Test 16: Checkbox can be toggled
(deftest "Checkbox Toggle"
	(defq html "<input type=\"checkbox\" id=\"agree\">")
	(defq doc (parse-html html))
	(defq checkbox (. doc :get-element-by-id "agree"))

	; Initially unchecked
	(assert-eq :nil (. checkbox :has-attribute "checked"))

	; Check it
	(. checkbox :set-attribute "checked" "checked")
	(assert-eq :t (. checkbox :has-attribute "checked"))

	; Uncheck it
	(. checkbox :remove-attribute "checked")
	(assert-eq :nil (. checkbox :has-attribute "checked")))

; Test 17: Radio button input
(deftest "Radio Button Element"
	(defq html "
		<input type=\"radio\" id=\"opt1\" name=\"choice\" value=\"a\" checked>
		<input type=\"radio\" id=\"opt2\" name=\"choice\" value=\"b\">")
	(defq doc (parse-html html))

	(defq radio1 (. doc :get-element-by-id "opt1"))
	(defq radio2 (. doc :get-element-by-id "opt2"))
	(assert-not-nil radio1)
	(assert-not-nil radio2)
	(assert-eq :t (. radio1 :has-attribute "checked"))
	(assert-eq :nil (. radio2 :has-attribute "checked"))
	(assert-eq "choice" (. radio1 :get-attribute "name"))
	(assert-eq "choice" (. radio2 :get-attribute "name")))

; Test 18: Select element with options
(deftest "Select Element"
	(defq html "
		<select id=\"country\">
			<option value=\"us\">United States</option>
			<option value=\"uk\" selected>United Kingdom</option>
			<option value=\"ca\">Canada</option>
		</select>")
	(defq doc (parse-html html))

	(defq select (. doc :get-element-by-id "country"))
	(assert-not-nil select)

	; Should have 3 option children
	(defq options (. doc :get-elements-by-tag-name "option"))
	(assert-eq 3 (length options)))

; Test 19: Input placeholder attribute
(deftest "Input Placeholder"
	(defq html "<input type=\"text\" id=\"name\" placeholder=\"Enter your name\">")
	(defq doc (parse-html html))
	(defq input (. doc :get-element-by-id "name"))

	(assert-element-attribute-eq input "placeholder" "Enter your name"))

; Test 20: Script can access and modify form values
(deftest "Script Access To Form Values"
	(defq html "
		<input type=\"text\" id=\"name\" value=\"Alice\">
		<script>
			(defq input (. document :get-element-by-id \"name\"))
			(defq old_value (. input :get-attribute \"value\"))
			(. input :set-attribute \"value\" \"Bob\")
			(defq new_value (. input :get-attribute \"value\"))
		</script>")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	; Check script read and wrote values
	(assert-eq "Alice" (. ctx :get-global "old_value"))
	(assert-eq "Bob" (. ctx :get-global "new_value"))

	; Verify actual DOM was modified
	(defq input (. doc :get-element-by-id "name"))
	(assert-eq "Bob" (. input :get-attribute "value")))

; Report test results
(test-report)
