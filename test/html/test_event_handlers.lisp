
;; Event Handler Tests (onmouseover, onmousedown, etc.)
;; TDD approach - tests first!

(import "lib/test/unittest.inc")
(import "lib/html/dom.inc")
(import "lib/html/parser.inc")
(import "lib/html/script.inc")

(deftest-suite "Event Handler Tests")

; Test 1: onmouseover handler
(deftest "onmouseover Handler"
	(defq html "<div id=\"box\" onmouseover=\"(defq hovered :t)\">Hover me</div>")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))

	; Trigger mouseover event
	(defq box (. doc :get-element-by-id "box"))
	(. box :dispatch-event (env :type "mouseover"))

	; Check variable was set
	(defq ctx (. executor :get-context))
	(assert-eq :t (. ctx :get-global "hovered")))

; Test 2: onmouseout handler
(deftest "onmouseout Handler"
	(defq html "<div id=\"box\" onmouseout=\"(defq left :t)\">Leave me</div>")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))

	; Trigger mouseout event
	(defq box (. doc :get-element-by-id "box"))
	(. box :dispatch-event (env :type "mouseout"))

	; Check variable was set
	(defq ctx (. executor :get-context))
	(assert-eq :t (. ctx :get-global "left")))

; Test 3: onmousedown handler
(deftest "onmousedown Handler"
	(defq html "<button id=\"btn\" onmousedown=\"(defq pressed :t)\">Press</button>")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))

	; Trigger mousedown event
	(defq btn (. doc :get-element-by-id "btn"))
	(. btn :dispatch-event (env :type "mousedown"))

	; Check variable was set
	(defq ctx (. executor :get-context))
	(assert-eq :t (. ctx :get-global "pressed")))

; Test 4: onmouseup handler
(deftest "onmouseup Handler"
	(defq html "<button id=\"btn\" onmouseup=\"(defq released :t)\">Release</button>")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))

	; Trigger mouseup event
	(defq btn (. doc :get-element-by-id "btn"))
	(. btn :dispatch-event (env :type "mouseup"))

	; Check variable was set
	(defq ctx (. executor :get-context))
	(assert-eq :t (. ctx :get-global "released")))

; Test 5: onchange handler
(deftest "onchange Handler"
	(defq html "<input id=\"field\" onchange=\"(defq changed :t)\">")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))

	; Trigger change event
	(defq field (. doc :get-element-by-id "field"))
	(. field :dispatch-event (env :type "change"))

	; Check variable was set
	(defq ctx (. executor :get-context))
	(assert-eq :t (. ctx :get-global "changed")))

; Test 6: onfocus handler
(deftest "onfocus Handler"
	(defq html "<input id=\"field\" onfocus=\"(defq focused :t)\">")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))

	; Trigger focus event
	(defq field (. doc :get-element-by-id "field"))
	(. field :dispatch-event (env :type "focus"))

	; Check variable was set
	(defq ctx (. executor :get-context))
	(assert-eq :t (. ctx :get-global "focused")))

; Test 7: onblur handler
(deftest "onblur Handler"
	(defq html "<input id=\"field\" onblur=\"(defq blurred :t)\">")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))

	; Trigger blur event
	(defq field (. doc :get-element-by-id "field"))
	(. field :dispatch-event (env :type "blur"))

	; Check variable was set
	(defq ctx (. executor :get-context))
	(assert-eq :t (. ctx :get-global "blurred")))

; Test 8: onsubmit handler
(deftest "onsubmit Handler"
	(defq html "<form id=\"myform\" onsubmit=\"(defq submitted :t)\"><input></form>")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))

	; Trigger submit event
	(defq form (. doc :get-element-by-id "myform"))
	(. form :dispatch-event (env :type "submit"))

	; Check variable was set
	(defq ctx (. executor :get-context))
	(assert-eq :t (. ctx :get-global "submitted")))

; Test 9: Multiple handlers on same element
(deftest "Multiple Event Handlers"
	(defq html "<div id=\"box\" onmouseover=\"(defq over :t)\" onmouseout=\"(defq out :t)\">Hover</div>")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq box (. doc :get-element-by-id "box"))
	(defq ctx (. executor :get-context))

	; Trigger mouseover
	(. box :dispatch-event (env :type "mouseover"))
	(assert-eq :t (. ctx :get-global "over"))

	; Trigger mouseout
	(. box :dispatch-event (env :type "mouseout"))
	(assert-eq :t (. ctx :get-global "out")))

; Test 10: Event handler with dialog function
(deftest "Event Handler With alert()"
	(defq html "<button id=\"btn\" onmouseover=\"(alert 'Mouse over!')\">Hover</button>")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))

	; Trigger mouseover event
	(defq btn (. doc :get-element-by-id "btn"))
	(. btn :dispatch-event (env :type "mouseover"))

	; Check alert was called
	(defq ctx (. executor :get-context))
	(defq win (. ctx 'window))
	(defq alerts (. win :get-alerts))
	(assert-eq 1 (length alerts))
	(assert-eq "Mouse over!" (first alerts)))
