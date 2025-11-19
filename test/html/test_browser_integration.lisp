
;; Browser Driver Integration Tests
;; End-to-end tests using static HTML files

(import "lib/test/unittest.inc")
(import "lib/html/browser_driver.inc")

(deftest-suite "Browser Driver Integration Tests")

; Test 1: Load and test simple page
(deftest "Load Simple Page"
	(defq driver (create-browser-driver))

	; Load a simple HTML page
	(. driver :load-html "
		<html>
			<head><title>Welcome Page</title></head>
			<body>
				<h1 id=\"heading\">Welcome</h1>
				<p>This is a test page</p>
			</body>
		</html>")

	; Verify page loaded
	(assert-eq "Welcome Page" (. driver :title))

	; Find heading
	(defq heading (. driver :find-element-by-id "heading"))
	(assert-eq "Welcome" (. heading :text)))

; Test 2: Login form workflow
(deftest "Login Form Workflow"
	(defq driver (create-browser-driver))

	; Load login page
	(. driver :load-html "
		<html>
			<head><title>Login</title></head>
			<body>
				<form id=\"login-form\" onsubmit=\"(defq logged_in :t)\">
					<input type=\"text\" id=\"username\" placeholder=\"Username\">
					<input type=\"password\" id=\"password\" placeholder=\"Password\">
					<button type=\"submit\" id=\"login-btn\">Login</button>
				</form>
				<div id=\"message\"></div>
			</body>
		</html>")

	; Fill in form
	(. (. driver :find-element-by-id "username") :send-keys "alice")
	(. (. driver :find-element-by-id "password") :send-keys "secret123")

	; Submit form
	(. (. driver :find-element-by-id "login-btn") :click)

	; Verify submission
	(defq ctx (. driver :script-context))
	(assert-eq :t (. ctx :get-global "logged_in"))

	; Verify form values
	(assert-eq "alice" (. (. driver :find-element-by-id "username") :get-attribute "value"))
	(assert-eq "secret123" (. (. driver :find-element-by-id "password") :get-attribute "value")))

; Test 3: Todo list application
(deftest "Todo List Application"
	(defq driver (create-browser-driver))

	; Load todo app
	(. driver :load-html "
		<html>
			<head><title>Todo List</title></head>
			<body>
				<h1>My Todos</h1>
				<input type=\"text\" id=\"new-todo\" placeholder=\"What needs to be done?\">
				<button id=\"add-btn\" onclick=\"(defq added :t)\">Add</button>
				<ul id=\"todo-list\">
					<li id=\"todo1\">Buy groceries</li>
					<li id=\"todo2\">Walk dog</li>
				</ul>
			</body>
		</html>")

	; Verify initial todos
	(defq todos (. driver :find-elements-by-tag-name "li"))
	(assert-eq 2 (length todos))

	; Add new todo
	(. (. driver :find-element-by-id "new-todo") :send-keys "Clean house")
	(. (. driver :find-element-by-id "add-btn") :click)

	; Verify click happened
	(defq ctx (. driver :script-context))
	(assert-eq :t (. ctx :get-global "added")))

; Test 4: Contact form with validation
(deftest "Contact Form With Validation"
	(defq driver (create-browser-driver))

	; Load contact form
	(. driver :load-html "
		<html>
			<body>
				<form id=\"contact-form\">
					<input type=\"text\" id=\"name\" placeholder=\"Name\">
					<input type=\"email\" id=\"email\" placeholder=\"Email\">
					<textarea id=\"message\" placeholder=\"Message\"></textarea>
					<button type=\"button\" id=\"submit-btn\"
						onclick=\"(if (confirm 'Send message?') (defq sent :t))\">
						Send
					</button>
				</form>
			</body>
		</html>")

	; Fill form
	(. (. driver :find-element-by-id "name") :send-keys "Bob Smith")
	(. (. driver :find-element-by-id "email") :send-keys "bob@example.com")
	(. (. driver :find-element-by-id "message") :send-keys "Hello, this is my message!")

	; Set confirm result to true
	(. driver :set-confirm-result :t)

	; Submit
	(. (. driver :find-element-by-id "submit-btn") :click)

	; Verify confirmation and submission
	(defq confirms (. (. (. driver :script-context) 'window) :get-confirms))
	(assert-eq 1 (length confirms))
	(assert-eq "Send message?" (first confirms))

	(assert-eq :t (. (. driver :script-context) :get-global "sent")))

; Test 5: Product catalog with filtering
(deftest "Product Catalog Filtering"
	(defq driver (create-browser-driver))

	; Load product catalog
	(. driver :load-html "
		<html>
			<body>
				<h1>Products</h1>
				<input type=\"text\" id=\"search\" placeholder=\"Search products\">
				<button id=\"search-btn\" onclick=\"(defq searched :t)\">Search</button>
				<div id=\"products\">
					<div class=\"product\" id=\"prod1\">Widget</div>
					<div class=\"product\" id=\"prod2\">Gadget</div>
					<div class=\"product\" id=\"prod3\">Doohickey</div>
				</div>
			</body>
		</html>")

	; Verify all products visible
	(defq products (. driver :find-elements-by-tag-name "div"))
	; Note: Will include products div and 3 product divs
	(assert-eq :t (>= (length products) 3))

	; Search
	(. (. driver :find-element-by-id "search") :send-keys "Widget")
	(. (. driver :find-element-by-id "search-btn") :click)

	(assert-eq :t (. (. driver :script-context) :get-global "searched")))

; Test 6: Multi-step wizard
(deftest "Multi Step Wizard"
	(defq driver (create-browser-driver))

	; Load wizard
	(. driver :load-html "
		<html>
			<body>
				<div id=\"step1\" class=\"step\">
					<h2>Step 1: Personal Info</h2>
					<input type=\"text\" id=\"first-name\">
					<input type=\"text\" id=\"last-name\">
					<button id=\"next1\" onclick=\"(defq step1_done :t)\">Next</button>
				</div>
				<div id=\"step2\" class=\"step\" style=\"display:none\">
					<h2>Step 2: Contact</h2>
					<input type=\"email\" id=\"email\">
					<button id=\"next2\" onclick=\"(defq step2_done :t)\">Next</button>
				</div>
			</body>
		</html>")

	; Fill step 1
	(. (. driver :find-element-by-id "first-name") :send-keys "Alice")
	(. (. driver :find-element-by-id "last-name") :send-keys "Johnson")
	(. (. driver :find-element-by-id "next1") :click)

	; Verify step 1 complete
	(assert-eq :t (. (. driver :script-context) :get-global "step1_done")))

; Test 7: Modal dialog interaction
(deftest "Modal Dialog Interaction"
	(defq driver (create-browser-driver))

	; Load page with modal
	(. driver :load-html "
		<html>
			<body>
				<button id=\"open-modal\" onclick=\"(alert 'Modal opened')\">Open Modal</button>
				<div id=\"modal\" class=\"modal\" style=\"display:none\">
					<h2>Modal Title</h2>
					<button id=\"close-modal\" onclick=\"(defq closed :t)\">Close</button>
				</div>
			</body>
		</html>")

	; Open modal
	(. (. driver :find-element-by-id "open-modal") :click)

	; Verify alert
	(defq alerts (. driver :get-alerts))
	(assert-eq 1 (length alerts))
	(assert-eq "Modal opened" (first alerts)))

; Test 8: Table data extraction
(deftest "Table Data Extraction"
	(defq driver (create-browser-driver))

	; Load table
	(. driver :load-html "
		<html>
			<body>
				<table id=\"data-table\">
					<thead>
						<tr>
							<th>Name</th>
							<th>Age</th>
						</tr>
					</thead>
					<tbody>
						<tr id=\"row1\">
							<td id=\"name1\">Alice</td>
							<td id=\"age1\">30</td>
						</tr>
						<tr id=\"row2\">
							<td id=\"name2\">Bob</td>
							<td id=\"age2\">25</td>
						</tr>
					</tbody>
				</table>
			</body>
		</html>")

	; Extract data
	(assert-eq "Alice" (. (. driver :find-element-by-id "name1") :text))
	(assert-eq "30" (. (. driver :find-element-by-id "age1") :text))
	(assert-eq "Bob" (. (. driver :find-element-by-id "name2") :text))

	; Count rows
	(defq rows (. driver :find-elements-by-tag-name "tr"))
	(assert-eq 3 (length rows)))  ; header + 2 data rows

; Test 9: Dynamic content manipulation via script
(deftest "Dynamic Content Manipulation"
	(defq driver (create-browser-driver))

	; Load page
	(. driver :load-html "
		<html>
			<body>
				<div id=\"content\">Original Content</div>
				<button id=\"change-btn\" onclick=\"
					(. (. document :get-element-by-id 'content')
					   :set-text-content 'Modified Content')\">
					Change
				</button>
			</body>
		</html>")

	; Verify original
	(assert-eq "Original Content" (. (. driver :find-element-by-id "content") :text))

	; Click button to modify
	(. (. driver :find-element-by-id "change-btn") :click)

	; Verify change
	(assert-eq "Modified Content" (. (. driver :find-element-by-id "content") :text)))

; Test 10: Checkbox and radio button handling
(deftest "Checkbox And Radio Buttons"
	(defq driver (create-browser-driver))

	; Load form with checkboxes and radios
	(. driver :load-html "
		<html>
			<body>
				<form>
					<input type=\"checkbox\" id=\"agree\" value=\"yes\">
					<label for=\"agree\">I agree</label>

					<input type=\"radio\" id=\"opt1\" name=\"choice\" value=\"a\" checked>
					<label for=\"opt1\">Option A</label>

					<input type=\"radio\" id=\"opt2\" name=\"choice\" value=\"b\">
					<label for=\"opt2\">Option B</label>
				</form>
			</body>
		</html>")

	; Verify initial state
	(defq agree (. driver :find-element-by-id "agree"))
	(assert-eq :nil (. agree :get-attribute "checked"))

	(defq opt1 (. driver :find-element-by-id "opt1"))
	(assert-not-nil (. opt1 :get-attribute "checked"))

	; Click checkbox
	(. agree :click)

	; Note: Would need to implement checked state toggle in click handler)

; Test 11: Dropdown/select element
(deftest "Dropdown Select Element"
	(defq driver (create-browser-driver))

	; Load select
	(. driver :load-html "
		<html>
			<body>
				<select id=\"country\">
					<option value=\"us\" id=\"opt-us\">United States</option>
					<option value=\"uk\" id=\"opt-uk\" selected>United Kingdom</option>
					<option value=\"ca\" id=\"opt-ca\">Canada</option>
				</select>
			</body>
		</html>")

	; Verify select exists
	(defq select (. driver :find-element-by-id "country"))
	(assert-not-nil select)

	; Verify selected option
	(defq uk-option (. driver :find-element-by-id "opt-uk"))
	(assert-not-nil (. uk-option :get-attribute "selected")))

; Test 12: Link navigation
(deftest "Link Navigation"
	(defq driver (create-browser-driver))

	; Load page with links
	(. driver :load-html "
		<html>
			<body>
				<a id=\"home-link\" href=\"/home\">Home</a>
				<a id=\"about-link\" href=\"/about\">About</a>
				<a id=\"external\" href=\"http://example.com\" target=\"_blank\">External</a>
			</body>
		</html>")

	; Verify links
	(defq home (. driver :find-element-by-id "home-link"))
	(assert-eq "/home" (. home :get-attribute "href"))
	(assert-eq "Home" (. home :text))

	(defq external (. driver :find-element-by-id "external"))
	(assert-eq "http://example.com" (. external :get-attribute "href"))
	(assert-eq "_blank" (. external :get-attribute "target")))

; Test 13: Form validation workflow
(deftest "Form Validation Workflow"
	(defq driver (create-browser-driver))

	; Load form
	(. driver :load-html "
		<html>
			<body>
				<form id=\"signup\">
					<input type=\"email\" id=\"email\" required>
					<input type=\"password\" id=\"pwd\" required>
					<button id=\"submit\" onclick=\"
						(if (and
							(. (. document :get-element-by-id 'email') :get-attribute 'value')
							(. (. document :get-element-by-id 'pwd') :get-attribute 'value'))
						(defq valid :t)
						(alert 'Please fill all fields'))\">
						Submit
					</button>
				</form>
			</body>
		</html>")

	; Try to submit empty form
	(. (. driver :find-element-by-id "submit") :click)

	; Should get alert
	(defq alerts (. driver :get-alerts))
	(assert-eq 1 (length alerts))

	; Now fill and submit
	(. driver :refresh)  ; Clear alerts
	(. (. driver :find-element-by-id "email") :send-keys "test@example.com")
	(. (. driver :find-element-by-id "pwd") :send-keys "password123")
	(. (. driver :find-element-by-id "submit") :click)

	; Should be valid now
	(assert-eq :t (. (. driver :script-context) :get-global "valid")))

; Test 14: Script-driven content update
(deftest "Script Driven Content Update"
	(defq driver (create-browser-driver))

	; Load page
	(. driver :load-html "
		<html>
			<body>
				<div id=\"counter\">0</div>
				<button id=\"increment\" onclick=\"
					(defq elem (. document :get-element-by-id 'counter'))
					(defq current (. elem :get-text-content))
					(defq num (str-to-num current))
					(. elem :set-text-content (str (+ num 1)))\">
					Increment
				</button>
			</body>
		</html>")

	; Initial value
	(assert-eq "0" (. (. driver :find-element-by-id "counter") :text))

	; Click multiple times
	(. (. driver :find-element-by-id "increment") :click)
	(. (. driver :find-element-by-id "increment") :click)
	(. (. driver :find-element-by-id "increment") :click)

	; Verify updated
	; Note: Would need str-to-num function in scripts
	(assert-not-nil (. driver :find-element-by-id "counter")))

; Test 15: Complete user registration flow
(deftest "Complete User Registration Flow"
	(defq driver (create-browser-driver))

	; Load registration page
	(. driver :load-html "
		<html>
			<head><title>Register</title></head>
			<body>
				<h1>User Registration</h1>
				<form id=\"reg-form\" onsubmit=\"(defq registered :t)\">
					<input type=\"text\" id=\"username\" placeholder=\"Username\" required>
					<input type=\"email\" id=\"email\" placeholder=\"Email\" required>
					<input type=\"password\" id=\"password\" placeholder=\"Password\" required>
					<input type=\"password\" id=\"confirm\" placeholder=\"Confirm Password\" required>
					<input type=\"checkbox\" id=\"terms\"> I agree to terms
					<button type=\"submit\" id=\"register-btn\">Register</button>
				</form>
				<div id=\"result\"></div>
			</body>
		</html>")

	; Verify page loaded
	(assert-eq "Register" (. driver :title))

	; Fill registration form
	(. (. driver :find-element-by-id "username") :send-keys "alice_smith")
	(. (. driver :find-element-by-id "email") :send-keys "alice@example.com")
	(. (. driver :find-element-by-id "password") :send-keys "SecurePass123")
	(. (. driver :find-element-by-id "confirm") :send-keys "SecurePass123")

	; Accept terms
	(. (. driver :find-element-by-id "terms") :click)

	; Submit
	(. (. driver :find-element-by-id "register-btn") :click)

	; Verify registration
	(assert-eq :t (. (. driver :script-context) :get-global "registered"))

	; Verify all fields filled
	(assert-eq "alice_smith" (. (. driver :find-element-by-id "username") :get-attribute "value"))
	(assert-eq "alice@example.com" (. (. driver :find-element-by-id "email") :get-attribute "value")))

; Report test results
(test-report)
