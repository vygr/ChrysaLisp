
;; Comprehensive DOM assertion tests
;; Demonstrates all DOM-specific assertions for HTML testing

(import "lib/test/unittest.inc")
(import "lib/html/parser.inc")
(import "lib/html/dom.inc")

(deftest-suite "DOM Assertion Tests")

(defun main ()
	(run-test-suite
		; Document validation
		(deftest "Document Validation"
			(defq html "<html><body><p>Test</p></body></html>")
			(defq doc (parse-html html))
			(assert-document-valid doc)
			(assert-not-nil doc))

		; Element tag assertions
		(deftest "Element Tag Names"
			(defq html "<div><p>Paragraph</p><span>Span</span></div>")
			(defq doc (parse-html html))
			(defq divs (. doc :get-elements-by-tag-name "div"))
			(assert-eq 1 (length divs))
			(defq div (first divs))
			(assert-element-tag div "div"))

		; Attribute assertions
		(deftest "Element Attributes"
			(defq html "<div id=\"main\" class=\"container active\" data-value=\"123\">Content</div>")
			(defq doc (parse-html html))
			(defq div (first (. doc :get-elements-by-tag-name "div")))

			; Check attribute existence
			(assert-element-has-attribute div "id")
			(assert-element-has-attribute div "class")
			(assert-element-has-attribute div "data-value")

			; Check attribute values
			(assert-element-attribute-eq div "id" "main")
			(assert-element-attribute-eq div "data-value" "123")
			(assert-element-id div "main"))

		; CSS class assertions
		(deftest "Element CSS Classes"
			(defq html "<div class=\"container active highlight\">Content</div>")
			(defq doc (parse-html html))
			(defq div (first (. doc :get-elements-by-tag-name "div")))

			(assert-element-has-class div "container")
			(assert-element-has-class div "active")
			(assert-element-has-class div "highlight"))

		; Child count assertions
		(deftest "Element Child Count"
			(defq html "<ul><li>One</li><li>Two</li><li>Three</li></ul>")
			(defq doc (parse-html html))
			(defq ul (first (. doc :get-elements-by-tag-name "ul")))

			(assert-element-has-children ul)
			(assert-element-child-count ul 3))

		(deftest "Element No Children"
			(defq html "<p><br></p>")
			(defq doc (parse-html html))
			(defq br (first (. doc :get-elements-by-tag-name "br")))
			(assert-element-no-children br))

		; Text content assertions
		(deftest "Element Text Content"
			(defq html "<p>Hello <strong>World</strong>!</p>")
			(defq doc (parse-html html))
			(defq p (first (. doc :get-elements-by-tag-name "p")))

			(assert-element-text-contains p "Hello")
			(assert-element-text-contains p "World")
			(assert-element-text-contains p "Hello World!"))

		; Elements count by tag
		(deftest "Count Elements by Tag"
			(defq html "
				<div>
					<p>First</p>
					<p>Second</p>
					<p>Third</p>
					<span>Span</span>
				</div>")
			(defq doc (parse-html html))

			(assert-elements-count doc "p" 3)
			(assert-elements-count doc "span" 1)
			(assert-elements-count doc "div" 1))

		; Parent-child relationships
		(deftest "Parent-Child Relationships"
			(defq html "<div><p>Paragraph</p></div>")
			(defq doc (parse-html html))
			(defq div (first (. doc :get-elements-by-tag-name "div")))
			(defq p (first (. doc :get-elements-by-tag-name "p")))

			(assert-element-has-parent p)
			(assert-element-parent p div))

		; Complex DOM structure
		(deftest "Complex DOM Structure"
			(defq html "
				<html>
					<body>
						<div id=\"header\" class=\"top\">
							<h1>Title</h1>
							<nav>
								<a href=\"/home\">Home</a>
								<a href=\"/about\">About</a>
							</nav>
						</div>
						<div id=\"content\" class=\"main\">
							<article>
								<h2>Article Title</h2>
								<p>First paragraph</p>
								<p>Second paragraph</p>
							</article>
						</div>
					</body>
				</html>")
			(defq doc (parse-html html))

			; Test header section
			(defq header (. doc :get-element-by-id "header"))
			(assert-element-id header "header")
			(assert-element-has-class header "top")
			(assert-element-has-children header)

			; Test navigation links
			(defq links (. doc :get-elements-by-tag-name "a"))
			(assert-eq 2 (length links))
			(assert-element-attribute-eq (first links) "href" "/home")
			(assert-element-attribute-eq (second links) "href" "/about")

			; Test content section
			(defq content (. doc :get-element-by-id "content"))
			(assert-element-id content "content")
			(assert-element-has-class content "main")

			; Test article structure
			(defq articles (. doc :get-elements-by-tag-name "article"))
			(assert-eq 1 (length articles))
			(defq article (first articles))
			(assert-element-has-children article)

			; Test paragraphs in article
			(defq paras (. doc :get-elements-by-tag-name "p"))
			(assert-eq 2 (length paras))
			(assert-element-text-contains (first paras) "First paragraph")
			(assert-element-text-contains (second paras) "Second paragraph"))

		; Nested elements
		(deftest "Deeply Nested Elements"
			(defq html "
				<div id=\"outer\">
					<div id=\"middle\">
						<div id=\"inner\">
							<p>Deep content</p>
						</div>
					</div>
				</div>")
			(defq doc (parse-html html))

			(defq outer (. doc :get-element-by-id "outer"))
			(defq middle (. doc :get-element-by-id "middle"))
			(defq inner (. doc :get-element-by-id "inner"))

			(assert-element-id outer "outer")
			(assert-element-id middle "middle")
			(assert-element-id inner "inner")

			; Check parent-child relationships
			(assert-element-has-parent middle)
			(assert-element-parent middle outer)
			(assert-element-parent inner middle)

			; Check content
			(defq p (first (. doc :get-elements-by-tag-name "p")))
			(assert-element-text-contains p "Deep content")
			(assert-element-parent p inner))

		; Table structure
		(deftest "Table DOM Structure"
			(defq html "
				<table id=\"data-table\">
					<tr>
						<th>Header 1</th>
						<th>Header 2</th>
					</tr>
					<tr>
						<td>Cell 1</td>
						<td>Cell 2</td>
					</tr>
				</table>")
			(defq doc (parse-html html))

			(defq table (. doc :get-element-by-id "data-table"))
			(assert-element-id table "data-table")
			(assert-element-tag table "table")

			(defq rows (. doc :get-elements-by-tag-name "tr"))
			(assert-eq 2 (length rows))

			(defq headers (. doc :get-elements-by-tag-name "th"))
			(assert-eq 2 (length headers))

			(defq cells (. doc :get-elements-by-tag-name "td"))
			(assert-eq 2 (length cells)))

		; Form elements
		(deftest "Form DOM Structure"
			(defq html "
				<form id=\"login-form\" action=\"/login\" method=\"post\">
					<input type=\"text\" name=\"username\" id=\"user\" />
					<input type=\"password\" name=\"password\" id=\"pass\" />
					<button type=\"submit\">Login</button>
				</form>")
			(defq doc (parse-html html))

			(defq form (. doc :get-element-by-id "login-form"))
			(assert-element-id form "login-form")
			(assert-element-tag form "form")
			(assert-element-attribute-eq form "action" "/login")
			(assert-element-attribute-eq form "method" "post")

			(defq inputs (. doc :get-elements-by-tag-name "input"))
			(assert-eq 2 (length inputs))

			(defq username (. doc :get-element-by-id "user"))
			(assert-element-attribute-eq username "type" "text")
			(assert-element-attribute-eq username "name" "username")

			(defq password (. doc :get-element-by-id "pass"))
			(assert-element-attribute-eq password "type" "password"))

		; List structure
		(deftest "List DOM Structure"
			(defq html "
				<ul id=\"menu\">
					<li class=\"menu-item\">Home</li>
					<li class=\"menu-item active\">Products</li>
					<li class=\"menu-item\">Contact</li>
				</ul>")
			(defq doc (parse-html html))

			(defq menu (. doc :get-element-by-id "menu"))
			(assert-element-id menu "menu")

			(defq items (. doc :get-elements-by-tag-name "li"))
			(assert-eq 3 (length items))

			; Check all have menu-item class
			(each! 0 -1 (lambda (item)
				(assert-element-has-class item "menu-item"))
				items)

			; Check active class on second item
			(assert-element-has-class (second items) "active"))
	))
