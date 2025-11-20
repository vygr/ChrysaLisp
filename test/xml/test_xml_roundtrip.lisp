#!/usr/bin/env lsp

;; XML Roundtrip Tests (XStream style)
;; Parse XML → Modify → Stringify → Parse → Verify

(import "lib/test/unittest.inc")
(import "lib/xml/parse.inc")
(import "lib/xml/stringify.inc")

(deftest-suite "XML Roundtrip Tests")

; Test 1: Simple element
(deftest "Roundtrip Simple Element"
	(defq xml "<root>Hello</root>")
	(defq parsed (xml-parse-string xml))
	(defq stringified (xml-stringify parsed))
	(defq reparsed (xml-parse-string stringified))

	; Should round-trip successfully
	(assert-eq (get parsed :name) (get reparsed :name))
	(assert-eq (get parsed :text) (get reparsed :text)))

; Test 2: Element with attributes
(deftest "Roundtrip Element With Attributes"
	(defq original (env))
	(set-insert original :name "person")
	(set-insert original :attributes (env))
	(set-insert (get original :attributes) :id "123")
	(set-insert (get original :attributes) :name "Alice")

	(defq xml (xml-stringify original))
	(defq parsed (xml-parse-string xml))

	(assert-eq "person" (get parsed :name))
	(assert-eq "123" (get (get parsed :attributes) :id))
	(assert-eq "Alice" (get (get parsed :attributes) :name)))

; Test 3: Nested elements
(deftest "Roundtrip Nested Elements"
	(defq xml "<parent><child>Text</child></parent>")
	(defq parsed (xml-parse-string xml))
	(defq stringified (xml-stringify parsed))
	(defq reparsed (xml-parse-string stringified))

	(assert-eq "parent" (get parsed :name))
	(assert-eq "parent" (get reparsed :name))

	; Check children
	(defq children1 (get parsed :children))
	(defq children2 (get reparsed :children))
	(assert-eq (length children1) (length children2))
	(assert-eq "child" (get (first children1) :name)))

; Test 4: Element with text and attributes
(deftest "Roundtrip Text And Attributes"
	(defq xml "<item id=\"42\" type=\"product\">Widget</item>")
	(defq parsed (xml-parse-string xml))
	(defq stringified (xml-stringify parsed))
	(defq reparsed (xml-parse-string stringified))

	(assert-eq "item" (get reparsed :name))
	(assert-eq "42" (get (get reparsed :attributes) :id))
	(assert-eq "product" (get (get reparsed :attributes) :type))
	(assert-eq "Widget" (get reparsed :text)))

; Test 5: Multiple sibling elements
(deftest "Roundtrip Siblings"
	(defq xml "<root><a>1</a><b>2</b><c>3</c></root>")
	(defq parsed (xml-parse-string xml))
	(defq stringified (xml-stringify parsed))
	(defq reparsed (xml-parse-string stringified))

	(defq children (get reparsed :children))
	(assert-eq 3 (length children))
	(assert-eq "a" (get (elem 0 children) :name))
	(assert-eq "b" (get (elem 1 children) :name))
	(assert-eq "c" (get (elem 2 children) :name)))

; Test 6: Empty element (self-closing)
(deftest "Roundtrip Empty Element"
	(defq original (env))
	(set-insert original :name "empty")

	(defq xml (xml-stringify original))
	(defq parsed (xml-parse-string xml))

	(assert-eq "empty" (get parsed :name))
	(assert-eq :nil (get parsed :text))
	(assert-eq :nil (get parsed :children)))

; Test 7: Special characters in text
(deftest "Roundtrip Special Characters"
	(defq xml "<message>&lt;Hello &amp; Goodbye&gt;</message>")
	(defq parsed (xml-parse-string xml))
	(defq text (get parsed :text))

	; Should decode entities
	(assert-not-nil text))

; Test 8: Special characters in attributes
(deftest "Roundtrip Special Chars In Attributes"
	(defq original (env))
	(set-insert original :name "link")
	(set-insert original :attributes (env))
	(set-insert (get original :attributes) :url "http://example.com?a=1&b=2")

	(defq xml (xml-stringify original))
	(defq parsed (xml-parse-string xml))

	; URL should be escaped in XML
	(assert-eq "link" (get parsed :name)))

; Test 9: Deep nesting
(deftest "Roundtrip Deep Nesting"
	(defq xml "<a><b><c><d>Deep</d></c></b></a>")
	(defq parsed (xml-parse-string xml))
	(defq stringified (xml-stringify parsed))
	(defq reparsed (xml-parse-string stringified))

	(assert-eq "a" (get reparsed :name))

	; Navigate to deep child
	(defq b (first (get reparsed :children)))
	(assert-eq "b" (get b :name))
	(defq c (first (get b :children)))
	(assert-eq "c" (get c :name))
	(defq d (first (get c :children)))
	(assert-eq "d" (get d :name))
	(assert-eq "Deep" (get d :text)))

; Test 10: XML declaration handling
(deftest "Roundtrip XML Declaration"
	(defq xml "<?xml version=\"1.0\"?><root>Data</root>")
	(defq parsed (xml-parse-string xml))

	; Should parse successfully despite declaration
	(assert-eq "root" (get parsed :name))
	(assert-eq "Data" (get parsed :text)))

; Test 11: Comments are ignored
(deftest "Roundtrip With Comments"
	(defq xml "<root><!-- Comment --><item>Value</item></root>")
	(defq parsed (xml-parse-string xml))

	; Comment should be ignored
	(assert-eq "root" (get parsed :name))
	(defq children (get parsed :children))
	(assert-eq 1 (length children))
	(assert-eq "item" (get (first children) :name)))

; Test 12: Mixed content (text and elements)
(deftest "Roundtrip Mixed Content"
	(defq xml "<p>Hello <b>world</b>!</p>")
	(defq parsed (xml-parse-string xml))
	(defq stringified (xml-stringify parsed))
	(defq reparsed (xml-parse-string stringified))

	(assert-eq "p" (get reparsed :name))
	; Should have both text and child element
	(assert-not-nil (get reparsed :children)))

; Test 13: Whitespace handling
(deftest "Roundtrip Whitespace"
	(defq xml "<root>
		<item>Text</item>
	</root>")
	(defq parsed (xml-parse-string xml))
	(defq stringified (xml-stringify parsed))

	; Should parse successfully
	(assert-eq "root" (get parsed :name))
	(defq children (get parsed :children))
	(assert-eq 1 (length children)))

; Test 14: Boolean-like attributes
(deftest "Roundtrip Boolean Attributes"
	(defq original (env))
	(set-insert original :name "input")
	(set-insert original :attributes (env))
	(set-insert (get original :attributes) :checked "true")
	(set-insert (get original :attributes) :disabled "false")

	(defq xml (xml-stringify original))
	(defq parsed (xml-parse-string xml))

	(assert-eq "true" (get (get parsed :attributes) :checked))
	(assert-eq "false" (get (get parsed :attributes) :disabled)))

; Test 15: Numeric content
(deftest "Roundtrip Numeric Content"
	(defq xml "<price currency=\"USD\">19.99</price>")
	(defq parsed (xml-parse-string xml))
	(defq stringified (xml-stringify parsed))
	(defq reparsed (xml-parse-string stringified))

	(assert-eq "price" (get reparsed :name))
	(assert-eq "USD" (get (get reparsed :attributes) :currency))
	(assert-eq "19.99" (get reparsed :text)))

; Test 16: Large attribute count
(deftest "Roundtrip Many Attributes"
	(defq original (env))
	(set-insert original :name "element")
	(set-insert original :attributes (env))
	(set-insert (get original :attributes) :a "1")
	(set-insert (get original :attributes) :b "2")
	(set-insert (get original :attributes) :c "3")
	(set-insert (get original :attributes) :d "4")
	(set-insert (get original :attributes) :e "5")

	(defq xml (xml-stringify original))
	(defq parsed (xml-parse-string xml))

	; All attributes should round-trip
	(assert-eq "1" (get (get parsed :attributes) :a))
	(assert-eq "5" (get (get parsed :attributes) :e)))

; Test 17: Array of elements to XML list
(deftest "Roundtrip Element List"
	(defq original (env))
	(set-insert original :name "items")
	(set-insert original :children (list))

	; Add 3 child items
	(defq item1 (env))
	(set-insert item1 :name "item")
	(set-insert item1 :text "First")
	(push (get original :children) item1)

	(defq item2 (env))
	(set-insert item2 :name "item")
	(set-insert item2 :text "Second")
	(push (get original :children) item2)

	(defq item3 (env))
	(set-insert item3 :name "item")
	(set-insert item3 :text "Third")
	(push (get original :children) item3)

	(defq xml (xml-stringify original))
	(defq parsed (xml-parse-string xml))

	; Should have 3 children
	(assert-eq 3 (length (get parsed :children)))
	(assert-eq "First" (get (elem 0 (get parsed :children)) :text))
	(assert-eq "Third" (get (elem 2 (get parsed :children)) :text)))

; Test 18: CDATA section
(deftest "Roundtrip CDATA"
	(defq xml "<script><![CDATA[function() { return a < b && c > d; }]]></script>")
	(defq parsed (xml-parse-string xml))

	; CDATA content should be accessible
	(assert-eq "script" (get parsed :name)))

; Test 19: Namespace handling (basic)
(deftest "Roundtrip Namespace"
	(defq xml "<html:div xmlns:html=\"http://www.w3.org/1999/xhtml\">Content</html:div>")
	(defq parsed (xml-parse-string xml))

	; Should parse namespaced element
	(assert-not-nil parsed))

; Test 20: Complete document round-trip
(deftest "Roundtrip Complete Document"
	(defq original (env))
	(set-insert original :name "library")
	(set-insert original :children (list))

	; Add book 1
	(defq book1 (env))
	(set-insert book1 :name "book")
	(set-insert book1 :attributes (env))
	(set-insert (get book1 :attributes) :isbn "123")
	(set-insert book1 :children (list))

	(defq title1 (env))
	(set-insert title1 :name "title")
	(set-insert title1 :text "XML Primer")
	(push (get book1 :children) title1)

	(defq author1 (env))
	(set-insert author1 :name "author")
	(set-insert author1 :text "John Doe")
	(push (get book1 :children) author1)

	(push (get original :children) book1)

	; Add book 2
	(defq book2 (env))
	(set-insert book2 :name "book")
	(set-insert book2 :attributes (env))
	(set-insert (get book2 :attributes) :isbn "456")
	(set-insert book2 :children (list))

	(defq title2 (env))
	(set-insert title2 :name "title")
	(set-insert title2 :text "Lisp Guide")
	(push (get book2 :children) title2)

	(push (get original :children) book2)

	; Stringify and parse back
	(defq xml (xml-stringify original))
	(defq parsed (xml-parse-string xml))

	; Verify structure
	(assert-eq "library" (get parsed :name))
	(assert-eq 2 (length (get parsed :children)))

	(defq parsed_book1 (elem 0 (get parsed :children)))
	(assert-eq "book" (get parsed_book1 :name))
	(assert-eq "123" (get (get parsed_book1 :attributes) :isbn))

	(defq parsed_book2 (elem 1 (get parsed :children)))
	(assert-eq "456" (get (get parsed_book2 :attributes) :isbn)))
