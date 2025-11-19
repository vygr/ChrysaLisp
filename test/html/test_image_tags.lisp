
;; Image Tag Tests (<img> support)
;; TDD approach - tests first!

(import "lib/test/unittest.inc")
(import "lib/html/dom.inc")
(import "lib/html/parser.inc")

(deftest-suite "Image Tag Tests")

; Test 1: Parse <img> tag
(deftest "Parse IMG Tag"
	(defq html "<img src=\"logo.cpm\" alt=\"Logo\">")
	(defq doc (parse-html html))

	(defq img (. doc :get-element-by-id "logo"))
	; Even without id, should find by tag
	(defq imgs (. doc :get-elements-by-tag-name "img"))
	(assert-eq 1 (length imgs))

	(defq img (first imgs))
	(assert-eq "logo.cpm" (. img :get-attribute "src"))
	(assert-eq "Logo" (. img :get-attribute "alt")))

; Test 2: IMG with width and height
(deftest "IMG With Dimensions"
	(defq html "<img src=\"photo.tga\" width=\"320\" height=\"240\" alt=\"Photo\">")
	(defq doc (parse-html html))

	(defq imgs (. doc :get-elements-by-tag-name "img"))
	(defq img (first imgs))

	(assert-eq "photo.tga" (. img :get-attribute "src"))
	(assert-eq "320" (. img :get-attribute "width"))
	(assert-eq "240" (. img :get-attribute "height"))
	(assert-eq "Photo" (. img :get-attribute "alt")))

; Test 3: Multiple images
(deftest "Multiple Images"
	(defq html "
		<div>
			<img src=\"image1.cpm\" alt=\"First\">
			<img src=\"image2.tga\" alt=\"Second\">
			<img src=\"image3.svg\" alt=\"Third\">
		</div>")
	(defq doc (parse-html html))

	(defq imgs (. doc :get-elements-by-tag-name "img"))
	(assert-eq 3 (length imgs))

	(assert-eq "image1.cpm" (. (elem 0 imgs) :get-attribute "src"))
	(assert-eq "image2.tga" (. (elem 1 imgs) :get-attribute "src"))
	(assert-eq "image3.svg" (. (elem 2 imgs) :get-attribute "src")))

; Test 4: IMG with ID
(deftest "IMG With ID"
	(defq html "<img id=\"logo\" src=\"logo.cpm\" alt=\"Logo\">")
	(defq doc (parse-html html))

	(defq img (. doc :get-element-by-id "logo"))
	(assert-not-nil img)
	(assert-eq "logo.cpm" (. img :get-attribute "src")))

; Test 5: IMG inside link
(deftest "IMG Inside Link"
	(defq html "<a href=\"page.html\"><img src=\"icon.cpm\" alt=\"Icon\"></a>")
	(defq doc (parse-html html))

	(defq links (. doc :get-elements-by-tag-name "a"))
	(assert-eq 1 (length links))

	(defq link (first links))
	(defq children (. link 'child_nodes))

	; Should have img child
	(defq has-img :nil)
	(each! 0 -1 (lambda (child)
		(when (and (= (. child 'node_type) NODE_ELEMENT)
				   (eql (. child 'tag_name) "img"))
			(setq has-img :t)))
		children)

	(assert-eq :t has-img))

; Test 6: IMG in table cell
(deftest "IMG In Table"
	(defq html "
		<table>
			<tr>
				<td><img src=\"product1.cpm\" alt=\"Product 1\"></td>
				<td><img src=\"product2.cpm\" alt=\"Product 2\"></td>
			</tr>
		</table>")
	(defq doc (parse-html html))

	(defq imgs (. doc :get-elements-by-tag-name "img"))
	(assert-eq 2 (length imgs)))

; Test 7: IMG with title attribute
(deftest "IMG With Title"
	(defq html "<img src=\"info.cpm\" alt=\"Info\" title=\"Click for more info\">")
	(defq doc (parse-html html))

	(defq imgs (. doc :get-elements-by-tag-name "img"))
	(defq img (first imgs))

	(assert-eq "Click for more info" (. img :get-attribute "title")))

; Test 8: IMG with onclick handler
(deftest "IMG With Onclick"
	(defq html "<img id=\"clickable\" src=\"button.cpm\" alt=\"Button\" onclick=\"(defq clicked :t)\">")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))

	; Click the image
	(defq img (. doc :get-element-by-id "clickable"))
	(. img :dispatch-event (env :type "click"))

	; Verify handler executed
	(defq ctx (. executor :get-context))
	(assert-eq :t (. ctx :get-global "clicked")))

; Test 9: IMG with class
(deftest "IMG With Class"
	(defq html "<img src=\"styled.cpm\" class=\"thumbnail\" alt=\"Thumbnail\">")
	(defq doc (parse-html html))

	(defq imgs (. doc :get-elements-by-tag-name "img"))
	(defq img (first imgs))

	(assert-eq :t (. img :has-class "thumbnail")))

; Test 10: IMG without alt (should still parse)
(deftest "IMG Without Alt"
	(defq html "<img src=\"image.cpm\">")
	(defq doc (parse-html html))

	(defq imgs (. doc :get-elements-by-tag-name "img"))
	(assert-eq 1 (length imgs)))

; Test 11: IMG with relative path
(deftest "IMG With Relative Path"
	(defq html "<img src=\"images/photo.tga\" alt=\"Photo\">")
	(defq doc (parse-html html))

	(defq imgs (. doc :get-elements-by-tag-name "img"))
	(defq img (first imgs))

	(assert-eq "images/photo.tga" (. img :get-attribute "src")))

; Test 12: IMG with absolute file:// URL
(deftest "IMG With File URL"
	(defq html "<img src=\"file:///home/user/image.cpm\" alt=\"Absolute\">")
	(defq doc (parse-html html))

	(defq imgs (. doc :get-elements-by-tag-name "img"))
	(defq img (first imgs))

	(assert-eq "file:///home/user/image.cpm" (. img :get-attribute "src")))

; Test 13: Script can access IMG attributes
(deftest "Script Access IMG Attributes"
	(defq html "
		<img id=\"myimg\" src=\"test.cpm\" alt=\"Test\">
		<script>
			(defq img (. document :get-element-by-id \"myimg\"))
			(defq src (. img :get-attribute \"src\"))
			(defq alt (. img :get-attribute \"alt\"))
		</script>")
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	; Verify script read attributes
	(assert-eq "test.cpm" (. ctx :get-global "src"))
	(assert-eq "Test" (. ctx :get-global "alt")))

; Test 14: Script can modify IMG src
(deftest "Script Modify IMG Src"
	(defq html "
		<img id=\"myimg\" src=\"old.cpm\" alt=\"Image\">
		<script>
			(defq img (. document :get-element-by-id \"myimg\"))
			(. img :set-attribute \"src\" \"new.cpm\")
		</script>")
	(defq doc (parse-html html))
	(execute-document-scripts doc)

	; Verify src changed
	(defq img (. doc :get-element-by-id "myimg"))
	(assert-eq "new.cpm" (. img :get-attribute "src")))

; Test 15: IMG in list
(deftest "IMG In List"
	(defq html "
		<ul>
			<li><img src=\"icon1.cpm\" alt=\"Icon 1\"> Item 1</li>
			<li><img src=\"icon2.cpm\" alt=\"Icon 2\"> Item 2</li>
		</ul>")
	(defq doc (parse-html html))

	(defq imgs (. doc :get-elements-by-tag-name "img"))
	(assert-eq 2 (length imgs)))
