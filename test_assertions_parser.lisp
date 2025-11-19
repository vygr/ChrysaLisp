(import "lib/test/unittest.inc")
(import "lib/html/parser.inc")
(import "lib/html/dom.inc")

(deftest-suite "Assertions Parser Test")

(deftest "Parse HTML and get elements"
	(defq html "<html><body><p>Hello World</p></body></html>")
	(defq doc (parse-html html))
	(assert-not-nil doc)
	(print "Got document")
	(defq ps (. doc :get-elements-by-tag-name "p"))
	(print "Got elements by tag name, count=" (length ps))
	(assert-eq 1 (length ps)))

(test-report)
