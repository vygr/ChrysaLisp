(import "lib/test/unittest.inc")
(import "lib/html/parser.inc")

(deftest-suite "Simple Parser Test")

(deftest "Check TOKEN_EOF exists"
	(assert-not-nil TOKEN_EOF)
	(print "TOKEN_EOF = " TOKEN_EOF))

(deftest "Create parser"
	(defq parser (html-parser))
	(assert-not-nil parser))

(test-report)
