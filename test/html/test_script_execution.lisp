
;; Script Execution Tests
;; Tests <script> tag support for ChrysaLisp Lisp execution
;; Test-driven development for DOM scripting

(import "lib/test/unittest.inc")
(import "lib/html/parser.inc")
(import "lib/html/dom.inc")

(deftest-suite "Script Execution Tests")

; Test 1: Parse HTML with script
(deftest "Parse HTML With Script"
	(defq html "<html><head><script>; test</script></head></html>")
	(defq doc (parse-html html))
	(assert-not-nil doc))

; Report test results
(test-report)
