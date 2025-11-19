
;; Basic HTML Tokenizer Tests
;; Minimal tests for tokenizer functionality

(import "lib/test/unittest.inc")
(import "lib/html/tokenizer.inc")

(deftest-suite "HTML Tokenizer Basic Tests")

(defq tok :nil)

(deftest "Create tokenizer"
	(setq tok (html-tokenizer))
	(assert-not-nil tok))

(deftest "Initialize tokenizer with HTML"
	(setq tok (html-tokenizer))
	(. tok :init "<div>test</div>")
	(assert-not-nil tok))

(deftest "Peek at first character"
	(setq tok (html-tokenizer))
	(. tok :init "<div>")
	(defq ch (. tok :peek))
	(assert-eq "<" ch))

(deftest "Advance position"
	(setq tok (html-tokenizer))
	(. tok :init "hello")
	(. tok :advance)
	(defq ch (. tok :peek))
	(assert-eq "e" ch))

(deftest "Skip whitespace"
	(setq tok (html-tokenizer))
	(. tok :init "   hello")
	(. tok :skip-whitespace)
	(defq ch (. tok :peek))
	(assert-eq "h" ch))

; Report test results
(test-report)
