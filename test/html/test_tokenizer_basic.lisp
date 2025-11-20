
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

(deftest "Read tag name - simple tag"
	(setq tok (html-tokenizer))
	(. tok :init "div>rest")
	(defq tag (. tok :read-tag-name))
	(assert-eq "div" tag))

(deftest "Read tag name - with hyphen"
	(setq tok (html-tokenizer))
	(. tok :init "my-tag>")
	(defq tag (. tok :read-tag-name))
	(assert-eq "my-tag" tag))

(deftest "Read tag name - with numbers"
	(setq tok (html-tokenizer))
	(. tok :init "h1 ")
	(defq tag (. tok :read-tag-name))
	(assert-eq "h1" tag))

(deftest "Read until character"
	(setq tok (html-tokenizer))
	(. tok :init "hello world")
	(defq text (. tok :read-until " "))
	(assert-eq "hello" text))

(deftest "Peek with offset"
	(setq tok (html-tokenizer))
	(. tok :init "hello")
	(defq ch0 (. tok :peek 0))
	(defq ch1 (. tok :peek 1))
	(assert-eq "h" ch0)
	(assert-eq "e" ch1))

(deftest "Advance multiple characters"
	(setq tok (html-tokenizer))
	(. tok :init "hello world")
	(. tok :advance 6)
	(defq ch (. tok :peek))
	(assert-eq "w" ch))

; Report test results
(test-report)
