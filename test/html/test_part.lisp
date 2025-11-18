#!/usr/bin/env lsp

;; Unit tests for HTML Part and View
;; Ported from khtml/autotests/khtmlparttest.cpp

(import "lib/test/unittest.inc")
(import "lib/html/part.inc")

(deftest-suite "HTML Part Tests")

;; Custom HTML Part that constructs view in init
(defun make-custom-html-part ()
	(defq view (html-view :init nil))
	(defq part (html-part :init view))
	part)

(deftest "Construct HTMLView from Part Init"
	; Test that HTMLView can be built from HTMLPart initialization
	(defq part (make-custom-html-part))
	(assert-not-nil part)
	(assert-not-nil (. part :get-view))
	(assert-eq part (. (. part :get-view) :get-part)))

(deftest "Construct HTMLView before Part"
	; Test that HTMLView can be constructed before HTMLPart
	(defq view (html-view :init nil))
	(assert-not-nil view)
	(defq part (html-part :init view))
	(assert-not-nil part)
	(assert-eq part (. view :get-part))
	(assert-eq view (. part :get-view)))

(deftest "Part has Document"
	; Test that HTMLPart has a document
	(defq part (html-part :init))
	(assert-not-nil (. part :get-document)))

(deftest "Begin Write End"
	; Test begin/write/end sequence
	(defq part (html-part :init))
	(assert-true (. part :begin))
	(assert-true (. part :write "<html><body>"))
	(assert-true (. part :write "<h1>Hello World</h1>"))
	(assert-true (. part :write "</body></html>"))
	(assert-true (. part :end))
	(defq html (. part :get-html))
	(assert-not-nil html)
	(assert-neq "" html))

(deftest "Set HTML Content"
	; Test setting HTML content directly
	(defq part (html-part :init))
	(defq html-content "<html><body><p>Test</p></body></html>")
	(assert-true (. part :set-html html-content))
	(assert-eq html-content (. part :get-html)))

(deftest "View Resize"
	; Test view resizing
	(defq view (html-view :init nil))
	(. view :resize 1024 768)
	(assert-eq 1024 (. view 'width))
	(assert-eq 768 (. view 'height)))

(defun main ()
	(run-test-suite
		(deftest "Construct HTMLView from Part Init"
			(defq part (make-custom-html-part))
			(assert-not-nil part)
			(assert-not-nil (. part :get-view))
			(assert-eq part (. (. part :get-view) :get-part)))

		(deftest "Construct HTMLView before Part"
			(defq view (html-view :init nil))
			(assert-not-nil view)
			(defq part (html-part :init view))
			(assert-not-nil part)
			(assert-eq part (. view :get-part))
			(assert-eq view (. part :get-view)))

		(deftest "Part has Document"
			(defq part (html-part :init))
			(assert-not-nil (. part :get-document)))

		(deftest "Begin Write End"
			(defq part (html-part :init))
			(assert-true (. part :begin))
			(assert-true (. part :write "<html><body>"))
			(assert-true (. part :write "<h1>Hello World</h1>"))
			(assert-true (. part :write "</body></html>"))
			(assert-true (. part :end))
			(defq html (. part :get-html))
			(assert-not-nil html)
			(assert-neq "" html))

		(deftest "Set HTML Content"
			(defq part (html-part :init))
			(defq html-content "<html><body><p>Test</p></body></html>")
			(assert-true (. part :set-html html-content))
			(assert-eq html-content (. part :get-html)))

		(deftest "View Resize"
			(defq view (html-view :init nil))
			(. view :resize 1024 768)
			(assert-eq 1024 (. view 'width))
			(assert-eq 768 (. view 'height)))))
