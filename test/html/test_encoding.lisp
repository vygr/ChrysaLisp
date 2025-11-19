
;; Unit tests for HTML encoding detector
;; Ported from khtml/autotests/kencodingdetectortest.cpp

(import "lib/test/unittest.inc")
(import "lib/html/encoding.inc")

(deftest-suite "Encoding Detector Tests")

(defq ed :nil)

(defun init-test-case ()
	; Initialize test case
	(setq ed (encoding-detector :init)))

(deftest "Set Encoding"
	(init-test-case)
	(assert-true (. ed :set-encoding "iso-8859-1" :user))
	(assert-true (. ed :set-encoding "utf-8" :user))
	(assert-eq "utf-8" (. ed :get-encoding)))

(deftest "Decode Valid UTF-8"
	(init-test-case)
	(. ed :set-encoding "utf-8" :user)
	(defq data1 "this should decode correctly")
	(defq s (. ed :decode data1))
	(assert-false (. ed :decoded-invalid-chars?))
	(assert-eq data1 s))

(deftest "Decode Invalid UTF-8"
	(init-test-case)
	(. ed :set-encoding "utf-8" :user)
	; Create string with invalid UTF-8 bytes
	(defq data2 (cat "this is an invalid utf-8 byte: " (char 0xBF) " and another one: " (char 0xBE)))
	(defq s2 (. ed :decode data2))
	(assert-true (. ed :decoded-invalid-chars?)))

(deftest "Reset Decoder"
	(init-test-case)
	(. ed :set-encoding "utf-8" :user)
	(defq data2 (cat "invalid: " (char 0xBF)))
	(. ed :decode data2)
	(assert-true (. ed :decoded-invalid-chars?))
	(. ed :reset)
	(assert-false (. ed :decoded-invalid-chars?)))

(deftest "Decode With Buffering - Auto Detection"
	(init-test-case)
	; Set to automatic detection
	(. ed :set-encoding "" :default)

	(defq data1 "this should be buffered")
	; Should buffer and return empty string
	(defq s (. ed :decode-with-buffering data1))
	(assert-eq "" s)
	(assert-false (. ed :decoded-invalid-chars?))

	; Force encoding
	(. ed :set-encoding "utf-8" :user)
	(assert-eq "utf-8" (. ed :get-encoding))

	; Flush buffer
	(defq s (. ed :flush))
	(assert-neq "" s))

(deftest "Decode With Buffering - HTTP Header"
	(init-test-case)
	; Set encoding from HTTP header (non-overridable)
	(. ed :set-encoding "utf-8" :http-header)

	(defq data1 "this should decode immediately")
	; Should decode immediately, not buffer
	(defq s (. ed :decode-with-buffering data1))
	(assert-neq "" s))

(deftest "Reset Empties Buffer"
	(init-test-case)
	(. ed :set-encoding "" :default)

	(defq data1 "buffered data")
	(. ed :decode-with-buffering data1)

	; Reset should empty buffer
	(. ed :reset)
	(defq s (. ed :flush))
	(assert-eq "" s))

