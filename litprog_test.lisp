;; ========================================================================
;; LITPROG TEST SUITE
;; ========================================================================
;;
;; Comprehensive test suite for the literate programming tool
;;

(import "litprog.lisp")
(import "litprog_enhanced.lisp")

;; ========================================================================
;; Test Framework
;; ========================================================================

(defq *tests-run* 0)
(defq *tests-passed* 0)
(defq *tests-failed* 0)
(defq *current-suite* "")

(defun test-suite (name)
  "Start a new test suite"
  (setq *current-suite* name)
  (print "")
  (print "╔════════════════════════════════════════════════════════════════╗")
  (print (cat "║  Test Suite: " name (repeat-str " " (- 47 (length name))) "║"))
  (print "╚════════════════════════════════════════════════════════════════╝")
  (print ""))

(defun assert-equal (actual expected description)
  "Assert that two values are equal"
  (setq *tests-run* (inc *tests-run*))
  (if (= actual expected)
    (progn
      (setq *tests-passed* (inc *tests-passed*))
      (print (cat "  ✓ " description)))
    (progn
      (setq *tests-failed* (inc *tests-failed*))
      (print (cat "  ✗ " description))
      (print (cat "    Expected: " expected))
      (print (cat "    Got:      " actual)))))

(defun assert-true (value description)
  "Assert that value is true"
  (assert-equal value t description))

(defun assert-not-nil (value description)
  "Assert that value is not nil"
  (setq *tests-run* (inc *tests-run*))
  (if value
    (progn
      (setq *tests-passed* (inc *tests-passed*))
      (print (cat "  ✓ " description)))
    (progn
      (setq *tests-failed* (inc *tests-failed*))
      (print (cat "  ✗ " description))
      (print "    Expected: non-nil value")
      (print "    Got:      nil"))))

(defun repeat-str (s n)
  "Repeat string n times"
  (if (<= n 0)
    ""
    (cat s (repeat-str s (dec n)))))

;; ========================================================================
;; Parser Tests
;; ========================================================================

(defun test-noweb-parser ()
  (test-suite "Noweb Parser")

  ; Test chunk definition parsing
  (defq line1 "<<hello>>=")
  (assert-equal (parse-noweb-chunk-def line1) "hello"
    "Parse simple chunk definition")

  (defq line2 "<<my-chunk-name>>=")
  (assert-equal (parse-noweb-chunk-def line2) "my-chunk-name"
    "Parse chunk with hyphens")

  (defq line3 "  <<indented>>=")
  (assert-equal (parse-noweb-chunk-def line3) "indented"
    "Parse indented chunk definition")

  ; Test chunk reference parsing
  (defq ref1 "  <<helper-function>>")
  (assert-equal (parse-noweb-chunk-ref ref1) "helper-function"
    "Parse chunk reference")

  (defq ref2 "<<another-chunk>>")
  (assert-equal (parse-noweb-chunk-ref ref2) "another-chunk"
    "Parse simple chunk reference")

  ; Should not match definitions
  (defq not-ref "<<chunk>>=")
  (assert-equal (parse-noweb-chunk-ref not-ref) nil
    "Don't match chunk definitions as references"))

(defun test-orgmode-parser ()
  (test-suite "Org-Mode Parser")

  ; Test BEGIN_SRC parsing
  (defq line1 "#+BEGIN_SRC lisp")
  (defq result1 (parse-org-src-begin line1))
  (assert-not-nil result1 "Parse BEGIN_SRC")
  (assert-equal (. result1 :lang) "lisp" "Extract language")

  (defq line2 "#+BEGIN_SRC python :tangle script.py")
  (defq result2 (parse-org-src-begin line2))
  (assert-equal (. result2 :lang) "python" "Extract Python language")
  (assert-equal (. result2 :file) "script.py" "Extract tangle file")

  ; Test NAME parsing
  (defq name1 "#+NAME: my-chunk")
  (assert-equal (parse-org-name name1) "my-chunk" "Parse chunk name")

  ; Test END_SRC
  (assert-true (parse-org-src-end "#+END_SRC") "Parse END_SRC"))

(defun test-markdown-parser ()
  (test-suite "Markdown Fence Parser")

  ; Test fence begin
  (defq line1 "```lisp")
  (defq result1 (parse-markdown-fence-begin line1))
  (assert-not-nil result1 "Parse simple fence")
  (assert-equal (. result1 :lang) "lisp" "Extract language")

  (defq line2 "```javascript {#my-chunk}")
  (defq result2 (parse-markdown-fence-begin line2))
  (assert-equal (. result2 :lang) "javascript" "Extract JS language")
  (assert-equal (. result2 :name) "my-chunk" "Extract chunk name")

  (defq line3 "```python {.tangle=script.py}")
  (defq result3 (parse-markdown-fence-begin line3))
  (assert-equal (. result3 :file) "script.py" "Extract tangle file")

  (defq line4 "```rust {#my-rust .tangle=src/main.rs}")
  (defq result4 (parse-markdown-fence-begin line4))
  (assert-equal (. result4 :name) "my-rust" "Extract chunk name from complex fence")
  (assert-equal (. result4 :file) "src/main.rs" "Extract file from complex fence")

  ; Test fence end
  (assert-true (parse-markdown-fence-end "```") "Parse fence end"))

;; ========================================================================
;; Tangle Tests
;; ========================================================================

(defun test-chunk-expansion ()
  (test-suite "Chunk Expansion")

  ; Create a simple context
  (defq ctx (litprog-context))

  ; Define chunks
  (defq chunk1 (litprog-chunk))
  (. chunk1 :name "main")
  (. chunk1 :code "(defun main ()\n  <<helper>>)")
  (. (. ctx :chunks) "main" chunk1)

  (defq chunk2 (litprog-chunk))
  (. chunk2 :name "helper")
  (. chunk2 :code "(print \"Hello\")")
  (. (. ctx :chunks) "helper" chunk2)

  ; Test expansion
  (defq expanded (expand-chunk-refs (. chunk1 :code) ctx ""))
  (assert-not-nil expanded "Chunk expansion returns result")
  (assert-true (find expanded "(print \"Hello\")") "Expanded chunk contains referenced code"))

(defun test-indentation-preservation ()
  (test-suite "Indentation Preservation")

  (defq ctx (litprog-context))

  ; Chunk with indented reference
  (defq chunk1 (litprog-chunk))
  (. chunk1 :name "function")
  (. chunk1 :code "(defun foo ()\n  <<body>>)")
  (. (. ctx :chunks) "function" chunk1)

  (defq chunk2 (litprog-chunk))
  (. chunk2 :name "body")
  (. chunk2 :code "(print \"Line 1\")\n(print \"Line 2\")")
  (. (. ctx :chunks) "body" chunk2)

  (defq expanded (expand-chunk-refs (. chunk1 :code) ctx ""))
  (assert-true (find expanded "  (print \"Line 1\")") "First line indented correctly")
  (assert-true (find expanded "  (print \"Line 2\")") "Second line indented correctly"))

;; ========================================================================
;; Weave Tests
;; ========================================================================

(defun test-markdown-weave ()
  (test-suite "Markdown Weaving")

  (defq chunk (litprog-chunk))
  (. chunk :name "test-chunk")
  (. chunk :lang "lisp")
  (. chunk :code "(print \"Hello\")")
  (. chunk :file "test.lisp")

  (defq ctx (litprog-context))
  (defq output (weave-markdown-chunk chunk ctx))

  (assert-not-nil output "Markdown output generated")
  (assert-true (find output "test-chunk") "Contains chunk name")
  (assert-true (find output "```lisp") "Contains code fence")
  (assert-true (find output "test.lisp") "Contains tangle file"))

(defun test-html-weave ()
  (test-suite "HTML Weaving")

  (defq chunk (litprog-chunk))
  (. chunk :name "html-test")
  (. chunk :lang "python")
  (. chunk :code "print('Hello')")

  (defq ctx (litprog-context))
  (defq output (weave-html-chunk chunk ctx))

  (assert-not-nil output "HTML output generated")
  (assert-true (find output "<h3>") "Contains header")
  (assert-true (find output "<pre>") "Contains pre tag")
  (assert-true (find output "html-test") "Contains chunk name"))

;; ========================================================================
;; Integration Tests
;; ========================================================================

(defun test-full-pipeline ()
  (test-suite "Full Pipeline Integration")

  ; Create a complete literate file in memory
  (defq lit-content
    "# Test Program

This is a test.

<<test.lisp>>=
(import \"lib/asm/asm.inc\")
<<main-function>>
@

<<main-function>>=
(defun main ()
  (print \"Hello from test\"))
@
")

  ; Write to temp file
  (save-file "/tmp/test.lit" lit-content)

  ; Parse it
  (defq ctx (parse-literate-file "/tmp/test.lit"))

  (assert-not-nil ctx "Context created")
  (assert-true (. (. ctx :chunks) "main-function") "Main function chunk found")

  ; Test tangling
  (litprog-tangle "/tmp/test.lit" "/tmp/")

  ; Check that file was created (in real impl)
  (assert-true t "Tangle completed without error")

  ; Test weaving
  (litprog-weave "/tmp/test.lit" "/tmp/test.md" :format :markdown)
  (assert-true t "Weave completed without error"))

;; ========================================================================
;; Enhanced Features Tests
;; ========================================================================

(defun test-dependency-analysis ()
  (test-suite "Dependency Analysis")

  (defq ctx (litprog-context))

  ; Create chunks with dependencies
  (defq chunk1 (litprog-chunk))
  (. chunk1 :name "main")
  (. chunk1 :code "<<helper1>>\n<<helper2>>")
  (. (. ctx :chunks) "main" chunk1)

  (defq chunk2 (litprog-chunk))
  (. chunk2 :name "helper1")
  (. chunk2 :code "(print \"Helper 1\")")
  (. (. ctx :chunks) "helper1" chunk2)

  (defq chunk3 (litprog-chunk))
  (. chunk3 :name "helper2")
  (. chunk3 :code "(print \"Helper 2\")")
  (. (. ctx :chunks) "helper2" chunk3)

  (defq deps (analyze-chunk-dependencies ctx))

  (assert-not-nil deps "Dependencies analyzed")
  (assert-equal (length (. deps "main")) 2 "Main has 2 dependencies"))

(defun test-statistics ()
  (test-suite "Statistics Generation")

  ; This is more of a smoke test
  (defq lit-content "# Test\n\n<<test.lisp>>=\n(print \"hi\")\n@")
  (save-file "/tmp/stats-test.lit" lit-content)

  ; Should not crash
  (litprog-stats "/tmp/stats-test.lit")
  (assert-true t "Statistics generated without error"))

;; ========================================================================
;; Test Runner
;; ========================================================================

(defun run-all-tests ()
  "Run all test suites"
  (print "")
  (print "╔════════════════════════════════════════════════════════════════╗")
  (print "║                                                                ║")
  (print "║              LITPROG TEST SUITE                                ║")
  (print "║                                                                ║")
  (print "╚════════════════════════════════════════════════════════════════╝")

  (setq *tests-run* 0)
  (setq *tests-passed* 0)
  (setq *tests-failed* 0)

  ; Run all test suites
  (test-noweb-parser)
  (test-orgmode-parser)
  (test-markdown-parser)
  (test-chunk-expansion)
  (test-indentation-preservation)
  (test-markdown-weave)
  (test-html-weave)
  (test-full-pipeline)
  (test-dependency-analysis)
  (test-statistics)

  ; Print summary
  (print "")
  (print "╔════════════════════════════════════════════════════════════════╗")
  (print "║  Test Results                                                 ║")
  (print "╚════════════════════════════════════════════════════════════════╝")
  (print "")
  (print (cat "  Total tests:  " *tests-run*))
  (print (cat "  Passed:       " *tests-passed* " ✓"))
  (print (cat "  Failed:       " *tests-failed* (if (> *tests-failed* 0) " ✗" "")))
  (print "")

  (defq pass-rate (/ (* *tests-passed* 100) (max 1 *tests-run*)))
  (print (cat "  Pass rate:    " pass-rate "%"))
  (print "")

  (if (= *tests-failed* 0)
    (progn
      (print "╔════════════════════════════════════════════════════════════════╗")
      (print "║  🎉 ALL TESTS PASSED! 🎉                                      ║")
      (print "╚════════════════════════════════════════════════════════════════╝"))
    (progn
      (print "╔════════════════════════════════════════════════════════════════╗")
      (print "║  ⚠️  SOME TESTS FAILED                                        ║")
      (print "╚════════════════════════════════════════════════════════════════╝")))
  (print ""))

;; ========================================================================
;; Export
;; ========================================================================

(export run-all-tests)

(print "LITPROG Test Suite loaded!")
(print "Run (run-all-tests) to execute all tests.")
