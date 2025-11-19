;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Pattern Matching Test for RegexpEngine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/text/regexp_engine.inc")

(print "\n=== RegexpEngine Matching Test ===\n")

(defq engine (RegexpEngine))
(defq test-num 0)
(defq pass-count 0)
(defq fail-count 0)

(defun test-match (desc pattern text should-match)
    (setq test-num (inc test-num))
    (print test-num ". " desc)
    (print "   Pattern: '" pattern "'")
    (print "   Text: '" text "'")
    (catch
        (progn
            (defq result (. engine :match-enhanced text pattern))
            (if result
                (progn
                    (print "   Result: Match at " (second result) "-" (third result))
                    (if should-match
                        (progn
                            (print "   ✓ PASS\n")
                            (setq pass-count (inc pass-count)))
                        (progn
                            (print "   ✗ FAIL (expected no match)\n")
                            (setq fail-count (inc fail-count)))))
                (progn
                    (print "   Result: No match")
                    (if should-match
                        (progn
                            (print "   ✗ FAIL (expected match)\n")
                            (setq fail-count (inc fail-count)))
                        (progn
                            (print "   ✓ PASS\n")
                            (setq pass-count (inc pass-count)))))))
        (progn
            (print "   ✗ ERROR: " _  "\n")
            (setq fail-count (inc fail-count)))))

; Test 1: Simple literal
(test-match "Simple literal" "hello" "hello world" :t)

; Test 2: Literal not found
(test-match "Literal not found" "xyz" "hello world" :nil)

; Test 3: Wildcard
(test-match "Wildcard" "h.llo" "hello world" :t)

; Test 4: Start anchor
(test-match "Start anchor match" "^hello" "hello world" :t)

; Test 5: Start anchor no match
(test-match "Start anchor no match" "^world" "hello world" :nil)

; Test 6: End anchor
(test-match "End anchor match" "world$" "hello world" :t)

; Summary
(print "=====================================")
(print "Tests run: " test-num)
(print "Passed: " pass-count)
(print "Failed: " fail-count)
(if (= fail-count 0)
    (print "\n✓ All tests passed!")
    (print "\n✗ Some tests failed"))
(print "=====================================\n")
