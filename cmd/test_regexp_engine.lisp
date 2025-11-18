;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RegexpEngine Test Suite
; Comprehensive tests for the enhanced regexp engine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/text/regexp_engine.inc")
(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: test_regexp_engine [options]

	options:
		-h --help: this help info.

	Test suite for RegexpEngine library.
	Tests basic patterns, named groups, lookaheads,
	quantifiers, and more.
")
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test Framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *test-count* 0)
(defq *test-passed* 0)
(defq *test-failed* 0)

(defun assert-equal (expected actual desc)
	; (assert-equal expected actual desc) -> :t | :nil
	(setq *test-count* (inc *test-count*))
	(if (eql expected actual)
		(progn
			(setq *test-passed* (inc *test-passed*))
			(print "  ✓ " desc)
			:t)
		(progn
			(setq *test-failed* (inc *test-failed*))
			(print "  ✗ " desc)
			(print "    Expected: " expected)
			(print "    Actual:   " actual)
			:nil)))

(defun assert-true (actual desc)
	; (assert-true actual desc) -> :t | :nil
	(assert-equal :t actual desc))

(defun assert-not-nil (actual desc)
	; (assert-not-nil actual desc) -> :t | :nil
	(setq *test-count* (inc *test-count*))
	(if actual
		(progn
			(setq *test-passed* (inc *test-passed*))
			(print "  ✓ " desc)
			:t)
		(progn
			(setq *test-failed* (inc *test-failed*))
			(print "  ✗ " desc)
			(print "    Expected: non-nil")
			(print "    Actual:   :nil")
			:nil)))

(defun assert-nil (actual desc)
	; (assert-nil actual desc) -> :t | :nil
	(assert-equal :nil actual desc))

(defun test-section (name)
	; (test-section name) -> :nil
	(print "\n[" name "]"))

(defun print-summary ()
	; (print-summary) -> :nil
	(print "\n" (str *line*))
	(print "Test Summary:")
	(print "  Total:  " *test-count*)
	(print "  Passed: " *test-passed*)
	(print "  Failed: " *test-failed*)
	(if (= *test-failed* 0)
		(print "\n✓ All tests passed!")
		(print "\n✗ Some tests failed"))
	(print (str *line*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test Cases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-basic-literals ()
	; Test basic literal matching
	(test-section "Basic Literal Matching")
	(defq engine (RegexpEngine))

	; Test simple literal
	(defq pattern "hello")
	(defq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile simple literal pattern")

	; Test literal with special chars (escaped)
	(setq pattern "hello\\.")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile literal with escaped dot")

	; Test multiple literals
	(setq pattern "abc")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile multiple consecutive literals"))

(defun test-character-classes ()
	; Test character class matching
	(test-section "Character Classes")
	(defq engine (RegexpEngine))

	; Test digit class
	(defq pattern "\\d")
	(defq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile digit class \\d")

	; Test word class
	(setq pattern "\\w")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile word class \\w")

	; Test whitespace class
	(setq pattern "\\s")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile whitespace class \\s")

	; Test custom character class
	(setq pattern "[abc]")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile custom class [abc]")

	; Test negated character class
	(setq pattern "[^abc]")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile negated class [^abc]")

	; Test range in character class
	(setq pattern "[a-z]")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile range class [a-z]"))

(defun test-anchors ()
	; Test anchor matching
	(test-section "Anchors")
	(defq engine (RegexpEngine))

	; Test start anchor
	(defq pattern "^hello")
	(defq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile start anchor ^")

	; Test end anchor
	(setq pattern "world$")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile end anchor $")

	; Test both anchors
	(setq pattern "^test$")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile start and end anchors"))

(defun test-quantifiers ()
	; Test quantifier matching
	(test-section "Quantifiers")
	(defq engine (RegexpEngine))

	; Test zero or more (*)
	(defq pattern "a*")
	(defq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile zero-or-more quantifier *")

	; Test one or more (+)
	(setq pattern "a+")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile one-or-more quantifier +")

	; Test zero or one (?)
	(setq pattern "a?")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile zero-or-one quantifier ?")

	; Test exact count {n}
	(setq pattern "a{3}")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile exact count quantifier {3}")

	; Test range {n,m}
	(setq pattern "a{2,5}")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile range quantifier {2,5}")

	; Test minimum {n,}
	(setq pattern "a{2,}")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile minimum quantifier {2,}"))

(defun test-non-greedy ()
	; Test non-greedy quantifiers
	(test-section "Non-Greedy Quantifiers")
	(defq engine (RegexpEngine))

	; Test non-greedy zero or more (*?)
	(defq pattern "a*?")
	(defq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile non-greedy *?")

	; Test non-greedy one or more (+?)
	(setq pattern "a+?")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile non-greedy +?")

	; Test non-greedy zero or one (??)"
	(setq pattern "a??")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile non-greedy ??")

	; Test non-greedy range
	(setq pattern "a{2,5}?")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile non-greedy range {2,5}?"))

(defun test-groups ()
	; Test capturing groups
	(test-section "Capturing Groups")
	(defq engine (RegexpEngine))

	; Test simple group
	(defq pattern "(abc)")
	(defq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile simple capturing group")

	; Test nested groups
	(setq pattern "(a(b)c)")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile nested capturing groups")

	; Test multiple groups
	(setq pattern "(a)(b)(c)")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile multiple capturing groups")

	; Test non-capturing group
	(setq pattern "(?:abc)")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile non-capturing group (?:)"))

(defun test-named-groups ()
	; Test named capturing groups
	(test-section "Named Capturing Groups")
	(defq engine (RegexpEngine))

	; Test simple named group
	(defq pattern "(?<name>\\w+)")
	(defq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile named group (?<name>)")

	; Test multiple named groups
	(setq pattern "(?<first>\\w+) (?<last>\\w+)")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile multiple named groups")

	; Test named group with numbers
	(setq pattern "(?<id>\\d+)")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile named group with digits"))

(defun test-alternation ()
	; Test alternation (OR) operator
	(test-section "Alternation")
	(defq engine (RegexpEngine))

	; Test simple alternation
	(defq pattern "cat|dog")
	(defq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile simple alternation cat|dog")

	; Test multiple alternations
	(setq pattern "a|b|c")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile multiple alternations")

	; Test alternation with groups
	(setq pattern "(cat|dog)")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile alternation in group"))

(defun test-lookahead ()
	; Test lookahead assertions
	(test-section "Lookahead Assertions")
	(defq engine (RegexpEngine))

	; Test positive lookahead
	(defq pattern "foo(?=bar)")
	(defq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile positive lookahead (?=)")

	; Test negative lookahead
	(setq pattern "foo(?!bar)")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile negative lookahead (?!)"))

(defun test-lookbehind ()
	; Test lookbehind assertions
	(test-section "Lookbehind Assertions")
	(defq engine (RegexpEngine))

	; Test positive lookbehind
	(defq pattern "(?<=foo)bar")
	(defq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile positive lookbehind (?<=)")

	; Test negative lookbehind
	(setq pattern "(?<!foo)bar")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile negative lookbehind (?<!)"))

(defun test-backreferences ()
	; Test backreferences
	(test-section "Backreferences")
	(defq engine (RegexpEngine))

	; Test simple backreference
	(defq pattern "(\\w+)\\s+\\1")
	(defq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile backreference \\1")

	; Test multiple backreferences
	(setq pattern "(\\w+)(\\d+)\\1\\2")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile multiple backreferences"))

(defun test-complex-patterns ()
	; Test complex real-world patterns
	(test-section "Complex Patterns")
	(defq engine (RegexpEngine))

	; Test email-like pattern
	(defq pattern "\\w+@\\w+\\.\\w+")
	(defq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile email-like pattern")

	; Test URL-like pattern
	(setq pattern "https?://[\\w.]+/\\w+")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile URL-like pattern")

	; Test phone number pattern
	(setq pattern "\\(?\\d{3}\\)?[- ]?\\d{3}[- ]?\\d{4}")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile phone number pattern")

	; Test date pattern
	(setq pattern "\\d{4}-\\d{2}-\\d{2}")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile date pattern YYYY-MM-DD")

	; Test IPv4 pattern
	(setq pattern "\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile IPv4 address pattern"))

(defun test-wildcard ()
	; Test wildcard (.) matching
	(test-section "Wildcard")
	(defq engine (RegexpEngine))

	; Test single wildcard
	(defq pattern "a.c")
	(defq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile wildcard pattern a.c")

	; Test multiple wildcards
	(setq pattern "...")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile multiple wildcards")

	; Test wildcard with quantifier
	(setq pattern ".*")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile wildcard with quantifier .*"))

(defun test-escape-sequences ()
	; Test escape sequences
	(test-section "Escape Sequences")
	(defq engine (RegexpEngine))

	; Test escaped special characters
	(defq pattern "\\(\\)\\[\\]\\{\\}")
	(defq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile escaped special characters")

	; Test escaped backslash
	(setq pattern "\\\\")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile escaped backslash \\\\")

	; Test newline
	(setq pattern "\\n")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile newline \\n")

	; Test tab
	(setq pattern "\\t")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile tab \\t"))

(defun test-optimization ()
	; Test pattern optimization
	(test-section "Pattern Optimization")
	(defq engine (RegexpEngine))

	; Test literal sequence optimization
	(defq pattern "abc")
	(defq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Optimize consecutive literals")

	; Test complex pattern optimization
	(setq pattern "a+b*c?")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile pattern with multiple quantifiers"))

(defun test-edge-cases ()
	; Test edge cases and error handling
	(test-section "Edge Cases")
	(defq engine (RegexpEngine))

	; Test empty pattern
	(defq pattern "")
	(defq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile empty pattern")

	; Test single character
	(setq pattern "a")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile single character")

	; Test long pattern
	(setq pattern "abcdefghijklmnopqrstuvwxyz")
	(setq ast (. engine :compile-enhanced pattern))
	(assert-not-nil ast "Compile long literal pattern"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main Test Runner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-all-tests ()
	; Run all test suites
	(defq *line* "================================================")

	(print (str *line*))
	(print "RegexpEngine Test Suite")
	(print "Testing Enhanced Regular Expression Engine")
	(print (str *line*))

	; Run all test suites
	(test-basic-literals)
	(test-character-classes)
	(test-anchors)
	(test-quantifiers)
	(test-non-greedy)
	(test-groups)
	(test-named-groups)
	(test-alternation)
	(test-lookahead)
	(test-lookbehind)
	(test-backreferences)
	(test-complex-patterns)
	(test-wildcard)
	(test-escape-sequences)
	(test-optimization)
	(test-edge-cases)

	; Print summary
	(print-summary))

(defun main ()
	; Main entry point
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(run-all-tests)))
