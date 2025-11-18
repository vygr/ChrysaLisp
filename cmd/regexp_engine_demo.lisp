;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RegexpEngine Demo
; Practical examples demonstrating the enhanced regexp engine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/text/regexp_engine.inc")
(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: regexp_engine_demo [options]

	options:
		-h --help: this help info.

	Demonstration of RegexpEngine capabilities.
	Shows practical examples of pattern matching,
	named groups, lookaheads, and more.
")
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Demo Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-header (title)
	; (print-header title) -> :nil
	(print "\n" (str *line*))
	(print title)
	(print (str *line*)))

(defun print-example (desc pattern text)
	; (print-example desc pattern text) -> :nil
	(print "\n[" desc "]")
	(print "  Pattern: " pattern)
	(print "  Text:    " text))

(defun print-result (result)
	; (print-result result) -> :nil
	(if result
		(progn
			(print "  ✓ Match found!")
			(print "    Start:  " (second result))
			(print "    End:    " (third result))
			(when (> (length (fourth result)) 0)
				(print "    Captures:")
				(each (lambda (cap)
					(print "      [" (first cap) "] "
						(second cap) "-" (third cap)
						" = \"" (fourth cap) "\""))
					(fourth result))))
		(print "  ✗ No match")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Demo Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun demo-basic-patterns ()
	; Demonstrate basic pattern matching
	(print-header "Basic Pattern Matching")
	(defq engine (RegexpEngine))

	; Simple literal
	(print-example "Simple literal" "world" "Hello world!")
	(print-result (. engine :match-enhanced "Hello world!" "world"))

	; Digit matching
	(print-example "Extract digits" "\\d+" "Order #12345")
	(print-result (. engine :match-enhanced "Order #12345" "\\d+"))

	; Word matching
	(print-example "Match word" "\\w+" "Hello")
	(print-result (. engine :match-enhanced "Hello" "\\w+")))

(defun demo-character-classes ()
	; Demonstrate character classes
	(print-header "Character Classes")
	(defq engine (RegexpEngine))

	; Custom class
	(print-example "Vowels" "[aeiou]" "Hello")
	(print-result (. engine :match-enhanced "Hello" "[aeiou]"))

	; Negated class
	(print-example "Non-vowels" "[^aeiou]" "Hello")
	(print-result (. engine :match-enhanced "Hello" "[^aeiou]"))

	; Range
	(print-example "Lowercase range" "[a-z]+" "Hello123")
	(print-result (. engine :match-enhanced "Hello123" "[a-z]+")))

(defun demo-quantifiers ()
	; Demonstrate quantifiers
	(print-header "Quantifiers")
	(defq engine (RegexpEngine))

	; Zero or more
	(print-example "Zero or more" "a*b" "aaab")
	(print-result (. engine :match-enhanced "aaab" "a*b"))

	; One or more
	(print-example "One or more" "a+b" "aaab")
	(print-result (. engine :match-enhanced "aaab" "a+b"))

	; Optional
	(print-example "Optional" "colou?r" "color")
	(print-result (. engine :match-enhanced "color" "colou?r"))

	; Exact count
	(print-example "Exact count" "\\d{3}" "12345")
	(print-result (. engine :match-enhanced "12345" "\\d{3}"))

	; Range count
	(print-example "Range count" "\\d{2,4}" "12345")
	(print-result (. engine :match-enhanced "12345" "\\d{2,4}")))

(defun demo-anchors ()
	; Demonstrate anchors
	(print-header "Anchors")
	(defq engine (RegexpEngine))

	; Start anchor
	(print-example "Start anchor" "^Hello" "Hello World")
	(print-result (. engine :match-enhanced "Hello World" "^Hello"))

	; End anchor
	(print-example "End anchor" "World$" "Hello World")
	(print-result (. engine :match-enhanced "Hello World" "World$"))

	; Both anchors
	(print-example "Exact match" "^test$" "test")
	(print-result (. engine :match-enhanced "test" "^test$")))

(defun demo-groups ()
	; Demonstrate capturing groups
	(print-header "Capturing Groups")
	(defq engine (RegexpEngine))

	; Simple group
	(print-example "Simple group" "(\\w+)" "Hello")
	(print-result (. engine :match-enhanced "Hello" "(\\w+)"))

	; Multiple groups
	(print-example "Multiple groups" "(\\w+) (\\w+)" "Hello World")
	(print-result (. engine :match-enhanced "Hello World" "(\\w+) (\\w+)"))

	; Nested groups
	(print-example "Nested groups" "((\\w+)\\s(\\w+))" "Hello World")
	(print-result (. engine :match-enhanced "Hello World" "((\\w+)\\s(\\w+))")))

(defun demo-named-groups ()
	; Demonstrate named capture groups
	(print-header "Named Capture Groups")
	(defq engine (RegexpEngine))

	; Named group for first/last name
	(print-example "Name parsing"
		"(?<first>\\w+) (?<last>\\w+)"
		"John Doe")
	(print-result (. engine :match-enhanced "John Doe"
		"(?<first>\\w+) (?<last>\\w+)"))

	; Named group for date
	(print-example "Date parsing"
		"(?<year>\\d{4})-(?<month>\\d{2})-(?<day>\\d{2})"
		"2024-12-25")
	(print-result (. engine :match-enhanced "2024-12-25"
		"(?<year>\\d{4})-(?<month>\\d{2})-(?<day>\\d{2})"))

	; Named group for email
	(print-example "Email parsing"
		"(?<user>\\w+)@(?<domain>\\w+)\\.(?<tld>\\w+)"
		"user@example.com")
	(print-result (. engine :match-enhanced "user@example.com"
		"(?<user>\\w+)@(?<domain>\\w+)\\.(?<tld>\\w+)")))

(defun demo-alternation ()
	; Demonstrate alternation
	(print-header "Alternation (OR)")
	(defq engine (RegexpEngine))

	; Simple alternation
	(print-example "Pet types" "cat|dog|bird" "I have a dog")
	(print-result (. engine :match-enhanced "I have a dog" "cat|dog|bird"))

	; Alternation with groups
	(print-example "Protocol" "(http|https|ftp)://" "https://example.com")
	(print-result (. engine :match-enhanced "https://example.com"
		"(http|https|ftp)://"))

	; Complex alternation
	(print-example "Yes/No" "(?:yes|no|maybe)" "Answer: yes")
	(print-result (. engine :match-enhanced "Answer: yes"
		"(?:yes|no|maybe)")))

(defun demo-lookahead ()
	; Demonstrate lookahead assertions
	(print-header "Lookahead Assertions")
	(defq engine (RegexpEngine))

	; Positive lookahead
	(print-example "Positive lookahead"
		"\\w+(?=@)" "user@example.com")
	(print-result (. engine :match-enhanced "user@example.com" "\\w+(?=@)"))

	; Negative lookahead
	(print-example "Negative lookahead"
		"\\d+(?!px)" "100px 200em")
	(print-result (. engine :match-enhanced "100px 200em" "\\d+(?!px)")))

(defun demo-real-world ()
	; Demonstrate real-world patterns
	(print-header "Real-World Patterns")
	(defq engine (RegexpEngine))

	; Email validation
	(print-example "Email"
		"\\w+@\\w+\\.\\w+"
		"contact@example.com")
	(print-result (. engine :match-enhanced "contact@example.com"
		"\\w+@\\w+\\.\\w+"))

	; URL parsing
	(print-example "URL"
		"https?://[\\w.]+/\\w+"
		"https://github.com/chrysalisp")
	(print-result (. engine :match-enhanced "https://github.com/chrysalisp"
		"https?://[\\w.]+/\\w+"))

	; Phone number
	(print-example "Phone"
		"\\(?\\d{3}\\)?[- ]?\\d{3}[- ]?\\d{4}"
		"(555) 123-4567")
	(print-result (. engine :match-enhanced "(555) 123-4567"
		"\\(?\\d{3}\\)?[- ]?\\d{3}[- ]?\\d{4}"))

	; IPv4 address
	(print-example "IPv4"
		"\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}"
		"192.168.1.1")
	(print-result (. engine :match-enhanced "192.168.1.1"
		"\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}"))

	; Hex color
	(print-example "Hex color"
		"#[A-Fa-f0-9]{6}"
		"Background: #FF5733")
	(print-result (. engine :match-enhanced "Background: #FF5733"
		"#[A-Fa-f0-9]{6}"))

	; ISO date
	(print-example "ISO date"
		"\\d{4}-\\d{2}-\\d{2}"
		"Published: 2024-12-25")
	(print-result (. engine :match-enhanced "Published: 2024-12-25"
		"\\d{4}-\\d{2}-\\d{2}")))

(defun demo-non-greedy ()
	; Demonstrate non-greedy quantifiers
	(print-header "Non-Greedy Quantifiers")
	(defq engine (RegexpEngine))

	; Greedy vs non-greedy
	(print-example "Greedy star"
		"<.+>"
		"<tag>content</tag>")
	(print-result (. engine :match-enhanced "<tag>content</tag>" "<.+>"))

	(print-example "Non-greedy star"
		"<.+?>"
		"<tag>content</tag>")
	(print-result (. engine :match-enhanced "<tag>content</tag>" "<.+?>"))

	; Non-greedy plus
	(print-example "Non-greedy plus"
		"a+?b"
		"aaab")
	(print-result (. engine :match-enhanced "aaab" "a+?b")))

(defun demo-performance ()
	; Demonstrate pattern caching and reuse
	(print-header "Performance: Pattern Caching")
	(defq engine (RegexpEngine))

	(print "\nCompiling pattern first time...")
	(defq pattern "\\w+@\\w+\\.\\w+")
	(defq start (pii-time))
	(defq ast1 (. engine :compile-enhanced pattern))
	(defq time1 (- (pii-time) start))
	(print "  Time: " time1 "ns")

	(print "\nCompiling same pattern second time (cached)...")
	(setq start (pii-time))
	(defq ast2 (. engine :compile-enhanced pattern))
	(defq time2 (- (pii-time) start))
	(print "  Time: " time2 "ns")

	(print "\nSpeedup: " (/ time1 time2) "x faster")
	(print "Cache hit: " (eql ast1 ast2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
	; Main entry point
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))

		(defq *line* "================================================")

		(print (str *line*))
		(print "RegexpEngine Demo")
		(print "Enhanced Regular Expression Engine for ChrysaLisp")
		(print (str *line*))

		; Run all demos
		(demo-basic-patterns)
		(demo-character-classes)
		(demo-quantifiers)
		(demo-anchors)
		(demo-groups)
		(demo-named-groups)
		(demo-alternation)
		(demo-lookahead)
		(demo-non-greedy)
		(demo-real-world)
		(demo-performance)

		(print "\n" (str *line*))
		(print "Demo Complete!")
		(print "For more information, see:")
		(print "  docs/reference/classes/RegexpEngine.md")
		(print (str *line*) "\n")))
