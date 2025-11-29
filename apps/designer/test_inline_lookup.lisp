;; Test Inline Lookup (No Functions)
(print "=== Test Inline Lookup ===")

; Define property types
(defq types (list
	(list "text" "string")
	(list "min_width" "number")
	(list "color" "color")))

(print "Types defined: " (length types))

; Test keyword to string conversion
(defq k :text)
(defq k-str (str k))
(print "Keyword: " k)
(print "As string: " k-str)
(defq k-str (slice k-str 1 (length k-str)))
(print "Without colon: " k-str)

; Manual lookup
(defq result "unknown")
(defq i 0)
(while (< i (length types))
	(defq entry (elem-get types i))
	(print "  Checking: " (elem-get entry 0))
	(when (eql (elem-get entry 0) k-str)
		(setq result (elem-get entry 1))
		(print "  MATCH!"))
	(setq i (+ i 1)))

(print "")
(print "Result: " result)
(print "✓ Test complete")
