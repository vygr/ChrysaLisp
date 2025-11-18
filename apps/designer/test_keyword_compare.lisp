;; Test Keyword Comparison
(print "=== Keyword Comparison Test ===")

; Test if we can compare keywords
(defq k1 ':text)
(defq k2 ':text)
(defq k3 ':min_width)

(print "k1 = " k1)
(print "k2 = " k2)
(print "k3 = " k3)
(print "")

(print "k1 = k2? " (eql k1 k2))
(print "k1 = k3? " (eql k1 k3))
(print "")

; Test with list of lists
(defq types (list
	(list ':text "string")
	(list ':min_width "number")))

(print "Types list: " types)
(print "First entry: " (elem-get types 0))
(print "First keyword: " (elem-get (elem-get types 0) 0))
(print "")

; Test manual loop
(defq target ':text)
(defq result "unknown")
(defq i 0)
(while (< i (length types))
	(defq entry (elem-get types i))
	(defq key (elem-get entry 0))
	(print "Comparing " key " with " target)
	(when (eql key target)
		(setq result (elem-get entry 1))
		(print "  Match!"))
	(setq i (inc i)))

(print "")
(print "Result: " result)
(print "✓ Test complete")
