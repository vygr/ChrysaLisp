;; Test Function Defined Locally
(print "=== Test Function Defined Locally ===")

; Define property types
(defq types (list
	(list "text" "string")
	(list "min_width" "number")
	(list "color" "color")))

; Define function locally
(defun get-prop-type (prop-sym)
	(defq prop-str (str prop-sym))
	(defq prop-str (slice prop-str 1 (length prop-str)))
	(defq result "unknown")
	(defq i 0)
	(while (< i (length types))
		(defq entry (elem-get types i))
		(when (eql (elem-get entry 0) prop-str)
			(setq result (elem-get entry 1)))
		(setq i (+ i 1)))
	result)

; Test it
(defq k1 :text)
(defq k2 :min_width)

(print "Testing locally defined function:")
(print "  :text -> " (get-prop-type k1))
(print "  :min_width -> " (get-prop-type k2))
(print "✓ Test complete")
