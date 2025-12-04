;; Super Simple Keyword Test
(print "Test 1: Can we use :text?")
(defq k :text)
(print "k = " k)

(print "")
(print "Test 2: Can we compare?")
(print "k eql :text? " (eql k :text))

(print "")
(print "✓ Done")
