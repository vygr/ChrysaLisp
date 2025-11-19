(import "lib/html/dom.inc")

(defq doc (html-document))
(. doc :init)
(print "Created document")

(defq ps (. doc :get-elements-by-tag-name "p"))
(print "Got elements: " ps)
(print "Count: " (length ps))
