(import "lib/test/unittest.inc")
(import "lib/html/dom.inc")

(deftest-suite "DOM Init Test")

(deftest "Create and init text node"
	(defq node (text-node))
	(print "Created node")
	(. node :init "Hello")
	(print "Initialized node"))

(test-report)
