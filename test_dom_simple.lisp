(import "lib/test/unittest.inc")
(import "lib/html/dom.inc")

(deftest-suite "Simple DOM Test")

(deftest "Create text node"
	(defq node (text-node))
	(print "Created text-node: " node))

(test-report)
