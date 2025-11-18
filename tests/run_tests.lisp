;Run basic tests for sed and awk
(print "=== Testing sed and awk commands ===\n")

;Test 1: sed substitute
(print "Test 1: sed substitute")
(print "Input: This is a test")
(print "Command: sed 's/test/example/'")
(print "Running...\n")
(popen "echo \"This is a test\" | cmd/sed 's/test/example/'" :read)

;Test 2: sed global substitute
(print "\nTest 2: sed global substitute")
(print "Input: foo foo foo")
(print "Command: sed 's/foo/bar/g'")
(print "Running...\n")
(popen "echo \"foo foo foo\" | cmd/sed 's/foo/bar/g'" :read)

;Test 3: awk print field
(print "\nTest 3: awk print field")
(print "Input: apple 10 red")
(print "Command: awk '{print $1}'")
(print "Running...\n")
(popen "echo \"apple 10 red\" | cmd/awk '{print $1}'" :read)

;Test 4: awk multiple fields
(print "\nTest 4: awk multiple fields")
(print "Input: apple 10 red")
(print "Command: awk '{print $1, $3}'")
(print "Running...\n")
(popen "echo \"apple 10 red\" | cmd/awk '{print $1, $3}'" :read)

(print "\n=== Tests complete ===")
