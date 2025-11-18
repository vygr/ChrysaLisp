;Test script for sed and awk commands
(import "lib/test/test.inc")

(defun test-sed ()
	(print "\n=== Testing sed ===\n")

	;Test 1: Basic substitute
	(print "Test 1: Basic substitute (s/line/LINE/)")
	(print "Expected: This is LINE one")
	(print "Actual: " (test-cmd "echo \"This is line one\" | sed 's/line/LINE/'"))

	;Test 2: Global substitute
	(print "\nTest 2: Global substitute (s/is/WAS/g)")
	(print "Expected: ThWAS WAS line one")
	(print "Actual: " (test-cmd "echo \"This is line one\" | sed 's/is/WAS/g'"))

	;Test 3: Delete lines with pattern
	(print "\nTest 3: Delete lines with pattern (/error/d)")
	(print "Expected: Lines without 'error'")
	(test-cmd "cat tests/test_sed.txt | sed '/error/d'")

	;Test 4: Print lines with pattern (-n + p)
	(print "\nTest 4: Print lines with pattern (-n '/error/p')")
	(print "Expected: Only lines with 'error'")
	(test-cmd "cat tests/test_sed.txt | sed -n '/error/p'")

	(print "\n"))

(defun test-awk ()
	(print "\n=== Testing awk ===\n")

	;Test 1: Print specific field
	(print "Test 1: Print first field")
	(print "Expected: Fruit names only")
	(test-cmd "cat tests/test_data.txt | awk '{print $1}'")

	;Test 2: Print multiple fields
	(print "\nTest 2: Print fields 1 and 3")
	(print "Expected: Fruit name and color")
	(test-cmd "cat tests/test_data.txt | awk '{print $1, $3}'")

	;Test 3: Pattern matching
	(print "\nTest 3: Lines matching 'red'")
	(test-cmd "cat tests/test_data.txt | awk '/red/ {print $1}'")

	;Test 4: Field separator
	(print "\nTest 4: With custom field separator")
	(test-cmd "echo \"name:age:city\" | awk -F: '{print $1, $3}'")

	;Test 5: BEGIN/END blocks
	(print "\nTest 5: Sum with BEGIN/END")
	(print "Expected: Sum of field 2 values (10+5+20+15+8=58)")
	(test-cmd "cat tests/test_data.txt | awk 'BEGIN {sum=0} {sum+=$2} END {print sum}'")

	;Test 6: NR variable
	(print "\nTest 6: Line numbers with NR")
	(test-cmd "cat tests/test_data.txt | awk '{print NR, $1}'")

	(print "\n"))

(defun test-cmd (cmd)
	; Execute command and return output
	; Note: This is a placeholder - actual implementation
	; would use the ChrysaLisp pipe system
	(print "[Would execute: " cmd "]")
	)

(defun main ()
	(print "ChrysaLisp sed and awk Test Suite")
	(print "===================================")

	(test-sed)
	(test-awk)

	(print "\nTests complete. Manual verification required.")
	(print "Run the individual commands to see actual output."))
