;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Simple RegexpEngine Test
; Quick sanity check for basic functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/text/regexp_engine.inc")

(defun test-basic ()
	(print "\n=== Testing Basic RegexpEngine ===\n")

	; Create engine
	(print "Creating RegexpEngine...")
	(defq engine (RegexpEngine))
	(print "  Created: " engine "\n")

	; Test 1: Simple literal
	(print "Test 1: Compile simple literal 'hello'")
	(defq ast (. engine :compile-enhanced "hello"))
	(if ast
		(print "  ✓ Compiled successfully")
		(print "  ✗ Compilation failed"))
	(print "  AST: " ast "\n")

	; Test 2: Digit class
	(print "Test 2: Compile digit class '\\d+'")
	(setq ast (. engine :compile-enhanced "\\d+"))
	(if ast
		(print "  ✓ Compiled successfully")
		(print "  ✗ Compilation failed"))
	(print "  AST: " ast "\n")

	; Test 3: Simple group
	(print "Test 3: Compile simple group '(abc)'")
	(setq ast (. engine :compile-enhanced "(abc)"))
	(if ast
		(print "  ✓ Compiled successfully")
		(print "  ✗ Compilation failed"))
	(print "  AST: " ast "\n")

	; Test 4: Character class
	(print "Test 4: Compile character class '[a-z]+'")
	(setq ast (. engine :compile-enhanced "[a-z]+"))
	(if ast
		(print "  ✓ Compiled successfully")
		(print "  ✗ Compilation failed"))
	(print "  AST: " ast "\n")

	(print "=== Basic Tests Complete ===\n"))

(defun main ()
	(catch
		(test-basic)
		(print "\nCaught error: " _)))
