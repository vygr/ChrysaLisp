
;; Master Test Runner for HTML Library
;; Runs all unit tests for the HTML parser/renderer

(import "lib/test/unittest.inc")

(defun run-test-file (filename)
	; Run a test file and return success status
	(print "")
	(print "========================================")
	(print "Running: " filename)
	(print "========================================")
	; TODO: Load and run the test file
	; For now, just indicate the file
	:t)

