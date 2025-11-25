(import "./exfat.inc")

(defun comprehensive-test ()
	(prin "=== Comprehensive Internal Read/Write Test ===")(print)
	
	;; Format in memory
	(defq fs (ExFat))
	(. fs :format 1 (memory-stream))
	(prin "Formatted.")(print)
	
	;; Mount
	(. fs :mount)
	(prin "Mounted.")(print)
	
	;; Create structure
	(prin "Creating structure...")(print)
	(. fs :create_dir "/dir1")
	(. fs :create_dir "/dir2")
	(. fs :create_dir "/dir1/subdir")
	
	(. fs :create_file "/file1.txt")
	(defq fh (. fs :open_file "/file1.txt" :write))
	(. fs :write_file fh "Content of file1")
	(. fs :close_file fh)
	
	(. fs :create_file "/file2.txt")
	(setq fh (. fs :open_file "/file2.txt" :write))
	(. fs :write_file fh "Content of file2")
	(. fs :close_file fh)
	
	(. fs :create_file "/dir1/nested.txt")
	(setq fh (. fs :open_file "/dir1/nested.txt" :write))
	(. fs :write_file fh "Nested in dir1")
	(. fs :close_file fh)
	
	(. fs :create_file "/dir1/subdir/deep.txt")
	(setq fh (. fs :open_file "/dir1/subdir/deep.txt" :write))
	(. fs :write_file fh "Deep nested file")
	(. fs :close_file fh)
	
	(prin "Created.")(print)(print)
	
	;; Read back
	(prin "=== Reading Back ===")(print)
	
	(defq success_count 0)
	(defq fail_count 0)
	
	;; Test file1.txt
	(prin "Reading /file1.txt... ")
	(setq fh (. fs :open_file "/file1.txt" :read))
	(if fh
		(progn
			(defq content (. fs :read_file fh 100))
			(. fs :close_file fh)
			(if (eql content "Content of file1")
				(progn (prin "✓ OK")(print) (setq success_count (+ success_count 1)))
				(progn (prin "✗ MISMATCH")(print) (setq fail_count (+ fail_count 1)))))
		(progn (prin "✗ FAILED")(print) (setq fail_count (+ fail_count 1))))
	
	;; Test file2.txt
	(prin "Reading /file2.txt... ")
	(setq fh (. fs :open_file "/file2.txt" :read))
	(if fh
		(progn
			(defq content (. fs :read_file fh 100))
			(. fs :close_file fh)
			(if (eql content "Content of file2")
				(progn (prin "✓ OK")(print) (setq success_count (+ success_count 1)))
				(progn (prin "✗ MISMATCH")(print) (setq fail_count (+ fail_count 1)))))
		(progn (prin "✗ FAILED")(print) (setq fail_count (+ fail_count 1))))
	
	;; Test nested.txt
	(prin "Reading /dir1/nested.txt... ")
	(setq fh (. fs :open_file "/dir1/nested.txt" :read))
	(if fh
		(progn
			(defq content (. fs :read_file fh 100))
			(. fs :close_file fh)
			(if (eql content "Nested in dir1")
				(progn (prin "✓ OK")(print) (setq success_count (+ success_count 1)))
				(progn (prin "✗ MISMATCH")(print) (setq fail_count (+ fail_count 1)))))
		(progn (prin "✗ FAILED")(print) (setq fail_count (+ fail_count 1))))
	
	;; Test deep.txt
	(prin "Reading /dir1/subdir/deep.txt... ")
	(setq fh (. fs :open_file "/dir1/subdir/deep.txt" :read))
	(if fh
		(progn
			(defq content (. fs :read_file fh 100))
			(. fs :close_file fh)
			(if (eql content "Deep nested file")
				(progn (prin "✓ OK")(print) (setq success_count (+ success_count 1)))
				(progn (prin "✗ MISMATCH")(print) (setq fail_count (+ fail_count 1)))))
		(progn (prin "✗ FAILED")(print) (setq fail_count (+ fail_count 1))))
	
	(print)
	(prin "=== Results ===")(print)
	(prin "Success: " success_count)(print)
	(prin "Failed: " fail_count)(print)
	(if (= fail_count 0)
		(progn (prin "✓ ALL TESTS PASSED!")(print))
		(progn (prin "✗ Some tests failed")(print)))
)

(catch
	(comprehensive-test)
	(progn (prin "Test Failed: " _)(print) :t))

;clean exit
((ffi "service/gui/lisp_deinit"))
