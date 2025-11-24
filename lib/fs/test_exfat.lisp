(import "./exfat.inc")

(defun test-exfat ()
	(prin "Testing ExFAT Filesystem...")(print)
	
	;; 1. Format
	(defq fs (ExFat))
	(. fs :format 1 (memory-stream))
	(prin "Formatted ExFAT.")(print)
	
	;; 2. Mount
	(. fs :mount)
	(prin "Mounted ExFAT.")(print)
	
	;; 3. Create Directory
	(defq root_cluster (get :root_cluster fs))
	(defq dir_cluster (. fs :create_dir "/testdir"))
	(prin "Created directory 'testdir' at cluster: " dir_cluster)(print)
	
	;; 4. Verify Directory
	(defq entry (. fs :find_entry root_cluster "testdir"))
	(if entry
		(progn (prin "Found 'testdir': " entry)(print))
		(throw "Failed to find 'testdir'"))
		
	;; 5. Create File in Root
	(defq file_cluster (. fs :alloc_cluster))
	(. fs :create_entry root_cluster "file1.txt" +EXFAT_ATTR_ARCHIVE file_cluster 0)
	(prin "Created 'file1.txt' at cluster: " file_cluster)(print)
	
	;; 6. Verify File
	(setq entry (. fs :find_entry root_cluster "file1.txt"))
	(if entry
		(progn (prin "Found 'file1.txt': " entry)(print))
		(throw "Failed to find 'file1.txt'"))
		
	;; 7. Create File in Subdirectory
	(defq subfile_cluster (. fs :alloc_cluster))
	(. fs :create_entry dir_cluster "sub.txt" +EXFAT_ATTR_ARCHIVE subfile_cluster 0)
	(prin "Created 'sub.txt' in 'testdir'")(print)
	
	;; 8. Resolve Path
	(defq info (. fs :resolve_path "testdir/sub.txt"))
	(if info
		(progn (prin "Resolved 'testdir/sub.txt': " info)(print))
		(throw "Failed to resolve 'testdir/sub.txt'"))
		
	;; 9. List Directory
	(prin "Listing Root Directory:")(print)
	(defq entries (. fs :list_dir root_cluster))
	(each! (lambda (e) (prin " - " (first e))(print)) (list entries))
	
	;; 10. Test Stat
	(prin "Testing :stat method:")(print)
	(defq stat_result (. fs :stat "/testdir"))
	(prin " /testdir -> " stat_result)(print)
	(if (not (third stat_result)) (throw "testdir should be a directory"))
	
	(setq stat_result (. fs :stat "/testdir/sub.txt"))
	(prin " /testdir/sub.txt -> " stat_result)(print)
	(if (third stat_result) (throw "sub.txt should be a file"))
	
	;; 11. Delete File
	(. fs :delete_entry root_cluster "file1.txt")
	(prin "Deleted 'file1.txt'")(print)
	
	;; 12. Verify Deletion
	(setq entry (. fs :find_entry root_cluster "file1.txt"))
	(if entry
		(throw "Error: 'file1.txt' should be deleted but was found")
		(progn (prin "Verified 'file1.txt' is gone")(print)))
	
	;; 13. Test create_file
	(prin "Testing :create_file method:")(print)
	(defq new_file_cluster (. fs :create_file "/newfile.txt"))
	(prin " Created '/newfile.txt' at cluster " new_file_cluster)(print)
	
	;; Verify with stat
	(setq stat_result (. fs :stat "/newfile.txt"))
	(prin " Stat: " stat_result)(print)
	(if (third stat_result) (throw "newfile.txt should be a file not a directory"))
	
	;; Create nested file
	(setq new_file_cluster (. fs :create_file "/testdir/nested.txt"))
	(prin " Created '/testdir/nested.txt' at cluster " new_file_cluster)(print)
	
	;; 14. Test delete_file
	(prin "Testing :delete_file method:")(print)
	(. fs :delete_file "/newfile.txt")
	(prin " Deleted '/newfile.txt'")(print)
	
	(prin "About to test file handle operations...")(print)
	
	;; 15. Test file handle operations
	(prin "Testing file handle operations:")(print)
	
	;; Open file for reading
	(prin " About to open file...")(print)
	(defq fh (. fs :open_file "/testdir/sub.txt" :read))
	(prin " Opened '/testdir/sub.txt', handle=" fh)(print)
	
	;; 16. Test write-seek-read cycle
	(prin "Testing write-seek-read cycle:")(print)
	
	;; Create a new file
	(. fs :create_file "/testdir/write_test.txt")
	(prin " Created '/testdir/write_test.txt'")(print)
	
	;; Open for writing
	(defq fh (. fs :open_file "/testdir/write_test.txt" :write))
	(prin " Opened for writing, handle=" fh)(print)
	
	;; Write data
	(defq test_data "Hello, ExFAT World!")
	(. fs :write_file fh test_data)
	(prin " Wrote data: '" test_data "'")(print)
	
	;; Close file
	(. fs :close_file fh)
	(prin " Closed file")(print)
	
	;; Open for reading
	(defq fh (. fs :open_file "/testdir/write_test.txt" :read))
	(prin " Opened for reading, handle=" fh)(print)
	
	;; Read data
	(defq read_data (. fs :read_file fh (length test_data)))
	(prin " Read data: '" read_data "'")(print)
	
	;; Verify
	(if (eql read_data test_data)
		(progn (prin " Verification SUCCESS")(print))
		(throw "Verification FAILED: Data mismatch" read_data))
		
	;; Close file
	(. fs :close_file fh)
	
	;; Test append
	(prin "Testing append:")(print)
	(defq fh (. fs :open_file "/testdir/write_test.txt" :append))
	(prin " Opened for append, handle=" fh)(print)
	
	(. fs :write_file fh " Appended data.")
	(prin " Appended data")(print)
	(. fs :close_file fh)
	
	;; Verify append
	(defq fh (. fs :open_file "/testdir/write_test.txt" :read))
	(defq full_data (. fs :read_file fh 100))
	(prin " Full data: '" full_data "'")(print)
	(. fs :close_file fh)
	
	(if (eql full_data "Hello, ExFAT World! Appended data.")
		(progn (prin " Append Verification SUCCESS")(print))
		(throw "Append Verification FAILED" full_data))

	;; 17. Test multi-cluster write
	(prin "Testing multi-cluster write (10KB):")(print)
	
	(. fs :create_file "/testdir/large.txt")
	(defq fh (. fs :open_file "/testdir/large.txt" :write))
	
	;; Generate 10KB data
	(defq pattern "0123456789ABCDEF") ;; 16 bytes
	(defq large_data (str-alloc 10240))
	(defq i 0)
	(while (< i 640) ;; 640 * 16 = 10240
		(defq j 0)
		(while (< j 16)
			(set-byte large_data (+ (* i 16) j) (get-ubyte pattern j))
			(setq j (+ j 1)))
		(setq i (+ i 1)))
		
	(prin " Writing 10KB data...")(print)
	(. fs :write_file fh large_data)
	(. fs :close_file fh)
	
	;; Verify
	(defq fh (. fs :open_file "/testdir/large.txt" :read))
	(defq read_back (. fs :read_file fh 10240))
	(. fs :close_file fh)
	
	(if (eql large_data read_back)
		(progn (prin " Multi-cluster Verification SUCCESS")(print))
		(throw "Multi-cluster Verification FAILED"))
		
	;; 18. Test delete_dir
	(prin "Testing delete_dir:")(print)
	(. fs :create_dir "/testdir/todelete")
	(prin " Created '/testdir/todelete'")(print)
	
	(. fs :delete_dir "/testdir/todelete")
	(prin " Deleted '/testdir/todelete'")(print)
	
	;; Verify deletion
	(defq info (. fs :stat "/testdir/todelete"))
	(if info
		(throw "Directory should have been deleted" info)
		(progn (prin " Directory deletion Verification SUCCESS")(print)))
		
	(prin "ExFAT Tests Completed Successfully.")(print)
)

(catch
	(test-exfat)
	(progn (prin "Test Failed with exception" _)(print) :t))

;clean exit
((ffi "service/gui/lisp_deinit"))
