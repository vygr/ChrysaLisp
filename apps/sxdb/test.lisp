;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; S-expression Database Test Suite
; Comprehensive testing of all SXDb functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "apps/sxdb/client.inc")

(defq *test_count* 0 *pass_count* 0 *fail_count* 0)

(defun test-assert (name condition)
	; (test-assert name condition)
	(++ *test_count*)
	(if condition
		(progn
			(++ *pass_count*)
			(print "  ✓ " name))
		(progn
			(++ *fail_count*)
			(print "  ✗ FAIL: " name))))

(defun test-equal (name expected actual)
	; (test-equal name expected actual)
	(test-assert name (eql expected actual)))

(defun test-not-nil (name value)
	; (test-not-nil name value)
	(test-assert name (not (nil? value))))

(defun test-is-list (name value)
	; (test-is-list name value)
	(test-assert name (list? value)))

(defun test-result-ok (name result)
	; (test-result-ok name result)
	(test-assert name (and (list? result) (eql (first result) :ok))))

(defun test-result-error (name result)
	; (test-result-error name result)
	(test-assert name (and (str? result) (starts-with "ERROR:" result))))

(defun extract-result (result)
	; (extract-result result) -> value
	(if (and (list? result) (eql (first result) :ok))
		(second result)
		result))

(defun print-section (title)
	; (print-section title)
	(print)
	(print "========================================")
	(print title)
	(print "========================================"))

(defun print-summary ()
	; (print-summary)
	(print-section "Test Summary")
	(print "Total tests:  " *test_count*)
	(print "Passed:       " *pass_count*)
	(print "Failed:       " *fail_count*)
	(if (= *fail_count* 0)
		(print "\nAll tests PASSED! ✓")
		(print "\nSome tests FAILED! ✗"))
	(print))

(defun cleanup-test-db (db db_path)
	; (cleanup-test-db db db_path)
	(catch (sxdb-close db db_path) :t)
	(catch (file-delete db_path) :t))

(defun main ()
	(print-section "SXDb Test Suite")

	;connect to database service
	(print "Connecting to SXDb service...")
	(unless (defq db (sxdb-connect))
		(print "ERROR: SXDb service not running!")
		(print "Please start the service first:")
		(print "  (import \"apps/sxdb/app.lisp\")")
		(return))
	(print "Connected!\n")

	(defq db_path "./test_sxdb.sxdb")
	(cleanup-test-db db db_path)

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Test 1: Database Operations
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(print-section "1. Database Operations")

	(defq result (sxdb-open db db_path))
	(test-result-ok "Open database" result)
	(test-not-nil "Database path returned" (extract-result result))

	(defq dbs (extract-result (sxdb-list-dbs db)))
	(test-is-list "List databases returns list" dbs)
	(test-assert "Database in list" (find db_path dbs))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Test 2: Index Creation
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(print-section "2. Index Creation")

	(test-result-ok "Create index on name" (sxdb-create-index db db_path "users" "name"))
	(test-result-ok "Create index on email" (sxdb-create-index db db_path "users" "email"))
	(test-result-ok "Create index on age" (sxdb-create-index db db_path "users" "age"))

	(test-result-error "Duplicate index fails" (sxdb-create-index db db_path "users" "name"))

	(defq indexes (extract-result (sxdb-list-indexes db db_path "users")))
	(test-equal "Three indexes created" 3 (length indexes))
	(test-assert "Name index exists" (find "name" indexes))
	(test-assert "Email index exists" (find "email" indexes))
	(test-assert "Age index exists" (find "age" indexes))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Test 3: Insert Operations
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(print-section "3. Insert Operations")

	(defq user1 (scatter (Emap) :name "Alice" :email "alice@example.com" :age 30))
	(defq id1 (extract-result (sxdb-insert db db_path "users" user1)))
	(test-not-nil "Insert user1 returns ID" id1)
	(test-equal "First ID is 1" 1 id1)

	(defq user2 (scatter (Emap) :name "Bob" :email "bob@example.com" :age 25))
	(defq id2 (extract-result (sxdb-insert db db_path "users" user2)))
	(test-equal "Second ID is 2" 2 id2)

	(defq user3 (scatter (Emap) :name "Charlie" :email "charlie@example.com" :age 35))
	(defq id3 (extract-result (sxdb-insert db db_path "users" user3)))
	(test-equal "Third ID is 3" 3 id3)

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Test 4: Find by ID
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(print-section "4. Find by ID")

	(defq found (extract-result (sxdb-find db db_path "users" id1)))
	(test-not-nil "Find by id1" found)
	(test-equal "Found correct name" "Alice" (. found :find "name"))
	(test-equal "Found has _id field" id1 (. found :find :_id))

	(defq not_found (extract-result (sxdb-find db db_path "users" 9999)))
	(test-equal "Find non-existent ID returns nil" :nil not_found)

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Test 5: Find by Indexed Field
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(print-section "5. Find by Indexed Field")

	(defq results (extract-result (sxdb-find-by db db_path "users" "name" "Bob")))
	(test-equal "Find by name returns 1 result" 1 (length results))
	(test-equal "Found correct user" "Bob" (. (first results) :find "name"))

	(defq results (extract-result (sxdb-find-by db db_path "users" "age" 30)))
	(test-equal "Find by age=30 returns 1 result" 1 (length results))

	(defq results (extract-result (sxdb-find-by db db_path "users" "age" 99)))
	(test-equal "Find non-existent value returns empty list" 0 (length results))

	(test-result-error "Find by non-indexed field fails"
		(sxdb-find-by db db_path "users" "city" "NYC"))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Test 6: Get All Records
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(print-section "6. Get All Records")

	(defq all_users (extract-result (sxdb-all db db_path "users")))
	(test-equal "All returns 3 users" 3 (length all_users))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Test 7: Update Operations
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(print-section "7. Update Operations")

	(test-result-ok "Update user"
		(sxdb-update db db_path "users" id1
			(scatter (Emap) :age 31 :city "New York")))

	(defq updated (extract-result (sxdb-find db db_path "users" id1)))
	(test-equal "Age updated to 31" 31 (. updated :find "age"))
	(test-equal "City added" "New York" (. updated :find "city"))
	(test-equal "Name unchanged" "Alice" (. updated :find "name"))

	;verify index was updated
	(defq results (extract-result (sxdb-find-by db db_path "users" "age" 30)))
	(test-equal "Old age value has 0 results" 0 (length results))
	(defq results (extract-result (sxdb-find-by db db_path "users" "age" 31)))
	(test-equal "New age value has 1 result" 1 (length results))

	(test-result-error "Update non-existent record fails"
		(sxdb-update db db_path "users" 9999 (Emap)))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Test 8: Query with Predicates
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(print-section "8. Query with Predicates")

	(defq results (extract-result
		(sxdb-query db db_path "users"
			(lambda (r) (> (. r :find "age") 26)))))
	(test-equal "Query age > 26 returns 2 results" 2 (length results))

	(defq results (extract-result
		(sxdb-query db db_path "users"
			(lambda (r) (starts-with "C" (. r :find "name"))))))
	(test-equal "Query name starts with C returns 1 result" 1 (length results))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Test 9: Delete Operations
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(print-section "9. Delete Operations")

	(test-result-ok "Delete user" (sxdb-delete db db_path "users" id2))

	(defq all_users (extract-result (sxdb-all db db_path "users")))
	(test-equal "After delete, 2 users remain" 2 (length all_users))

	(defq deleted (extract-result (sxdb-find db db_path "users" id2)))
	(test-equal "Deleted user not found" :nil deleted)

	;verify index was updated
	(defq results (extract-result (sxdb-find-by db db_path "users" "name" "Bob")))
	(test-equal "Deleted user not in index" 0 (length results))

	(test-result-error "Delete non-existent record fails"
		(sxdb-delete db db_path "users" 9999))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Test 10: Collection Operations
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(print-section "10. Collection Operations")

	;insert into another collection
	(defq product (scatter (Emap) :name "Widget" :price 19.99))
	(test-not-nil "Insert into products collection"
		(extract-result (sxdb-insert db db_path "products" product)))

	(defq collections (extract-result (sxdb-list-collections db db_path)))
	(test-equal "Two collections exist" 2 (length collections))
	(test-assert "Users collection exists" (find "users" collections))
	(test-assert "Products collection exists" (find "products" collections))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Test 11: Statistics
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(print-section "11. Statistics")

	(defq stats (extract-result (sxdb-stats db db_path "users")))
	(test-not-nil "Stats returned" stats)
	(test-equal "Record count is 2" 2 (. stats :find :record_count))
	(test-equal "Next ID is 4" 4 (. stats :find :next_id))
	(test-equal "Index count is 3" 3 (. stats :find :index_count))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Test 12: Index Management
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(print-section "12. Index Management")

	(test-result-ok "Drop age index" (sxdb-drop-index db db_path "users" "age"))

	(defq indexes (extract-result (sxdb-list-indexes db db_path "users")))
	(test-equal "Two indexes remain" 2 (length indexes))
	(test-assert "Age index removed" (not (find "age" indexes)))

	(test-result-error "Drop non-existent index fails"
		(sxdb-drop-index db db_path "users" "nonexistent"))

	(test-result-error "Find-by on dropped index fails"
		(sxdb-find-by db db_path "users" "age" 31))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Test 13: Persistence
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(print-section "13. Persistence")

	;close and reopen
	(test-result-ok "Close database" (sxdb-close db db_path))

	(test-result-ok "Reopen database" (sxdb-open db db_path))

	;verify data persisted
	(defq all_users (extract-result (sxdb-all db db_path "users")))
	(test-equal "After reopen, 2 users exist" 2 (length all_users))

	(defq found (extract-result (sxdb-find db db_path "users" id1)))
	(test-not-nil "User 1 still exists" found)
	(test-equal "User 1 data persisted" "Alice" (. found :find "name"))
	(test-equal "User 1 updates persisted" 31 (. found :find "age"))

	;verify indexes persisted
	(defq indexes (extract-result (sxdb-list-indexes db db_path "users")))
	(test-equal "Indexes persisted" 2 (length indexes))

	(defq results (extract-result (sxdb-find-by db db_path "users" "name" "Alice")))
	(test-equal "Index still works after reload" 1 (length results))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Test 14: Multiple Databases
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(print-section "14. Multiple Databases")

	(defq db_path2 "./test_sxdb2.sxdb")
	(cleanup-test-db db db_path2)

	(test-result-ok "Open second database" (sxdb-open db db_path2))

	(defq dbs (extract-result (sxdb-list-dbs db)))
	(test-equal "Two databases open" 2 (length dbs))

	(defq user4 (scatter (Emap) :name "Dave" :email "dave@example.com"))
	(test-not-nil "Insert into second database"
		(extract-result (sxdb-insert db db_path2 "users" user4)))

	(defq users1 (extract-result (sxdb-all db db_path "users")))
	(defq users2 (extract-result (sxdb-all db db_path2 "users")))
	(test-equal "First database has 2 users" 2 (length users1))
	(test-equal "Second database has 1 user" 1 (length users2))

	(test-result-ok "Close second database" (sxdb-close db db_path2))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Test 15: Error Handling
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(print-section "15. Error Handling")

	(test-result-error "Operation on closed database fails"
		(sxdb-all db db_path2))

	(test-result-error "Operation on non-existent database fails"
		(sxdb-all db "./nonexistent.sxdb"))

	(test-result-error "Close non-open database fails"
		(sxdb-close db "./nonexistent.sxdb"))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Cleanup
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(print-section "Cleanup")

	(sxdb-close db db_path)
	(print "Closed test database")

	(catch (file-delete db_path) :t)
	(catch (file-delete db_path2) :t)
	(print "Deleted test files")

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Summary
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(print-summary)

	;return exit code
	(if (= *fail_count* 0) 0 1))
