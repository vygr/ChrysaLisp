;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; S-expression Database Example
; Demonstrates all features of the SXDb service
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "apps/sxdb/client.inc")

(defun print-result (label result)
	; Pretty print a result
	(print label ": " result))

(defun print-section (title)
	; Print a section header
	(print)
	(print "========================================")
	(print title)
	(print "========================================"))

(defun main ()
	(print-section "S-expression Database Example")

	;connect to database service
	(print "Connecting to SXDb service...")
	(unless (defq db (sxdb-connect))
		(print "ERROR: SXDb service not running!")
		(print "Please start the service first: (import \"apps/sxdb/app.lisp\")")
		(return))
	(print "Connected!")

	;database file path
	(defq db_path "./test.sxdb")

	(print-section "1. Opening Database")
	(print-result "Open database" (sxdb-open db db_path))

	(print-section "2. Creating Indexes")
	(print-result "Create index on 'name'" (sxdb-create-index db db_path "users" "name"))
	(print-result "Create index on 'email'" (sxdb-create-index db db_path "users" "email"))
	(print-result "Create index on 'age'" (sxdb-create-index db db_path "users" "age"))

	(print-section "3. Inserting Records")
	(defq users (list
		(scatter (Emap) :name "Alice" :email "alice@example.com" :age 30)
		(scatter (Emap) :name "Bob" :email "bob@example.com" :age 25)
		(scatter (Emap) :name "Charlie" :email "charlie@example.com" :age 35)
		(scatter (Emap) :name "Diana" :email "diana@example.com" :age 28)
		(scatter (Emap) :name "Eve" :email "eve@example.com" :age 30)))

	(defq user_ids (list))
	(each (lambda (user)
		(defq result (sxdb-insert db db_path "users" user))
		(print-result (cat "Insert " (. user :find "name")) result)
		(when (list? result)
			(push user_ids (second result))))
		users)

	(print-section "4. Finding Records by ID")
	(when (> (length user_ids) 0)
		(defq id (first user_ids))
		(print-result (cat "Find record with id " id) (sxdb-find db db_path "users" id)))

	(print-section "5. Finding Records by Indexed Field")
	(print-result "Find by name='Alice'" (sxdb-find-by db db_path "users" "name" "Alice"))
	(print-result "Find by age=30" (sxdb-find-by db db_path "users" "age" 30))

	(print-section "6. Querying Records")
	(print-result "Query: age > 28"
		(sxdb-query db db_path "users"
			(lambda (record) (> (. record :find "age") 28))))

	(print-section "7. Updating Records")
	(when (> (length user_ids) 0)
		(defq id (first user_ids))
		(print-result (cat "Update record " id)
			(sxdb-update db db_path "users" id
				(scatter (Emap) :age 31 :city "New York")))
		(print-result "After update" (sxdb-find db db_path "users" id)))

	(print-section "8. Getting All Records")
	(print-result "All users" (sxdb-all db db_path "users"))

	(print-section "9. Collection Statistics")
	(print-result "Stats" (sxdb-stats db db_path "users"))

	(print-section "10. Listing Resources")
	(print-result "List databases" (sxdb-list-dbs db))
	(print-result "List collections" (sxdb-list-collections db db_path))
	(print-result "List indexes" (sxdb-list-indexes db db_path "users"))

	(print-section "11. Deleting Records")
	(when (> (length user_ids) 1)
		(defq id (second user_ids))
		(print-result (cat "Delete record " id) (sxdb-delete db db_path "users" id))
		(print-result "After delete" (sxdb-all db db_path "users")))

	(print-section "12. Dropping Indexes")
	(print-result "Drop index on 'age'" (sxdb-drop-index db db_path "users" "age"))
	(print-result "Remaining indexes" (sxdb-list-indexes db db_path "users"))

	(print-section "13. Closing Database")
	(print-result "Close database" (sxdb-close db db_path))

	(print-section "Example Complete!")
	(print "Database file saved to: " db_path)
	(print "You can inspect the file contents - it's human-readable S-expressions!"))
