;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; S-expression Database Client Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun sxdb-connect ()
	; (sxdb-connect) -> mbox | :nil
	; Connect to the SXDb service
	(defq services (mail-enquire "SXDb,"))
	(if (> (length services) 0)
		(progn
			(defq service (split (first services) ","))
			(defq mbox (second service))
			mbox)
		:nil))

(defun sxdb-send (db_mbox cmd &rest args)
	; (sxdb-send db_mbox cmd [args]) -> response
	; Send a command to the database and wait for response
	(defq reply_mbox (task-mbox))
	(mail-send db_mbox (cat (list reply_mbox cmd) args))
	(defq response (mail-read reply_mbox))
	response)

(defun sxdb-open (db_mbox path)
	; (sxdb-open db_mbox path) -> result
	; Open or create a database
	(sxdb-send db_mbox :open path))

(defun sxdb-close (db_mbox path)
	; (sxdb-close db_mbox path) -> result
	; Close a database
	(sxdb-send db_mbox :close path))

(defun sxdb-insert (db_mbox path coll record)
	; (sxdb-insert db_mbox path coll record) -> id
	; Insert a record into a collection
	(sxdb-send db_mbox :insert path coll record))

(defun sxdb-find (db_mbox path coll id)
	; (sxdb-find db_mbox path coll id) -> record | :nil
	; Find a record by id
	(sxdb-send db_mbox :find path coll id))

(defun sxdb-find-by (db_mbox path coll field value)
	; (sxdb-find-by db_mbox path coll field value) -> (list records)
	; Find records by indexed field
	(sxdb-send db_mbox :find-by path coll field value))

(defun sxdb-update (db_mbox path coll id updates)
	; (sxdb-update db_mbox path coll id updates) -> result
	; Update a record
	(sxdb-send db_mbox :update path coll id updates))

(defun sxdb-delete (db_mbox path coll id)
	; (sxdb-delete db_mbox path coll id) -> result
	; Delete a record
	(sxdb-send db_mbox :delete path coll id))

(defun sxdb-all (db_mbox path coll)
	; (sxdb-all db_mbox path coll) -> (list records)
	; Get all records in a collection
	(sxdb-send db_mbox :all path coll))

(defun sxdb-create-index (db_mbox path coll field)
	; (sxdb-create-index db_mbox path coll field) -> result
	; Create an index on a field
	(sxdb-send db_mbox :create-index path coll field))

(defun sxdb-drop-index (db_mbox path coll field)
	; (sxdb-drop-index db_mbox path coll field) -> result
	; Drop an index
	(sxdb-send db_mbox :drop-index path coll field))

(defun sxdb-query (db_mbox path coll pred)
	; (sxdb-query db_mbox path coll pred) -> (list records)
	; Query records with a predicate function
	(sxdb-send db_mbox :query path coll pred))

(defun sxdb-list-dbs (db_mbox)
	; (sxdb-list-dbs db_mbox) -> (list db_paths)
	; List all open databases
	(sxdb-send db_mbox :list-dbs))

(defun sxdb-list-collections (db_mbox path)
	; (sxdb-list-collections db_mbox path) -> (list coll_names)
	; List all collections in a database
	(sxdb-send db_mbox :list-collections path))

(defun sxdb-list-indexes (db_mbox path coll)
	; (sxdb-list-indexes db_mbox path coll) -> (list field_names)
	; List all indexes in a collection
	(sxdb-send db_mbox :list-indexes path coll))

(defun sxdb-stats (db_mbox path coll)
	; (sxdb-stats db_mbox path coll) -> stats
	; Get collection statistics
	(sxdb-send db_mbox :stats path coll))

;module
(export-symbols '(
	sxdb-connect
	sxdb-send
	sxdb-open
	sxdb-close
	sxdb-insert
	sxdb-find
	sxdb-find-by
	sxdb-update
	sxdb-delete
	sxdb-all
	sxdb-create-index
	sxdb-drop-index
	sxdb-query
	sxdb-list-dbs
	sxdb-list-collections
	sxdb-list-indexes
	sxdb-stats
))
(env-pop)
