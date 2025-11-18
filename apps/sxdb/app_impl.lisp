;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; S-expression Database Service
; Natural persistence format for Lisp data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/collections/tree.inc")

;;;;;;;;;;;;;;;;;
; Database Schema
;;;;;;;;;;;;;;;;;

; Database structure:
; (Emap
;   :name "database_name"
;   :path "/path/to/db.sxdb"
;   :collections (Emap
;     "collection_name" (Emap
;       :data (Emap id -> record)
;       :next_id num
;       :indexes (Emap field_name -> (Emap field_value -> (list ids...)))
;     )
;   )
; )

;;;;;;;;;;;;;;;;;
; Helper Functions
;;;;;;;;;;;;;;;;;

(defun db-error (msg &rest args)
	; (db-error msg [args]) -> error_msg
	(setq *response* (cat "ERROR: " msg))
	(when args
		(setq *response* (cat *response* " " (str args))))
	:nil)

(defun db-success (result)
	; (db-success result) -> result
	(setq *response* (list :ok result))
	result)

(defun get-collection (db coll_name)
	; (get-collection db coll_name) -> collection | :nil
	(defq collections (. db :find :collections))
	(if collections
		(. collections :find coll_name)))

(defun ensure-collection (db coll_name)
	; (ensure-collection db coll_name) -> collection
	(defq collections (. db :find :collections))
	(unless collections
		(. db :insert :collections (setq collections (Emap))))
	(defq coll (. collections :find coll_name))
	(unless coll
		(setq coll (scatter (Emap)
			:data (Emap)
			:next_id 1
			:indexes (Emap)))
		(. collections :insert coll_name coll))
	coll)

(defun generate-id (coll)
	; (generate-id coll) -> id
	(defq id (. coll :find :next_id))
	(. coll :insert :next_id (inc id))
	id)

(defun index-add (coll field value id)
	; (index-add coll field value id)
	(defq indexes (. coll :find :indexes))
	(when (defq index (. indexes :find field))
		(defq value_str (str value))
		(defq id_list (. index :find value_str))
		(unless id_list
			(. index :insert value_str (setq id_list (list))))
		(unless (find id id_list)
			(push id_list id))))

(defun index-remove (coll field value id)
	; (index-remove coll field value id)
	(defq indexes (. coll :find :indexes))
	(when (defq index (. indexes :find field))
		(defq value_str (str value))
		(when (defq id_list (. index :find value_str))
			(. index :insert value_str (filter (# (/= %0 id)) id_list)))))

(defun index-record (coll record id)
	; (index-record coll record id)
	(defq indexes (. coll :find :indexes))
	(each (lambda (field)
		(when (defq value (. record :find field))
			(index-add coll field value id)))
		(. indexes :keys)))

(defun deindex-record (coll record id)
	; (deindex-record coll record id)
	(defq indexes (. coll :find :indexes))
	(each (lambda (field)
		(when (defq value (. record :find field))
			(index-remove coll field value id)))
		(. indexes :keys)))

;;;;;;;;;;;;;;;;;
; Database Operations
;;;;;;;;;;;;;;;;;

(defun cmd-open (db_path)
	; (cmd-open db_path) -> db
	; Open or create a database
	(when (. *databases* :find db_path)
		(return (db-error "Database already open" db_path)))

	(defq db :nil)
	(if (defq stream (file-stream db_path))
		;load existing database
		(setq db (tree-load stream))
		;create new database
		(setq db (scatter (Emap)
			:name (last (split db_path "/"))
			:path db_path
			:collections (Emap))))

	(. db :insert :path db_path)
	(. *databases* :insert db_path db)
	(db-success (list :path db_path :name (. db :find :name))))

(defun cmd-close (db_path)
	; (cmd-close db_path) -> :ok
	; Close and save a database
	(unless (defq db (. *databases* :find db_path))
		(return (db-error "Database not open" db_path)))

	;save database
	(when (defq stream (file-stream db_path +file_open_write))
		(tree-save stream db))

	(. *databases* :remove db_path)
	(db-success :closed))

(defun cmd-insert (db_path coll_name record)
	; (cmd-insert db_path coll_name record) -> id
	; Insert a record into a collection
	(unless (defq db (. *databases* :find db_path))
		(return (db-error "Database not open" db_path)))

	(defq coll (ensure-collection db coll_name))
	(defq data (. coll :find :data))
	(defq id (generate-id coll))

	;add id to record
	(. record :insert :_id id)

	;add to data
	(. data :insert id record)

	;update indexes
	(index-record coll record id)

	(db-success id))

(defun cmd-find (db_path coll_name id)
	; (cmd-find db_path coll_name id) -> record | :nil
	; Find a record by id
	(unless (defq db (. *databases* :find db_path))
		(return (db-error "Database not open" db_path)))

	(unless (defq coll (get-collection db coll_name))
		(return (db-success :nil)))

	(defq data (. coll :find :data))
	(db-success (. data :find id)))

(defun cmd-find-by (db_path coll_name field value)
	; (cmd-find-by db_path coll_name field value) -> (list records)
	; Find records by indexed field
	(unless (defq db (. *databases* :find db_path))
		(return (db-error "Database not open" db_path)))

	(unless (defq coll (get-collection db coll_name))
		(return (db-success (list))))

	(defq indexes (. coll :find :indexes))
	(unless (defq index (. indexes :find field))
		(return (db-error "No index on field" field)))

	(defq value_str (str value))
	(defq id_list (. index :find value_str))
	(unless id_list
		(return (db-success (list))))

	(defq data (. coll :find :data))
	(defq results (list))
	(each (lambda (id)
		(when (defq record (. data :find id))
			(push results record)))
		id_list)

	(db-success results))

(defun cmd-update (db_path coll_name id updates)
	; (cmd-update db_path coll_name id updates) -> :ok | :nil
	; Update a record
	(unless (defq db (. *databases* :find db_path))
		(return (db-error "Database not open" db_path)))

	(unless (defq coll (get-collection db coll_name))
		(return (db-error "Collection not found" coll_name)))

	(defq data (. coll :find :data))
	(unless (defq record (. data :find id))
		(return (db-error "Record not found" id)))

	;deindex old values
	(deindex-record coll record id)

	;apply updates
	(each (lambda ((k v))
		(. record :insert k v))
		(. updates :entries))

	;reindex new values
	(index-record coll record id)

	(db-success :updated))

(defun cmd-delete (db_path coll_name id)
	; (cmd-delete db_path coll_name id) -> :ok | :nil
	; Delete a record
	(unless (defq db (. *databases* :find db_path))
		(return (db-error "Database not open" db_path)))

	(unless (defq coll (get-collection db coll_name))
		(return (db-error "Collection not found" coll_name)))

	(defq data (. coll :find :data))
	(unless (defq record (. data :find id))
		(return (db-error "Record not found" id)))

	;deindex
	(deindex-record coll record id)

	;remove from data
	(. data :remove id)

	(db-success :deleted))

(defun cmd-all (db_path coll_name)
	; (cmd-all db_path coll_name) -> (list records)
	; Get all records in a collection
	(unless (defq db (. *databases* :find db_path))
		(return (db-error "Database not open" db_path)))

	(unless (defq coll (get-collection db coll_name))
		(return (db-success (list))))

	(defq data (. coll :find :data))
	(db-success (. data :values)))

(defun cmd-create-index (db_path coll_name field)
	; (cmd-create-index db_path coll_name field) -> :ok
	; Create an index on a field
	(unless (defq db (. *databases* :find db_path))
		(return (db-error "Database not open" db_path)))

	(defq coll (ensure-collection db coll_name))
	(defq indexes (. coll :find :indexes))

	(when (. indexes :find field)
		(return (db-error "Index already exists" field)))

	;create index
	(defq index (Emap))
	(. indexes :insert field index)

	;populate index with existing data
	(defq data (. coll :find :data))
	(each (lambda ((id record))
		(when (defq value (. record :find field))
			(index-add coll field value id)))
		(. data :entries))

	(db-success :created))

(defun cmd-drop-index (db_path coll_name field)
	; (cmd-drop-index db_path coll_name field) -> :ok
	; Drop an index
	(unless (defq db (. *databases* :find db_path))
		(return (db-error "Database not open" db_path)))

	(unless (defq coll (get-collection db coll_name))
		(return (db-error "Collection not found" coll_name)))

	(defq indexes (. coll :find :indexes))
	(unless (. indexes :find field)
		(return (db-error "Index does not exist" field)))

	(. indexes :remove field)
	(db-success :dropped))

(defun cmd-query (db_path coll_name pred)
	; (cmd-query db_path coll_name pred) -> (list records)
	; Query records with a predicate function
	(unless (defq db (. *databases* :find db_path))
		(return (db-error "Database not open" db_path)))

	(unless (defq coll (get-collection db coll_name))
		(return (db-success (list))))

	(defq data (. coll :find :data))
	(defq results (list))
	(each (lambda ((id record))
		(when (eval (list pred record))
			(push results record)))
		(. data :entries))

	(db-success results))

(defun cmd-list-dbs ()
	; (cmd-list-dbs) -> (list db_paths)
	; List all open databases
	(db-success (. *databases* :keys)))

(defun cmd-list-collections (db_path)
	; (cmd-list-collections db_path) -> (list coll_names)
	; List all collections in a database
	(unless (defq db (. *databases* :find db_path))
		(return (db-error "Database not open" db_path)))

	(defq collections (. db :find :collections))
	(if collections
		(db-success (. collections :keys))
		(db-success (list))))

(defun cmd-list-indexes (db_path coll_name)
	; (cmd-list-indexes db_path coll_name) -> (list field_names)
	; List all indexes in a collection
	(unless (defq db (. *databases* :find db_path))
		(return (db-error "Database not open" db_path)))

	(unless (defq coll (get-collection db coll_name))
		(return (db-success (list))))

	(defq indexes (. coll :find :indexes))
	(db-success (. indexes :keys)))

(defun cmd-stats (db_path coll_name)
	; (cmd-stats db_path coll_name) -> stats
	; Get collection statistics
	(unless (defq db (. *databases* :find db_path))
		(return (db-error "Database not open" db_path)))

	(unless (defq coll (get-collection db coll_name))
		(return (db-error "Collection not found" coll_name)))

	(defq data (. coll :find :data))
	(defq indexes (. coll :find :indexes))

	(db-success (scatter (Emap)
		:record_count (length (. data :keys))
		:next_id (. coll :find :next_id)
		:index_count (length (. indexes :keys))
		:indexes (. indexes :keys))))

;;;;;;;;;;;;;;;;;
; Message Dispatcher
;;;;;;;;;;;;;;;;;

(defun dispatch-message (msg)
	; (dispatch-message msg)
	(defq reply_id (getf msg 0)
		cmd (getf msg 1)
		args (slice msg 2 -1))

	(setq *response* :nil)

	(case cmd
		(:open (apply cmd-open args))
		(:close (apply cmd-close args))
		(:insert (apply cmd-insert args))
		(:find (apply cmd-find args))
		(:find-by (apply cmd-find-by args))
		(:update (apply cmd-update args))
		(:delete (apply cmd-delete args))
		(:all (apply cmd-all args))
		(:create-index (apply cmd-create-index args))
		(:drop-index (apply cmd-drop-index args))
		(:query (apply cmd-query args))
		(:list-dbs (apply cmd-list-dbs args))
		(:list-collections (apply cmd-list-collections args))
		(:list-indexes (apply cmd-list-indexes args))
		(:stats (apply cmd-stats args))
		(:t (setq *response* (cat "ERROR: Unknown command " (str cmd)))))

	;send response
	(when reply_id
		(mail-send reply_id *response*)))

;;;;;;;;;;;;;;;;;
; Main Service Loop
;;;;;;;;;;;;;;;;;

(defun main ()
	(defq *databases* (Emap)
		service (mail-declare (task-mbox) "SXDb" "S-expression Database 1.0")
		mbox (task-mbox))

	(while :t
		(defq msg (mail-read mbox))
		(dispatch-message msg))

	;cleanup (never reached in this simple implementation)
	(mail-forget service))
