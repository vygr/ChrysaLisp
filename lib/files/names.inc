;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; names of things: a path of the host's, as all the file code takes, or
; store:path, an object of this machine's store, service/storage/, or
; store://who/path, one of another store, who its id or the start of the
; system id of a machine of it, as mesh lists it, had as a client: this
; machine is no member of it, and none of it is kept here
;
; These say of either what the host says of a file: a stream of it, if
; it is there, how long it is, what is in a folder of it. A folder of the
; store is the start of the paths of objects, up to a /, store: or
; store://who/ the top.
; An app that is to take objects of the store as well as files opens
; what it is given by (name-stream), not (file-stream).
;
; A stream of an object is read by the functions of the task that made
; it, so it is read only by that task. What hands a stream to another
; task to read, a picture's loader to the stages of its pipeline, is
; given (name-whole-stream): all of it, read first.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "service/storage/app.inc")

(defq +name_store "store:" +name_stores "store://")

(defun name-store-top (name)
	; (name-store-top name) -> :nil | top
	;the top of the store a name is of, store: or store://who/
	(cond
		((starts-with +name_stores name)
			(defq rest (slice name (length +name_stores) -1))
			(cat +name_stores (if (defq at (find "/" rest)) (slice rest 0 at) rest) "/"))
		((starts-with +name_store name) +name_store)))

(defun name-in-store (name)
	; (name-in-store name) -> :nil | path
	;the path in the store of a name of it, "" for its top
	(if (defq top (name-store-top name)) (slice name (min (length top) (length name)) -1)))

(defun name-service (name)
	; (name-service name) -> :nil | netid
	;the Storage service a name of a store is had of, this machine's or a
	;member of the other store, (storage-service-of)
	(if (starts-with +name_stores name)
		(storage-service-of (slice (name-store-top name) (length +name_stores) -2))
		(storage-service)))

(defun name-stream (name &optional mode)
	; (name-stream name [mode]) -> :nil | stream
	;the thing named as a stream, mode as (file-stream) has it
	(cond
		((defq path (name-in-store name)) (if (defq svc (name-service name)) (storage-stream path mode svc)))
		(mode (file-stream name mode))
		((file-stream name))))

(defun name-whole-stream (name)
	; (name-whole-stream name) -> :nil | stream
	;the thing named as a stream any task of this node can read: a file's
	;own, or all of an object, read now, as a string stream
	(if (defq path (name-in-store name))
		(and (defq svc (name-service name)) (defq data (storage-get path svc)) (string-stream data))
		(file-stream name)))

(defun name-size (name)
	; (name-size name) -> :nil | bytes
	;how long the thing named is, :nil if there is none, or it is a folder
	(cond
		((defq path (name-in-store name))
			(and (defq svc (name-service name)) (defq entry (storage-stat path svc))
				(not (elem-get entry 6)) (elem-get entry 2)))
		((defq info (pii-fstat name))
			(unless (folder-mode? (third info)) (second info)))))

(defun name-list (folder)
	; (name-list folder) -> ((name ...) (type ...))
	;what is in a folder, as (pii-dirlist) says it, unzipped: each name, and
	;its type, "d" a folder, "f" a file
	(cond
		((defq path (name-in-store folder))
			(defq start (if (empty? path) "" (cat path "/")) names (list) types (list) svc (name-service folder))
			(each (lambda (entry)
					(unless (elem-get entry 6)
						(defq rest (slice (first entry) (length start) -1))
						(cond
							((defq at (find "/" rest))
								(unless (find (defq name (slice rest 0 at)) names)
									(push names name) (push types "d")))
							((nempty? rest) (push names rest) (push types "f")))))
				(ifn (if svc (storage-list start svc)) (list)))
			(list names types))
		((dir-list folder))))

(defun name-folder? (name)
	; (name-folder? name) -> :nil | :t
	;a folder of the host, or of the store, one there is an object in
	(cond
		((defq path (name-in-store name))
			(or (empty? path) (and (defq svc (name-service name))
					(some (# (not (elem-get %0 6))) (ifn (storage-list (cat path "/") svc) (list))) :t)))
		((and (defq info (pii-fstat name)) (folder-mode? (third info))))))

(defun name-there? (name)
	; (name-there? name) -> :nil | :t
	;a file, an object, or a folder of either, of that name
	(or (name-size name) (name-folder? name)))
