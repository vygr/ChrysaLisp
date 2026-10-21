;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; names of things: a path of the host's, as all the file code takes, or
; store:path, an object of this machine's store, service/storage/
;
; These say of either what the host says of a file: a stream of it, if
; it is there, how long it is, what is in a folder of it. A folder of the
; store is the start of the paths of objects, up to a /, store: the top.
; An app that is to take objects of the store as well as files opens
; what it is given by (name-stream), not (file-stream).
;
; A stream of an object is read by the functions of the task that made
; it, so it is read only by that task. What hands a stream to another
; task to read, a picture's loader to the stages of its pipeline, is
; given (name-whole-stream): all of it, read first.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "service/storage/app.inc")

(defq +name_store "store:")

(defun name-in-store (name)
	; (name-in-store name) -> :nil | path
	;the path in the store of a name of it, "" for its top
	(if (starts-with +name_store name) (slice name (length +name_store) -1)))

(defun name-stream (name &optional mode)
	; (name-stream name [mode]) -> :nil | stream
	;the thing named as a stream, mode as (file-stream) has it
	(cond
		((defq path (name-in-store name)) (storage-stream path mode))
		(mode (file-stream name mode))
		((file-stream name))))

(defun name-whole-stream (name)
	; (name-whole-stream name) -> :nil | stream
	;the thing named as a stream any task of this node can read: a file's
	;own, or all of an object, read now, as a string stream
	(if (defq path (name-in-store name))
		(if (defq data (storage-get path)) (string-stream data))
		(file-stream name)))

(defun name-size (name)
	; (name-size name) -> :nil | bytes
	;how long the thing named is, :nil if there is none, or it is a folder
	(cond
		((defq path (name-in-store name))
			(and (defq entry (storage-stat path)) (not (elem-get entry 6)) (elem-get entry 2)))
		((defq info (pii-fstat name))
			(if (/= (logand (third info) 0xf000) 0x4000) (second info)))))

(defun name-list (folder)
	; (name-list folder) -> ((name ...) (type ...))
	;what is in a folder, as (pii-dirlist) says it, unzipped: each name, and
	;its type, "4" a folder, "8" a file
	(cond
		((defq path (name-in-store folder))
			(defq start (if (empty? path) "" (cat path "/")) names (list) types (list))
			(each (lambda (entry)
					(unless (elem-get entry 6)
						(defq rest (slice (first entry) (length start) -1))
						(cond
							((defq at (find "/" rest))
								(unless (find (defq name (slice rest 0 at)) names)
									(push names name) (push types "4")))
							((nempty? rest) (push names rest) (push types "8")))))
				(ifn (storage-list start) (list)))
			(list names types))
		((unzip (split (ifn (pii-dirlist folder) "") ",") 2))))

(defun name-folder? (name)
	; (name-folder? name) -> :nil | :t
	;a folder of the host, or of the store, one there is an object in
	(cond
		((defq path (name-in-store name))
			(or (empty? path) (and (storage-service)
				(some (# (not (elem-get %0 6))) (ifn (storage-list (cat path "/")) (list))) :t)))
		((and (defq info (pii-fstat name)) (= (logand (third info) 0xf000) 0x4000)))))

(defun name-there? (name)
	; (name-there? name) -> :nil | :t
	;a file, an object, or a folder of either, of that name
	(or (name-size name) (name-folder? name)))
