;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Stores, what the app is asked to do, done: ./app.lisp
;
; A task of its own, so that the app goes on drawing while it runs. It is
; mailed the once, a list as (str) prints it:
;
;	(reply what from to rule names copies)
;
; reply the mailbox it tells, in hex. what :copy, :delete or :copies. from
; and to each a side, (:host folder) or (:store svc folder), svc the
; service in hex, the folder of a store "" for its top. rule what is done
; with a thing that is at to already: :skip it, :replace it, or keep
; :both, the new one with a number in its name. names what was taken in
; the folder of from, (name folder) each, folder :t for a folder, all
; under which is done. copies the dial of how many keep, for :copies.
;
; A copy goes from a file of the host into a store, from a store out to
; a file, or from a store to a store, with no file between: only the
; blocks the store it goes to has not got are sent, so what has been
; copied before, or is there by another name, is little sent. One that
; is the same object at both is left. That is what a merge is, a folder
; copied to one of the same name: what is in one and not the other is
; copied, and what is in both is as the rule says.
;
; It tells reply, each a list as (str) prints it: (:plan count bytes)
; once it knows what there is, (:at bytes count name) every so often as
; it goes, (:not name why) of each that could not be done, and at the
; end (:done done skipped failed bytes ms stopped). Any mail to it, and
; it stops at the block it is at, what it was doing then left undone.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "service/storage/app.inc")
(import "lib/files/files.inc")

;how often it says how it is going, microseconds
(defq +tell_rate 250000)

(defun tell (&rest what)
	(mail-send *reply* (str what)))

(defun side-svc (side)
	(if (eql (first side) :store) (second side)))

(defun side-path (side name)
	;a name in the folder of a side
	(defq folder (last side))
	(cond
		((eql (first side) :host) (if (ends-with "/" folder) (cat folder name) (cat folder "/" name)))
		((empty? folder) name)
		((cat folder "/" name))))

(defun side-there (side path)
	;what is at a path of a side now, :nil if nothing
	(if (eql (first side) :host)
		(pii-fstat path)
		(storage-stat path (side-svc side))))

(defun host-bytes (file)
	(if (defq info (pii-fstat file)) (second info) 0))

(defun things-of (side names)
	;each thing to be done, (path name bytes): the files or objects taken,
	;and every one under a folder taken, its name from the folder of the
	;side down. Not those whose names begin with a dot, of the host
	(defq out (list) svc (side-svc side))
	(each (lambda ((name folder))
			(defq path (side-path side name))
			(cond
				((and folder (eql (first side) :host))
					(each (lambda (file)
							(unless (starts-with "." (path-name file))
								(push out (list file (cat name (slice file (length path) -1)) (host-bytes file)))))
						(files-all path '(""))))
				((eql (first side) :host)
					(push out (list path name (host-bytes path))))
				(folder
					(each (lambda (entry)
							(push out (list (first entry) (cat name (slice (first entry) (length path) -1))
								(elem-get entry +store_entry_bytes))))
						(ifn (storage-list (cat path "/") svc) (list))))
				((defq entry (storage-stat path svc))
					(push out (list path name (elem-get entry +store_entry_bytes))))))
		names)
	out)

(defun free-name (side path)
	;a path like this one that has nothing at it: name (2).ext, name (3).ext
	(defq name (path-name path) folder (slice path 0 (- (length path) (length name)))
		stem (path-stem name) ext (path-ext name) n 1 try path)
	(while (side-there side try)
		(setq try (cat folder stem " (" (str (++ n)) ")" ext)))
	try)

(defun work-tick (bytes)
	;told of each part done: how it is going said every so often, and :nil
	;once it has been told to stop
	(setq *bytes* (+ *bytes* bytes))
	(when (> (- (pii-time) *told*) +tell_rate)
		(setq *told* (pii-time))
		(tell :at *bytes* *count* *name*))
	(not (setq *stopped* (or *stopped* (and (mail-poll (list (task-mbox))) :t)))))

(defun copy-one (path name bytes)
	;one thing copied to its name in the folder of to, as the rule says if
	;there is one there already
	(defq to_path (side-path *to* name) was (side-there *to* to_path) start *bytes*
		from_svc (side-svc *from*) to_svc (side-svc *to*)
		same (and was from_svc to_svc (defq entry (storage-stat path from_svc))
			(eql (elem-get entry +store_entry_hash) (elem-get was +store_entry_hash))))
	(cond
		((or same (and was (eql *rule* :skip))) (++ *skipped*))
		(:t (and was (eql *rule* :both) (setq to_path (free-name *to* to_path) was :nil))
			;a store's path is set provided it is what was seen, so a write of
			;another's in the meantime is not lost
			(defq from_was (cond ((not to_svc) :nil) (was (elem-get was +store_entry_hash)) (""))
				got (first (cond
					((not from_svc) (storage-put-file to_path path from_was to_svc work-tick))
					((not to_svc) (storage-get-file path to_path from_svc work-tick))
					((storage-copy-object path to_path from_svc to_svc from_was work-tick)))))
			(case got
				(:ok (++ *done*))
				(:stopped)
				(:t (++ *failed*) (tell :not name got)))))
	;all of it gone past, whatever was told of it, unless it was stopped
	(unless *stopped* (setq *bytes* (+ start bytes))))

(defun main ()
	(bind '(reply what from to rule names copies) (first (read (string-stream (mail-read (task-mbox))))))
	(defq *reply* (hex-decode reply) *rule* rule *bytes* 0 *count* 0 *name* "" *told* 0
		*done* 0 *skipped* 0 *failed* 0 *stopped* :nil t0 (pii-time)
		*from* (if (eql (first from) :store) (list :store (hex-decode (second from)) (last from)) from)
		*to* (if (eql (first to) :store) (list :store (hex-decode (second to)) (last to)) to)
		things (things-of *from* names) svc (side-svc *from*))
	;a delete or a dial goes by how many, there are no bytes to go through
	(tell :plan (length things) (if (eql what :copy) (reduce (# (+ %0 (third %1))) things 0) (length things)))
	(each (lambda ((path name bytes))
			(unless *stopped*
				(setq *name* name)
				(case what
					(:copy (copy-one path name bytes))
					(:delete
						(if (eql (defq got (first (storage-delete path :nil svc))) :ok)
							(++ *done*) (++ *failed*) (tell :not name got)))
					(:copies
						(if (eql (defq got (first (storage-copies path copies svc))) :ok)
							(++ *done*) (++ *failed*) (tell :not name got))))
				(++ *count*)
				(unless (eql what :copy) (work-tick 1))))
		things)
	(tell :done *done* *skipped* *failed* *bytes* (/ (- (pii-time) t0) 1000) *stopped*))
