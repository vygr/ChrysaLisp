;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Storage, a folder of the host put in a store and read back
;
; A task of its own, started by the Storage app, so that the app goes on
; drawing while it runs. It is mailed, a line each: who to tell, the
; service to ask, the folder, and what to put before each path. Every file
; under the folder is put, as a stream, (storage-put-file), and then each
; is read back as a stream and compared with the file, byte for byte,
; (storage-same-file?). obj, deps and folders whose names begin with a dot
; are not gone in to.
;
; It says how it is going, every few files, "put" or "read", how many of
; how many, and the path, and at the end "done" and: how many were put,
; how many read back the same, of how many, how many bytes, and how long
; each took, ms. Any mail to it and it stops.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "service/storage/app.inc")
(import "lib/files/files.inc")

(defun main ()
	(bind '(reply svc root prefix) (split (mail-read (task-mbox)) (ascii-char 10)))
	(defq reply (hex-decode reply) svc (hex-decode svc) going :t
		files (filter (# (not (starts-with "./deps/" %0))) (files-all root '("")))
		count (length files) put 0 same 0 bytes 0 t0 (pii-time))
	(defun path-of (file) (cat prefix (if (starts-with "./" file) (slice file 2 -1) file)))
	(defun stopped? () (if (mail-poll (list (task-mbox))) (setq going :nil)) (not going))
	(each (lambda (file)
		(unless (stopped?)
			(if (eql (first (storage-put-file (path-of file) file :nil svc)) :ok) (++ put))
			(if (= 0 (% (!) 8)) (mail-send reply (cat "put " (str (inc (!))) " " (str count) " " (path-of file))))))
		files)
	(defq t1 (pii-time))
	(each (lambda (file)
		(unless (stopped?)
			(when (defq size (storage-same-file? (path-of file) file svc))
				(++ same)
				(setq bytes (+ bytes size)))
			(if (= 0 (% (!) 8)) (mail-send reply (cat "read " (str (inc (!))) " " (str count) " " (path-of file))))))
		files)
	(if going (mail-send reply (join (map (const str)
		(list "done" put same count bytes (/ (- t1 t0) 1000) (/ (- (pii-time) t1) 1000))) " "))))
