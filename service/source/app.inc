;source RPC calls: the files of a system, for a task of it sent to run on
;another. Each system has one @Source, which only its own tasks look up.
;A task opened with (source-task) is given that service's mailbox and the
;system's id, and imports its files from there, as a stream: a file stream
;where that is its own system, the text asked of the service where not.
;docs/ai_digest/source_service.md

(defq +source_wait 10)

(structure +source_rpc 0
	(netid reply_id))

(defun source-service ()
	; (source-service) -> :nil | netid
	;the @Source of this system
	(mail-first "@Source"))

(defun source-ask (svc path)
	; (source-ask svc path) -> :nil | text
	;a file's text from a @Source, :nil if it has none it will give, or it
	;says nothing in the time. A new mailbox each time: an answer that comes
	;late is to a mailbox nobody reads
	(defq select (list (mail-mbox) (mail-mbox)))
	(mail-timeout (second select) (task-timeout +source_wait) 0)
	(mail-send svc (cat (setf-> (str-alloc +source_rpc_size)
		(+source_rpc_reply_id (first select))) path))
	(when (= (mail-select select) 0)
		(mail-timeout (second select) 0 0)
		(defq answer (mail-read (first select)))
		(if (starts-with "y" answer) (slice answer 1 -1))))

(defun source-stream (sid svc path)
	; (source-stream sid svc path) -> :nil | stream
	;a file of a system as a stream, by the system id in hex and its
	;@Source: a file stream on that system, the text asked of it on another
	(if (eql sid (hex-encode (system-id)))
		(if (/= (age path) 0) (file-stream path))
		(and (defq text (source-ask svc path)) (string-stream text))))

(defun source-task (path)
	; (source-task path) -> task
	;what to open, with (open-task) and the rest, for a Lisp file of this
	;system to run on any node of the mesh: it and what it imports are read
	;from this system, and need not be where it runs. The path itself if
	;this system has no @Source, read where it runs as any task is
	(ifn (defq svc (source-service)) path
		(cat "(import {service/source/app.inc})" (ascii-char 10)
			"(source-import {" (hex-encode (system-id)) "} {" (hex-encode svc) "})" (ascii-char 10)
			"(import {" path "})")))

(defmacro source-import (sid svc)
	; (source-import sid svc)
	;an import, in this environment, that reads from the system of the id
	;by its @Source, the mailbox in hex: as root's own, a module once, its
	;relative paths from the module that imports it, but by (source-stream)
	(static-qq (defq *source_system* ,sid *source_service* (hex-decode ,svc)
		import (lambda (module &optional dst_env)
			(setq module (path-to-absolute module))
			(defq dst_env (ifn dst_env (penv))
				import_sym (sym (cat "*module_" module "*")))
			(unless (get import_sym dst_env)
				(unless (defq stream (source-stream *source_system* *source_service* module))
					(throw "No such file !" module))
				(def dst_env import_sym :t)
				(callback (const repl) dst_env stream module))))))
