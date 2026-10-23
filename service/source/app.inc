;source RPC calls: the files of a system, for a task of it sent to run on
;another. Each system has one @Source, which only its own tasks look up.
;A task opened with (source-task) is given that service's mailbox and the
;system's id, and imports its files from there, as a stream: a file stream
;where that is its own system, the text asked of the service where not.
;docs/ai_digest/source_service.md

(import "lib/asm/lisp.inc")

;a boot image from before function servers has no lisp_server, and every
;session starts this service: there the name is not bound, and a task is
;never given a server
(catch (ffi "sys/load/lisp_server" task-server) :t)
; (task-server folder) -> :t

(defq +source_wait 10)

(structure +source_rpc 0
	(netid reply_id))

(defun source-service ()
	; (source-service) -> :nil | netid
	;the @Source of this system
	(mail-first "@Source"))

(defun source-here ()
	; (source-here) -> netid
	;this system's @Source, started if it has none, as a node that only
	;runs what it is sent may not
	(unless (source-service) (open-child "service/source/app.lisp" +kn_call_run))
	(until (source-service) (task-sleep 10000))
	(source-service))

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

(defun source-mode ()
	; (source-mode) -> str
	;the build of this node, which VP it wants
	(if (catch (progn (length "abc" 1) :nil) :t) "debug" "release"))

(defun source-links (fn)
	; (source-links fn) -> (name ...)
	;the functions a native function links to, by the paths in it
	(defq names (list) lo (get-short fn +fn_header_links) po (get-short fn +fn_header_paths))
	(while (/= lo po)
		(push names (get-cstr fn (+ (get-long fn lo) lo)))
		(setq lo (+ lo +ptr_size)))
	names)

(defun source-native (path)
	; (source-native path)
	;a native function of the system that sent this task, made ready in the
	;task's server folder by this system's own @Source, which keeps a
	;translator for this CPU: what the node has built is server zero's, the
	;waypoint of this machine, and nothing is asked
	(unless (or (eql *source_system* (hex-encode (system-id)))
			(/= (age (cat (load-path) path)) 0)
			(/= (age (cat (load-path) *source_folder* path)) 0))
		(source-ask (source-here) (cat "+" *source_system* " " (hex-encode *source_service*) " " (source-mode) " " path))))

(defun source-ffi-form (path more)
	; (source-ffi-form path more) -> form
	;what (ffi path ...) is in a task sent from another system: the native
	;made ready first, then bound as ever
	(list 'progn (list 'source-native path) (cat (list *source_ffi* path) more)))

(defmacro source-import (sid svc)
	; (source-import sid svc)
	;an import, in this environment, that reads from the system of the id
	;by its @Source, the mailbox in hex: as root's own, a module once, its
	;relative paths from the module that imports it, but by (source-stream).
	;And an ffi that has a native from there too, translated here, bound by
	;the task's own function server, source/<sid>/ under the load path
	(static-qq (progn
		(defq *source_system* ,sid *source_service* (hex-decode ,svc)
			*source_folder* (cat "source/" ,sid "/") *source_ffi* ffi
			import (lambda (module &optional dst_env)
				(setq module (path-to-absolute module))
				(defq dst_env (ifn dst_env (penv))
					import_sym (sym (cat "*module_" module "*")))
				(unless (get import_sym dst_env)
					(unless (defq stream (source-stream *source_system* *source_service* module))
						(throw "No such file !" module))
					(def dst_env import_sym :t)
					(callback (const repl) dst_env stream module))))
		(unless (eql *source_system* (hex-encode (system-id))) (task-server *source_folder*))
		(redefmacro ffi (path &rest more) (source-ffi-form path more)))))
