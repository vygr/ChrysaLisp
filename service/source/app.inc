;source RPC calls: the files of a system, for a task of it sent to run on
;another. Each system has one @Source, which only its own tasks look up.
;A task opened with (source-task) is given that service's mailbox and the
;system's id, and imports its files from there, as a stream: a file stream
;where that is its own system, the text asked of the service where not.
;docs/ai_digest/source_service.md

(import "lib/asm/lisp.inc")
(import "lib/crypto/sha256.inc")

;a boot image from before function servers has no lisp_server, and every
;session starts this service: there the name is not bound, and a task is
;never given a server
(catch (ffi "sys/load/lisp_server" task-server) :t)
; (task-server [name]) -> :t | (name ...)
(catch (ffi "sys/load/lisp_place" task-place) :t)
; (task-place natives) -> :t | :nil

(defq +source_wait 10 +source_seek 2)

(structure +source_rpc 0
	(netid reply_id))

(defun source-caching? ()
	; (source-caching?) -> :t | :nil
	;may translations be kept on this system's disk: not if the system says
	;not, *env_source_cache* :nil, for its security or as it has no disk of
	;its own, and then every one is made each time it is wanted
	(if (def? '*env_source_cache*) *env_source_cache* :t))

(defun source-given? (path)
	; (source-given? path) -> :t | :nil
	;a file that can be imported, Lisp, by a path from the root of the tree:
	;none above it, none in a user's own folder, usr/env.inc is the
	;system's. Nothing else of the tree is given, its keys are in it
	(defq parts (split path "/"))
	(and (or (ends-with ".lisp" path) (ends-with ".inc" path))
		(not (or (starts-with "/" path) (find ".." parts)
			(and (eql (first parts) "usr") (> (length parts) 2))))))

(defun source-waypoint ()
	; (source-waypoint) -> :nil | commit
	;the waypoint this system's server zero is of: the commit make install
	;built at, obj/installed, if no make has built since, which takes the
	;note away, and the tree is noted at that waypoint still, the first line
	;of the file waypoint. Of two systems at the same, each has what the
	;other has, nothing is asked or sent, lib/sync/sync.inc
	(defq made (first (split (ifn (load "obj/installed") "") (ascii-char 10)))
		at (first (split (ifn (load "waypoint") "") (ascii-char 10))))
	(and made (nempty? made) (eql made at) at))

(defun source-service ()
	; (source-service) -> :nil | netid
	;the @Source of this system
	(mail-lowest "@Source"))

(defun source-here ()
	; (source-here) -> netid
	;this system's @Source, started if it has none, as a node that only
	;runs what it is sent may not
	(unless (source-service) (open-child "service/source/app.lisp" +kn_call_run))
	(until (source-service) (task-sleep 10000))
	(source-service))

(defun source-asked (svc path wait)
	; (source-asked svc path wait) -> :nil | answer
	;what a @Source says, "y" and the text or "n", :nil if it says nothing
	;in wait seconds. A new mailbox each time: an answer that comes late is
	;to a mailbox nobody reads
	(defq select (list (mail-mbox) (mail-mbox)))
	(mail-timeout (second select) (task-timeout wait) 0)
	(mail-send svc (cat (setf-> (str-alloc +source_rpc_size)
		(+source_rpc_reply_id (first select))) path))
	(when (= (mail-select select) 0)
		(mail-timeout (second select) 0 0)
		(mail-read (first select))))

(defun source-ask (svc path &optional wait)
	; (source-ask svc path [wait]) -> :nil | text
	;a file's text from a @Source, :nil if it has none it will give, or it
	;says nothing in the time, +source_wait seconds or wait. With :t as long
	;as it takes: one that says nothing for +source_wait is asked no more,
	;the one of the system now is, (source-service). Two started at once,
	;one gives way to the other and goes when nothing comes to it, so what
	;is sent to it after is to a mailbox nobody reads
	(defq answer (source-asked svc path (if (eql wait :t) +source_wait (ifn wait +source_wait))))
	(while (and (not answer) (eql wait :t))
		(setq svc (ifn (source-service) svc) answer (source-asked svc path +source_wait)))
	(and answer (starts-with "y" answer) (slice answer 1 -1)))

(defun source-pack (entries)
	; (source-pack entries) -> str
	;files, (path tag text) each, one after another: its path, its tag and
	;the length of its text on lines of their own, then the text
	(apply (const cat) (cat (list "") (map (lambda ((path tag text))
			(cat path (ascii-char 10) tag (ascii-char 10) (str (length text)) (ascii-char 10) text))
		entries))))

(defun source-unpack (data)
	; (source-unpack str) -> ((path tag text) ...)
	(defq out (list) at 0 lf (ascii-char 10))
	(while (< at (length data))
		(defq a (find lf data at) b (find lf data (inc a)) c (find lf data (inc b))
			n (str-as-num (slice data (inc b) c)))
		(push out (list (slice data at a) (slice data (inc a) b) (slice data (inc c) (+ (inc c) n))))
		(setq at (+ (inc c) n)))
	out)

(defun source-seek (sid reply)
	; (source-seek sid reply) -> :nil | :t
	;which @Source a system has now, by its id in hex, asked of a kernel of
	;it: a task pinned to one of its nodes mails reply ?sid and a line,
	;that @Source in hex, empty if it has none. What a task was sent with
	;is the one of then, one started again since has another mailbox. :nil
	;if no node of it is known
	(when (defq node (first (lisp-nodes (hex-decode sid))))
		(open-task (cat "(mail-send (hex-decode {" (hex-encode reply) "}) (cat {?" sid "} (ascii-char 10)"
				" (if (defq s (mail-lowest {@Source})) (hex-encode s) {})))")
			node +kn_call_pin 0 (mail-mbox))
		:t))

(defun source-find (sid)
	; (source-find sid) -> :nil | netid
	;the @Source a system has now, (source-seek) and its answer waited for
	(defq select (list (mail-mbox) (mail-mbox)))
	(when (source-seek sid (first select))
		(mail-timeout (second select) (task-timeout +source_wait) 0)
		(when (= (mail-select select) 0)
			(mail-timeout (second select) 0 0)
			(if (nempty? (defq got (second (split (mail-read (first select)) (ascii-char 10)))))
				(hex-decode got)))))

(defun source-stream (sid svc path)
	; (source-stream sid svc path) -> :nil | stream
	;a file of a system as a stream, by the system id in hex and its
	;@Source: a file stream on that system. On another the @Source of this
	;one is asked, once, for the file and all it imports, all the way down:
	;it has them of that system, asked once for all the tasks here. Each is
	;L, the copy here is the same, as what the two systems have in common
	;is, T and the text, or n. What this task is given it keeps, and its
	;imports after are read from that
	(defq got (ifn (get '*source_got*) (Fmap 11)))
	(cond
		((eql sid (hex-encode (system-id)))
			(if (/= (age path) 0) (file-stream path)))
		;a system at the same waypoint as this one has the same file
		((get '*source_same*)
			(and (source-given? path) (/= (age path) 0) (file-stream path)))
		((or (. got :find path)
				;the @Source here may have to find that system's again, so
				;more than one wait
				(and (defq answer (source-ask (source-here) (cat "%" sid " " (hex-encode svc) " " path) (* 3 +source_wait)))
					(progn (each (lambda ((p tag text)) (. got :insert p (list tag text))) (source-unpack answer))
						(. got :find path))))
			(bind '(tag text) (. got :find path))
			(cond
				((eql tag "L") (if (/= (age path) 0) (file-stream path)))
				((eql tag "T") (string-stream text))))))

(defun source-task (path)
	; (source-task path) -> task
	;what to open, with (open-task) and the rest, for a Lisp file of this
	;system to run on any node of the mesh: it and what it imports are read
	;from this system, and need not be where it runs. A task that was sent
	;from another system sends its tasks as of that one, which has its
	;files, and the waypoint it is of if it is at one. The path itself if
	;there is no @Source, read where it runs as any task is
	(defq sid (get '*source_system*) svc (get '*source_service*) wp (get '*source_waypoint*))
	(unless sid (setq sid (hex-encode (system-id)) svc (source-service) wp (source-waypoint)))
	(ifn svc path
		(cat "(import {service/source/app.inc})" (ascii-char 10)
			"(source-import {" sid "} {" (hex-encode svc) "} {" (ifn wp "") "})" (ascii-char 10)
			"(import {" path "})")))

(defun source-mode ()
	; (source-mode) -> str
	;the build of this node, which VP it wants: normal, a validate node is
	;not told apart, and takes the VP of a normal one
	"normal")

(defun source-links (fn)
	; (source-links fn) -> (name ...)
	;the functions a native function links to, by the paths in it
	(defq names (list) lo (get-ushort fn +fn_header_links) po (get-ushort fn +fn_header_paths))
	(while (/= lo po)
		(push names (get-cstr fn (+ (get-long fn lo) lo)))
		(setq lo (+ lo +ptr_size)))
	names)

(defun source-split (text)
	; (source-split text) -> (native ...)
	;native functions one after another, each as long as its header says
	(defq natives (list) at 0)
	(while (< at (length text))
		(defq len (get-ushort text (+ at +fn_header_length)))
		(push natives (slice text at (+ at len)))
		(setq at (+ at len)))
	natives)

(defun source-native (path)
	; (source-native path)
	;a native function of the system that sent this task, and all it calls
	;that this node has not the same of, made ready by this system's own
	;@Source, which keeps a translator for this CPU, and placed in the
	;task's function server: in memory, none of it kept in a file of the
	;task's. What the node has the same VP of is server zero's, nothing is
	;placed for it, nor for a system at the same waypoint as this one
	(unless (or (eql *source_system* (hex-encode (system-id))) *source_same*)
		(and (defq natives (source-ask (source-here)
				(cat "+" *source_system* " " (hex-encode *source_service*) " " (source-mode) " " path)
				(* 3 +source_wait)))
			(nempty? natives)
			(task-place (source-split natives)))))

(defun source-ffi-form (path more)
	; (source-ffi-form path more) -> form
	;what (ffi path ...) is in a task sent from another system: the native
	;made ready first, then bound as ever
	(list 'progn (list 'source-native path) (cat (list *source_ffi* path) more)))

(defmacro source-import (sid svc &optional wp)
	; (source-import sid svc [wp])
	;an import, in this environment, that reads from the system of the id
	;by its @Source, the mailbox in hex: as root's own, a module once, its
	;relative paths from the module that imports it, but by (source-stream).
	;And an ffi that has a native from there too, translated here, placed in
	;the task's own function server, named by the system id. wp the waypoint
	;that system is at, "" if none: at the same as this one, its files and
	;natives are this one's, and nothing is asked
	(static-qq (progn
		(defq *source_system* ,sid *source_service* (hex-decode ,svc) *source_got* (Fmap 31)
			*source_waypoint* ,(if (and wp (nempty? wp)) wp)
			*source_same* ,(and wp (nempty? wp) (eql wp (source-waypoint)))
			*source_ffi* ffi
			import (lambda (module &optional dst_env)
				(setq module (path-to-absolute module))
				(defq dst_env (ifn dst_env (penv))
					import_sym (sym (cat "*module_" module "*")))
				(unless (get import_sym dst_env)
					(unless (defq stream (source-stream *source_system* *source_service* module))
						(throw "No such file !" module))
					(def dst_env import_sym :t)
					(callback (const repl) dst_env stream module))))
		(unless (eql *source_system* (hex-encode (system-id))) (task-server *source_system*))
		(redefmacro ffi (path &rest more) (source-ffi-form path more)))))
