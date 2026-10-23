(import "./app.inc")
(import "lib/boot/image.inc")
(import "lib/crypto/sha256.inc")

(defun source-given? (path)
	; (source-given? path) -> :t | :nil
	;a file that can be imported, Lisp, by a path from the root of the tree:
	;none above it, none in a user's own folder, usr/env.inc is the
	;system's. Nothing else of the tree is given, its keys are in it
	(defq parts (split path "/"))
	(and (or (ends-with ".lisp" path) (ends-with ".inc" path))
		(not (or (starts-with "/" path) (find ".." parts)
			(and (eql (first parts) "usr") (> (length parts) 2))))))

(defun source-entry (mode name)
	; (source-entry mode name) -> :nil | (entry file)
	;the VP of a function as this system built it, the entry of its bundle
	;in obj/vp/<mode>/, one of the bundles of the function's folder
	(defq dir (cat "obj/vp/" mode "/" (path-dir name) "/"))
	(and (find mode '("release" "debug" "validate")) (not (find ".." (split name "/")))
		(some (lambda (file kind)
				(and (eql kind "8") (ends-with ".vp" file)
					(defq stream (file-stream (cat dir file)))
					(progn (while (and (defq entry (first (read stream))) (nql (first entry) name)))
						(if entry (list entry (cat dir file))))))
			(first (defq listed (dir-list dir))) (second listed))))

(defun source-hash (mode name)
	; (source-hash mode name) -> :nil | hex
	;the hash of a function's VP as this system built it, the text of its
	;entry, kept till its bundle is built again
	(defq key (cat mode " " name) kept (. *source_hashes* :find key))
	(cond
		((and kept (= (age (first kept)) (second kept))) (third kept))
		((defq found (source-entry mode name))
			(bind '(entry file) found)
			(defq hash (hex-encode (sha256 (str entry))))
			(. *source_hashes* :insert key (list file (age file) hash))
			hash)))

(defun source-translator ()
	; (source-translator)
	;the translator for this CPU, made the first time it is wanted and kept:
	;the op and register lists of the VP, the translator, this CPU's
	;emitter. Its id is the hash of those files: a translation is kept by
	;the hash of its VP and that, so one by another translator is not used
	(unless (get 'emit-translate *source_env*)
		(bind '(& cpu abi) (map (const sym) (split (load-path) "/")))
		(defq files (list "lib/asm/regs.inc" "lib/asm/ops.inc" "lib/trans/vp.inc" (cat "lib/trans/" cpu ".inc")))
		(def *source_env* '*cpu* cpu '*abi* abi '*build_mode* (if (eql (source-mode) "debug") 1 0)
			'validatecase (macro (&rest e) (if (>= *build_mode* 2) `(progn ~e)))
			'*source_trans_id* (hex-encode (sha256 (apply (const cat) (cat (list "") (map (# (ifn (load %0) "")) files))))))
		(each! (# (import %0 *source_env*)) (list files) 0 3)))

(defun source-cache (hash mode)
	; (source-cache hash mode) -> path
	;where the native of VP of that hash is kept, by this translator
	(source-translator)
	(cat (load-path) "cache/" (hex-encode (sha256 (cat hash " " mode " " (get '*source_trans_id* *source_env*))))))

(defun source-make (entry mode)
	; (source-make entry mode) -> native
	;a function's VP translated for this CPU, kept in the cache if it may be
	(source-translator)
	(defq out ((get 'emit-translate *source_env*) (second entry)))
	(if (source-caching?) (save out (source-cache (hex-encode (sha256 (str entry))) mode)))
	out)

(defun source-manifest (mode name)
	; (source-manifest mode name) -> :nil | text
	;a function of this system and every one it calls, its name and the
	;hash of its VP, a line each, not the system's own, what a boot image
	;has, which is each machine's own
	(defq todo (list name) seen (list) lines (list))
	(while (defq name (pop todo))
		(unless (or (find name seen) (nempty? (boot-funcs (list name))))
			(push seen name)
			(cond
				((defq hash (source-hash mode name))
					(push lines (cat name " " hash))
					(defq native (ifn (load (cat (load-path) name)) (source-make (first (source-entry mode name)) mode)))
					(each (# (push todo %0)) (source-links native)))
				((setq lines :nil todo (list))))))
	(if lines (join lines (ascii-char 10))))

(defun source-entries (mode names)
	; (source-entries mode names) -> text
	;the VP of those functions of this system, an entry each
	(join (map (# (str (first (source-entry mode %0)))) names) (ascii-char 10)))

(defun source-translate (args)
	; (source-translate request) -> :nil | natives
	;the request is the system id, its @Source in hex, the build mode and
	;the function, with a space between each. A native of another system,
	;and all it calls, made ready for a task of it here, in one asking: the
	;hashes of them all of that system's @Source, each looked at here, and
	;the VP of those not here asked for at once. Of a hash this node's own
	;VP has it is the node's, server zero's, and is not given unless the
	;node has not built it and may not keep it. Of one the cache has, that.
	;The rest are translated, kept if they may be. What is given is the
	;natives, one after another, for the task to place
	(bind '(sid svc mode name) (split args " "))
	(defq svc (hex-decode svc) self (eql svc (task-mbox))
		manifest (if self (source-manifest mode name) (source-ask svc (cat "#" mode " " name)))
		natives (list) missing (list))
	(when manifest
		(each (lambda (line)
				(bind '(name theirs) (split line " "))
				(cond
					((eql theirs (source-hash mode name))
						(when (= (age (cat (load-path) name)) 0)
							(defq out (source-make (first (source-entry mode name)) mode))
							(if (source-caching?) (save out (cat (load-path) name)) (push natives out))))
					((and (source-caching?) (defq kept (load (source-cache theirs mode))))
						(push natives kept))
					((push missing name))))
			(split manifest (ascii-char 10)))
		(when (nempty? missing)
			(defq text (if self (source-entries mode missing)
					(source-ask svc (cat "$" mode " " (join missing " "))))
				stream (string-stream (ifn text "")) got 0)
			(while (defq entry (first (read stream)))
				(when (find (first entry) missing)
					(++ got)
					(push natives (source-make entry mode))))
			(if (/= got (length missing)) (setq manifest :nil))))
	(if manifest (apply (const cat) (cat (list "") natives))))

(defun main ()
	(defq service (mail-declare (task-mbox) "@Source" "Source Service 0.2") *source_env* (env)
		*source_hashes* (Fmap 101))
	(while :t
		(defq msg (mail-read (task-mbox)) path (slice msg +source_rpc_size -1))
		(mail-send (getf msg +source_rpc_reply_id)
			(cond
				((starts-with "+" path)
					(if (defq natives (source-translate (slice path 1 -1))) (cat "y" natives) "n"))
				;the hashes of a function and all it calls, "#mode name"
				((starts-with "#" path)
					(bind '(mode name) (split (slice path 1 -1) " "))
					(if (defq text (source-manifest mode name)) (cat "y" text) "n"))
				;the VP of functions, "$mode name ..."
				((starts-with "$" path)
					(defq names (split (slice path 1 -1) " "))
					(cat "y" (source-entries (first names) (rest names))))
				((and (source-given? path) (defq text (load path))) (cat "y" text))
				("n")))))
