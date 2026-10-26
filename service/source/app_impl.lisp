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
	;in obj/vp/, one of the bundles of the function's folder
	(defq dir (cat (if (eql mode "validate") "obj/vp_validate/" "obj/vp/") (path-dir name) "/"))
	(and (find mode '("normal" "validate")) (not (find ".." (split name "/")))
		(some (lambda (file kind)
				(and (eql kind "f") (ends-with ".vp" file)
					(defq stream (file-stream (cat dir file)))
					(progn (while (and (defq entry (first (read stream))) (nql (first entry) name)))
						(if entry (list entry (cat dir file))))))
			(first (defq listed (dir-list dir))) (second listed))))

(defun source-vp-links (entry)
	; (source-vp-links entry) -> (name ...)
	;the functions a function calls, from its VP, with nothing translated:
	;the strings after the label its header gives for where its paths begin
	(defq code (second entry)
		paths (elem-get (some (# (and (list? %0) (eql (first %0) 'emit-short) %0)) code) 4)
		at (some (# (and (list? %0) (eql (first %0) 'emit-label) (eql (second (second %0)) paths) (!))) code))
	(map (const second) (filter! (# (and (list? %0) (eql (first %0) 'emit-string))) code (inc at) -1)))

(defun source-known (mode name)
	; (source-known mode name) -> :nil | (hash links)
	;the hash of a function's VP as this system built it, the text of its
	;entry, and the functions it calls, kept till its bundle is built
	;again: a bundle is read and its VP parsed once, not each time asked
	(defq key (cat mode " " name) kept (. *source_hashes* :find key))
	(cond
		((and kept (= (age (first kept)) (second kept))) (slice kept 2 -1))
		((defq found (source-entry mode name))
			(bind '(entry file) found)
			(defq hash (hex-encode (sha256 (str entry))) links (source-vp-links entry))
			(. *source_hashes* :insert key (list file (age file) hash links))
			(list hash links))))

(defun source-hash (mode name)
	; (source-hash mode name) -> :nil | hex
	(first (source-known mode name)))

(defun source-links-of (mode name)
	; (source-links-of mode name) -> (name ...)
	(second (source-known mode name)))

(defun source-file-hash (path)
	; (source-file-hash path) -> :nil | hex
	;the hash of a file that is given, kept till the file changes: what a
	;task elsewhere reads its own copy of if it has the same
	(defq at (age path) kept (. *source_files* :find path))
	(cond
		((= at 0) :nil)
		((and kept (= (first kept) at)) (second kept))
		((defq text (load path))
			(defq hash (hex-encode (sha256 text)))
			(. *source_files* :insert path (list at hash))
			hash)))

(defun source-translator ()
	; (source-translator)
	;the translator for this CPU, made the first time it is wanted and kept:
	;the op and register lists of the VP, and the translator. Its id is the
	;hash of those files and of the translator's VP source: a translation is
	;kept by the hash of its VP and that, so one by another translator is
	;not used
	(unless (get 'emit-translate *source_env*)
		(bind '(& cpu abi) (map (const sym) (split (load-path) "/")))
		(defq files (list "lib/asm/regs.inc" "lib/asm/ops.inc" "lib/trans/vp.inc"
			"lib/trans/vpb.inc" "lib/trans/vpb_ops.inc" "lib/trans/vpb_trans.inc" (cat "lib/trans/" cpu ".vp")))
		(def *source_env* '*cpu* cpu '*abi* abi '*build_mode* 1
			'validatecase (macro (&rest e) (if (>= *build_mode* 2) `(progn ~e)))
			'*source_trans_id* (hex-encode (sha256 (apply (const cat) (cat (list "") (map (# (ifn (load %0) "")) files))))))
		(each! (# (import %0 *source_env*)) (list files) 0 3)))

(defun source-cache (hash)
	; (source-cache hash) -> path
	;where the native of VP of that hash is kept, by this translator. Not
	;by the build mode: the VP of a mode that is not the same has another
	;hash, and what is the same, most below the Lisp bindings, is made once
	(source-translator)
	(cat (load-path) "cache/" (hex-encode (sha256 (cat hash " " (get '*source_trans_id* *source_env*))))))

(defun source-make (entry)
	; (source-make entry) -> native
	;a function's VP translated for this CPU, kept in the cache if it may be
	(source-translator)
	(defq out ((get 'emit-translate *source_env*) (second entry)))
	(if (source-caching?) (save out (source-cache (hex-encode (sha256 (str entry))))))
	out)

(defun source-make-bin (bin hash)
	; (source-make-bin bin hash) -> native
	;a function's VP binary, as another system sends it, translated for this
	;CPU, kept in the cache by the hash its manifest gave if it may be
	(source-translator)
	(defq out ((get 'vp-translate-bin *source_env*) bin))
	(if (source-caching?) (save out (source-cache hash)))
	out)

(defun source-manifest (mode name)
	; (source-manifest mode name) -> :nil | text
	;a function of this system and every one it calls, its name and the
	;hash of its VP, a line each, not the system's own, what a boot image
	;has, which is each machine's own. Read from the VP alone: the system
	;that sends translates nothing, where it lands does
	(defq todo (list name) seen (list) lines (list))
	(while (defq name (pop todo))
		(unless (or (find name seen) (nempty? (boot-funcs (list name))))
			(push seen name)
			(cond
				((defq hash (source-hash mode name))
					(push lines (cat name " " hash))
					(each (# (push todo %0)) (source-links-of mode name)))
				((setq lines :nil todo (list))))))
	(if lines (join lines (ascii-char 10))))

(defun source-entries (mode names)
	; (source-entries mode names) -> data
	;the VP of those functions of this system that it has, as VP binary,
	;lib/trans/vpb.inc, one bundle of them: compact, and read where it
	;lands with no parse
	(source-translator)
	(defq encode (get 'vpb-encode *source_env*))
	((get 'vpb-bundle *source_env*)
		(reduce (# (if (defq found (source-entry mode %1)) (push %0 (encode (first found))) %0))
			names (list))))

(defun source-ask-again (sid svc request)
	; (source-ask-again sid svc request) -> (svc answer)
	;a request of a system's @Source, and if it says nothing, of the one a
	;kernel of that system says it has now, which is kept
	(cond
		((defq got (source-ask svc request)) (list svc got))
		((and (defq found (source-find sid)) (nql found svc))
			(. *source_svcs* :insert sid (hex-encode found))
			(list found (source-ask found request)))
		((list svc :nil))))

(defun source-translate (args)
	; (source-translate request) -> :nil | natives
	;the request is the system id, its @Source in hex, the build mode and
	;the function, with a space between each. A native of another system,
	;and all it calls, made ready for a task of it here, in one asking: the
	;hashes of them all of that system's @Source, each looked at here, and
	;the VP of those not here asked for at once. Of a hash this node's own
	;VP has it is the node's, server zero's, and is not given unless the
	;node has not built it and may not keep it, or it calls, all the way
	;down, one of that system's own that the node has not the same of: a
	;link is to the task's own, so it is given, the node's translation, to
	;be linked in the task's server. Of one the cache has, that. The rest
	;are translated, kept if they may be. What is given is the natives, one
	;after another, for the task to place
	(bind '(sid svc mode name) (split args " "))
	(defq svc (hex-decode (ifn (. *source_svcs* :find sid) svc)) self (eql svc (task-mbox))
		;the manifest of another system's function is asked of it once and kept
		manifest (if self (source-manifest mode name)
			(. *source_kept* :find (cat (hex-encode svc) " #" mode " " name)))
		natives (list) missing (list) same (list) given (list) theirs_own (list) hashes (Fmap 31))
	(unless (or self manifest)
		(bind '(svc manifest) (source-ask-again sid svc (cat "#" mode " " name)))
		(if manifest (. *source_kept* :insert (cat (hex-encode svc) " #" mode " " name) manifest)))
	(when manifest
		(each (lambda (line)
				(bind '(name theirs) (split line " "))
				(. hashes :insert name theirs)
				(cond
					((eql theirs (source-hash mode name))
						(push same name)
						(when (= (age (cat (load-path) name)) 0)
							(defq out (source-make (first (source-entry mode name))))
							(cond
								((source-caching?) (save out (cat (load-path) name)))
								((push natives out) (push given name)))))
					((push theirs_own name)
						(if (and (source-caching?) (defq kept (load (source-cache theirs))))
							(push natives kept)
							(push missing name)))))
			(split manifest (ascii-char 10)))
		;the node's own that link to theirs, and to those, till no more
		(defq more :t)
		(while more
			(setq more :nil)
			(each (lambda (name)
					(when (and (not (find name theirs_own))
							(some (# (find %0 theirs_own)) (source-links-of mode name)))
						(push theirs_own name)
						(setq more :t)
						(unless (find name given)
							(push natives (ifn (load (cat (load-path) name))
								(source-make (first (source-entry mode name))))))))
				same))
		(when (nempty? missing)
			(defq data (ifn (if self (source-entries mode missing)
					(source-ask svc (cat "$" mode " " (join missing " ")))) "")
				got 0)
			;VP binary, or if it is not that, the text of the entries, from a
			;@Source of before VP binary was sent, a system not at this one's
			;version. One that is neither, cut short, is none, not the end of
			;the service
			(defq made (list))
			(catch (each (lambda (bin)
					(when (find (defq name ((get 'vpb-name *source_env*) bin)) missing)
						(++ got)
						(push made (source-make-bin bin (. hashes :find name)))))
					((get 'vpb-unbundle *source_env*) data))
				(setq got -1))
			(when (/= got (length missing))
				(setq got 0 made (list))
				(catch (progn
						(defq stream (string-stream data))
						(while (defq entry (first (read stream)))
							(when (find (first entry) missing)
								(++ got)
								(push made (source-make entry)))))
					(setq got -1)))
			(setq natives (cat natives made))
			(if (/= got (length missing)) (setq manifest :nil))))
	(if manifest (apply (const cat) (cat (list "") natives))))

(defun source-tree (root)
	; (source-tree root) -> (path ...)
	;a file that is given and all it imports, all the way down, itself
	;first: what a task elsewhere that imports it will want. By the imports
	;its text names, one made at run time is asked for when it is made.
	;Kept till one of them changes
	(defq kept (. *source_trees* :find root))
	(cond
		((and kept (every (lambda ((path at)) (= (age path) at)) (first kept)))
			(second kept))
		(:t (defq paths (list root) at 0)
			(while (< at (length paths))
				(defq file (elem-get paths at))
				(each (lambda (line)
						(when (found? line "(import ")
							(defq pieces (split line (const (cat (ascii-char 34) "{}"))))
							(each! (lambda (piece)
									(and (ends-with "(import " (elem-get pieces (dec (!))))
										(defq name (catch (path-to-absolute piece file) :nil))
										(/= (age name) 0) (source-given? name) (not (find name paths))
										(push paths name)))
								(list pieces) 1)))
					(split (ifn (load file) "") (ascii-char 10)))
				(setq at (inc at)))
			(. *source_trees* :insert root (list (map (# (list %0 (age %0))) paths) paths))
			paths)))

(defun source-bundle (svc paths)
	; (source-bundle svc paths) -> answer
	;what a task here is given of those files of that system, as kept
	(cat "y" (source-pack (map (# (cat (list %0) (. *source_kept* :find (cat svc " " %0)))) paths))))

(defun source-count (what)
	;one more of what this service has done, (source-stats)
	(. *source_counts* :insert what (inc (ifn (. *source_counts* :find what) 0))))

(defun source-tell (svc request)
	;a request to the @Source of another system, its answer to come back
	(source-count :told)
	(mail-send (hex-decode svc) (cat (setf-> (str-alloc +source_rpc_size)
		(+source_rpc_reply_id *source_back*)) request)))

(defun source-done (key)
	;the file of the key and all it imports are kept, the tasks that wait
	;for it are given them
	(defq at (find " " key) svc (slice key 0 at) tree (. *source_known* :find key))
	(when (defq out (. *source_out* :find key))
		(. *source_out* :erase key)
		(each (# (mail-send %0 (if tree (source-bundle svc tree) "n"))) (second out))))

(defun source-fetch (args)
	; (source-fetch args) -> :nil | answer
	;a file of another system for a task here, "%sid svc path", and all it
	;imports. That system is asked once, for the hash of each, and the text
	;of those that are not the same as the copy here, and the answers kept:
	;L, the copy here is the same, T and the text, or n. Each task that
	;wants the file while that is asked waits, and is answered as it comes,
	;and every one after from what was kept. :nil is that it is being
	;asked, (source-beat) sees to one not answered. What is kept is of that
	;@Source: one started again has another mailbox, and the one found
	;since is asked in place of the one the task was sent with
	(bind '(sid svc path) (split args " "))
	(defq svc (ifn (. *source_svcs* :find sid) svc)
		key (cat svc " " path) out (. *source_out* :find key))
	(source-count :asked)
	(cond
		((not (source-given? path)) "n")
		((defq tree (. *source_known* :find key)) (source-count :kept) (source-bundle svc tree))
		((. *source_kept* :find key) (source-count :kept) (source-bundle svc (list path)))
		(out (push (second out) (getf *source_msg* +source_rpc_reply_id)) :nil)
		(:t (. *source_out* :insert key (list (pii-time) (list (getf *source_msg* +source_rpc_reply_id)) sid path :nil))
			(source-tell svc (cat "&" key (ascii-char 10) path))
			:nil)))

(defun source-beat ()
	;each second: a file asked of another system's @Source and not answered
	;in +source_seek, a kernel of that system is asked which it has now,
	;once. One not answered in a task's wait has none, its tasks are told
	(defq now (pii-time))
	(each (lambda ((key out))
			(bind '(at waiters sid path sought) out)
			(cond
				((> (- now at) (task-timeout (* 2 +source_wait))) (source-done key))
				((and (not sought) (> (- now at) (task-timeout +source_seek)))
					(. *source_out* :insert key (list at waiters sid path :t))
					(source-count :sought)
					(source-seek sid *source_back*))))
		(. *source_out* :tolist)))

(defun source-found (sid svc)
	;a kernel of that system says which @Source it has now. One other than
	;was asked: kept, and each file asked of the old one is asked of it.
	;None: the tasks that wait are told
	(each (lambda ((key out))
			(bind '(& waiters out_sid path &) out)
			(when (eql out_sid sid)
				(defq old (slice key 0 (find " " key)))
				(cond
					((empty? svc) (source-done key))
					((nql svc old)
						(. *source_out* :erase key)
						(defq key (cat svc " " path))
						(cond
							((defq there (. *source_out* :find key))
								(each (# (push (second there) %0)) waiters))
							(:t (. *source_out* :insert key (list (pii-time) waiters sid path :t))
								(source-tell svc (cat "&" key (ascii-char 10) path))))))))
		(. *source_out* :tolist))
	(if (nempty? svc) (. *source_svcs* :insert sid svc)))

(defun source-fetched (msg)
	;an answer of another system. &: the hash of each file of the tree,
	;those the same as the copy here are kept as L, and the text of the
	;rest asked for. *: those texts, kept as T. Then the tasks that wait
	;are given them. ?: which @Source a system has now, (source-seek)
	(defq at (find (ascii-char 10) msg) key (slice msg 1 at) body (slice msg (inc at) -1)
		at (find " " key) svc (if at (slice key 0 at)))
	(cond
		((starts-with "?" msg) (source-found key body))
		((starts-with "&" msg)
			(cond
				((eql body "n") (source-done key))
				(:t (defq tree (list) want (list))
					(each (lambda (line)
							(bind '(path theirs) (split line " "))
							(push tree path)
							(if (eql theirs (source-file-hash path))
								(. *source_kept* :insert (cat svc " " path) (list "L" ""))
								(push want path)))
						(split body (ascii-char 10)))
					(. *source_known* :insert key tree)
					(if (empty? want) (source-done key)
						(source-tell svc (cat "*" key (ascii-char 10) (join want (ascii-char 10))))))))
		((starts-with "*" msg)
			(each (lambda ((path tag text)) (. *source_kept* :insert (cat svc " " path) (list tag text)))
				(source-unpack body))
			(source-done key))))

(defun source-tree-answer (args)
	; (source-tree-answer args) -> answer
	;"&key\npath" of a @Source elsewhere: the key back, then the path and
	;hash of the file and each it imports, a line each, n if not given
	(defq at (find (ascii-char 10) args) key (slice args 0 at) file (slice args (inc at) -1))
	(cat "&" key (ascii-char 10)
		(if (and (source-given? file) (/= (age file) 0))
			(join (map (# (cat %0 " " (source-file-hash %0))) (source-tree file)) (ascii-char 10))
			"n")))

(defun source-texts-answer (args)
	; (source-texts-answer args) -> answer
	;"*key\npath..." of a @Source elsewhere: the key back, then those files
	(defq paths (split args (ascii-char 10)) key (first paths))
	(cat "*" key (ascii-char 10)
		(source-pack (map! (# (if (source-given? %0) (list %0 "T" (ifn (load %0) "")) (list %0 "n" "")))
			(list paths) 1 -1))))

(defun source-answer (request)
	; (source-answer request) -> :nil | answer
	;what a request is answered with, :nil if it is answered later
	(cond
		;what it has done, "!": tasks' asks, those answered from what was
		;kept, and the asks it made of other systems
		((eql request "!")
			(cat "y" (str (map (# (ifn (. *source_counts* :find %0) 0)) '(:asked :kept :told :sought)))))
		;a file of another system for a task here, "%sid svc path"
		((starts-with "%" request) (source-fetch (slice request 1 -1)))
		;a file and all it imports, of a @Source elsewhere, "&key\npath"
		((starts-with "&" request) (source-tree-answer (slice request 1 -1)))
		;the texts of files, of a @Source elsewhere, "*key\npath..."
		((starts-with "*" request) (source-texts-answer (slice request 1 -1)))
		((starts-with "+" request)
			(if (defq natives (source-translate (slice request 1 -1))) (cat "y" natives) "n"))
		;the hashes of a function and all it calls, "#mode name"
		((starts-with "#" request)
			(bind '(mode name) (split (slice request 1 -1) " "))
			(if (defq text (source-manifest mode name)) (cat "y" text) "n"))
		;the VP binary of functions, "$mode name ..."
		((starts-with "$" request)
			(defq names (split (slice request 1 -1) " "))
			(cat "y" (source-entries (first names) (rest names))))
		((and (source-given? request) (defq text (load request))) (cat "y" text))
		("n")))

(defun main ()
	;one to a system: two started at once give way to the lowest, (mail-single).
	;Each checks as a task asks it and each second; the other stops saying its
	;name, sends on to the one that stays what still comes to it, answers the
	;tasks it was asking for already, and goes when nothing has come for as
	;long as a task waits
	(defq service (mail-declare (task-mbox) "@Source" "Source Service 0.2") *source_env* (env)
		*source_hashes* (Fmap 101) *source_files* (Fmap 101) *source_out* (Fmap 31) *source_kept* (Fmap 101)
		*source_known* (Fmap 31) *source_trees* (Fmap 31) *source_counts* (Fmap 11) *source_svcs* (Fmap 11)
		*source_back* (mail-mbox) select (list (task-mbox) *source_back* (mail-mbox)) *source_msg* :nil
		winner :nil heard (pii-time) running :t)
	(mail-timeout (third select) 1000000 0)
	(while running
		(defq idx (mail-select select) msg (mail-read (elem-get select idx)))
		(unless winner
			(when (setq winner (mail-single "@Source" (task-mbox)))
				(mail-forget service)
				(setq heard (pii-time))))
		(cond
			((= idx 2)
				(mail-timeout (third select) 1000000 0)
				(source-beat)
				(and winner (. *source_out* :empty?)
					(> (- (pii-time) heard) (task-timeout +source_wait))
					(setq running :nil)))
			((= idx 1) (source-fetched msg))
			(winner (mail-send winner msg) (setq heard (pii-time)))
			((defq *source_msg* msg answer (source-answer (slice msg +source_rpc_size -1)))
				(mail-send (getf msg +source_rpc_reply_id) answer)))))
