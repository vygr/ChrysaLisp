;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A tree of files made the same on another machine.
;
; Each side has a tree of hashes of its files, lib/hash/tree.inc, a
; SHA-256 of each file, and of each folder the hash of what is in it.
; The tops are compared, one number each. If they are the same the
; trees are, and that is all that is said. If not, the folders are
; compared from the top down, only those whose hashes differ, and only
; a file that is not there, or is not the same, goes over. Two people
; with notepads who meet, compare pages, and copy across what the
; other has not got.
;
; The other machine has to have said it will take a sync, sync -a,
; which starts the service there, service/sync/. That is all the say
; so is, a machine with a *Sync service takes one and a machine with
; none can not be sent one. The * is a service seen from every
; machine, where an @ is one for its own machine. It writes only under
; the root it was started with. It all goes as mail between the two,
; over whatever links there are, no task is started on the other
; machine.
;
; What is left out is said by rules in the form of a .gitignore, the
; one the tree has is used, so what git does not keep is not sent,
; obj/ and the like. Both sides list by the same rules.
;
; These are the parts, cmd/sync.lisp is the command.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././crypto/sha256.inc")
(import "././hash/tree.inc")

;what is asked of the service, and what it says back
;list was the whole list of files, asked for and sent each time. A
;service that is asked for it now says it does not know it, so an older
;sync finds no answer, and does not send every file. root is the top of
;its tree, by the rules that come with it, and folder is what is in one
;folder of that tree
(enums +sync_type 0
	(enum list put del quit mode root folder))

(structure +sync_rpc 0
	(netid reply_id)
	(uint type mode)
	(long at total)
	(offset data))

(structure +sync_reply 0
	(int status)
	(offset data))

;a file goes over this much at a time, and how long an answer is waited for
(defq +sync_chunk 131072 +sync_wait 120000000)

(defun sync-rules (text)
	; (sync-rules text) -> rules
	;the rules of a .gitignore, each (anchored dir_only segments). Those
	;that take back a rule, !, are not known and are left out. The .git
	;folder is never listed
	(defq out (list (list :nil :t (list ".git"))))
	(each (lambda (line)
		(setq line (trim line (const (char-class " \t\r"))))
		(unless (or (eql line "") (starts-with "#" line) (starts-with "!" line))
			(defq dir_only (ends-with "/" line))
			(if dir_only (setq line (slice line 0 -2)))
			(defq anchored (starts-with "/" line))
			(if anchored (setq line (slice line 1 -1)))
			(defq segs (split line "/"))
			(if (nempty? segs)
				(push out (list (or anchored (> (length segs) 1)) dir_only segs)))))
		(split text (ascii-char 10)))
	out)

(defun sync-seg? (pat seg)
	; (sync-seg? pat seg) -> :nil | :t
	;does one part of a path match one part of a rule, which may have a *
	(if (defq star (find "*" pat))
		(and (>= (length seg) (dec (length pat)))
			(starts-with (slice pat 0 star) seg)
			(ends-with (slice pat (inc star) -1) seg))
		(eql pat seg)))

(defun sync-ignored? (rules segs is_dir)
	; (sync-ignored? rules segs is_dir) -> :nil | :t
	;is a path, as its parts, left out ? The folders above it are taken
	;to have been asked of already, as a walk down the tree does
	(if (some (lambda ((anchored dir_only pats))
			(and (or is_dir (not dir_only))
				(if anchored
					(and (= (length pats) (length segs))
						(not (some (# (not (sync-seg? %0 %1))) pats segs)))
					(sync-seg? (first pats) (last segs)))))
		rules) :t))

(defun sync-safe? (path)
	; (sync-safe? path) -> :nil | :t
	;a path the service will write to, by how it is written. Under its
	;root, and not up out of it. No .., no start at the top of the host's
	;files or of a drive, no ~, and no character below a space
	(and (str? path) (nempty? path) (not (starts-with "/" path)) (not (starts-with "~" path))
		(not (find ":" path)) (not (find (ascii-char 92) path)) (not (found? path "//"))
		(not (some (# (< (code %0) 32)) path))
		(not (some (# (or (eql %0 "..") (eql %0 ".") (eql %0 ".git"))) (split path "/")))))

(defun sync-kind (folder entry)
	; (sync-kind folder entry) -> :nil | kind
	;what the host says an entry of a folder is, "4" a folder, "8" a file,
	;anything else something else, a link for one. :nil if it is not there
	(defq found :nil)
	(each! (lambda (it kind) (if (eql it entry) (setq found kind)))
		(unzip (split (pii-dirlist folder) ",") 2))
	found)

(defun sync-inside? (root path &optional real)
	; (sync-inside? root path [real]) -> :nil | :t
	;is a path, safe as written, under the root as the host has it ? Every
	;folder on the way has to be a folder, and the file a file, or not
	;there yet. A link is neither, so a link in the tree to somewhere else
	;is never gone through, and nothing is written or removed beyond it.
	;real is a set of the folders found to be so, to ask the host the once
	(defq at root ok :t segs (split path "/") end (dec (length segs)))
	(some! (lambda (seg)
		(defq is_last (= (!) end) next (cat at "/" seg))
		(cond
			((and (not is_last) real (. real :find next)))
			(:t (defq kind (sync-kind at seg))
				(cond
					;not there, so nothing is below it, and it will be made
					((not kind) (setq end -1))
					((eql kind (if is_last "8" "4"))
						(if (and real (not is_last)) (. real :insert next)))
					(:t (setq ok :nil)))))
		(setq at next)
		(or (not ok) (= end -1)))
		(list segs))
	ok)

(defun sync-walk (root rules)
	; (sync-walk root rules) -> paths
	;every file under the root that the rules do not leave out, each a
	;path from the root, in order
	(defq stack (list "") out (list) folder :nil)
	(while (setq folder (pop stack))
		(each! (lambda (entry kind)
			(unless (or (eql entry ".") (eql entry ".."))
				(defq path (if (eql folder "") entry (cat folder "/" entry)) is_dir (eql kind "4"))
				(unless (sync-ignored? rules (split path "/") is_dir)
					(cond
						(is_dir (push stack path))
						((eql kind "8") (push out path))))))
			(unzip (split (pii-dirlist (if (eql folder "") root (cat root "/" folder))) ",") 2)))
	(sort out))

(defun sync-list (root rules &optional kept live)
	; (sync-list root rules [kept live]) -> ((path size hash mode) ...)
	;the files of a tree, with the size and the SHA-256 of each. With a
	;file to keep them in, a hash is not worked out again for a file whose
	;time and size are as they were. One changed in the last two seconds is
	;always hashed, its time may not change if it is changed again.
	;live is a map to hold them in between one call and the next, for a
	;task that stays, a service. The file is then read the once, and is
	;what is there when the task starts again
	(defq cache (ifn live (Fmap 101)) dirty :nil now (/ (pii-time) 1000000) text :nil)
	(when (and kept (= (. cache :size) 0) (setq text (load kept)))
		(each (lambda (line)
			(when (>= (length (defq parts (split line " "))) 4)
				(. cache :insert (join (slice parts 3 -1) " ")
					(list (str-as-num (first parts)) (str-as-num (second parts)) (third parts)))))
			(split text (ascii-char 10))))
	(defq lines (list) out (list))
	(each (lambda (path)
		(when (defq info (pii-fstat (cat root "/" path)))
			(defq mtime (first info) size (second info) hit (. cache :find path))
			(unless (and hit (= (first hit) mtime) (= (second hit) size) (> (- now mtime) 2))
				(setq hit (list mtime size (hex-encode (sha256 (ifn (load (cat root "/" path)) "")))) dirty :t)
				(. cache :insert path hit)
				(task-slice))
			(push lines (cat (str mtime) " " (str size) " " (third hit) " " path))
			;the mode is who may read, write and run it, the low 9 bits
			(push out (list path size (third hit) (logand (third info) 511)))))
		(sync-walk root rules))
	(if (and kept dirty) (save (join lines (ascii-char 10)) kept))
	out)

(defun sync-tree (root rules &optional kept no_modes live)
	; (sync-tree root rules [kept no_modes live]) -> tree
	;the tree of hashes of the files under a root. The mode of a file is
	;part of it, unless no_modes, for a host that has no such thing. kept
	;and live are as (sync-list) has them
	(hash-tree (map (lambda ((path size hash mode)) (list path hash (if no_modes "-" (str mode))))
		(sync-list root rules kept live))))

(defun sync-tell (svc reply_mbox kind data &optional at total mode)
	; (sync-tell svc reply_mbox kind data [at total mode])
	;a message to a sync service
	(mail-send svc (cat (setf-> (str-alloc +sync_rpc_size)
		(+sync_rpc_reply_id reply_mbox)
		(+sync_rpc_type kind)
		(+sync_rpc_mode (ifn mode 0))
		(+sync_rpc_at (ifn at 0))
		(+sync_rpc_total (ifn total 0))) data)))

(defun sync-hear (reply_mbox &optional wait)
	; (sync-hear reply_mbox [wait]) -> :nil | (status data)
	;what it says back, :nil if it does not in the time
	(when (defq reply (mail-read-timeout reply_mbox (ifn wait +sync_wait)))
		(list (getf reply +sync_reply_status) (slice reply +sync_reply_data -1))))

(defun sync-root (svc rules_text &optional no_modes wait)
	; (sync-root svc rules_text [no_modes wait]) -> :nil | :old | hash
	;the top of the tree a sync service has, by those rules. It works it
	;out when asked, and keeps it to be asked of its folders. :nil if it
	;does not answer, :old if it is a sync from before there were trees
	(defq reply_mbox (mail-mbox))
	(sync-tell svc reply_mbox +sync_type_root rules_text 0 0 (if no_modes 1 0))
	(cond
		((not (defq reply (sync-hear reply_mbox wait))) :nil)
		((/= (first reply) 0) :old)
		((second reply))))

(defun sync-push (svc root rules_text &optional check remove kept no_modes)
	; (sync-push svc root rules_text [check remove kept no_modes])
	;	-> :nil | :old | (sent bytes removed failed send gone remoded)
	;make the tree a sync service has the same as the one under root. With
	;check nothing is changed there. With remove, what is there and not
	;here is removed. send and gone are the paths that differ, and the ones
	;only there. A file is given the mode it has here, who may read, write
	;and run it, and one that is the same but for its mode has that set,
	;unless no_modes, for a host that has no such thing. :nil if the
	;service does not answer, :old if it is from before there were trees
	(defq reply_mbox (mail-mbox) rules (sync-rules rules_text))
	;it works out its tree as this one is, by the same rules
	(sync-tell svc reply_mbox +sync_type_root rules_text 0 0 (if no_modes 1 0))
	(defq mine (sync-tree root rules kept no_modes) reply (sync-hear reply_mbox))
	(cond
		((not reply) :nil)
		((/= (first reply) 0) :old)
		(:t (defq send (list) gone (list) remode (list) lost :nil
				stack (if (eql (second reply) (hash-tree-root mine)) (list) (list ""))
				theirs_only (list))
			;the tops differ. Folder by folder from the top, only the ones
			;whose hashes differ
			(while (and (not lost) (defq at (pop stack)))
				(sync-tell svc reply_mbox +sync_type_folder at)
				(defq pre (if (eql at "") "" (cat at "/")))
				(cond
					((and (setq reply (sync-hear reply_mbox)) (= (first reply) 0))
						(bind '(differ meta lost_there into only_mine only_theirs)
							(hash-tree-compare (hash-tree-kids mine at) (hash-tree-read (second reply))))
						(each (# (push send (cat pre %0))) differ)
						(each (lambda ((name m)) (push remode (list (cat pre name) (str-as-num m)))) meta)
						(each (# (push gone (cat pre %0))) lost_there)
						(each (# (push stack (cat pre %0))) into)
						;a folder only here, all of it goes. One only there is
						;asked after if what is in it is wanted
						(each (# (each (# (push send %0)) (hash-tree-under mine (cat pre %0)))) only_mine)
						(each (# (push theirs_only (cat pre %0))) only_theirs))
					(:t (setq lost :t))))
			(progn
				(while (and (not lost) (defq at (pop theirs_only)))
					(sync-tell svc reply_mbox +sync_type_folder at)
					(cond
						((and (setq reply (sync-hear reply_mbox)) (= (first reply) 0))
							(each (lambda ((name kind &rest _))
								(push (if (eql kind "d") theirs_only gone) (cat at "/" name)))
								(hash-tree-read (second reply))))
						(:t (setq lost :t)))))
			(sort send) (sort gone)
			(defq sent 0 bytes 0 removed 0 failed (if lost 1 0) remoded 0)
			(unless (or check lost)
				(each (lambda (path)
					(defq data (ifn (load (cat root "/" path)) "") total (length data) at 0 ok :t
						mode (if no_modes 0 (logand (third (ifn (pii-fstat (cat root "/" path)) '(0 0 0))) 511)))
					;a file of nothing still has to be made there, so once round at least
					(while (and ok (or (< at total) (= at 0)))
						(defq end (min total (+ at +sync_chunk)))
						(sync-tell svc reply_mbox +sync_type_put
							(cat path (ascii-char 10) (slice data at end)) at total mode)
						(setq ok (and (defq reply (sync-hear reply_mbox)) (= (first reply) 0))
							at (if (= end at) (inc total) end)))
					(if ok (setq sent (inc sent) bytes (+ bytes total)) (setq failed (inc failed))))
					send)
				(each (lambda ((path mode))
					(sync-tell svc reply_mbox +sync_type_mode path 0 0 mode)
					(if (and (defq reply (sync-hear reply_mbox)) (= (first reply) 0))
						(setq remoded (inc remoded)) (setq failed (inc failed))))
					remode)
				(when remove
					(each (lambda (path)
						(sync-tell svc reply_mbox +sync_type_del path)
						(if (and (defq reply (sync-hear reply_mbox)) (= (first reply) 0))
							(setq removed (inc removed)) (setq failed (inc failed))))
						gone)))
			(list sent bytes removed failed send gone (if check (length remode) remoded)))))

(defun sync-services (&optional name)
	; (sync-services [name]) -> ((mbox system_id machine root) ...)
	;the machines that will take a sync, as their services say
	(reduce (lambda (out entry)
		;an entry is the name, the mailbox, the system id and what the service said
		(defq parts (split entry ",") info (split (ifn (elem-get parts 3) "") " "))
		(if (>= (length info) 5)
			(push out (list (hex-decode (second parts)) (third parts) (elem-get info 3)
				(join (slice info 4 -1) " "))))
		out) (mail-enquire (cat (ifn name "*Sync") ",")) (list)))
