;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A tree of files made the same on another machine.
;
; Each side lists its files, a name and a SHA-256 of each. The lists
; are compared, and only a file that is not there, or is not the
; same, goes over. Two people with notepads who meet, compare pages,
; and copy across what the other has not got.
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

(import "lib/crypto/sha256.inc")

;what is asked of the service, and what it says back
(enums +sync_type 0
	(enum list put del quit))

(structure +sync_rpc 0
	(netid reply_id)
	(uint type)
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
		(setq line (trim line (static-q " \t\r")))
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

(defun sync-list (root rules &optional kept)
	; (sync-list root rules [kept]) -> ((path size hash) ...)
	;the files of a tree, with the size and the SHA-256 of each. With a
	;file to keep them in, a hash is not worked out again for a file whose
	;time and size are as they were. One changed in the last two seconds is
	;always hashed, its time may not change if it is changed again
	(defq cache (Fmap 101) dirty :nil now (/ (pii-time) 1000000) text :nil)
	(when (and kept (setq text (load kept)))
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
				(task-slice))
			(push lines (cat (str mtime) " " (str size) " " (third hit) " " path))
			(push out (list path size (third hit)))))
		(sync-walk root rules))
	(if (and kept dirty) (save (join lines (ascii-char 10)) kept))
	out)

(defun sync-list-text (entries)
	; (sync-list-text entries) -> str
	;a list as it goes in a message, a line a file
	(join (map (lambda ((path size hash)) (cat hash " " (str size) " " path)) entries) (ascii-char 10)))

(defun sync-list-read (text)
	; (sync-list-read text) -> ((path size hash) ...)
	(reduce (lambda (out line)
		(if (>= (length (defq parts (split line " "))) 3)
			(push out (list (join (slice parts 2 -1) " ") (str-as-num (second parts)) (first parts))))
		out) (split text (ascii-char 10)) (list)))

(defun sync-diff (mine theirs)
	; (sync-diff mine theirs) -> (send remove)
	;the paths to send, not there or not the same, and the paths that are
	;there and not here
	(defq have (Fmap 101) keep (Fset 101))
	(each (lambda ((path size hash)) (. have :insert path hash)) theirs)
	(each (lambda ((path size hash)) (. keep :insert path)) mine)
	(list (reduce (lambda (out (path size hash))
			(if (nql hash (ifn (. have :find path) "")) (push out path)) out) mine (list))
		(reduce (lambda (out (path size hash))
			(unless (. keep :find path) (push out path)) out) theirs (list))))

(defun sync-tell (svc reply_mbox kind data &optional at total)
	; (sync-tell svc reply_mbox kind data [at total])
	;a message to a sync service
	(mail-send svc (cat (setf-> (str-alloc +sync_rpc_size)
		(+sync_rpc_reply_id reply_mbox)
		(+sync_rpc_type kind)
		(+sync_rpc_at (ifn at 0))
		(+sync_rpc_total (ifn total 0))) data)))

(defun sync-hear (reply_mbox &optional wait)
	; (sync-hear reply_mbox [wait]) -> :nil | (status data)
	;what it says back, :nil if it does not in the time
	(when (defq reply (mail-read-timeout reply_mbox (ifn wait +sync_wait)))
		(list (getf reply +sync_reply_status) (slice reply +sync_reply_data -1))))

(defun sync-push (svc root rules_text &optional check remove kept)
	; (sync-push svc root rules_text [check remove kept]) -> :nil | (sent bytes removed failed send gone)
	;make the tree a sync service has the same as the one under root. With
	;check nothing is changed there. With remove, what is there and not
	;here is removed. send and gone are the paths that differ, and the ones
	;only there. :nil if the service does not answer
	(defq reply_mbox (mail-mbox) rules (sync-rules rules_text))
	;it lists its tree as this one is listed
	(sync-tell svc reply_mbox +sync_type_list rules_text)
	(defq mine (sync-list root rules kept) reply (sync-hear reply_mbox))
	(when (and reply (= (first reply) 0))
		(bind '(send gone) (sync-diff mine (sync-list-read (second reply))))
		(defq sent 0 bytes 0 removed 0 failed 0)
		(unless check
			(each (lambda (path)
				(defq data (ifn (load (cat root "/" path)) "") total (length data) at 0 ok :t)
				;a file of nothing still has to be made there, so once round at least
				(while (and ok (or (< at total) (= at 0)))
					(defq end (min total (+ at +sync_chunk)))
					(sync-tell svc reply_mbox +sync_type_put
						(cat path (ascii-char 10) (slice data at end)) at total)
					(setq ok (and (defq reply (sync-hear reply_mbox)) (= (first reply) 0))
						at (if (= end at) (inc total) end)))
				(if ok (setq sent (inc sent) bytes (+ bytes total)) (setq failed (inc failed))))
				send)
			(when remove
				(each (lambda (path)
					(sync-tell svc reply_mbox +sync_type_del path)
					(if (and (defq reply (sync-hear reply_mbox)) (= (first reply) 0))
						(setq removed (inc removed)) (setq failed (inc failed))))
					gone)))
		(list sent bytes removed failed send gone)))

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
