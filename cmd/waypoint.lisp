(import "lib/options/options.inc")
(import "lib/sync/sync.inc")
(import "lib/streams/deflate.inc")
(import "lib/files/files.inc")

(defq usage `(
(("-h" "--help")
"Usage: waypoint [options]

    options:
        -h --help: this help info.
        -c --check: say where the tree is, and change nothing.

    Note the waypoint this machine's tree is at, the file waypoint at
    its root: the commit git has it at, when that was made, read from
    .git, and the top of its tree of hashes. Run it when the tree is
    exactly a commit of master or of a release, after a git pull; it is
    not noted if it is not.

    Only a tree that is exactly at its waypoint takes part in the herd,
    a sync to or from a machine that is not one of its own: a tree with
    a change of its own since is not, and nothing is pushed to it or
    taken from it. Pull and note the waypoint again and it is back.
    lib/sync/sync.inc, Who takes a sync.

    It is exactly the commit if git's index is the commit's tree, each
    file is the size, time and mode the index has, and there is no file
    git does not know of and is not told to leave out. A file only
    touched is taken as changed till git status has looked at it.

    If this machine takes a sync, sync -a, the waypoint is offered to
    the others that do, and goes round the herd by itself.

    With -c, the waypoint it was last at and if it is still exactly that.")
(("-c" "--check") ,(opt-flag 'opt_c))
))

;the ends of git's files, a line's end and spaces
(defq +git_space (char-class (cat " " (ascii-char 9) (ascii-char 10) (ascii-char 13))))

(defun git-commit ()
	; (git-commit) -> :nil | commit
	;the commit git has the tree at: the one its HEAD names, by its ref, as
	;a file of its own or in the packed refs, or HEAD itself
	(when (defq head (load ".git/HEAD"))
		(setq head (trim head +git_space))
		(cond
			((starts-with "ref: " head)
				(defq ref (slice head 5 -1))
				(cond
					((defq one (load (cat ".git/" ref))) (trim one +git_space))
					((some (lambda (line)
							(defq parts (split line " "))
							(and (= (length parts) 2) (eql (second parts) ref) (first parts)))
						(split (ifn (load ".git/packed-refs") "") (ascii-char 10))))))
			((= (length head) 40) head))))

;the objects of git, read for the time of a commit. An object is a file of
;its own, loose, or in a pack, found by the pack's index; one in a pack may
;be a delta, the changes to another

(defq *idxs* :nil)

(defmacro git-be32 (s i)
	; (git-be32 str index) -> num
	(static-qq (code-be ,s 4 ,i)))

(defun git-unzip (stream)
	; (git-unzip stream) -> :nil | str
	;the zlib stream that is next in stream, unpacked
	(defq out (string-stream (cat "")))
	(if (inflate-decompress stream out :t) (str out)))

(defun git-loose (sha)
	; (git-loose sha) -> :nil | (type data)
	(and (defq z (load (cat ".git/objects/" (slice sha 0 2) "/" (slice sha 2 -1))))
		(defq raw (git-unzip (string-stream z)))
		(list (slice raw 0 (find " " raw)) (slice raw (inc (find (ascii-char 0) raw)) -1))))

(defun git-idx-find (idx sha)
	; (git-idx-find idx sha) -> :nil | offset
	;where in its pack an object is, by a pack index of version 2: the count
	;of names to each first byte, the names in order, their sums, their
	;offsets, and the offsets too big for 31 bits
	(defq b (str-to-num (cat "0x" (slice sha 0 2))) n (git-be32 idx 1028)
		lo (if (= b 0) 0 (git-be32 idx (+ 8 (* 4 (dec b))))) hi (git-be32 idx (+ 8 (* 4 b))) at :nil)
	(while (and (not at) (< lo hi))
		(defq mid (/ (+ lo hi) 2) i (+ 1032 (* mid 20))
			c (cmp (hex-encode (slice idx i (+ i 20))) sha))
		(cond
			((= c 0) (setq at mid))
			((< c 0) (setq lo (inc mid)))
			((setq hi mid))))
	(when at
		(defq o (git-be32 idx (+ 1032 (* n 24) (* at 4))))
		(if (bits? o 0x80000000)
			(+ (<< (git-be32 idx (defq i (+ 1032 (* n 28) (* 8 (logand o 0x7fffffff))))) 32) (git-be32 idx (+ i 4)))
			o)))

(defun git-delta-size (d i)
	; (git-delta-size data index) -> (num index)
	(defq n 0 shift 0 b 128)
	(while (bits? b 128)
		(setq b (code d 1 i) i (inc i) n (logior n (<< (logand b 127) shift)) shift (+ shift 7)))
	(list n i))

(defun git-delta-apply (base d)
	; (git-delta-apply base delta) -> str
	;a delta's copies from base and its own bytes, after the sizes of the
	;base and of what it makes
	(bind '(& i) (git-delta-size d 0))
	(bind '(& i) (git-delta-size d i))
	(defq out (list))
	(while (< i (length d))
		(defq op (code d 1 i) i (inc i))
		(cond
			((bits? op 128)
				(defq off 0 size 0)
				(each (lambda (k)
					(when (bits? op (<< 1 k))
						(setq off (logior off (<< (code d 1 i) (* 8 k))) i (inc i)))) '(0 1 2 3))
				(each (lambda (k)
					(when (bits? op (<< 16 k))
						(setq size (logior size (<< (code d 1 i) (* 8 k))) i (inc i)))) '(0 1 2))
				(push out (slice base off (+ off (if (= size 0) 0x10000 size)))))
			((> op 0)
				(push out (slice d i (+ i op)))
				(setq i (+ i op)))))
	(apply (const cat) out))

(defun git-pack-entry (pack at)
	; (git-pack-entry pack offset) -> :nil | (kind data [base])
	;the entry at offset in a pack: its kind and size, then it zipped. A
	;whole object is its kind, 1 to 4, and its bytes. A delta is 6 and the
	;offset of its base, so far back in the pack, or 7 and the name of its
	;base, and the changes to it
	(defq s (file-stream pack))
	(stream-seek s at 0)
	(defq b (read-char s) kind (logand (>> b 4) 7))
	(while (bits? b 128) (setq b (read-char s)))
	(cond
		((<= 1 kind 4) (if (defq data (git-unzip s)) (list kind data)))
		((= kind 6)
			(defq b (read-char s) n (logand b 127))
			(while (bits? b 128)
				(setq b (read-char s) n (logior (<< (inc n) 7) (logand b 127))))
			(if (defq delta (git-unzip s)) (list kind delta (- at n))))
		((= kind 7)
			(defq sha (hex-encode (read-blk s 20)))
			(if (defq delta (git-unzip s)) (list kind delta sha)))))

(defun git-find (sha)
	; (git-find sha) -> :nil | (:loose (type data)) | (:pack pack offset)
	(setq sha (to-upper sha))
	(unless *idxs*
		(setq *idxs* (map (# (list (cat (slice %0 0 -5) ".pack") (load %0))) (files-all ".git/objects/pack" '(".idx")))))
	(cond
		((defq obj (git-loose (to-lower sha))) (list :loose obj))
		((some (lambda ((pack idx)) (if (defq at (git-idx-find idx sha)) (list :pack pack at))) *idxs*))))

(defun git-object (sha)
	; (git-object sha) -> :nil | (type data)
	;an object of git by its name, loose or in a pack. One in a pack may be
	;a delta of another, which may be a delta of another: the deltas are
	;gathered back to a whole object, then put on it, the last first
	(defq deltas (list) obj :nil where (git-find sha))
	(while (and where (not obj))
		(cond
			((eql (first where) :loose) (setq obj (second where)))
			((defq entry (git-pack-entry (second where) (third where)))
				(bind '(kind data &optional base) entry)
				(cond
					((= kind 6) (push deltas data) (setq where (list :pack (second where) base)))
					((= kind 7) (push deltas data) (setq where (git-find base)))
					((setq obj (list (elem-get '("" "commit" "tree" "blob" "tag") kind) data)))))
			((setq where :nil))))
	(when obj
		(defq data (second obj))
		(while (defq delta (pop deltas)) (setq data (git-delta-apply data delta)))
		(list (first obj) data)))

;is the tree exactly the commit ? As git status first looks: what git's
;index has is what the commit's tree has, nothing added to it or taken out;
;each file is the size and time and mode the index has for it; and there
;is no file git does not know of and is not told to leave out

(defun git-index ()
	; (git-index) -> :nil | str | ((path sha mode size mtime) ...)
	;the entries of .git/index, of version 2 or 3: a name of what is wrong if
	;it can not be read, or a merge is not finished
	(defq idx (load ".git/index") out (list) why :nil)
	(cond
		((not (and idx (> (length idx) 12) (eql (slice idx 0 4) "DIRC"))) "no index of git can be read")
		((not (<= 2 (defq version (git-be32 idx 4)) 3)) (cat "the index of git is of version " (str version) ", which is not read"))
		(:t (defq i 12)
			(times (git-be32 idx 8)
				(unless why
					;the times, the device, the inode, the mode, the owner, the
					;size, the name of its blob, and its flags
					(defq flags (logior (<< (code idx 1 (+ i 60)) 8) (code idx 1 (+ i 61)))
						start (+ i 62 (if (bits? flags 0x4000) 2 0))
						end (find (ascii-char 0) idx start))
					(if (/= (logand (>> flags 12) 3) 0) (setq why "a merge is not finished"))
					(push out (list (slice idx start end) (hex-encode (slice idx (+ i 40) (+ i 60)))
						(git-be32 idx (+ i 24)) (git-be32 idx (+ i 36)) (git-be32 idx (+ i 8))))
					;each entry is a whole number of 8 bytes, with a 0 at least
					(setq i (+ i (logand (+ (- end i) 8) -8)))))
			(ifn why out))))

(defun git-tree-files (sha)
	; (git-tree-files sha) -> :nil | ((path sha) ...)
	;every file of a tree of git, the trees in it gone into, a tree's entry
	;its mode, its name, a 0 and the 20 bytes of the name of its object
	(defq out (list) todo (list (list "" sha)) bad :nil)
	(while (and (not bad) (defq job (pop todo)))
		(bind '(pre tsha) job)
		(cond
			((and (defq obj (git-object tsha)) (eql (first obj) "tree"))
				(defq data (second obj) i 0)
				(while (< i (length data))
					(defq sp (find " " data i) nul (find (ascii-char 0) data sp)
						name (cat pre (slice data (inc sp) nul))
						osha (hex-encode (slice data (inc nul) (+ nul 21))))
					(if (eql (slice data i sp) "40000")
						(push todo (list (cat name "/") osha))
						(push out (list name osha)))
					(setq i (+ nul 21))))
			((setq bad :t))))
	(unless bad out))

(defun git-dirty (commit)
	; (git-dirty commit) -> :nil | str
	;:nil if the tree is exactly that commit, or what is not
	(defq entries (git-index) obj (git-object commit))
	(cond
		((str? entries) entries)
		((not (and obj (eql (first obj) "commit") (starts-with "tree " (second obj))))
			(cat "the commit " commit " can not be read from .git"))
		((not (defq files (git-tree-files (to-upper (slice (second obj) 5 45)))))
			"the tree of the commit can not be read from .git")
		(:t (defq staged (Fmap 101) why :nil)
			(each (lambda ((path sha)) (. staged :insert path sha)) files)
			(some (lambda ((path sha mode size mtime))
					(defq info (pii-fstat path) was (. staged :find path))
					(setq why (cond
						((not was) (cat path " is added, and not committed"))
						((nql was sha) (cat path " is changed, and not committed"))
						((not info) (cat path " is gone"))
						((or (/= (second info) size) (/= (first info) mtime))
							(cat path " is changed, or its time is: git status, then waypoint again"))
						((nql (bits? (third info) 0o100) (bits? mode 0o100))
							(cat path " is changed in who may run it")))))
				entries)
			(unless why
				(if (/= (length entries) (length files)) (setq why "a file of the commit is taken out, and not committed")))
			(unless why
				(defq known (Fset 101))
				(each (# (. known :insert (first %0))) entries)
				(if (defq extra (some (# (unless (. known :find %0) %0)) (sync-walk "." (sync-own-rules "."))))
					(setq why (cat extra " is not known to git"))))
			why)))

(defun git-commit-time (sha)
	; (git-commit-time sha) -> :nil | microseconds
	;when a commit was made, as its committer line says, seconds since 1970
	(and (defq obj (git-object sha)) (eql (first obj) "commit")
		(defq line (some (# (if (starts-with "committer " %0) %0)) (split (second obj) (ascii-char 10))))
		(defq words (split line " ")) (>= (length words) 2)
		(* (str-to-num (elem-get words (- (length words) 2))) 1000000)))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_c :nil args (options stdio usage)))
		(defq kept (sync-kept ".") top (sync-top "." kept)
			at (sync-waypoint "."))
		(cond
			(opt_c
				(cond
					((not at) (print "No waypoint noted, this tree takes no part in the herd."))
					((eql (third at) top)
						(print "At waypoint " (first at) ", exactly, it takes part in the herd."))
					(:t (print "Was at waypoint " (first at) ", changed since, it takes no part in the herd."))))
			((not (defq commit (git-commit)))
				(print "No commit to note, git has none here."))
			((defq why (git-dirty commit))
				(print "Not noted, the tree is not exactly the commit " commit ": " why "."))
			(:t (defq made (git-commit-time commit))
				(save (sync-waypoint-text commit (ifn made (pii-time)) top) "waypoint")
				(print "Waypoint " commit " noted, this tree takes part in the herd.")
				;this machine's sync, if it takes one, offers it to the herd
				(when (defq svc (some (# (if (eql (second %0) (hex-encode (system-id))) (first %0))) (sync-services)))
					(sync-tell svc (defq mbox (mail-mbox)) +sync_type_spread "")
					(and (defq reply (sync-hear mbox 5000000)) (= (first reply) 0)
						(print "It is offered to the herd.")))
				(unless made (print "The time of the commit could not be read from .git, the time now is kept in its place."))))))
