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
    .git, and the top of its tree of hashes. Run it when the tree is exactly a commit of master or of
    a release, after a git pull.

    Only a tree that is exactly at its waypoint takes part in the herd,
    a sync to or from a machine that is not one of its own: a tree with
    a change of its own since is not, and nothing is pushed to it or
    taken from it. Pull and note the waypoint again and it is back.
    lib/sync/sync.inc, Who takes a sync.

    With -c, the waypoint it was last at and if it is still exactly that.")
(("-c" "--check") ,(opt-flag 'opt_c))
))

(defun git-commit ()
	; (git-commit) -> :nil | commit
	;the commit git has the tree at: the one its HEAD names, by its ref, as
	;a file of its own or in the packed refs, or HEAD itself
	(when (defq head (load ".git/HEAD"))
		(setq head (trim head))
		(cond
			((starts-with "ref: " head)
				(defq ref (slice head 5 -1))
				(cond
					((defq one (load (cat ".git/" ref))) (trim one))
					((some (lambda (line)
							(defq parts (split line " "))
							(and (= (length parts) 2) (eql (second parts) ref) (first parts)))
						(split (ifn (load ".git/packed-refs") "") (ascii-char 10))))))
			((= (length head) 40) head))))

;the objects of git, read for the time of a commit. An object is a file of
;its own, loose, or in a pack, found by the pack's index; one in a pack may
;be a delta, the changes to another

(defq *idxs* :nil)

(defun git-be32 (s i)
	; (git-be32 str index) -> num
	(logior (<< (code s 1 i) 24) (<< (code s 1 (+ i 1)) 16) (<< (code s 1 (+ i 2)) 8) (code s 1 (+ i 3))))

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
		(defq kept (cat "obj/" (cpu) "/" (abi) "/sync_hashes") top (sync-top "." kept)
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
			(:t (defq made (git-commit-time commit))
				(save (sync-waypoint-text commit (ifn made (pii-time)) top) "waypoint")
				(print "Waypoint " commit " noted, this tree takes part in the herd.")
				(unless made (print "The time of the commit could not be read from .git, the time now is kept in its place."))))))
