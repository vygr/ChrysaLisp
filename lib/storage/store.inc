;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a store, what one Storage service keeps
;
; An object store on an exFAT volume, lib/fs/exfat.inc, a file of the host
; that is the image of a disk, or the disk. It is what one member of a
; Storage system holds, and all that there is when there is the one.
;
; A block is some bytes, kept under the SHA-256 of them. So a block never
; changes, two that are the same are one, and one that has gone wrong does
; not match its name and is known for it.
;
; An object is the list of its blocks, a manifest, which is a block too.
; The manifest of a big one, of more than +store_manifest_names blocks, is
; a tree of blocks of no more than that many names each, so that it is no
; bigger than any other block, (store-manifest-make). A path is a name
; for one object, for now: the path map is the one part of
; a store that changes, and the one part members will ever have to agree
; on. An entry of it is
;
;	(path hash size vv writer seen deleted copies)
;
; hash is the name of the manifest. vv says how many times each member has
; written the path, as far as the writer of this entry knew: one entry
; comes after another if it has as many or more of every member's, and two
; that each have more of some member's were written apart, neither knowing
; of the other. writer is the member that made it, and seen the members
; that are known to have it, this one among them. One that is deleted
; stays, with no object, so that a member who was away does not bring the
; object back. copies is how many members are to keep the blocks of the
; object, 0 for as many as the store says, (store-copies).
;
; There is one way to write: a path is set to an object provided it is
; now what the writer says it is, (store-commit).
;
; On the volume:
;
;	store		what it is: the store's id, the member's, the block size
;	members		the members, a line each
;	retired		those that were members and are no more
;	pins		the paths this member holds all of, a line each
;	pinned		what every member holds all of, as far as is known here
;	paths		the path map, a line an entry
;	journal		the paths set since the map was last written, see below
;	held		the blocks kept, each folder and the names in it, as the
;			store was closed: it is opened with this, not by reading
;			every folder, and the file goes as soon as a block is put
;			or let go of, so it is never other than what is kept
;	blocks/wN/name	a block, its name in hex, in the folder blocks were
;			put in as it was, so many to a folder: a run of blocks
;			put is a write of one directory, not one each, and a
;			flash stick is slow at the many
;	blocks/xx/y	a block, its name in hex, xx the first two of it: of a
;			store of before there were wN, read still
;
; Nothing here calls itself, and nothing here knows of mail or of another
; member: service/storage/ is that.
;
; A task gives way to the others of its node only when it says so, and a
; store does a great deal between one mail and the next: every block that is
; put, had or looked over, and every entry that is met, gives way first,
; (task-breathe). Without it a store at work held its node for seconds, and
; whatever else was on the node was late.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././fs/exfat.inc")
(import "././crypto/sha256.inc")

;module
(env-push)

(defq +store_magic "CHRYSA_STORE 1" +store_object_magic "CHRYSA_OBJECT 1" +store_tree_magic "CHRYSA_TREE 1"
	;how many names a block of a manifest has at most, 65 bytes each, so
	;it is no bigger than a block of the object
	+store_manifest_names 1000
	;how many are in a bucket of a map of a store, of its blocks or paths
	+store_per_bucket 32
	;the first line of the file of the blocks kept, (store-held-save)
	+store_held_magic "CHRYSA_HELD 1"
	+store_block_size 65536 +store_copies 3 +store_lf "\n"
	;how many blocks are put in a folder before the next is begun: the
	;directory of so many is a cluster of a volume
	+store_folder_blocks 256
	;how long the journal of the path map is at most, it is no more than
	;a 64th of the volume. 0 and there is none, and every change of a
	;path writes the whole map as it did
	+store_journal_size 1048576)

(enums +store_entry 0
	(enum path hash bytes vv writer seen deleted copies))

;what a member is to keep, kept till what it was worked out of changes:
;of who, all it is to keep, the blocks that are others' with who they
;are, the manifests it has not got with the paths that are them, what
;it wants, what is others', and who as a list
(enums +plan 0
	(enum key wanted others unknown missing extras among))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the counts of who has written a path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun store-vv-bump (vv member)
	; (store-vv-bump vv member) -> vv
	;one more write by a member. A vv is ((member count) ...), in the
	;order of the members' ids, so two that say the same are the same
	(defq out (list) done :nil)
	(each (lambda ((who count))
		(cond
			((eql who member) (push out (list who (inc count))) (setq done :t))
			((push out (list who count)))))
		vv)
	(unless done (push out (list member 1)))
	(sort out (# (cmp (first %0) (first %1)))))

(defun store-vv-count (vv member)
	; (store-vv-count vv member) -> num
	(ifn (some (lambda ((who count)) (if (eql who member) count)) vv) 0))

(defun store-vv-cmp (a b)
	; (store-vv-cmp a b) -> :same | :after | :before | :apart
	;a against b: after it, it has as many or more of every member's
	;writes, before it, or apart, each has some the other has not
	(defq more :nil less :nil)
	(each (lambda ((who count))
		(defq other (store-vv-count b who))
		(cond ((> count other) (setq more :t)) ((< count other) (setq less :t))))
		a)
	(each (lambda ((who count))
		(if (< (store-vv-count a who) count) (setq less :t)))
		b)
	(cond
		((and more less) :apart)
		(more :after)
		(less :before)
		(:same)))

(defun store-vv-merge (a b)
	; (store-vv-merge a b) -> vv
	;the most of each member's writes that either has
	(defq out (map (lambda ((who count)) (list who (max count (store-vv-count b who)))) a))
	(each (lambda ((who count))
		(unless (some (lambda ((had &ignore)) (eql had who)) a) (push out (list who count))))
		b)
	(sort out (# (cmp (first %0) (first %1)))))

(defun store-vv-total (vv)
	; (store-vv-total vv) -> num
	;how many times the path has been written, as far as it knows
	(reduce (lambda (sum (who count)) (+ sum count)) vv 0))

(defun store-vv-text (vv)
	(if (empty? vv) "-" (join (map (lambda ((who count)) (cat who ":" (str count))) vv) ",")))

(defun store-vv-read (text)
	(if (eql text "-") (list)
		(map (lambda (part) (defq at (find ":" part))
			(list (slice part 0 at) (str-as-num (slice part (inc at) -1))))
			(split text ","))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the files that say what it is
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun store-lines (text)
	; (store-lines text) -> (line ...)
	(filter (const nempty?) (split (ifn text "") +store_lf)))

(defun store-text (lines)
	; (store-text lines) -> text
	(apply (const cat) (cat (list "") (map (# (cat %0 +store_lf)) lines))))

(defun store-head-save (store)
	(exfat-save (get :vol store) "store" (store-text (list +store_magic
		(cat "store " (get :id store))
		(cat "member " (get :member store))
		(cat "block " (str (get :block_size store)))
		(cat "copies " (str (get :copies store)))))))

(defun store-entry-line (entry)
	;the path is last, it may have spaces in it
	;how many copies, if it is said, is after the deleted, 0/5: an entry
	;written before there was such a thing reads as not said
	(bind '(name hash size vv writer seen deleted copies) entry)
	(join (list (if (eql hash "") "-" (hex-encode hash)) (str size) (store-vv-text vv) writer
		(if (empty? seen) "-" (join seen ",")) (cat (if deleted "1" "0") (if (> copies 0) (cat "/" (str copies)) "")) name) " "))

(defun store-entry-read (line)
	(defq parts (split line " "))
	(when (>= (length parts) 7)
		(bind '(hash size vv writer seen deleted) (slice parts 0 6))
		(list (join (slice parts 6 -1) " ") (if (eql hash "-") "" (hex-decode hash)) (str-as-num size)
			(store-vv-read vv) writer (if (eql seen "-") (list) (split seen ",")) (starts-with "1" deleted)
			(if (defq at (find "/" deleted)) (str-as-num (slice deleted (inc at) -1)) 0))))

;What each member has pinned is known to every member: it goes with the
;path map when two compare notes. A gopher is sent to a member that holds
;all of an object, and has nobody to ask who that is. A member's line is
;its own to write, with a count it adds to each time, so of two that are
;had for a member the one of the greater count is the later, and there is
;nothing to choose between.

(defun store-pinned-lines (store)
	; (store-pinned-lines store) -> (line ...)
	;what every member has pinned, as lines, member count prefix: a line a
	;prefix, the prefix last, it may have spaces in it. A member that has
	;none pinned now is a line with no prefix, so that is known too
	(defq lines (list))
	(. (get :pinned store) :each (lambda (member row)
		(defq head (cat member " " (str (first row))))
		(if (= (length row) 1) (push lines head)
			(each! (# (push lines (cat head " " %0))) (list row) 1 -1))))
	(sort lines))

(defun store-pinned-read (store lines)
	; (store-pinned-read store lines) -> :nil | :t
	;lines of (store-pinned-lines), as another member has them or as they
	;were kept: of each member, what is of a greater count than is had
	;here is taken. If it is of this member, its pins are that now: an
	;image put back from an older copy. Was anything new
	(defq rows (Fmap 11) new :nil me (get :member store))
	(each (lambda (line)
		(defq parts (split line " "))
		(when (>= (length parts) 2)
			(defq member (first parts) count (str-as-num (second parts))
				row (ifn (. rows :find member) (list count)))
			(if (> count (first row)) (setq row (list count)))
			(and (= count (first row)) (> (length parts) 2) (push row (join (slice parts 2 -1) " ")))
			(. rows :insert member row)))
		lines)
	(. rows :each (lambda (member row)
		(defq was (. (get :pinned store) :find member))
		(when (or (not was) (> (first row) (first was)))
			(. (get :pinned store) :insert member (cat (list (first row)) (sort (rest row))))
			(when (eql member me) (def store :pins (sort (rest row))) (def store :plan :nil))
			(setq new :t))))
	new)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the path map, as it is held
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun store-buckets (count least)
	; (store-buckets count least) -> num
	;how many buckets a map of a store has for so many in it: one for every
	;+store_per_bucket. A find of a str whose hash it has already is a look
	;through its bucket, and so many is no slower than one a bucket, timed
	(max least (/ count +store_per_bucket)))

(defun store-grow (table count)
	; (store-grow table count) -> table
	;a map of a store made twice as big when it has more than so many to a
	;bucket, as the store grows: once in a while, not at every insert
	(defq buckets (get :num_buckets table))
	(if (> count (* buckets +store_per_bucket)) (. table :resize (* buckets 2)))
	table)

(defun store-span (store)
	; (store-span store) -> num
	;how many blocks the objects of a store are made of at most, how long
	;they are in blocks, or how many it keeps if more: what a map of its
	;blocks is to be big enough for
	(defq span_blocks 0 span_size (get :block_size store))
	(. (get :map store) :each (lambda (path entry)
		(unless (elem-get entry +store_entry_deleted)
			(setq span_blocks (+ span_blocks 1 (/ (elem-get entry +store_entry_bytes) span_size))))))
	(max span_blocks (get :blocks store)))

(defun store-map-insert (store name entry)
	;an entry goes in the path map, in place of what that path was. How
	;many objects there are is kept as it goes, it is asked with every
	;put, and to count them was a walk of the map each time
	(defq was (. (get :map store) :find name))
	(def store :objects (+ (get :objects store)
		(if (elem-get entry +store_entry_deleted) 0 1)
		(if (and was (not (elem-get was +store_entry_deleted))) -1 0)))
	;what this member's share is, is to be worked out again
	(def store :plan :nil)
	(unless was (def store :paths (inc (get :paths store))))
	(. (store-grow (get :map store) (get :paths store)) :insert name entry))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the journal of the path map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The path map is written whole, and at 2,000 paths that is half a
; megabyte, for a path set. So a path that is set here is not written with
; the map: its entry is added to the end of a file, the journal, and the
; map is as it was on the volume. When the store is opened the map is read
; and then the journal, an entry at a time, and each that is later than
; what the map has takes its place.
;
; The journal is a file of a set length, made once, and written where it
; is: nothing of the volume but its own bytes changes when it is added
; to. It begins with a line that is its count, a number. An entry is a
; line of it: 8 hex digits, a space, and the line of the entry as the map
; has it. The digits are the first of the SHA-256 of the count, a space
; and that line, so a line that was half written when the power went, and
; a line left from before the count was last changed, are not taken, and
; the reading stops there.
;
; When the map is written whole, (store-save), for the journal is full or
; for anything else, the count is one more and the journal is that one
; line: all that was in it is in the map. If that is not reached, the map
; written and the journal as it was, what is in the journal is no later
; than the map, and none of it is taken.
;
; What is taken from another member, (store-meet), goes the same way: a
; member that is told of the paths the others set would else write its
; whole map for each telling.
;
; +store_journal_size is 0 and none of this is done. A store that has no
; journal file is given one when it is opened; one that has is read
; whether or not it is still written to.

(defun store-find (store name)
	; (store-find store path) -> :nil | entry
	;what a path is now, one that is deleted too
	(. (get :map store) :find name))

(defun store-list (store &optional prefix deleted)
	; (store-list store [prefix deleted]) -> (entry ...)
	;the paths that begin so, in order, with those deleted if asked
	(setd prefix "")
	(defq out (list))
	(. (get :map store) :each (lambda (name entry)
		(and (starts-with prefix name) (or deleted (not (elem-get entry +store_entry_deleted)))
			(push out entry))))
	(sort out (# (cmp (first %0) (first %1)))))

(defun store-entries (store)
	; (store-entries store) -> (entry ...)
	;every entry of the path map, those deleted too, what is told to
	;another member
	(store-list store "" :t))

(defun store-journal-mark (count line)
	(cat (slice (hex-encode (sha256 (cat (str count) " " line))) 0 8) " " line +store_lf))

(defun store-journal-write (store at data)
	;bytes of the journal, where they are on the volume
	(defq vol (get :vol store) clusters (get :journal store) size (exfat-cluster-size vol) from 0)
	(while (< from (length data))
		(defq in (% at size) some (min (- (length data) from) (- size in)))
		(exfat-write vol (+ (exfat-cluster-offset vol (elem-get clusters (/ at size))) in)
			(slice data from (+ from some)))
		(setq from (+ from some) at (+ at some))))

(defun store-journal-reset (store)
	;it has nothing in it, as it does when the map has just been written
	(when (def? :journal store)
		(def store :journal_count (inc (get :journal_count store)))
		(defq head (cat (str (get :journal_count store)) +store_lf))
		(store-journal-write store 0 head)
		(def store :journal_at (length head))))

(defun store-journal-add (store entries)
	; (store-journal-add store entries) -> :nil | :t
	;entries of the path map are on the volume, at the end of the journal,
	;as the one write. :nil if there is no journal or no room in it for
	;them all, and the map is to be written
	(when (def? :journal store)
		(defq count (get :journal_count store) at (get :journal_at store)
			lines (apply (const cat) (cat (list "") (map (# (store-journal-mark count (store-entry-line %0))) entries))))
		(when (<= (+ at (length lines)) (get :journal_size store))
			(store-journal-write store at lines)
			(exfat-sync (get :vol store))
			(def store :journal_at (+ at (length lines)) :digest :nil)
			:t)))

(defun store-journal-open (store)
	;the journal of a store that has just had its map read: what it has
	;that is later than the map is taken, and it is added to from there.
	;One that has none is given one. How many entries were taken
	(defq vol (get :vol store) took 0
		want (min +store_journal_size (>> (* (get :cluster_count vol) (exfat-cluster-size vol)) 6)))
	(and (not (exfat-find vol "journal")) (> want 2)
		(progn (exfat-begin vol)
			(exfat-save vol "journal" (cat "0" +store_lf (exfat-zeros (- want 2))))
			(exfat-end vol)))
	(when (defq found (exfat-find vol "journal"))
		(bind '(& & size start no_fat &rest _) found)
		(defq text (exfat-data vol start size no_fat) end (ifn (find +store_lf text) 0)
			count (str-as-num (cat "0" (slice text 0 end))) at (min size (inc end)) more :t)
		(while (and more (< at size) (defq end (find +store_lf text at)))
			(defq line (slice text (+ at 9) end))
			(cond
				((and (> (- end at) 9) (eql (slice text at (inc end)) (store-journal-mark count line))
						(defq entry (store-entry-read line)))
					(defq was (store-find store (first entry)))
					(when (or (not was) (eql :after (store-vv-cmp (elem-get entry +store_entry_vv) (elem-get was +store_entry_vv))))
						(store-map-insert store (first entry) entry)
						(++ took))
					(setq at (inc end)))
				((setq more :nil))))
		(when (> +store_journal_size 0)
			(def store :journal (exfat-chain vol start size no_fat) :journal_size size
				:journal_count count :journal_at at)))
	took)

(defun store-save (store)
	; (store-save store) -> store
	;the path map, the members and the pins, as they are now. Whatever
	;changes any of them ends here, so what was worked out from them, the
	;digest, is not held any longer
	(task-breathe)
	(def store :digest :nil :dirty :nil)
	(defq vol (get :vol store) lines (list))
	(. (get :map store) :each (lambda (name entry) (push lines (store-entry-line entry))))
	(exfat-begin vol)
	(exfat-save vol "paths" (store-text (sort lines)))
	(exfat-save vol "members" (store-text (get :members store)))
	(exfat-save vol "retired" (store-text (get :retired store)))
	(exfat-save vol "pins" (store-text (get :pins store)))
	(exfat-save vol "pinned" (store-text (store-pinned-lines store)))
	;all that the journal has is in the map now
	(store-journal-reset store)
	(exfat-end vol)
	store)

(defun store-count (store)
	;a store counts the changes to its path map from when it was opened,
	;and has for each path the count at its last change, so that another
	;member can be told only what is new since it last asked,
	;(store-since). The counts are of this opening of it, which has a
	;name, :epoch: one who knew the counts of another is told everything
	(def store :rev 0 :revs (Fmap (store-buckets (get :paths store) 101)) :dirty :nil
		:epoch (hex-encode (slice (sha256 (cat (str (pii-time)) (get :member store) (str (random 1000000)))) 0 8))))

(defun store-touch (store name)
	;a path is not as it was
	(def store :rev (inc (get :rev store)))
	(. (store-grow (get :revs store) (get :paths store)) :insert name (get :rev store)))

(defun store-later (store)
	;the path map or the members are not as they were, and it is not
	;written yet: what was learnt from another member can be learnt
	;again, so whoever keeps the store writes it when it has a moment,
	;(store-save), and till then :dirty says there is something to write
	(def store :digest :nil :dirty (ifn (get :dirty store) (pii-time))))

(defun store-at (store)
	; (store-at store) -> str
	;where its count of changes is now, to give back to (store-since)
	(cat (get :epoch store) " " (str (get :rev store))))

(defun store-since (store &optional at)
	; (store-since store [at]) -> (entry ...)
	;the entries of the path map, those deleted too, that are not as they
	;were at that count, as (store-at) gave it. All of them if it is not
	;given, or is of another opening of the store
	(defq parts (split (ifn at "") " ") revs (get :revs store) out (list))
	(cond
		((and (= (length parts) 2) (eql (first parts) (get :epoch store)))
			(defq rev (str-as-num (second parts)))
			(. (get :map store) :each (lambda (name entry)
				(if (> (ifn (. revs :find name) 0) rev) (push out entry))))
			(sort out (# (cmp (first %0) (first %1)))))
		((store-entries store))))

(defun store-cluster-shift (size block_size)
	; (store-cluster-shift size block_size) -> num
	;the clusters of a store's volume no bigger than its blocks: a block is
	;a file, and takes a cluster at least, so of 64KB blocks on clusters
	;of 128KB, those of a volume over 32GB, half of it was never used. No
	;smaller than exFAT's own for its size, nor than 4096 bytes
	(defq shift 0)
	(while (< (<< 512 (inc shift)) (inc block_size)) (++ shift))
	(max 3 (min (exfat-cluster-shift size) shift)))

(defun store-make (device size id member &optional block_size label copies)
	; (store-make path | stream size id member [block_size label copies]) -> :nil | :t
	;a new store with nothing in it, on a new volume of that many bytes.
	;id is the store's, any str with no space in it, member the id of the
	;member whose it is, the system id of its machine in hex. A path is a
	;new file of the host, a stream is written where it is and must be
	;that long. copies is how many members keep a block, 3
	(when (and (exfat-format device size (ifn label "STORE") (store-cluster-shift size (ifn block_size +store_block_size)))
			(defq vol (exfat-mount device)))
		(def (defq store (env 1)) :vol vol :id id :member member
			:block_size (ifn block_size +store_block_size) :copies (ifn copies +store_copies)
			:map (Fmap 11) :objects 0 :paths 0 :members (list member) :retired (list) :pins (list) :pinned (Fmap 11) :held (Fmap 11) :held_saved :nil :blocks 0 :manifests (Fmap 11)
			:folders (Fset 11) :folder_next 0 :folder_count 0)
		(store-count store)
		(exfat-begin vol)
		(store-head-save store)
		(exfat-mkdir vol "blocks")
		(exfat-end vol)
		(store-journal-open store)
		(store-save store)
		:t))

(defun store-blocks-found (store &optional going)
	; (store-blocks-found store [going]) -> :nil | ((name folder) ...)
	;every block that is kept here, whatever is in it, as the volume has
	;it, and the folder it is in. With going, it is asked before each
	;folder is read, the long part of a big store's open, and if it says
	;:nil the walk is given up, :nil. A block is in a folder of blocks/,
	;so the folders are listed one after another, no walk of a tree: the
	;blocks of one share the one string of its path
	(defq vol (get :vol store) out (list))
	(some (lambda (entry)
			(cond
				((not (second entry)) :nil)
				((and going (not (going))) (setq out :nil) :t)
				(:t (defq folder (first entry) path (cat "blocks/" folder)
						prefix (if (starts-with "w" folder) "" folder))
					(each (lambda (file)
							(unless (second file) (push out (list (hex-decode (cat prefix (first file))) path))))
						(exfat-dir vol entry))
					:nil)))
		(ifn (exfat-list vol "blocks") '()))
	out)

(defun store-held-save (store)
	; (store-held-save store) -> store
	;the names of the blocks kept here, and their folders, as the file
	;held: what a store is opened with next time, in place of reading
	;every folder of blocks/, which of a million blocks took minutes. Each
	;folder, how many, then their names as they are, 32 bytes each; and at
	;the end how many in all, so that a file not all written is not taken
	(defq groups (Fmap 101) parts (list +store_held_magic +store_lf) total 0)
	(. (get :held store) :each (lambda (name folder)
		(unless (defq names (. groups :find folder)) (. groups :insert folder (setq names (list))))
		(push names name)))
	(. groups :each (lambda (folder names)
		(push parts folder " " (str (length names)) +store_lf)
		(each (# (push parts %0)) names)
		(setq total (+ total (length names)))))
	(push parts "end " (str total) +store_lf)
	(exfat-save (get :vol store) "held" (apply (const cat) parts))
	(def store :held_saved :t)
	store)

(defun store-held-read (store)
	; (store-held-read store) -> :nil | ((folder names) ...)
	;the blocks kept here as the file held has them, (store-held-save). :nil
	;if there is none, or it is not whole
	(when (and (defq data (exfat-load (get :vol store) "held"))
			(starts-with (cat +store_held_magic +store_lf) data))
		(defq at (inc (length +store_held_magic)) out (list) total 0 whole :nil)
		(while (and (not whole) (< at (length data)) (defq end (find +store_lf data at)))
			(bind '(folder count) (split (slice data at end) " "))
			(setq count (str-as-num (ifn count "-1")) at (inc end))
			(cond
				((eql folder "end") (setq whole (if (= count total) :t :bad)))
				((or (< count 0) (> (+ at (* count 32)) (length data))) (setq whole :bad))
				(:t (push out (list folder (map! (# (slice data %0 (+ %0 32))) (list (range at (+ at (* count 32)) 32)))))
					(setq at (+ at (* count 32)) total (+ total count)))))
		(if (eql whole :t) out)))

(defun store-held-stale (store)
	; (store-held-stale store) -> store
	;what is kept here is about to change: the file held is no longer
	;what is kept, and goes, before the change is made
	(when (get :held_saved store)
		(def store :held_saved :nil)
		(exfat-delete (get :vol store) "held"))
	store)

(defun store-blocks (store)
	; (store-blocks store) -> (name ...)
	;every block that is kept here, whatever is in it, as the volume has it
	(map (const first) (store-blocks-found store)))

(defun store-open (device member &optional going)
	; (store-open path | stream member [going]) -> :nil | :stopped | store
	;a store that is there. :nil if the volume is not one. It is opened by
	;the member it was made for, or last opened by. Opened by another, a
	;machine given a new id, a disk moved to another machine, that member
	;takes the place of the one it was, in the members too: there is not
	;one more member, and (get :was store) is who it was. going is as
	;(store-blocks-found) has it: an open given up is :stopped, and has
	;written nothing but a journal where there was none
	(when (and (defq vol (exfat-mount device))
			(defq head (store-lines (exfat-load vol "store")))
			(nempty? head) (eql (first head) +store_magic))
		(def (defq store (env 1)) :vol vol :id "" :member member :was :nil
			:block_size +store_block_size :copies +store_copies
			:map (Fmap 101) :objects 0 :paths 0 :members (list) :retired (list) :pins (list) :pinned (Fmap 11)
			:folders (Fset 101) :folder_next 0 :folder_count 0)
		(defq had "")
		(each! (lambda (line)
			(defq at (find " " line) key (slice line 0 at) val (slice line (inc at) -1))
			(cond
				((eql key "store") (def store :id val))
				((eql key "member") (setq had val))
				((eql key "block") (def store :block_size (str-as-num val)))
				((eql key "copies") (def store :copies (str-as-num val))))) (list head) 1 -1)
		(def store :members (store-lines (exfat-load vol "members"))
			:retired (store-lines (exfat-load vol "retired"))
			:pins (store-lines (exfat-load vol "pins")))
		(store-pinned-read store (store-lines (exfat-load vol "pinned")))
		;a store of before there was such a thing has pins and has not said so
		(and (nempty? (get :pins store)) (not (. (get :pinned store) :find (get :member store)))
			(. (get :pinned store) :insert (get :member store) (cat (list 1) (get :pins store))))
		(each (lambda (line)
			(if (defq entry (store-entry-read line))
				(store-map-insert store (first entry) entry)))
			(store-lines (exfat-load vol "paths")))
		(store-journal-open store)
		(store-count store)
		;the names of the blocks it has, and where each is. Blocks are put
		;in the last folder of them, the highest wN, while it has room
		(def store :held (Fmap 1021) :blocks 0 :manifests (Fmap (store-buckets (get :objects store) 101)) :plan :nil :held_saved :nil)
		;asked first whatever way the blocks are had: an open told to stop
		;is given up, a quick one too
		(defq high -1 in_last 0 groups (if (or (not going) (going)) (store-held-read store)) walked (not groups))
		;of a store not closed as it should be, or of before there was the
		;file held, every folder of blocks/ is read, and the file written
		(when (and walked (or (not going) (going)))
			(defq found (store-blocks-found store going) at "")
			(setq groups (if found (list)))
			(each (lambda ((name folder))
					(unless (eql folder at) (push groups (list (setq at folder) (list))))
					(push (second (last groups)) name))
				(ifn found '())))
		(cond
			((not groups) :stopped)
			(:t
				;a bucket of the map for every so many blocks, a store has a
				;million of them, and the number of a folder once for all
				;its blocks, which come together
				(defq held (. (get :held store) :resize (store-buckets (reduce (# (+ %0 (length (second %1)))) groups 0) 1021)))
				(each (lambda ((folder names))
						(each (# (. held :insert %0 folder)) names)
						(defq n (if (starts-with "blocks/w" folder) (str-as-num (slice folder (const (length "blocks/w")) -1)) -1))
						(when (>= n 0)
							(cond
								((> n high) (setq high n in_last (length names)))
								((= n high) (setq in_last (+ in_last (length names)))))))
					groups)
				(def store :folder_next (max 0 high) :folder_count in_last :blocks (. held :size) :held_saved (not walked))
				(when walked
					(exfat-begin vol)
					(store-held-save store)
					(exfat-end vol))
				(unless (eql had member)
					;it is this member's now
					(def store :was had
						:members (map (# (if (eql %0 had) member %0)) (get :members store)))
					(unless (find member (get :members store)) (push (get :members store) member))
					(exfat-begin vol)
					(store-head-save store)
					(exfat-end vol)
					(store-save store))
				store))))

(defun store-close (store)
	; (store-close store) -> :nil
	;all that it has is on the device
	(if (get :dirty store) (store-save store))
	(unless (get :held_saved store)
		(exfat-begin (get :vol store))
		(store-held-save store)
		(exfat-end (get :vol store)))
	(exfat-sync (get :vol store))
	:nil)

(defun store-resize (store device size)
	; (store-resize store path size) -> :nil | store
	;the store moved to a new volume of that many bytes, made there: every
	;file of this one copied to it as it is, the blocks above all, and
	;then the path map, the members and the pins written there as the
	;store has them, which a store that was full may not have had room to
	;write, and a journal of the new volume's own size. A copy on the one
	;machine, no member asked for anything: bigger, or smaller if what
	;there is fits. :nil if it can not be made or does not fit, and the
	;store is on the volume it was on, as it was. The old volume is left
	;as it was
	;the names the function given to (exfat-walk) uses are not names it
	;has, vol and path, it is called from inside it and would have those
	(store-close store)
	(defq resize_from (get :vol store) id (get :id store) resize_ok :nil)
	(when (and (exfat-format device size (slice id 0 (min 11 (length id))) (store-cluster-shift size (get :block_size store)))
			(defq resize_to (exfat-mount device)))
		(setq resize_ok :t)
		(exfat-begin resize_to)
		(exfat-walk resize_from "" (lambda (resize_path resize_entry)
			(task-breathe)
			(defq resize_at (if (starts-with "/" resize_path) (slice resize_path 1 -1) resize_path))
			(and resize_ok (nql resize_at "journal")
				(setq resize_ok (if (second resize_entry)
					(or (exfat-find resize_to resize_at) (exfat-mkdir resize_to resize_at))
					(exfat-save resize_to resize_at (exfat-load resize_from resize_at)))))))
		(exfat-end resize_to))
	(when resize_ok
		(def store :vol resize_to)
		(store-journal-open store)
		(store-save store)
		(exfat-sync resize_to)
		store))

(defun store-folder (store)
	; (store-folder store) -> path
	;the folder blocks are put in now, the next once one has so many
	(when (>= (get :folder_count store) +store_folder_blocks)
		(def store :folder_next (inc (get :folder_next store)) :folder_count 0))
	(cat "blocks/w" (str (get :folder_next store))))

(defun store-block-file (store name)
	; (store-block-file store name) -> path
	;where on the volume a block of that name is kept, or is to be put
	(defq hex (hex-encode name) folder (. (get :held store) :find name))
	(cond
		((not folder) (cat (store-folder store) "/" hex))
		((starts-with "blocks/w" folder) (cat folder "/" hex))
		((cat folder "/" (slice hex 2 -1)))))

(defun store-block-has? (store name)
	; (store-block-has? store name) -> :nil | :t
	;is a block of that name kept here. The names of what is kept are
	;held, read from the volume once as it is opened: to look one up on
	;the volume is to read two directories of it
	(if (. (get :held store) :find name) :t))

(defun store-block-get (store name)
	; (store-block-get store name) -> :nil | data
	;the block of that name. :nil if it is not kept here, and :nil if what
	;is kept is not it: it has gone wrong, and is no use to anyone
	;one that is not kept is known not to be, with no look at the volume:
	;a member is asked for many it has not got, by those looking for one
	(task-breathe)
	(and (store-block-has? store name)
		(defq found (exfat-load (get :vol store) (store-block-file store name)))
		(eql (sha256 found) name) found))

(defun store-manifest-text (size names)
	; (store-manifest-text size names) -> data
	;what is kept for an object: how long it is, and its blocks in order
	(store-text (cat (list +store_object_magic (str size)) (map (const hex-encode) names))))

(defun store-tree-text (size height names)
	; (store-tree-text size height names) -> data
	;a block of the manifest of a big object, over the blocks of its
	;manifest that are one lower, the lowest of them over its blocks
	(store-text (cat (list +store_tree_magic (str size) (str height)) (map (const hex-encode) names))))

(defun store-manifest-read (data)
	; (store-manifest-read data) -> :nil | (size height names)
	;a block of a manifest. Of height 0 its names are of the object's
	;blocks, of more they are of blocks of the manifest one lower
	(defq lines (store-lines data))
	(cond
		((and (> (length lines) 1) (eql (first lines) +store_object_magic))
			(list (str-as-num (second lines)) 0 (map! (const hex-decode) (list lines) 2 -1)))
		((and (> (length lines) 2) (eql (first lines) +store_tree_magic))
			(list (str-as-num (second lines)) (str-as-num (third lines)) (map! (const hex-decode) (list lines) 3 -1)))))

(defun store-manifest-make (size names put)
	; (store-manifest-make size names put) -> :nil | hash
	; (put data) -> :nil | hash
	;the manifest of an object of these blocks, each block of it given to
	;put, which keeps it and says its name: the name of its top. One of no
	;more than +store_manifest_names blocks is the one block, as it always
	;was. A bigger one is a tree of them, no block of it bigger than a
	;block of the object, so no message is ever the whole of it. Only the
	;top says how long the object is. :nil if put could not keep one
	(defq height 0 level names)
	(while (and level (> (length level) +store_manifest_names))
		(defq up (list) at 0)
		(while (and up (< at (length level)))
			(defq part (slice level at (min (length level) (+ at +store_manifest_names)))
				hash (put (if (= height 0) (store-manifest-text 0 part) (store-tree-text 0 height part))))
			(setq up (if hash (push up hash)) at (+ at +store_manifest_names)))
		(setq level up height (inc height)))
	(and level (put (if (= height 0) (store-manifest-text size level) (store-tree-text size height level)))))

(defun store-manifest-walk (hash fetch &optional parts)
	; (store-manifest-walk hash fetch [parts]) -> :nil | (size names tree)
	; (fetch name) -> :nil | data
	;the object a manifest is of, each block of the manifest had by fetch:
	;how long it is, the names of its blocks in order, and the names of
	;the blocks of its manifest, the top first. :nil if a block of the
	;manifest can not be had. parts, if given, a list of two lists, is
	;given the names of the blocks of the manifest had and those not
	(defq level (list hash) size :nil names (list) tree (list) whole :t)
	(while level
		(defq lower (list))
		(each (lambda (name)
				(cond
					((and (defq data (fetch name)) (defq block (store-manifest-read data)))
						(push tree name)
						(unless size (setq size (first block)))
						(each (# (push (if (= (second block) 0) names lower) %0)) (third block)))
					(:t (setq whole :nil)
						(if parts (push (second parts) name)))))
			level)
		(setq level (if (nempty? lower) lower)))
	(if parts (each (# (push (first parts) %0)) tree))
	(if whole (list size names tree)))

(defun store-manifest (store hash &optional parts)
	; (store-manifest store hash [parts]) -> :nil | (size names tree)
	;the object of that name, if all its manifest is kept here, as
	;(store-manifest-walk). One that has been read is held: a name is of
	;the one thing for ever, and what an object is made of is asked again
	;and again
	;one that is held is so while its blocks are all still kept here
	(defq known (get :manifests store))
	(cond
		((and (defq object (. known :find hash)) (every (# (store-block-has? store %0)) (third object)))
			(if parts (each (# (push (first parts) %0)) (third object)))
			object)
		((defq object (store-manifest-walk hash (# (store-block-get store %0)) parts))
			(. (store-grow known (get :objects store)) :insert hash object)
			object)))

(defun store-members (store)
	; (store-members store) -> (member ...)
	;who the members are meant to be, in order: every one that was ever
	;joined and has not been retired, away or not
	(sort (filter (# (not (find %0 (get :retired store)))) (get :members store))))

(defun store-rank (name among copies)
	; (store-rank name among copies) -> (member ...)
	;of those members, the ones that are to keep a block of that name, as
	;many as there are to be copies. It is worked out of the name and who
	;they are and nothing else: anything that knows the two knows who
	;keeps a block, a gopher too, and has nobody to ask.
	;
	;The members are put in an order that is the block's own, and the
	;first so many keep it. So to ask for more copies adds members to
	;those that keep it and moves it from none, and to ask for fewer
	;takes the last of them away
	(defq scored (sort (map (# (list (sha256 (cat name %0)) %0)) among)
		(# (cmp (first %1) (first %0)))))
	(map! (const second) (list scored) 0 (min copies (length scored))))

(defun store-holders (store name &optional among copies)
	; (store-holders store name [among copies]) -> (member ...)
	;the members that are to keep a block of that name, as many as there
	;are to be copies, the store's if it is not said, of the members, or
	;of those given, the ones that are there. Each member is given a
	;number made from its id and the name, and those with the greatest
	;keep it: so every member works out the same, nobody has to be asked,
	;and when a member comes or goes only the blocks that are its share
	;move
	;
	;What was worked out is kept, the members in the block's order, for
	;as long as it is asked of the same members: a member asks it of
	;every block it knows of each time it looks to its share, and it is a
	;hash and a sort for each
	(defq among (ifn among (store-members store)) key (join among " ") kept (def? :holders store)
		copies (if (and copies (> copies 0)) copies (get :copies store)))
	(unless (and kept (eql (first kept) key))
		(def store :holders (setq kept (list key (Fmap (store-buckets (store-span store) 101))))))
	(unless (defq order (. (second kept) :find name))
		(. (second kept) :insert name (setq order (store-rank name among (length among)))))
	(slice order 0 (min copies (length order))))

(defun store-pinned? (store name)
	; (store-pinned? store path) -> :nil | :t
	(if (some (# (starts-with %0 name)) (get :pins store)) :t))

(defun store-plan-object (store plan path entry)
	;the object of a path, into what is wanted and what is others'. If
	;its manifest is not here, what it is made of is known when it comes
	(defq hash (elem-get entry +store_entry_hash) me (get :member store) wanted (elem-get plan +plan_wanted)
		others (elem-get plan +plan_others) among (elem-get plan +plan_among)
		unknown (elem-get plan +plan_unknown) parts (list (list) (list)))
	;every block of a manifest is wanted, a big one's are a tree of them
	(. wanted :insert hash)
	(defq object (store-manifest store hash parts))
	(each (# (. wanted :insert %0)) (cat (first parts) (second parts)))
	(each (# (. (elem-get plan +plan_missing) :insert %0)) (second parts))
	(cond
		(object
			(defq pinned (store-pinned? store path) copies (elem-get entry +store_entry_copies))
			(each (lambda (name)
				(defq holders (store-holders store name among copies))
				(cond
					((or pinned (find me holders)) (. wanted :insert name))
					;as another object has it, it may have more keepers
					((> (length holders) (length (ifn (. others :find name) '()))) (. others :insert name holders))))
				(second object)))
		(:t ;what it is made of is known when the blocks of its manifest
			;that are not here come
			(each (lambda (name)
					(defq paths (ifn (. unknown :find name) (list)))
					(unless (find path paths) (. unknown :insert name (cat paths (list path)))))
				(second parts)))))

(defun store-plan (store among)
	; (store-plan store among) -> plan
	;what this member is to keep among those members, as it is now
	(defq key (join among " ") plan (get :plan store))
	(unless (and plan (eql (elem-get plan +plan_key) key))
		(defq span (store-span store))
		(def store :plan (setq plan (list key (Fset (store-buckets span 101)) (Fmap (store-buckets span 101)) (Fmap 11)
			(Fset (store-buckets span 101)) (Fset (store-buckets span 101)) among)))
		(. (get :map store) :each (lambda (path entry)
			(unless (elem-get entry +store_entry_deleted)
				(task-breathe)
				(store-plan-object store plan path entry))))
		(defq wanted (elem-get plan +plan_wanted) missing (elem-get plan +plan_missing) extras (elem-get plan +plan_extras))
		(. wanted :each (lambda (name) (unless (store-block-has? store name) (. missing :insert name))))
		;a block that is another's to keep for one object may be mine for another
		(. (elem-get plan +plan_others) :each (lambda (name holders)
			(and (not (. wanted :find name)) (store-block-has? store name) (. extras :insert name)))))
	plan)

(defun store-plan-held (store name)
	;a block that is kept now
	(when (defq plan (get :plan store))
		(. (elem-get plan +plan_missing) :erase name)
		(and (not (. (elem-get plan +plan_wanted) :find name)) (. (elem-get plan +plan_others) :find name)
			(. (elem-get plan +plan_extras) :insert name))
		;a block of a manifest: what its object is made of may now be known,
		;and of its blocks those this member is to keep and has not got are
		;wanted, and those it has and are others' are theirs. Of a tree, the
		;blocks of the manifest under this one are wanted now too
		(when (defq paths (. (elem-get plan +plan_unknown) :find name))
			(. (elem-get plan +plan_unknown) :erase name)
			(defq wanted (elem-get plan +plan_wanted) others (elem-get plan +plan_others))
			(each (lambda (path)
				(when (defq entry (. (get :map store) :find path))
					(store-plan-object store plan path entry)
					(each (lambda (block)
						(cond
							((. wanted :find block)
								(. (elem-get plan +plan_extras) :erase block)
								(unless (store-block-has? store block) (. (elem-get plan +plan_missing) :insert block)))
							((and (. others :find block) (store-block-has? store block))
								(. (elem-get plan +plan_extras) :insert block))))
						(ifn (second (store-manifest store (elem-get entry +store_entry_hash))) '()))))
				paths))))

(defun store-plan-dropped (store name)
	;a block that is no longer kept
	(when (defq plan (get :plan store))
		(. (elem-get plan +plan_extras) :erase name)
		(if (. (elem-get plan +plan_wanted) :find name) (. (elem-get plan +plan_missing) :insert name))))

(defun store-block-put (store data)
	; (store-block-put store data) -> :nil | name
	;a block of this data is kept, and its name. If it is kept already
	;there is no more to do. :nil if there is no room for it
	(task-breathe)
	(defq vol (get :vol store) name (sha256 data))
	(cond
		((store-block-has? store name) name)
		(:t (defq folder (store-folder store) file (cat folder "/" (hex-encode name)))
			(exfat-begin vol)
			;the folders of blocks are made as they are wanted and never go,
			;and to look for one was a read of the folder all the blocks are in
			(unless (. (get :folders store) :find folder)
				(unless (exfat-find vol folder) (exfat-mkdir vol folder))
				(. (get :folders store) :insert folder))
			(store-held-stale store)
			(defq kept (exfat-save vol file data))
			(exfat-end vol)
			(when kept
				(. (store-grow (get :held store) (inc (get :blocks store))) :insert name folder)
				(def store :folder_count (inc (get :folder_count store)) :blocks (inc (get :blocks store)))
				(store-plan-held store name)
				name))))

(defun store-block-drop (store name)
	; (store-block-drop store name) -> :nil | :t
	(defq file (store-block-file store name))
	(when (store-block-has? store name)
		(store-held-stale store)
		(def store :blocks (dec (get :blocks store))))
	(. (get :held store) :erase name)
	(. (get :manifests store) :erase name)
	(store-plan-dropped store name)
	(exfat-delete (get :vol store) file))

(defun store-held (store)
	; (store-held store) -> (name ...)
	;the names of the blocks kept here, as they are held: what
	;(store-block-has?) goes by. No reading of the volume
	(defq out (list))
	(. (get :held store) :each (lambda (name _) (push out name)))
	out)

;;;;;;;;;
; objects
;;;;;;;;;

(defun store-cut (store data)
	; (store-cut store data) -> (block ...)
	;an object as the blocks it is kept in, each the block size but the
	;last. One of nothing is no blocks
	(defq size (get :block_size store))
	(map (# (slice data %0 (min (length data) (+ %0 size)))) (range 0 (length data) size)))

(defun store-stat (store name)
	; (store-stat store path) -> :nil | entry
	;what a path is now, :nil if there is no object of that name
	(and (defq entry (store-find store name)) (not (elem-get entry +store_entry_deleted)) entry))

(defun store-now (store name)
	; (store-now store path) -> hash
	;the name of the object a path is now, "" if there is none: what a
	;writer says it is setting it from
	(if (defq entry (store-stat store name)) (elem-get entry +store_entry_hash) ""))

(defun store-set (store name hash size from deleted &optional copies)
	;the one way a path is changed here. from is what the writer says it
	;is now, :nil if it does not say. How many copies there are to be of
	;it is of the path, and stays as it is unless it is given
	(defq was (store-find store name) member (get :member store))
	(cond
		((and from (nql from (store-now store name))) (list :stale (store-stat store name)))
		(:t (defq entry (list name hash size
				(store-vv-bump (if was (elem-get was +store_entry_vv) (list)) member) member (list member) deleted
				(ifn copies (if was (elem-get was +store_entry_copies) 0))))
			(store-map-insert store name entry)
			(store-touch store name)
			;to the journal, or with no journal, or one that is full, the
			;whole map
			(unless (store-journal-add store (list entry)) (store-save store))
			(list :ok entry))))

(defun store-commit (store name names size &optional from)
	; (store-commit store path names size [from]) -> (:ok entry) | (:stale entry) | (:full :nil) | (:missing names)
	;a path is set to the object of these blocks, which are kept here
	;already, provided it is now the object from: "" for none, :nil not
	;to say. :stale with what it is if it is not. :missing with those of
	;the blocks that are not kept here
	(defq missing (filter (# (not (store-block-has? store %0))) names))
	(cond
		((nempty? missing) (list :missing missing))
		((and from (nql from (store-now store name))) (list :stale (store-stat store name)))
		((defq hash (store-manifest-make size names (# (store-block-put store %0))))
			(store-set store name hash size from :nil))
		((list :full :nil))))

(defun store-commit-root (store name hash &optional from)
	; (store-commit-root store path hash [from]) -> (:ok entry) | (:stale entry) | (:missing names)
	;a path is set to the object of the manifest of that name, as
	;(store-commit), every block of the manifest and of the object kept
	;here already: what is not is :missing, the blocks of the manifest that
	;are not, or if they all are, those of the object. So the writer of a
	;big object sends the blocks of its manifest as blocks, and then only
	;the name of its top, (store-manifest-make)
	(defq parts (list (list) (list)))
	(cond
		((not (defq object (store-manifest store hash parts))) (list :missing (second parts)))
		((nempty? (defq missing (filter (# (not (store-block-has? store %0))) (second object))))
			(list :missing missing))
		((and from (nql from (store-now store name))) (list :stale (store-stat store name)))
		((store-set store name hash (first object) from :nil))))

(defun store-put (store name data &optional from)
	; (store-put store path data [from]) -> (:ok entry) | (:stale entry) | (:full :nil)
	;a path is set to an object of this data, provided it is now from, as
	;(store-commit). Nothing is kept if it is not
	(cond
		((and from (nql from (store-now store name))) (list :stale (store-stat store name)))
		(:t (defq vol (get :vol store) names (list))
			(exfat-begin vol)
			(each (# (push names (store-block-put store %0))) (store-cut store data))
			(exfat-end vol)
			;a block there was no room for has no name
			(if (notevery (const identity) names) (list :full :nil)
				(store-commit store name names (length data) from)))))

(defun store-delete (store name &optional from)
	; (store-delete store path [from]) -> (:ok entry) | (:stale entry) | (:none :nil)
	;there is no object of that name from now, provided it is from, as
	;(store-commit). Its blocks are there till they are swept
	(cond
		((not (store-stat store name)) (list :none :nil))
		((store-set store name "" 0 from :t))))

(defun store-rename (store from to &optional was)
	; (store-rename store from to [was]) -> (:ok entry) | (:stale entry) | (:none :nil)
	;the path to is set to the object from is, with its copies, and from is
	;then no more, provided to is now the object was, as (store-commit). The
	;two in one call: a service, one task, answers nothing between them
	(defq entry (store-stat store from))
	(cond
		((not entry) (list :none :nil))
		((and was (nql was (store-now store to))) (list :stale (store-stat store to)))
		(:t (defq got (store-set store to (elem-get entry +store_entry_hash) (elem-get entry +store_entry_bytes)
				was :nil (elem-get entry +store_entry_copies)))
			(store-set store from "" 0 :nil :t)
			got)))

(defun store-copies (store name &optional copies)
	; (store-copies store path [copies]) -> :nil | num | (:ok entry) | (:none :nil)
	;how many members are to keep the blocks of the object a path is: the
	;dial. As many as the store says, unless it is set, to more for what
	;must not be lost or is read a great deal, to fewer for what is big
	;and can be had again. 0 sets it back to what the store says.
	;
	;It is a write of the path like any other, by this member, and the
	;others hear of it as they do of any: then those that are now to keep
	;its blocks fetch them, and those that no longer are let go of them.
	;Without copies, what it is, :nil if there is no such object
	(defq entry (store-stat store name))
	(cond
		((not copies)
			(if entry (if (> (defq said (elem-get entry +store_entry_copies)) 0) said (get :copies store))))
		((not entry) (list :none :nil))
		((store-set store name (elem-get entry +store_entry_hash) (elem-get entry +store_entry_bytes) :nil :nil (max 0 copies)))))

;;;;;;;;;;;
; snapshots
;;;;;;;;;;;
;
; A snapshot is paths: of each object there is under a prefix, a path
; snapshots/<name>/<its path> set to the same object. No block is copied,
; an object is the list of its blocks and they never change, and those
; blocks are kept for as long as the snapshot is, a path names them. So
; every member has it as it has any path, and what a snapshot is of can
; be had, read, sent a gopher, as any object is.

(defq +store_snapshots "snapshots/")

(defun store-snapshot-name? (name)
	; (store-snapshot-name? name) -> :nil | :t
	;a name a snapshot may have: not empty, and no / or space in it
	(and (nempty? name) (not (find "/" name)) (not (find " " name)) :t))

(defun store-snapshot (store name &optional prefix)
	; (store-snapshot store name [prefix]) -> :nil | count
	;a snapshot of the objects whose paths begin so, all if not said, how
	;many. Not of the paths of other snapshots. :nil if the name is not one
	;a snapshot may have
	(when (store-snapshot-name? name)
		(defq at (cat +store_snapshots name "/") count 0)
		(each (lambda (entry)
			(bind '(path hash bytes vv writer seen deleted copies) entry)
			(unless (starts-with +store_snapshots path)
				(store-set store (cat at path) hash bytes :nil :nil copies)
				(++ count)))
			(store-list store (ifn prefix "")))
		count))

(defun store-snapshots (store)
	; (store-snapshots store) -> ((name count) ...)
	;the snapshots there are, and how many objects each is of
	(defq out (Fmap 11) names (list))
	(each (lambda (entry)
		(defq after (slice (first entry) (length +store_snapshots) -1) name (slice after 0 (ifn (find "/" after) 0)))
		(when (nempty? name)
			(unless (. out :find name) (push names name))
			(. out :insert name (inc (ifn (. out :find name) 0)))))
		(store-list store +store_snapshots))
	(map (# (list %0 (. out :find %0))) (sort names)))

(defun store-restore (store name &optional prefix)
	; (store-restore store name [prefix]) -> :nil | (restored newer)
	;the objects whose paths begin so are as they were in the snapshot,
	;all of it if not said: each path that is not as it was is set back,
	;and one deleted since is had again. How many were set back, and how
	;many paths there are now under the prefix that the snapshot has not,
	;which are left. :nil if there is no such snapshot
	(defq at (cat +store_snapshots name "/") prefix (ifn prefix "") restored 0
		snap (filter (# (starts-with (cat at prefix) (first %0))) (store-list store at))
		had (Fset (store-buckets (length snap) 101)))
	(when (nempty? snap)
		(each (lambda (entry)
			(bind '(path hash bytes vv writer seen deleted copies) entry)
			(defq path (slice path (length at) -1))
			(. had :insert path)
			(unless (eql (store-now store path) hash)
				(store-set store path hash bytes :nil :nil copies)
				(++ restored)))
			snap)
		(list restored (length (filter (# (and (not (starts-with +store_snapshots (first %0))) (not (. had :find (first %0)))))
			(store-list store prefix))))))

(defun store-snapshot-drop (store name)
	; (store-snapshot-drop store name) -> count
	;a snapshot is gone, its paths deleted, how many. Its blocks are let go
	;of by a sweep, if nothing else is made of them
	(defq count 0)
	(each (lambda (entry) (if (eql (first (store-delete store (first entry))) :ok) (++ count)))
		(if (store-snapshot-name? name) (store-list store (cat +store_snapshots name "/")) (list)))
	count)

(defun store-walk (store name state visit &optional at)
	; (store-walk store path state visit [at]) -> :nil | (state)
	; (visit state data at count) -> (state next)
	;an object read a block at a time, where the reading says to go. The
	;function is given what it had, a block, which block it is and how
	;many there are, and gives what it has now and the block to read
	;next, :nil when it is done. It starts at the first, or at at. What
	;it has at the end is given in a list, so that an end of :nil can be
	;told from :nil: there is no such object, or a block it went to is
	;not kept here. A block there is not, past the end, is the end.
	;
	;This is how a search is sent to the data: what it has is small, and
	;with the function is all there is of it, so it can be carried on by
	;whoever has the next block
	(when (and (defq entry (store-stat store name))
			(defq object (store-manifest store (elem-get entry +store_entry_hash))))
		(defq names (second object) count (length names) lost :nil)
		(setd at 0)
		(while (and at (not lost) (< -1 at count))
			(cond
				((defq data (store-block-get store (elem-get names at)))
					(bind '(state at) (visit state data at count)))
				((setq lost :t))))
		(unless lost (list state))))

(defun store-fold (store name state step)
	; (store-fold store path state step) -> :nil | (state)
	; (step state data index) -> state
	;an object read a block at a time, in order, each given to a function
	;with what the last one gave: what it gives at the end, as (store-walk).
	;A name is found where a function is called from, not where it is
	;written: what (store-walk) calls its function must not be step, or
	;the step below would be this very lambda
	(store-walk store name state (lambda (state data at count) (list (step state data at) (inc at)))))

(defun store-get (store name)
	; (store-get store path) -> :nil | data
	;all of an object
	(if (defq got (store-fold store name (list) (lambda (parts data at) (push parts data))))
		(apply (const cat) (cat (list "") (first got)))))

(defun store-read (store name offset len)
	; (store-read store path offset len) -> :nil | data
	;part of an object, only the blocks that part is in are read
	(when (and (defq entry (store-stat store name))
			(defq object (store-manifest store (elem-get entry +store_entry_hash))))
		(defq size (get :block_size store) end (min (first object) (+ offset len))
			offset (clamp offset 0 end) parts (list "") lost :nil
			at (/ offset size))
		(while (and (not lost) (< (* at size) end))
			(if (defq data (store-block-get store (elem-get (second object) at)))
				(push parts (slice data (max 0 (- offset (* at size))) (min (length data) (- end (* at size)))))
				(setq lost :t))
			(++ at))
		(unless lost (apply (const cat) parts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; members, and two of them comparing notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun store-join (store members &optional retired later)
	; (store-join store members [retired later]) -> :nil | :t
	;these are members too, and those are retired, as another member has
	;it. Both only ever grow, so two that are put together are the same
	;whichever way round, and a member that is retired stays retired. Was
	;there anything new
	(defq new :nil)
	(each (lambda (who) (unless (find who (get :members store)) (push (get :members store) who) (setq new :t))) members)
	(each (lambda (who) (unless (find who (get :retired store)) (push (get :retired store) who) (setq new :t)))
		(ifn retired (list)))
	(when new
		(def store :members (sort (get :members store)) :retired (sort (get :retired store)))
		(if later (store-later store) (store-save store))
		:t))

(defun store-most? (store entry)
	; (store-most? store entry) -> :nil | :t
	;do most of the members have this entry, as far as is known here. At
	;most one of two entries written apart can be so
	(defq members (store-members store))
	(> (* 2 (length (filter (# (find %0 members)) (elem-get entry +store_entry_seen)))) (length members)))

(defun store-other-name (entry)
	; (store-other-name entry) -> path
	;the name an entry is kept under when another keeps its path: the
	;path, and the start of the name of its object. Every member makes
	;the same name of it
	(cat (elem-get entry +store_entry_path) "~" (slice (hex-encode (elem-get entry +store_entry_hash)) 0 8)))

(defun store-keeps (store a b)
	; (store-keeps store a b) -> a | b
	;of two entries for a path that were written apart, the one that
	;keeps it. One that most members have, over one that they have not.
	;Else the one written more times, and else the one whose object has
	;the greater name: that is only so that every member chooses the same
	(defq most_a (store-most? store a) most_b (store-most? store b)
		total_a (store-vv-total (elem-get a +store_entry_vv)) total_b (store-vv-total (elem-get b +store_entry_vv)))
	(cond
		((and most_a (not most_b)) a)
		((and most_b (not most_a)) b)
		((> total_a total_b) a)
		((> total_b total_a) b)
		((>= (cmp (elem-get a +store_entry_hash) (elem-get b +store_entry_hash)) 0) a)
		(b)))

(defun store-seen (a b)
	; (store-seen a b) -> (member ...)
	;the members that are in either list, once each, in order
	(defq out (cat a))
	(each (# (unless (find %0 out) (push out %0))) b)
	(sort out))

(defun store-meet (store entries &optional later)
	; (store-meet store entries [later]) -> (changed ...)
	;what another member has in its path map is set against what this one
	;has. The paths that are not as they were here, their entries now.
	;
	;An entry of theirs that comes after mine is taken, one that comes
	;before is not, mine will be taken there. The same entry: each now
	;knows the other has it. Two written apart: one keeps the path,
	;(store-keeps), with the counts of both, so that it comes after both
	;and whoever meets it next takes it; the other, if it is an object and
	;not a delete, is kept under another name, (store-other-name). Both
	;members come to the same, whichever does this. With later it is not
	;written to the volume now, (store-later)
	(defq map (get :map store) me (get :member store) changed (list))
	(each (lambda (theirs)
		(task-breathe)
		(bind '(name hash bytes vv writer seen deleted copies) theirs)
		(defq mine (. map :find name) now :nil)
		(cond
			((not mine)
				(setq now (list name hash bytes vv writer (store-seen seen (list me)) deleted copies)))
			(:t (defq how (store-vv-cmp vv (elem-get mine +store_entry_vv)) my_hash (elem-get mine +store_entry_hash))
				(cond
					((eql how :after)
						(setq now (list name hash bytes vv writer (store-seen seen (list me)) deleted copies)))
					((eql how :before))
					((and (eql how :same) (eql hash my_hash) (= copies (elem-get mine +store_entry_copies)))
						;the one entry, and who has it is more than either knew
						(defq both (store-seen seen (elem-get mine +store_entry_seen)))
						(unless (= (length both) (length (elem-get mine +store_entry_seen)))
							(setq now (cat (slice mine 0 +store_entry_seen) (list both) (slice mine (inc +store_entry_seen) -1)))))
					(:t ;written apart, or come to the same counts by two ways
						(defq keeps (store-keeps store theirs mine) other (if (eql keeps theirs) mine theirs)
							both_vv (store-vv-merge vv (elem-get mine +store_entry_vv)))
						(setq now (list name (elem-get keeps +store_entry_hash) (elem-get keeps +store_entry_bytes) both_vv
							(elem-get keeps +store_entry_writer) (list me) (elem-get keeps +store_entry_deleted)
							;of two that were set apart, the more copies: to have too many is the lesser harm
							(max copies (elem-get mine +store_entry_copies))))
						(unless (or (elem-get other +store_entry_deleted)
								(eql (elem-get other +store_entry_hash) (elem-get keeps +store_entry_hash)))
							(defq aside (store-other-name other))
							(unless (. map :find aside)
								(store-map-insert store aside (defq kept (cat (list aside) (slice other 1 +store_entry_seen) (list (list me))
									(slice other (inc +store_entry_seen) -1))))
								(store-touch store aside)
								(push changed kept)))))))
		(when now
			(store-map-insert store name now)
			(store-touch store name)
			(push changed now)))
		entries)
	;to the journal, as a path set here is. One of the same counts as the
	;map has, more members known to have it and no more, is not taken
	;when the journal is read: it is learnt again
	(and (nempty? changed) (not (store-journal-add store changed))
		(if later (store-later store) (store-save store)))
	changed)

(defun store-digest (store)
	; (store-digest store) -> hash
	;the path map and the members, as one name: two members with the same
	;have nothing to tell each other. It is asked for by every member each
	;time it looks, and is of every path there is, so it is held till
	;something changes, (store-save)
	(or (def? :digest store)
		(progn
			(defq lines (list))
			(. (get :map store) :each (lambda (name entry) (push lines (store-entry-line entry))))
			(def store :digest (sha256 (store-text (cat (sort lines) (list "members") (store-members store)
				(list "retired") (sort (cat (get :retired store)))
				(list "pinned") (store-pinned-lines store))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; what it holds, and keeping it so
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun store-wanted (store &optional prefix)
	; (store-wanted store [prefix]) -> (held missing)
	;the blocks the objects of these paths are made of, as two sets of
	;names in hex: those kept here, and those that are not. The blocks of
	;its manifest are of an object too. An object whose manifest is not
	;all kept here has those of it missing, what its other blocks are is
	;not known
	(defq held (Fset (store-buckets (store-span store) 101)) missing (Fset 11))
	(each (lambda (entry)
		(defq parts (list (list) (list)))
		(when (defq object (store-manifest store (elem-get entry +store_entry_hash) parts))
			(each (# (. (if (store-block-has? store %0) held missing) :insert (hex-encode %0))) (second object)))
		(each (# (. held :insert (hex-encode %0))) (first parts))
		(each (# (. missing :insert (hex-encode %0))) (second parts)))
		(store-list store prefix))
	(list held missing))

(defun store-share (store among)
	; (store-share store among) -> (wants extras)
	;this member's share, among the members that are there: the blocks it
	;is to keep and has not got, names, and those it has and is not to
	;keep, each (name holders), the members that are to.
	;
	;It is to keep every manifest, a member has to know what an object is
	;made of to look after any of it. Of the blocks of an object, those
	;whose keepers it is one of, (store-holders), and all of them if its
	;path is pinned. How many keepers a block has is its object's to say,
	;(store-copies), and one that two objects are made of has as many as
	;the one that asks for more. A block that no object is made of is
	;neither, that is for (store-sweep)
	;
	;What it is to keep is worked out the once, (store-plan), and kept
	;till the path map, the pins or who is there change. What it wants
	;and what is others' are kept as blocks come and go: a tender asks
	;this many times a look, and to work it out each time was a walk of
	;every block of every object
	(defq plan (store-plan store among) wants (list) extras (list) others (elem-get plan +plan_others))
	(. (elem-get plan +plan_missing) :each (# (push wants %0)))
	(. (elem-get plan +plan_extras) :each (# (push extras (list %0 (. others :find %0)))))
	(list wants extras))

(defun store-sweep (store)
	; (store-sweep store) -> num
	;the blocks that no object is made of are let go of, how many. Not
	;while an object is half written: its blocks are kept before its path
	;is set
	(defq vol (get :vol store) live (first (store-wanted store)) gone 0)
	(exfat-begin vol)
	(each (lambda (name)
		(unless (. live :find (hex-encode name))
			(if (store-block-drop store name) (++ gone))))
		(store-blocks store))
	(exfat-end vol)
	gone)

(defun store-check (store &optional mend)
	; (store-check store [mend]) -> (name ...)
	;the blocks kept here that are not what their names say, gone wrong
	;on the disk. With mend they are let go of, to be had again from
	;another member that has them right
	(defq bad (filter (# (not (store-block-get store %0))) (store-blocks store)))
	(when (and mend (nempty? bad))
		(exfat-begin (get :vol store))
		(each (# (store-block-drop store %0)) bad)
		(exfat-end (get :vol store)))
	bad)

(defun store-pin (store prefix &optional off)
	; (store-pin store prefix [off]) -> store
	;this member holds all of every object whose path begins so, wherever
	;else it is kept, and does not let go of it. With off it no longer
	;does
	(def store :plan :nil :pins (if off (filter (# (nql %0 prefix)) (get :pins store))
		(sort (if (find prefix (get :pins store)) (get :pins store) (cat (get :pins store) (list prefix))))))
	;and it is told to the others, with the path map: a count that this
	;member alone adds to, so what it said last is known to be the last
	(defq me (get :member store) was (. (get :pinned store) :find me))
	(. (get :pinned store) :insert me (cat (list (inc (if was (first was) 0))) (get :pins store)))
	(store-save store))

(defun store-pinned-take (store lines &optional later)
	; (store-pinned-take store lines [later]) -> :nil | :t
	;what another member says is pinned, taken in and kept
	(when (store-pinned-read store lines)
		(if later (store-later store) (store-save store))
		:t))

(defun store-pinned-by (store name)
	; (store-pinned-by store path) -> (member ...)
	;the members that hold all of the object of that path, as far as is
	;known here: it is under what they have pinned. Not one that is retired
	(defq out (list) retired (get :retired store))
	(. (get :pinned store) :each (lambda (member row)
		(and (not (find member retired)) (some! (# (starts-with %0 name)) (list row) :nil 1 -1) (push out member))))
	(sort out))

(defun store-pins-missing (store)
	; (store-pins-missing store) -> (name ...)
	;the blocks of what is pinned that are not kept here yet, in hex.
	;None, and what is pinned is all here: the machine can be taken away
	(defq out (list))
	(each (lambda (prefix)
		(. (second (store-wanted store prefix)) :each (# (unless (find %0 out) (push out %0)))))
		(get :pins store))
	(sort out))

(defun store-info (store)
	; (store-info store) -> (objects blocks free)
	;how many objects, how many blocks kept, and the room left in bytes
	;counted, not listed and put in order to be counted: it is asked often
	(list (get :objects store) (get :blocks store) (exfat-free (get :vol store))))

;module
(export-symbols '(store-make store-cluster-shift store-open store-close store-resize store-save
	store-members store-join store-most? store-other-name store-keeps store-meet store-digest
	store-entries store-holders store-rank store-seen store-share store-since store-at store-later
	store-block-put store-block-get store-block-has? store-block-drop store-blocks store-held store-block-file +store_folder_blocks
	store-cut store-manifest store-manifest-text store-tree-text store-manifest-read store-manifest-make store-manifest-walk
	store-find store-stat store-now
	store-commit store-commit-root store-put store-delete store-rename store-copies store-get store-read store-fold store-walk store-list
	store-wanted store-sweep store-check store-pin store-pinned? store-pins-missing store-pinned-lines store-pinned-take store-pinned-by store-info
	store-vv-bump store-vv-cmp store-vv-merge store-vv-count store-vv-total store-vv-text store-vv-read
	store-entry-line store-entry-read store-journal-write
	store-snapshot store-snapshots store-restore store-snapshot-drop store-snapshot-name? +store_snapshots
	+store_block_size +store_copies +store_manifest_names +store_entry_path +store_entry_hash +store_entry_bytes
	+store_entry_vv +store_entry_writer +store_entry_seen +store_entry_deleted +store_entry_copies))
(env-pop)
