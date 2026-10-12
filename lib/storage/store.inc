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
; An object is the list of its blocks, a manifest, which is a block too. A
; path is a name for one object, for now: the path map is the one part of
; a store that changes, and the one part members will ever have to agree
; on. An entry of it is
;
;	(path hash size vv writer seen deleted)
;
; hash is the name of the manifest. vv says how many times each member has
; written the path, as far as the writer of this entry knew: one entry
; comes after another if it has as many or more of every member's, and two
; that each have more of some member's were written apart, neither knowing
; of the other. writer is the member that made it, and seen the members
; that are known to have it, this one among them. One that is deleted
; stays, with no object, so that a member who was away does not bring the
; object back.
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
;	paths		the path map, a line an entry
;	blocks/xx/y	a block, its name in hex, xx the first two of it
;
; Nothing here calls itself, and nothing here knows of mail or of another
; member: service/storage/ is that.
;
; A task gives way to the others of its node only when it says so, and a
; store does a great deal between one mail and the next: every block that is
; put, had or looked over, and every entry that is met, gives way first,
; (task-slice). Without it a store at work held its node for seconds, and
; whatever else was on the node was late.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/fs/exfat.inc")
(import "lib/crypto/sha256.inc")

;module
(env-push)

(defq +store_magic "CHRYSA_STORE 1" +store_object_magic "CHRYSA_OBJECT 1"
	+store_block_size 65536 +store_copies 3 +store_lf (ascii-char 10))

(enums +store_entry 0
	(enum path hash bytes vv writer seen deleted))

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
	(bind '(name hash size vv writer seen deleted) entry)
	(join (list (if (eql hash "") "-" (hex-encode hash)) (str size) (store-vv-text vv) writer
		(if (empty? seen) "-" (join seen ",")) (if deleted "1" "0") name) " "))

(defun store-entry-read (line)
	(defq parts (split line " "))
	(when (>= (length parts) 7)
		(bind '(hash size vv writer seen deleted) (slice parts 0 6))
		(list (join (slice parts 6 -1) " ") (if (eql hash "-") "" (hex-decode hash)) (str-as-num size)
			(store-vv-read vv) writer (if (eql seen "-") (list) (split seen ",")) (eql deleted "1"))))

(defun store-save (store)
	; (store-save store) -> store
	;the path map, the members and the pins, as they are now. Whatever
	;changes any of them ends here, so what was worked out from them, the
	;digest, is not held any longer
	(task-slice)
	(def store :digest :nil :dirty :nil)
	(defq vol (get :vol store) lines (list))
	(. (get :map store) :each (lambda (name entry) (push lines (store-entry-line entry))))
	(exfat-begin vol)
	(exfat-save vol "paths" (store-text (sort lines)))
	(exfat-save vol "members" (store-text (get :members store)))
	(exfat-save vol "retired" (store-text (get :retired store)))
	(exfat-save vol "pins" (store-text (get :pins store)))
	(exfat-end vol)
	store)

(defun store-count (store)
	;a store counts the changes to its path map from when it was opened,
	;and has for each path the count at its last change, so that another
	;member can be told only what is new since it last asked,
	;(store-since). The counts are of this opening of it, which has a
	;name, :epoch: one who knew the counts of another is told everything
	(def store :rev 0 :revs (Fmap 1021) :dirty :nil
		:epoch (hex-encode (slice (sha256 (cat (str (pii-time)) (get :member store) (str (random 1000000)))) 0 8))))

(defun store-touch (store name)
	;a path is not as it was
	(def store :rev (inc (get :rev store)))
	(. (get :revs store) :insert name (get :rev store)))

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

(defun store-make (device size id member &optional block_size label copies)
	; (store-make path | stream size id member [block_size label copies]) -> :nil | :t
	;a new store with nothing in it, on a new volume of that many bytes.
	;id is the store's, any str with no space in it, member the id of the
	;member whose it is, the system id of its machine in hex. A path is a
	;new file of the host, a stream is written where it is and must be
	;that long. copies is how many members keep a block, 3
	(when (and (exfat-format device size (ifn label "STORE")) (defq vol (exfat-mount device)))
		(def (defq store (env 1)) :vol vol :id id :member member
			:block_size (ifn block_size +store_block_size) :copies (ifn copies +store_copies)
			:map (Fmap 11) :members (list member) :retired (list) :pins (list) :held (Fset 11) :manifests (Fmap 11))
		(store-count store)
		(exfat-begin vol)
		(store-head-save store)
		(exfat-mkdir vol "blocks")
		(exfat-end vol)
		(store-save store)
		:t))

(defun store-open (device member)
	; (store-open path | stream member) -> :nil | store
	;a store that is there. :nil if the volume is not one. It is opened by
	;the member it was made for, or last opened by. Opened by another, a
	;machine given a new id, a disk moved to another machine, that member
	;takes the place of the one it was, in the members too: there is not
	;one more member, and (get :was store) is who it was
	(when (and (defq vol (exfat-mount device))
			(defq head (store-lines (exfat-load vol "store")))
			(nempty? head) (eql (first head) +store_magic))
		(def (defq store (env 1)) :vol vol :id "" :member member :was :nil
			:block_size +store_block_size :copies +store_copies
			:map (Fmap 101) :members (list) :retired (list) :pins (list))
		(defq had "")
		(each (lambda (line)
			(defq at (find " " line) key (slice line 0 at) val (slice line (inc at) -1))
			(cond
				((eql key "store") (def store :id val))
				((eql key "member") (setq had val))
				((eql key "block") (def store :block_size (str-as-num val)))
				((eql key "copies") (def store :copies (str-as-num val)))))
			(rest head))
		(def store :members (store-lines (exfat-load vol "members"))
			:retired (store-lines (exfat-load vol "retired"))
			:pins (store-lines (exfat-load vol "pins")))
		(each (lambda (line)
			(if (defq entry (store-entry-read line))
				(. (get :map store) :insert (first entry) entry)))
			(store-lines (exfat-load vol "paths")))
		(store-count store)
		;the names of the blocks it has
		(def store :held (Fset 1021) :manifests (Fmap 1021))
		(each (# (. (get :held store) :insert %0)) (store-blocks store))
		(unless (eql had member)
			;it is this member's now
			(def store :was had
				:members (map (# (if (eql %0 had) member %0)) (get :members store)))
			(unless (find member (get :members store)) (push (get :members store) member))
			(exfat-begin vol)
			(store-head-save store)
			(exfat-end vol)
			(store-save store))
		store))

(defun store-close (store)
	; (store-close store) -> :nil
	;all that it has is on the device
	(if (get :dirty store) (store-save store))
	(exfat-sync (get :vol store))
	:nil)

;;;;;;;;
; blocks
;;;;;;;;

(defun store-block-file (name)
	; (store-block-file name) -> path
	;where on the volume a block of that name is kept
	(defq hex (hex-encode name))
	(cat "blocks/" (slice hex 0 2) "/" (slice hex 2 -1)))

(defun store-block-has? (store name)
	; (store-block-has? store name) -> :nil | :t
	;is a block of that name kept here. The names of what is kept are
	;held, read from the volume once as it is opened: to look one up on
	;the volume is to read two directories of it
	(if (. (get :held store) :find name) :t))

(defun store-block-put (store data)
	; (store-block-put store data) -> :nil | name
	;a block of this data is kept, and its name. If it is kept already
	;there is no more to do. :nil if there is no room for it
	(task-slice)
	(defq vol (get :vol store) name (sha256 data) file (store-block-file name)
		folder (slice file 0 (const (length "blocks/xx"))))
	(cond
		((store-block-has? store name) name)
		(:t (exfat-begin vol)
			(unless (exfat-find vol folder) (exfat-mkdir vol folder))
			(defq kept (exfat-save vol file data))
			(exfat-end vol)
			(when kept
				(. (get :held store) :insert name)
				name))))

(defun store-block-get (store name)
	; (store-block-get store name) -> :nil | data
	;the block of that name. :nil if it is not kept here, and :nil if what
	;is kept is not it: it has gone wrong, and is no use to anyone
	;one that is not kept is known not to be, with no look at the volume:
	;a member is asked for many it has not got, by those looking for one
	(task-slice)
	(and (store-block-has? store name)
		(defq found (exfat-load (get :vol store) (store-block-file name)))
		(eql (sha256 found) name) found))

(defun store-block-drop (store name)
	; (store-block-drop store name) -> :nil | :t
	(. (get :held store) :erase name)
	(. (get :manifests store) :erase name)
	(exfat-delete (get :vol store) (store-block-file name)))

(defun store-held (store)
	; (store-held store) -> (name ...)
	;the names of the blocks kept here, as they are held: what
	;(store-block-has?) goes by. No reading of the volume
	(defq out (list))
	(. (get :held store) :each (# (push out %0)))
	out)

(defun store-blocks (store)
	; (store-blocks store) -> (name ...)
	;every block that is kept here, whatever is in it, as the volume has it
	(defq out (list))
	(exfat-walk (get :vol store) "blocks" (lambda (file entry)
		(unless (second entry)
			(defq parts (split file "/"))
			(push out (hex-decode (cat (elem-get parts 1) (elem-get parts 2)))))))
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

(defun store-manifest-text (size names)
	; (store-manifest-text size names) -> data
	;what is kept for an object: how long it is, and its blocks in order
	(store-text (cat (list +store_object_magic (str size)) (map (const hex-encode) names))))

(defun store-manifest (store hash)
	; (store-manifest store hash) -> :nil | (size names)
	;the object of that name, if its manifest is kept here. One that has
	;been read is held: a name is of the one thing for ever, and what an
	;object is made of is asked again and again
	(defq known (get :manifests store))
	(cond
		((. known :find hash))
		((and (defq lines (store-lines (store-block-get store hash)))
				(> (length lines) 1) (eql (first lines) +store_object_magic))
			(. known :insert hash (defq object (list (str-as-num (second lines)) (map (const hex-decode) (slice lines 2 -1)))))
			object)))

(defun store-find (store name)
	; (store-find store path) -> :nil | entry
	;what a path is now, one that is deleted too
	(. (get :map store) :find name))

(defun store-stat (store name)
	; (store-stat store path) -> :nil | entry
	;what a path is now, :nil if there is no object of that name
	(and (defq entry (store-find store name)) (not (elem-get entry +store_entry_deleted)) entry))

(defun store-now (store name)
	; (store-now store path) -> hash
	;the name of the object a path is now, "" if there is none: what a
	;writer says it is setting it from
	(if (defq entry (store-stat store name)) (elem-get entry +store_entry_hash) ""))

(defun store-set (store name hash size from deleted)
	;the one way a path is changed here. from is what the writer says it
	;is now, :nil if it does not say
	(defq was (store-find store name) member (get :member store))
	(cond
		((and from (nql from (store-now store name))) (list :stale (store-stat store name)))
		(:t (defq entry (list name hash size
				(store-vv-bump (if was (elem-get was +store_entry_vv) (list)) member) member (list member) deleted))
			(. (get :map store) :insert name entry)
			(store-touch store name)
			(store-save store)
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
		((defq hash (store-block-put store (store-manifest-text size names)))
			(store-set store name hash size from :nil))
		((list :full :nil))))

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
			offset (max 0 (min offset end)) parts (list "") lost :nil
			at (/ offset size))
		(while (and (not lost) (< (* at size) end))
			(if (defq data (store-block-get store (elem-get (second object) at)))
				(push parts (slice data (max 0 (- offset (* at size))) (min (length data) (- end (* at size)))))
				(setq lost :t))
			(++ at))
		(unless lost (apply (const cat) parts))))

(defun store-list (store &optional prefix deleted)
	; (store-list store [prefix deleted]) -> (entry ...)
	;the paths that begin so, in order, with those deleted if asked
	(setd prefix "")
	(defq out (list))
	(. (get :map store) :each (lambda (name entry)
		(and (starts-with prefix name) (or deleted (not (elem-get entry +store_entry_deleted)))
			(push out entry))))
	(sort out (# (cmp (first %0) (first %1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; members, and two of them comparing notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun store-members (store)
	; (store-members store) -> (member ...)
	;who the members are meant to be, in order: every one that was ever
	;joined and has not been retired, away or not
	(sort (filter (# (not (find %0 (get :retired store)))) (get :members store))))

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
		(task-slice)
		(bind '(name hash bytes vv writer seen deleted) theirs)
		(defq mine (. map :find name) now :nil)
		(cond
			((not mine)
				(setq now (list name hash bytes vv writer (store-seen seen (list me)) deleted)))
			(:t (defq how (store-vv-cmp vv (elem-get mine +store_entry_vv)) my_hash (elem-get mine +store_entry_hash))
				(cond
					((eql how :after)
						(setq now (list name hash bytes vv writer (store-seen seen (list me)) deleted)))
					((eql how :before))
					((and (eql how :same) (eql hash my_hash))
						;the one entry, and who has it is more than either knew
						(defq both (store-seen seen (elem-get mine +store_entry_seen)))
						(unless (= (length both) (length (elem-get mine +store_entry_seen)))
							(setq now (cat (slice mine 0 +store_entry_seen) (list both) (slice mine (inc +store_entry_seen) -1)))))
					(:t ;written apart, or come to the same counts by two ways
						(defq keeps (store-keeps store theirs mine) other (if (eql keeps theirs) mine theirs)
							both_vv (store-vv-merge vv (elem-get mine +store_entry_vv)))
						(setq now (list name (elem-get keeps +store_entry_hash) (elem-get keeps +store_entry_bytes) both_vv
							(elem-get keeps +store_entry_writer) (list me) (elem-get keeps +store_entry_deleted)))
						(unless (or (elem-get other +store_entry_deleted)
								(eql (elem-get other +store_entry_hash) (elem-get keeps +store_entry_hash)))
							(defq aside (store-other-name other))
							(unless (. map :find aside)
								(. map :insert aside (defq kept (cat (list aside) (slice other 1 +store_entry_seen) (list (list me))
									(slice other (inc +store_entry_seen) -1))))
								(store-touch store aside)
								(push changed kept)))))))
		(when now
			(. map :insert name now)
			(store-touch store name)
			(push changed now)))
		entries)
	(if (nempty? changed) (if later (store-later store) (store-save store)))
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
				(list "retired") (sort (cat (get :retired store))))))))))

(defun store-entries (store)
	; (store-entries store) -> (entry ...)
	;every entry of the path map, those deleted too, what is told to
	;another member
	(store-list store "" :t))

(defun store-holders (store name &optional among)
	; (store-holders store name [among]) -> (member ...)
	;the members that are to keep a block of that name, as many as there
	;are to be copies, of the members, or of those given, the ones that
	;are there. Each member is given a number made from its id and the
	;name, and those with the greatest keep it: so every member works out
	;the same, nobody has to be asked, and when a member comes or goes
	;only the blocks that are its share move
	;
	;What was worked out is kept, for as long as it is asked of the same
	;members: a member asks it of every block it knows of each time it
	;looks to its share, and it is a hash and a sort for each
	(defq among (ifn among (store-members store)) key (join among " ") kept (def? :holders store))
	(unless (and kept (eql (first kept) key))
		(def store :holders (setq kept (list key (Fmap 4093)))))
	(cond
		((. (second kept) :find name))
		(:t (defq scored (sort (map (# (list (sha256 (cat name %0)) %0)) among)
				(# (cmp (first %1) (first %0))))
				holders (map (const second) (slice scored 0 (min (get :copies store) (length scored)))))
			(. (second kept) :insert name holders)
			holders)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; what it holds, and keeping it so
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun store-wanted (store &optional prefix)
	; (store-wanted store [prefix]) -> (held missing)
	;the blocks the objects of these paths are made of, as two sets of
	;names in hex: those kept here, and those that are not. An object
	;whose manifest is not kept here is one block missing, what its other
	;blocks are is not known
	(defq held (Fset 101) missing (Fset 11))
	(each (lambda (entry)
		(defq hash (elem-get entry +store_entry_hash))
		(cond
			((defq object (store-manifest store hash))
				(. held :insert (hex-encode hash))
				(each (# (. (if (store-block-has? store %0) held missing) :insert (hex-encode %0))) (second object)))
			((. missing :insert (hex-encode hash)))))
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
	;path is pinned. A block that no object is made of is neither, that
	;is for (store-sweep)
	(defq me (get :member store) wanted (Fset 4093) others (Fmap 4093) wants (list) extras (list))
	(each (lambda (entry)
		(task-slice)
		(defq hash (elem-get entry +store_entry_hash) pinned (store-pinned? store (first entry)))
		(. wanted :insert hash)
		(when (defq object (store-manifest store hash))
			(each (lambda (name)
				(defq holders (store-holders store name among))
				(if (or pinned (find me holders)) (. wanted :insert name) (. others :insert name holders)))
				(second object))))
		(store-list store))
	(. wanted :each (lambda (name) (unless (store-block-has? store name) (push wants name))))
	;a block that is another's to keep for one object may be mine for another
	(. others :each (lambda (name holders)
		(and (not (. wanted :find name)) (store-block-has? store name) (push extras (list name holders)))))
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
	(def store :pins (if off (filter (# (nql %0 prefix)) (get :pins store))
		(sort (if (find prefix (get :pins store)) (get :pins store) (cat (get :pins store) (list prefix))))))
	(store-save store))

(defun store-pinned? (store name)
	; (store-pinned? store path) -> :nil | :t
	(if (some (# (starts-with %0 name)) (get :pins store)) :t))

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
	(defq objects 0 blocks 0)
	(. (get :map store) :each (lambda (name entry) (unless (elem-get entry +store_entry_deleted) (++ objects))))
	(. (get :held store) :each (lambda (name) (++ blocks)))
	(list objects blocks (exfat-free (get :vol store))))

;module
(export-symbols '(store-make store-open store-close store-save
	store-members store-join store-most? store-other-name store-keeps store-meet store-digest
	store-entries store-holders store-seen store-share store-since store-at store-later
	store-block-put store-block-get store-block-has? store-block-drop store-blocks store-held store-block-file
	store-cut store-manifest store-manifest-text store-find store-stat store-now
	store-commit store-put store-delete store-get store-read store-fold store-walk store-list
	store-wanted store-sweep store-check store-pin store-pinned? store-pins-missing store-info
	store-vv-bump store-vv-cmp store-vv-merge store-vv-count store-vv-total store-vv-text store-vv-read
	store-entry-line store-entry-read
	+store_block_size +store_copies +store_entry_path +store_entry_hash +store_entry_bytes
	+store_entry_vv +store_entry_writer +store_entry_seen +store_entry_deleted))
(env-pop)
