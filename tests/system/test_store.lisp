(report-header "System: a store, blocks named by what is in them, objects, and a path map with one way to write")

(import "lib/storage/store.inc")

(defun so-new (size id member &optional block_size)
	;a new store in a memory stream, opened by its member, and the stream
	(defq stream (string-stream (cat (exfat-zeros size))))
	(list (and (store-make stream size id member block_size) (store-open stream member)) stream))

(defun so-bytes (len seed)
	;bytes that are not all the same, and differ with the seed
	(defq out (cat (exfat-zeros len)))
	(each (# (set-byte out %0 (logand (+ (* %0 7) (>> %0 8) seed) 0xff))) (range 0 len))
	out)

(defun so-is (result) (first result))
(defun so-paths (store &optional prefix deleted) (map (const first) (store-list store prefix deleted)))

;;;;;;;;;;;;;;;;;;;;;;;;
; made, opened, and whose
;;;;;;;;;;;;;;;;;;;;;;;;

(bind '(so stream) (so-new 0x100000 "games" "aaaa" 4096))
(assert-true "a store is made and opened" so)
(assert-eq "it is the store it was made as" "games" (get :id so))
(assert-eq "and its member's" "aaaa" (get :member so))
(assert-list-eq "who is the one member" '("aaaa") (get :members so))
(assert-eq "with the block size it was given" 4096 (get :block_size so))
(assert-eq "opened by the member it was made for, it was nobody else's" :nil (get :was so))
(assert-list-eq "and has nothing in it" '(0 0) (slice (store-info so) 0 2))
;a volume that is no store
(defq so_plain (string-stream (cat (exfat-zeros 0x100000))))
(exfat-format so_plain 0x100000 "PLAIN")
(assert-eq "a volume that is not a store is not opened as one" :nil (store-open so_plain "aaaa"))

;;;;;;;;
; blocks
;;;;;;;;

(defq so_b1 (so-bytes 3000 1) so_n1 (store-block-put so so_b1))
(assert-eq "a block's name is the SHA-256 of it" (sha256 so_b1) so_n1)
(assert-true "it is kept" (store-block-has? so so_n1))
(assert-eq "and had back as it was" so_b1 (store-block-get so so_n1))
(assert-eq "kept again it is the same name" so_n1 (store-block-put so so_b1))
(assert-eq "and still the one block" 1 (length (store-blocks so)))
(assert-eq "a name of nothing kept is nothing" :nil (store-block-get so (sha256 "not here")))
(assert-list-eq "nothing kept is wrong" '() (store-check so))
;what is kept under a name is made not to be it, as a disk gone wrong would
(exfat-save (get :vol so) (store-block-file so_n1) (so-bytes 3000 2))
(assert-eq "a block that is not what its name says is not given" :nil (store-block-get so so_n1))
(assert-list-eq "and is found" (list so_n1) (store-check so))
(assert-list-eq "mended, it is let go of" (list so_n1) (store-check so :t))
(assert-eq "and is not kept" :nil (store-block-has? so so_n1))

;;;;;;;;;
; objects
;;;;;;;;;

(defq so_small (so-bytes 100 3) so_one (so-bytes 4096 4) so_big (so-bytes 10000 5))
(assert-eq "a small object is put" :ok (so-is (store-put so "a/small" so_small)))
(assert-eq "one of just a block" :ok (so-is (store-put so "a/one" so_one)))
(assert-eq "one of more than two" :ok (so-is (store-put so "a/big" so_big)))
(assert-eq "one of nothing" :ok (so-is (store-put so "a/none" "")))
(assert-eq "the small one is had back" so_small (store-get so "a/small"))
(assert-eq "the one of a block" so_one (store-get so "a/one"))
(assert-eq "the big one" so_big (store-get so "a/big"))
(assert-eq "the one of nothing is nothing, and is there" "" (store-get so "a/none"))
(assert-eq "what there is none of is not" :nil (store-get so "a/nothing"))
(defq so_e (store-stat so "a/big"))
(assert-eq "an entry has how long it is" 10000 (elem-get so_e +store_entry_bytes))
(assert-eq "who wrote it" "aaaa" (elem-get so_e +store_entry_writer))
(assert-list-eq "and that the one member has it" '("aaaa") (elem-get so_e +store_entry_seen))
(assert-list-eq "the big one is three blocks, in order, the last short"
	(list 3 4096 4096 1808)
	(progn (defq so_m (store-manifest so (elem-get so_e +store_entry_hash)))
		(cat (list (length (second so_m))) (map (# (length (store-block-get so %0))) (second so_m)))))
(assert-list-eq "the paths, in order" '("a/big" "a/none" "a/one" "a/small") (so-paths so))
(assert-list-eq "and those that begin so" '("a/big") (so-paths so "a/b"))

;part of one, and a block at a time
(assert-eq "part of an object, across two blocks" (slice so_big 4000 4200) (store-read so "a/big" 4000 200))
(assert-eq "from its start" (slice so_big 0 10) (store-read so "a/big" 0 10))
(assert-eq "to past its end is to its end" (slice so_big 9990 10000) (store-read so "a/big" 9990 500))
(assert-eq "from past its end is nothing" "" (store-read so "a/big" 20000 10))
(assert-eq "of what there is none of" :nil (store-read so "a/nothing" 0 10))
(assert-list-eq "a fold is given each block in order, with what the last gave"
	'(((0 4096) (1 4096) (2 1808)))
	(store-fold so "a/big" (list) (lambda (state data at) (push state (list at (length data))))))
(assert-list-eq "what it gives is in a list, so that :nil is an answer"
	'(:nil) (store-fold so "a/none" :nil (lambda (state data at) :t)))
(assert-eq "and there is none of what there is none of" :nil
	(store-fold so "a/nothing" 0 (lambda (state data at) state)))
(assert-eq "the sum of every byte, a block at a time, is the sum of them all"
	(reduce (# (+ %0 (code so_big 1 %1))) (range 0 (length so_big)) 0)
	(first (store-fold so "a/big" 0 (lambda (sum data at)
		(reduce (# (+ %0 (code data 1 %1))) (range 0 (length data)) sum)))))

;a walk goes where it says: each block read says which is next, or that it is done
(assert-list-eq "a walk told to go on is a fold" '((0 1 2))
	(store-walk so "a/big" (list) (lambda (state data at count) (list (push state at) (inc at)))))
(assert-list-eq "one that says it is done at the first block reads no more" '((0))
	(store-walk so "a/big" (list) (lambda (state data at count) (list (push state at) :nil))))
(assert-list-eq "one that goes to the last, then the first, and is told how many there are" '(((2 3) (0 3)))
	(store-walk so "a/big" (list) (lambda (state data at count)
		(list (push state (list at count)) (if (= at 2) 0 :nil))) 2))
(assert-list-eq "one sent to a block there is not is done" '((0))
	(store-walk so "a/big" (list) (lambda (state data at count) (list (push state at) 99))))
(assert-list-eq "of an object of nothing, nothing is read" '(:start)
	(store-walk so "a/none" :start (lambda (state data at count) (list :read :nil))))
;a search of a sorted object by halves reads few of its blocks
(defq so_sorted (apply (const cat) (map (# (cat (pad (str %0) 7 "0") (ascii-char 10))) (range 0 4096))))
(store-put so "a/sorted" so_sorted)
(defun so-search (want)
	;the block of 512 numbers in order that a number is in: (found reads)
	(first (store-walk so "a/sorted" (list :nil 0 0 7)
		(lambda ((found reads lo hi) data at count)
			(defq low (str-as-num (slice data 0 7)) high (str-as-num (slice data -9 -2)))
			(cond
				((< want low) (list (list :nil (inc reads) lo (dec at)) (if (> at lo) (/ (+ lo (dec at)) 2))))
				((> want high) (list (list :nil (inc reads) (inc at) hi) (if (< at hi) (/ (+ (inc at) hi) 2))))
				((list (list at (inc reads) lo hi) :nil))))
		3)))
(assert-list-eq "the block a number is in, found in no more than 4 reads of 8"
	'(0 :t) (progn (defq so_f (so-search 100)) (list (first so_f) (<= (second so_f) 4))))
(assert-list-eq "and of one near the end" '(7 :t) (progn (defq so_f (so-search 4000)) (list (first so_f) (<= (second so_f) 4))))
(assert-list-eq "and in the middle, where it starts, in one" '(3 1) (slice (so-search 1600) 0 2))

;two that are the same are one
(defq so_count (second (store-info so)))
(assert-eq "the same data under another name" :ok (so-is (store-put so "b/copy" so_big)))
(assert-eq "is no more blocks: its object is the one there was" so_count (second (store-info so)))
(assert-eq "the two paths are the one object"
	(elem-get (store-stat so "a/big") +store_entry_hash) (elem-get (store-stat so "b/copy") +store_entry_hash))

;;;;;;;;;;;;;;;;;;;;;;;
; the one way to write
;;;;;;;;;;;;;;;;;;;;;;;

(assert-eq "a path that is not there, set provided it is not" :ok (so-is (store-put so "c/x" "one" "")))
(defq so_h1 (store-now so "c/x"))
(assert-list-eq "set again provided it is not: it is, and here is what it is"
	(list :stale so_h1) (progn (defq so_r (store-put so "c/x" "two" "")) (list (first so_r) (elem-get (second so_r) +store_entry_hash))))
(assert-eq "and it is as it was" "one" (store-get so "c/x"))
(assert-eq "set from what it is" :ok (so-is (store-put so "c/x" "two" so_h1)))
(assert-eq "it is the new" "two" (store-get so "c/x"))
(assert-eq "set from what it was, by one who did not see that" :stale (so-is (store-put so "c/x" "three" so_h1)))
(assert-eq "it is still the new" "two" (store-get so "c/x"))
(assert-eq "set by one that does not say" :ok (so-is (store-put so "c/x" "four")))
(assert-eq "it is that" "four" (store-get so "c/x"))
(assert-list-eq "it has been written three times, all by the one member" '(("aaaa" 3)) (elem-get (store-stat so "c/x") +store_entry_vv))
(defq so_kept (store-block-put so "kept"))
(assert-list-eq "a path set to blocks, one of them not kept here: which"
	(list :missing (list (sha256 "nowhere")))
	(store-commit so "c/y" (list so_kept (sha256 "nowhere")) 10))
(assert-eq "two of them not, the one that was let go of too" 2
	(length (second (store-commit so "c/y" (list (sha256 "nowhere") so_kept so_n1) 10))))
(assert-eq "and no path was made" :nil (store-stat so "c/y"))

;who has written, and which came after
(test-cases
	(store-vv-cmp '(("a" 1)) '(("a" 1))) :same
	(store-vv-cmp '(("a" 2)) '(("a" 1))) :after
	(store-vv-cmp '(("a" 1)) '(("a" 2))) :before
	(store-vv-cmp '(("a" 1) ("b" 1)) '(("a" 1))) :after
	(store-vv-cmp '(("a" 1)) '(("a" 1) ("b" 1))) :before
	(store-vv-cmp '(("a" 2)) '(("a" 1) ("b" 1))) :apart
	(store-vv-cmp '(("a" 5) ("b" 1)) '(("a" 4) ("b" 2))) :apart
	(store-vv-cmp '() '(("a" 1))) :before
	(store-vv-cmp '() '()) :same)
(assert-list-eq "a write by a member is one more of its"
	'(("a" 2) ("b" 1)) (store-vv-bump '(("a" 1) ("b" 1)) "a"))
(assert-list-eq "a member that has not written yet, in its place"
	'(("a" 1) ("b" 1) ("c" 4)) (store-vv-bump '(("a" 1) ("c" 4)) "b"))
(assert-list-eq "two put together are the most of each"
	'(("a" 5) ("b" 2) ("c" 1)) (store-vv-merge '(("a" 5) ("b" 1)) '(("a" 4) ("b" 2) ("c" 1))))
(assert-list-eq "and so come after both" '(:after :after)
	(list (store-vv-cmp (store-vv-merge '(("a" 5) ("b" 1)) '(("a" 4) ("b" 2))) '(("a" 5) ("b" 1)))
		(store-vv-cmp (store-vv-merge '(("a" 5) ("b" 1)) '(("a" 4) ("b" 2))) '(("a" 4) ("b" 2)))))
(assert-eq "how many times a path has been written" 7 (store-vv-total '(("a" 5) ("b" 2))))
(assert-list-eq "an entry written as a line is read as it was, a path with spaces in it too"
	(list "d/a name with spaces" (sha256 "x") 12 '(("aaaa" 2) ("bbbb" 1)) "bbbb" '("aaaa" "bbbb") :nil 0)
	(store-entry-read (store-entry-line (list "d/a name with spaces" (sha256 "x") 12 '(("aaaa" 2) ("bbbb" 1)) "bbbb" '("aaaa" "bbbb") :nil 0))))

;how many copies there are to be of a path, the dial
(assert-list-eq "an entry that says how many copies is read as it was, and one written before it could is read as not saying"
	(list 5 0 :t)
	(list (elem-get (store-entry-read (store-entry-line (list "d/x" (sha256 "x") 12 '(("aaaa" 2)) "aaaa" '("aaaa") :nil 5))) +store_entry_copies)
		(elem-get (store-entry-read "- 0 aaaa:1 aaaa aaaa 1 d/old one") +store_entry_copies)
		(elem-get (store-entry-read "- 0 aaaa:1 aaaa aaaa 1/2 d/old one") +store_entry_deleted)))
(defq so_writes (store-vv-total (elem-get (store-find so "a/one") +store_entry_vv)) so_was (store-now so "a/one"))
(assert-eq "a path is to have as many copies as the store says, till it is set" 3 (store-copies so "a/one"))
(assert-eq "it is set to five" :ok (so-is (store-copies so "a/one" 5)))
(assert-list-eq "it is five, the object is as it was, and that was a write of the path" (list 5 so_was (inc so_writes))
	(list (store-copies so "a/one") (store-now so "a/one") (store-vv-total (elem-get (store-find so "a/one") +store_entry_vv))))
(store-put so "a/one" (cat (store-get so "a/one") "!"))
(assert-eq "written again, it is still five: it is the path's" 5 (store-copies so "a/one"))
(store-copies so "a/one" 0)
(assert-eq "set to none, it is what the store says again" 3 (store-copies so "a/one"))
(assert-list-eq "of no such object: none to ask of, none to set" '(:nil :none)
	(list (store-copies so "a/no such") (so-is (store-copies so "a/no such" 4))))

;;;;;;;;;;;;;;;;;;;;;;
; deleted, and swept
;;;;;;;;;;;;;;;;;;;;;;

(assert-eq "an object is deleted" :ok (so-is (store-delete so "a/small")))
(assert-eq "there is none of it" :nil (store-get so "a/small"))
(assert-eq "its path is not listed" :nil (find "a/small" (so-paths so)))
(assert-true "but is, with those deleted" (find "a/small" (so-paths so "" :t)))
(assert-list-eq "and its entry says so, a write more" '(:t 2)
	(list (elem-get (store-find so "a/small") +store_entry_deleted) (store-vv-total (elem-get (store-find so "a/small") +store_entry_vv))))
(assert-eq "deleted again, there is none to" :none (so-is (store-delete so "a/small")))
(assert-eq "deleted provided it is what it is not" :stale (so-is (store-delete so "a/one" (sha256 "other"))))
(assert-eq "put again, it is there, a write more" 3
	(progn (store-put so "a/small" so_small) (store-vv-total (elem-get (store-stat so "a/small") +store_entry_vv))))
;the blocks of what was written over, and deleted, are still kept
(store-delete so "a/small")
(defq so_before (second (store-info so)) so_gone (store-sweep so))
(assert-true "a sweep lets go of the blocks no object is made of" (> so_gone 0))
(assert-eq "that many" (- so_before so_gone) (second (store-info so)))
(assert-eq "none of an object that is there" so_big (store-get so "a/big"))
(assert-eq "nor of the other path to it" so_big (store-get so "b/copy"))
(assert-eq "swept again there is nothing to" 0 (store-sweep so))
(store-delete so "a/big")
(assert-eq "one of two paths to an object deleted, and swept: nothing goes" 0 (store-sweep so))
(assert-eq "the object is there by the other" so_big (store-get so "b/copy"))

;;;;;;
; pins
;;;;;;

(store-pin so "b/")
(assert-eq "a path under what is pinned is pinned" :t (store-pinned? so "b/copy"))
(assert-eq "one that is not is not" :nil (store-pinned? so "c/x"))
(assert-list-eq "all of what is pinned is here" '() (store-pins-missing so))
(defq so_lost (first (second (store-manifest so (store-now so "b/copy")))))
(store-block-drop so so_lost)
(assert-list-eq "a block of it gone: that is what is missing" (list (hex-encode so_lost)) (store-pins-missing so))
(assert-eq "and the object is not to be had whole" :nil (store-get so "b/copy"))
(assert-eq "nor folded" :nil (store-fold so "b/copy" 0 (lambda (state data at) state)))
(assert-eq "the block put back, it is" so_big (progn (store-block-put so (slice so_big 0 4096)) (store-get so "b/copy")))
(assert-list-eq "what is pinned is known by who has it pinned" (list (get :member so)) (store-pinned-by so "b/copy"))
(assert-list-eq "and a path that is not under it by nobody" '() (store-pinned-by so "c/x"))
(store-pin so "b/" :t)
(assert-eq "no longer pinned" :nil (store-pinned? so "b/copy"))
(assert-list-eq "nor known to be by anybody" '() (store-pinned-by so "b/copy"))
;what another member has pinned, as it tells it: a count that is its own
(assert-eq "what another member says it has pinned is taken" :t (store-pinned-take so '("zzzz 2 b/" "zzzz 2 with a space/")))
(assert-list-eq "and is known" '("zzzz") (store-pinned-by so "b/copy"))
(assert-list-eq "a prefix with a space in it too" '("zzzz") (store-pinned-by so "with a space/x"))
(assert-eq "the same again is nothing new" :nil (store-pinned-take so '("zzzz 2 b/" "zzzz 2 with a space/")))
(assert-eq "nor is what it said before that" :nil (store-pinned-take so '("zzzz 1 c/")))
(assert-list-eq "which is not taken" '() (store-pinned-by so "c/x"))
(assert-eq "what it says after is, it has none now" :t (store-pinned-take so '("zzzz 3")))
(assert-list-eq "and it is known to have none" '() (store-pinned-by so "b/copy"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; it is on the volume, and whose it is
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(store-pin so "user/me/")
(store-close so)
(defq so_again (store-open stream "aaaa"))
(assert-true "opened again from the volume" so_again)
(assert-list-eq "the paths are as they were" (so-paths so "" :t) (so-paths so_again "" :t))
(assert-eq "an object is had" so_big (store-get so_again "b/copy"))
(assert-eq "an entry is as it was" (str (store-find so "c/x")) (str (store-find so_again "c/x")))
(assert-list-eq "and the pins" '("user/me/") (get :pins so_again))
(assert-list-eq "and who has what pinned, as it was" (store-pinned-lines so) (store-pinned-lines so_again))
(assert-list-eq "this member among them" (list (get :member so_again)) (store-pinned-by so_again "user/me/x"))
(assert-eq "it is still nobody else's" :nil (get :was so_again))
;opened by another member: a machine given a new id, a disk moved
(defq so_moved (store-open stream "bbbb"))
(assert-eq "opened by another member it is that member's" "bbbb" (get :member so_moved))
(assert-eq "who it was is said" "aaaa" (get :was so_moved))
(assert-list-eq "and that member is in the place of the one it was, not one more" '("bbbb") (get :members so_moved))
(assert-eq "what it holds is as it was" so_big (store-get so_moved "b/copy"))
(assert-list-eq "a write by it is its own, after those of the one before"
	'(("aaaa" 3) ("bbbb" 1)) (progn (store-put so_moved "c/x" "five") (elem-get (store-stat so_moved "c/x") +store_entry_vv)))
(assert-eq "opened once more by it, it was its own" :nil (get :was (store-open stream "bbbb")))

;;;;;;;;;
; no room
;;;;;;;;;

(bind '(so_tiny so_tiny_stream) (so-new 0x100000 "tiny" "aaaa" 32768))
(assert-eq "an object that fits" :ok (so-is (store-put so_tiny "x" (so-bytes 20000 6))))
(defq so_turn (so-bytes 0x10000 7))
(assert-eq "an object of a pattern that comes round is few blocks, however long: 2MB in a store of 1MB" :ok
	(so-is (store-put so_tiny "round" (apply (const cat) (map (lambda (&) so_turn) (range 0 32))))))
;bytes of which no two blocks are the same
(defq so_much (apply (const cat) (map (# (cat (sha256 (str "much" %0)) (exfat-zeros 32736))) (range 0 40))))
(assert-eq "one there is no room for" :full (so-is (store-put so_tiny "y" so_much)))
(assert-eq "has no path" :nil (store-stat so_tiny "y"))
(assert-eq "and what was there is" (so-bytes 20000 6) (store-get so_tiny "x"))
(assert-true "the room its blocks took is had back by a sweep" (> (store-sweep so_tiny) 0))
(assert-eq "one that fits, after" :ok (so-is (store-put so_tiny "z" (so-bytes 30000 8))))

;;;;;;;;;;;;;
; the journal
;;;;;;;;;;;;;

;a path that is set is a line at the end of the journal, and the map on
;the volume is as it was. A store that is opened reads both
(bind '(so_j so_j_stream) (so-new 0x400000 "journal" "aaaa" 4096))
(assert-true "a store has a journal" (def? :journal so_j))
(assert-true "a 64th of its volume, or a little less" (<= 0xf000 (get :journal_size so_j) 0x10000))
(defq so_j_first (get :journal_at so_j))
(store-put so_j "a" (so-bytes 5000 1))
(store-put so_j "b c" (so-bytes 6000 2))
(store-put so_j "a" (so-bytes 7000 3))
(store-delete so_j "b c")
(store-put so_j "d" (so-bytes 100 4))
(assert-true "a path that is set is added to it" (> (get :journal_at so_j) so_j_first))
(assert-eq "and the map on the volume has none of them" "" (exfat-load (get :vol so_j) "paths"))
;not closed: opened again as it is on the volume
(defq so_j2 (store-open so_j_stream "aaaa"))
(assert-list-eq "opened, it has the paths, the last of each" '("a" "d") (so-paths so_j2))
(assert-eq "what they are" (so-bytes 7000 3) (store-get so_j2 "a"))
(assert-list-eq "and the one deleted, as deleted" '("a" "b c" "d") (so-paths so_j2 :nil :t))
(assert-eq "it goes on from where the journal ended" (get :journal_at so_j) (get :journal_at so_j2))
(store-put so_j2 "e" (so-bytes 100 5))
(assert-list-eq "and what is added then is there the next time" '("a" "d" "e") (so-paths (store-open so_j_stream "aaaa")))
;the map written whole: the journal is empty, and what was in it is not read again
(store-save so_j2)
(assert-eq "the map written, the journal is as it began but for its count" (length (cat (str (get :journal_count so_j2)) " ")) (get :journal_at so_j2))
(assert-true "of one more" (> (get :journal_count so_j2) (get :journal_count so_j)))
(assert-list-eq "and the store opens to the same" '("a" "d" "e") (so-paths (defq so_j3 (store-open so_j_stream "aaaa"))))
;a line that was half written
(store-put so_j3 "f" (so-bytes 100 6))
(defq so_j_good (get :journal_at so_j3))
(store-put so_j3 "g" (so-bytes 100 7))
(store-journal-write so_j3 (+ so_j_good 20) "XXXX")
(exfat-sync (get :vol so_j3))
(assert-list-eq "a line of the journal that is not whole is not taken, nor what is after it" '("a" "d" "e" "f")
	(so-paths (defq so_j4 (store-open so_j_stream "aaaa"))))
(assert-eq "and it is added to from there" so_j_good (get :journal_at so_j4))
;a journal that fills
(defq so_j_count (get :journal_count so_j4))
(each (# (store-put so_j4 "over and over" (so-bytes 50 (logand %0 3)))) (range 0 1000))
(assert-true "a journal that is full, and the map is written and it begins again" (> (get :journal_count so_j4) so_j_count))
(defq so_j5 (store-open so_j_stream "aaaa"))
(assert-list-eq "nothing lost by it" '("a" "d" "e" "f" "over and over") (so-paths so_j5))
(assert-eq "and the last that was set is what is had" (so-bytes 50 3) (store-get so_j5 "over and over"))

;;;;;;;;;;;
; snapshots
;;;;;;;;;;;

(bind '(so_s so_s_stream) (so-new 0x400000 "snaps" "aaaa" 4096))
(store-put so_s "docs/a" (so-bytes 9000 1))
(store-put so_s "docs/b" (so-bytes 5000 2))
(store-put so_s "other" (so-bytes 3000 3))
(defq so_s_blocks (length (store-blocks so_s)))
(assert-eq "a name with a / is not a snapshot's" :nil (store-snapshot so_s "a/b" "docs/"))
(assert-eq "a snapshot of the objects under a prefix" 2 (store-snapshot so_s "before" "docs/"))
(assert-eq "costs no block" so_s_blocks (length (store-blocks so_s)))
(assert-list-eq "and is listed, with how many it is of" '(("before" 2)) (store-snapshots so_s))
(assert-eq "an object of it is had as it was" (so-bytes 9000 1) (store-get so_s "snapshots/before/docs/a"))
;changed since
(store-put so_s "docs/a" (so-bytes 7000 4))
(store-delete so_s "docs/b")
(store-put so_s "docs/c" (so-bytes 2000 5))
(store-sweep so_s)
(assert-eq "a sweep keeps the blocks of what a snapshot is of" (so-bytes 9000 1) (store-get so_s "snapshots/before/docs/a"))
(assert-eq "a snapshot of all is not of other snapshots, three are left" 3 (store-snapshot so_s "all"))
(assert-list-eq "two snapshots" '(("all" 3) ("before" 2)) (store-snapshots so_s))
(assert-list-eq "restored: one changed and one deleted are set back, one made since is left" '(2 1)
	(store-restore so_s "before" "docs/"))
(assert-eq "as it was" (so-bytes 9000 1) (store-get so_s "docs/a"))
(assert-eq "and the deleted one is there again" (so-bytes 5000 2) (store-get so_s "docs/b"))
(assert-eq "the one made since is still there" (so-bytes 2000 5) (store-get so_s "docs/c"))
(assert-eq "a snapshot there is not, is nothing to restore" :nil (store-restore so_s "never"))
(assert-eq "a snapshot dropped, its paths are deleted" 2 (store-snapshot-drop so_s "before"))
(assert-list-eq "and it is not listed" '(("all" 3)) (store-snapshots so_s))
(assert-eq "what is still named is kept by a sweep" (so-bytes 7000 4) (store-get so_s "snapshots/all/docs/a"))
(store-snapshot-drop so_s "all")
(store-put so_s "docs/a" (so-bytes 100 6))
(assert-true "with no snapshot, a sweep lets go of what nothing is made of" (> (store-sweep so_s) 0))

