(report-header "System: Storage, the service that keeps a store, asked by mail, and a gopher sent to an object")

(import "service/storage/app.inc")

;a service of a name of its own, on a store in a file of its own: it is
;not the system's, and two runs at once are not each other's
(defq sg_name (cat "@StoreTest" (str (pii-pid)))
	sg_image (cat "tests/scratch/store_" (str (pii-pid)) ".img")
	sg_group (cat "*StoreTest" (str (pii-pid)))
	sg_svc (storage-start sg_image :size 0x100000 :id "testing" :name sg_name :block 4096 :group sg_group :tick 0))

(defun sg-walk (&rest args)
	;what a gopher came back with, in a list, less where it had been
	(if (defq back (apply (const storage-walk) args)) (list (first back))))

(defun sg-bytes (len seed)
	(defq out (cat (str-alloc len)))
	(each (# (set-byte out %0 (logand (+ (* %0 7) (>> %0 8) seed) 0xff))) (range 0 len))
	out)

(assert-true "the service is started, on a store it made" sg_svc)
(when sg_svc
	(defq sg_info (storage-info sg_svc))
	(assert-eq "it says what store it keeps" "testing" (. sg_info :find :id))
	(assert-eq "that its member is this system" (hex-encode (system-id)) (. sg_info :find :member))
	(assert-list-eq "the one member" (list (hex-encode (system-id))) (. sg_info :find :members))
	(assert-eq "the block size" 4096 (. sg_info :find :block_size))
	(assert-eq "and that there is nothing in it" 0 (. sg_info :find :objects))
	(assert-eq "started again by that name, it is the one there is" sg_svc (storage-start sg_image :name sg_name))

	;objects
	(defq sg_small (sg-bytes 100 1) sg_big (sg-bytes 10000 2))
	(assert-eq "a small object is put" :ok (first (storage-put "a/small" sg_small :nil sg_svc)))
	(assert-eq "one of three blocks" :ok (first (storage-put "a/big" sg_big :nil sg_svc)))
	(assert-eq "one of nothing" :ok (first (storage-put "a/none" "" :nil sg_svc)))
	(assert-eq "the small one is had back" sg_small (storage-get "a/small" sg_svc))
	(assert-eq "the big one" sg_big (storage-get "a/big" sg_svc))
	(assert-eq "the one of nothing" "" (storage-get "a/none" sg_svc))
	(assert-eq "what there is none of is not" :nil (storage-get "a/nothing" sg_svc))
	(assert-eq "what a path is: how long" 10000 (elem-get (storage-stat "a/big" sg_svc) +store_entry_bytes))
	(assert-eq "and of what there is none of" :nil (storage-stat "a/nothing" sg_svc))
	(assert-list-eq "the paths" '("a/big" "a/none" "a/small") (map (const first) (storage-list "" sg_svc)))
	(assert-list-eq "and those that begin so" '("a/small") (map (const first) (storage-list "a/s" sg_svc)))
	(assert-eq "part of one, across two blocks" (slice sg_big 4000 4200) (storage-read "a/big" 4000 200 sg_svc))
	(assert-eq "to past its end" (slice sg_big 9990 10000) (storage-read "a/big" 9990 500 sg_svc))
	(defq sg_blocks (. (storage-info sg_svc) :find :blocks))
	(storage-put "b/copy" sg_big :nil sg_svc)
	(assert-eq "the same bytes under another name are no more blocks" sg_blocks (. (storage-info sg_svc) :find :blocks))

	;the one way to write
	(assert-eq "a path set provided there is none" :ok (first (storage-put "c/x" "one" "" sg_svc)))
	(defq sg_h1 (elem-get (storage-stat "c/x" sg_svc) +store_entry_hash))
	(assert-list-eq "again provided there is none: there is, and it says what"
		(list :stale sg_h1)
		(progn (defq sg_r (storage-put "c/x" "two" "" sg_svc)) (list (first sg_r) (elem-get (second sg_r) +store_entry_hash))))
	(assert-eq "set from what it is" :ok (first (storage-put "c/x" "two" sg_h1 sg_svc)))
	(assert-eq "set from what it was, by one who did not see that" :stale (first (storage-put "c/x" "three" sg_h1 sg_svc)))
	(assert-eq "it is what the one who saw made it" "two" (storage-get "c/x" sg_svc))
	(assert-eq "deleted provided it is what it is not" :stale (first (storage-delete "c/x" sg_h1 sg_svc)))
	(assert-eq "deleted" :ok (first (storage-delete "c/x" :nil sg_svc)))
	(assert-eq "there is none of it" :nil (storage-get "c/x" sg_svc))
	(assert-eq "deleted again there is none to" :none (first (storage-delete "c/x" :nil sg_svc)))

	;a gopher
	(assert-list-eq "a gopher sent to an object goes to each block in turn and comes back with what it has"
		'(((0 4096) (1 4096) (2 1808)))
		(sg-walk "a/big" (list) '(lambda (state data at count) (list (push state (list at (length data))) (inc at)))
			:nil :nil sg_svc))
	(assert-list-eq "one that says it is done at the first block" '((0 3))
		(sg-walk "a/big" :nil '(lambda (state data at count) (list (list at count) :nil)) :nil :nil sg_svc))
	(assert-list-eq "one told to begin at the last" '(2)
		(sg-walk "a/big" :nil '(lambda (state data at count) (list at :nil)) 2 :nil sg_svc))
	(assert-eq "the sum of every byte, worked out where the blocks are"
		(reduce (# (+ %0 (code sg_big 1 %1))) (range 0 (length sg_big)) 0)
		(first (storage-walk "a/big" 0 '(lambda (sum data at count)
			(list (reduce (# (+ %0 (code data 1 %1))) (range 0 (length data)) sum) (inc at))) :nil :nil sg_svc)))
	(assert-list-eq "what it has can be a str with spaces and quotes in it, and comes back as it was"
		(list (cat "a " (ascii-char 34) "b" (ascii-char 34) " c 3"))
		(sg-walk "a/big" (cat "a " (ascii-char 34) "b" (ascii-char 34) " c")
			'(lambda (state data at count) (list (cat state " " (str count)) :nil)) :nil :nil sg_svc))
	(assert-list-eq "and it says where it had been, the one member, from the first block" (list (list (hex-encode (system-id)) 0))
		(second (storage-walk "a/big" 0 '(lambda (state data at count) (list state (inc at))) :nil :nil sg_svc)))
	(assert-eq "one sent to what there is none of comes back with nothing" :nil
		(storage-walk "a/nothing" 0 '(lambda (state data at count) (list state :nil)) :nil :nil sg_svc))
	;a release build is given only what is right, a function that throws
	;is one only a checked build says is wrong
	(if *test_checked*
		(assert-eq "one whose function throws comes back with nothing, and the service is still there" :nil
			(storage-walk "a/big" 0 '(lambda (state data at count) (throw "no" 0)) :nil :nil sg_svc))
		(test-skip "one whose function throws comes back with nothing" "needs an error checked build"))
	(assert-eq "it is" sg_big (storage-get "a/big" sg_svc))
	;an object written as a stream, as a file is: the path is set to it when
	;the stream is flushed, and when it is let go of
	(defq sg_out (storage-stream "w/one" +file_open_write sg_svc))
	(assert-true "a stream to write an object is given" sg_out)
	(write-blk sg_out "hello ")
	(write-blk sg_out "world")
	(assert-eq "written and not flushed, there is no object yet" :nil (storage-stat "w/one" sg_svc))
	(stream-flush sg_out)
	(assert-eq "flushed, the path is what was written" "hello world" (storage-get "w/one" sg_svc))
	(write-blk sg_out ", again")
	(assert-eq "written on after the flush, it is still what it was" "hello world" (storage-get "w/one" sg_svc))
	(setq sg_out :nil)
	(assert-eq "let go of, it is all that was written" "hello world, again" (storage-get "w/one" sg_svc))
	;of more than a block, and of blocks exactly
	(defq sg_long (sg-bytes 10000 5) sg_out (storage-stream "w/long" +file_open_write sg_svc))
	(each (# (write-blk sg_out (slice sg_long %0 (min 10000 (+ %0 700))))) (range 0 10000 700))
	(setq sg_out :nil)
	(assert-eq "10,000 bytes written 700 at a time are the object, three blocks" sg_long (storage-get "w/long" sg_svc))
	(assert-eq "its length is what was written" 10000 (third (storage-stat "w/long" sg_svc)))
	(defq sg_out (storage-stream "w/long" +file_open_write sg_svc))
	(write-blk sg_out (slice sg_long 0 8192))
	(setq sg_out :nil)
	(assert-eq "written again shorter, two blocks exactly, it is that and no more" (slice sg_long 0 8192) (storage-get "w/long" sg_svc))
	;written on the end of
	(defq sg_out (storage-stream "w/long" +file_open_append sg_svc))
	(write-blk sg_out (slice sg_long 8192 9000))
	(setq sg_out :nil)
	(assert-eq "appended to whole blocks" (slice sg_long 0 9000) (storage-get "w/long" sg_svc))
	(defq sg_out (storage-stream "w/long" +file_open_append sg_svc))
	(write-blk sg_out (slice sg_long 9000 -1))
	(stream-flush sg_out)
	(assert-eq "appended to a last short block, and over the end of it" sg_long (storage-get "w/long" sg_svc))
	(setq sg_out :nil)
	(assert-eq "let go of after its flush, with no more written, it is as it was" sg_long (storage-get "w/long" sg_svc))
	(defq sg_out (storage-stream "w/long" +file_open_append sg_svc) sg_out :nil)
	(assert-eq "opened to append and let go of with nothing written, as it was" sg_long (storage-get "w/long" sg_svc))
	(defq sg_out (storage-stream "w/new" +file_open_append sg_svc))
	(write-blk sg_out "made")
	(setq sg_out :nil)
	(assert-eq "appended to what there is none of, it is made" "made" (storage-get "w/new" sg_svc))
	(defq sg_out (storage-stream "w/new" +file_open_write sg_svc) sg_out :nil)
	(assert-eq "opened to write and let go of with nothing written, an object of nothing, as a file" 0 (third (storage-stat "w/new" sg_svc)))
	;what a stream writes, a stream reads
	(defq sg_out (storage-stream "w/lines" +file_open_write sg_svc))
	(each (# (write-line sg_out (cat "line " (str %0)))) (range 0 500))
	(setq sg_out :nil)
	(defq sg_in (storage-stream "w/lines" +file_open_read sg_svc) sg_lines (list))
	(lines! (# (push sg_lines %0) :nil) sg_in)
	(assert-eq "500 lines written with (write-line) are read with (lines!)" "(500 \qline 0\q \qline 499\q)"
		(str (list (length sg_lines) (first sg_lines) (last sg_lines))))
	(assert-true "no mode is to read, as it was" (storage-stream "w/lines" :nil sg_svc))
	;a program that is to come through anything writes a path of its own,
	;sees all went as it should, and renames it to where it is to be
	(storage-put "w/doc" "the old" :nil sg_svc)
	(defq sg_out (storage-stream "w/doc.part" +file_open_write sg_svc))
	(write-blk sg_out "the new, whole")
	(setq sg_out :nil)
	(assert-eq "written to a path of its own, the path it is for is as it was" "the old" (storage-get "w/doc" sg_svc))
	(assert-eq "renamed, it is the new" :ok (first (storage-rename "w/doc.part" "w/doc" :nil sg_svc)))
	(assert-list-eq "the new whole at the path, the path of its own gone" '("the new, whole" :nil)
		(list (storage-get "w/doc" sg_svc) (storage-stat "w/doc.part" sg_svc)))
	(assert-eq "a path that is not, renamed, is none" :none (first (storage-rename "w/no such" "w/doc" :nil sg_svc)))
	(storage-put "w/doc.part" "another" :nil sg_svc)
	(assert-eq "provided the path it is to is what it is not, stale, and nothing changed" :stale
		(first (storage-rename "w/doc.part" "w/doc" (sha256 "not it") sg_svc)))
	(assert-list-eq "both as they were" '("the new, whole" "another")
		(list (storage-get "w/doc" sg_svc) (storage-get "w/doc.part" sg_svc)))
	(assert-eq "provided it is what it is, renamed" :ok
		(first (storage-rename "w/doc.part" "w/doc" (elem-get (storage-stat "w/doc" sg_svc) +store_entry_hash) sg_svc)))
	(assert-eq "to one there is none of, provided none" :ok (first (storage-rename "w/doc" "w/fresh" "" sg_svc)))
	(assert-eq "it is there" "another" (storage-get "w/fresh" sg_svc))

	;a function has a second for a block. It is made to look at the clock
	;wherever it can go round, so one that never comes back is done with
	(defq sg_start (pii-time))
	(assert-eq "one whose function goes round for ever comes back with nothing" :nil
		(storage-walk "a/big" 0 '(lambda (state data at count) (while :t) (list state :nil)) :nil 10 sg_svc))
	(assert-true "in a second, near enough" (< 900000 (- (pii-time) sg_start) 3000000))
	(assert-eq "and one that goes round in what it gives each" :nil
		(storage-walk "a/big" 0 '(lambda (state data at count)
			(each (# (until (not %0))) (list 1 2 3)) (list state :nil)) :nil 10 sg_svc))
	(assert-eq "and one that catches what is thrown at it, and comes back late" :nil
		(storage-walk "a/big" 0 '(lambda (state data at count)
			(catch (while :t) :t) (list state :nil)) :nil 10 sg_svc))
	(assert-list-eq "what is quoted in a function is not code, it comes back as it was" '((while 1 2))
		(sg-walk "a/big" 0 '(lambda (state data at count) (list '(while 1 2) :nil)) :nil :nil sg_svc))
	(assert-eq "the gophers after them are run as ever"
		(reduce (# (+ %0 (code sg_big 1 %1))) (range 0 (length sg_big)) 0)
		(first (storage-walk "a/big" 0 '(lambda (sum data at count)
			(list (reduce (# (+ %0 (code data 1 %1))) (range 0 (length data)) sum) (inc at))) :nil :nil sg_svc)))
	;a search by halves of 4096 numbers in order, 512 a block
	(storage-put "a/sorted" (apply (const cat) (map (# (cat (pad (str %0) 7 "0") (ascii-char 10))) (range 0 4096))) :nil sg_svc)
	(defun sg-search (want)
		(first (storage-walk "a/sorted" (list want :nil 0 0 7)
			'(lambda ((want found reads lo hi) data at count)
				(defq low (str-as-num (slice data 0 7)) high (str-as-num (slice data -9 -2)))
				(cond
					((< want low) (list (list want :nil (inc reads) lo (dec at)) (if (> at lo) (/ (+ lo (dec at)) 2))))
					((> want high) (list (list want :nil (inc reads) (inc at) hi) (if (< at hi) (/ (+ (inc at) hi) 2))))
					((list (list want at (inc reads) lo hi) :nil))))
			3 :nil sg_svc)))
	(assert-list-eq "a gopher that searches by halves finds the block a number is in, in 4 reads of 8 at most"
		'(0 :t 7 :t 3 1)
		(progn (defq sg_a (sg-search 100) sg_b (sg-search 4000) sg_c (sg-search 1600))
			(list (second sg_a) (<= (third sg_a) 4) (second sg_b) (<= (third sg_b) 4) (second sg_c) (third sg_c))))

	;pins, a sweep, a check
	(assert-list-eq "a pin" '("b/") (storage-pin "b/" :nil sg_svc))
	(assert-list-eq "all of what is pinned is here" '() (storage-pins-missing sg_svc))
	(assert-list-eq "the pin off" '() (storage-pin "b/" :t sg_svc))
	(storage-delete "a/big" :nil sg_svc)
	(storage-delete "a/sorted" :nil sg_svc)
	(assert-true "a sweep lets go of the blocks of what was deleted" (> (storage-sweep sg_svc) 0))
	(assert-eq "not of what is there by another path" sg_big (storage-get "b/copy" sg_svc))
	(assert-list-eq "nothing kept is wrong" '() (storage-check :nil sg_svc))

	;stopped and started, it is what it was
	(assert-eq "the service is stopped" :t (storage-stop sg_svc))
	(defq sg_t0 (pii-time))
	(while (and (storage-service sg_name) (< (- (pii-time) sg_t0) (task-timeout 5))) (task-sleep 10000))
	(assert-eq "and is not there" :nil (storage-service sg_name))
	(assert-true "started again on the store" (setq sg_svc (storage-start sg_image :name sg_name :group sg_group :tick 0)))
	(assert-eq "what was put is there" sg_big (storage-get "b/copy" sg_svc))
	(assert-eq "what was deleted is not" :nil (storage-get "c/x" sg_svc))
	(assert-eq "it is the store it was" "testing" (. (storage-info sg_svc) :find :id))
	(storage-stop sg_svc))
(pii-remove sg_image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a machine made a member, and remembered
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;all of it in files of its own here: the machine's own are not touched
(defq sg_key (cat "tests/scratch/key_" (str (pii-pid))) sg_said (cat "tests/scratch/member_" (str (pii-pid)))
	sg_claim (cat "tests/scratch/claim_" (str (pii-pid)) ".img"))
(assert-eq "a machine with no key has no store of its mesh" :nil (storage-key-id sg_key))
(save (cat "a pass phrase" (ascii-char 10)) sg_key)
(defq sg_id (storage-key-id sg_key))
(assert-true "one with a key has: an id made from it" (and (str? sg_id) (starts-with "mesh-" sg_id) (= (length sg_id) 21)))
(assert-true "which is not the key, nor has it in it" (not (found? sg_id "pass")))
(save "a pass phrase" sg_key)
(assert-eq "the same key gives the same id, whatever is round it" sg_id (storage-key-id sg_key))
(save "another pass phrase" sg_key)
(assert-true "another key, another store" (nql sg_id (storage-key-id sg_key)))
(pii-remove sg_key)

(assert-eq "a machine that was never made a member has nothing kept" :nil (storage-member sg_said))
(storage-member-save "storage/store.img" 4096 :nil :nil sg_said)
(defq sg_was (storage-member sg_said))
(assert-eq "made one: where its store is and how big are kept" "storage/store.img 4096"
	(cat (. sg_was :find :image) " " (str (. sg_was :find :size))))
(assert-eq "an id and copies that were not said are not" "(:nil :nil)" (str (list (. sg_was :find :id) (. sg_was :find :copies))))
(storage-member-save "a folder/with a space.img" 512 "show" 2 sg_said)
(setq sg_was (storage-member sg_said))
(assert-eq "said, they are, and a path with a space in it" "a folder/with a space.img 512 show 2"
	(join (list (. sg_was :find :image) (str (. sg_was :find :size)) (. sg_was :find :id) (str (. sg_was :find :copies))) " "))
(storage-member-forget sg_said)
(assert-eq "forgotten, there is nothing kept" :nil (storage-member sg_said))
;a machine that travels says so, and is said to by itself
(storage-member-save "storage/store.img" 4096 :nil :nil sg_said)
(assert-eq "a member that was not said to travel does not" :nil (. (storage-member sg_said) :find :travels))
(assert-eq "it is made one that does" :t (storage-member-travels :t sg_said))
(assert-eq "and is" :t (. (storage-member sg_said) :find :travels))
(assert-eq "what else it was told is kept" "storage/store.img" (. (storage-member sg_said) :find :image))
(storage-member-travels :nil sg_said)
(assert-eq "and made one that does not" :nil (. (storage-member sg_said) :find :travels))
(storage-member-forget sg_said)
(assert-eq "no member, it can not travel" :nil (storage-member-travels :t sg_said))
(assert-eq "nor pin" :nil (storage-user-pin :nil sg_said))


;who serves a store: one process, the first that says so and is still there
(assert-eq "a store nobody has is this process's to serve" :t (storage-claim sg_claim))
(assert-eq "and again, it has it already" :t (storage-claim sg_claim))
(save "1" (cat sg_claim ".member"))
(if (pii-alive 1)
	(assert-eq "one that a process still there has is not" :nil (storage-claim sg_claim))
	(test-pass "one that a process still there has is not"))
(save "999999999" (cat sg_claim ".member"))
(assert-eq "one that a process that has gone had is nobody's, and is taken" :t (storage-claim sg_claim))
(storage-unclaim sg_claim)
(assert-eq "let go of, the note of it is gone" 0 (age (cat sg_claim ".member")))

;how many keep a block: three, or as many members as there are
(assert-eq "three are to keep a block: of five members three do, of two both, of one that one" "(3 2 1)"
	(str (map (# (length (store-rank "a block" %0 3))) '(("a" "b" "c" "d" "e") ("a" "b") ("a")))))
