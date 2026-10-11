(report-header "System: Storage, the service that keeps a store, asked by mail, and a gopher sent to an object")

(import "service/storage/app.inc")

;a service of a name of its own, on a store in a file of its own: it is
;not the system's, and two runs at once are not each other's
(defq sg_name (cat "@StoreTest" (str (pii-pid)))
	sg_image (cat "tests/scratch/store_" (str (pii-pid)) ".img")
	sg_group (cat "*StoreTest" (str (pii-pid)))
	sg_svc (storage-start sg_image :size 0x400000 :id "testing" :name sg_name :block 4096 :group sg_group :tick 0))

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
	(assert-eq "one whose function throws comes back with nothing, and the service is still there" :nil
		(storage-walk "a/big" 0 '(lambda (state data at count) (throw "no" 0)) :nil :nil sg_svc))
	(assert-eq "it is" sg_big (storage-get "a/big" sg_svc))
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
