(report-header "System: Storage, a store of five members: notes compared, blocks kept by three, a split, a gopher that goes to the blocks")

(import "service/storage/app.inc")

;five members of one store, each a service of its own name with a store in
;a file of its own, all on this node. Their tenders look only when told,
;so each step here is done when it says it is. A member that is not there
;is not waited for at all, grace 0
(defq sn_pid (str (pii-pid)) sn_group (cat "*StoreMembers" sn_pid)
	sn_ids '("m-a" "m-b" "m-c" "m-d" "m-e") sn_svcs (list))

(defun sn-image (id) (cat "tests/scratch/members_" sn_pid "_" id ".img"))
(defun sn-start (id)
	(storage-start (sn-image id) :size 0x100000 :id "shared" :name (cat "@SM" sn_pid id) :block 4096
		:group sn_group :member id :tick 0 :grace 0))
(defun sn-of (id) (elem-get sn_svcs (find id sn_ids)))

(defun sn-settle (&optional svcs)
	;every tender looks, till none of them does anything, and not for ever.
	;How many times round
	(defq rounds 0 did 1)
	(while (and (> did 0) (< (++ rounds) 12))
		(setq did 0)
		(each (# (when %0 (setq did (+ did (reduce (const +) (ifn (storage-settle %0) (list 0)) 0)))))
			(ifn svcs sn_svcs)))
	rounds)

(defun sn-blocks (name svc)
	;the names of the blocks of an object, as that member has it
	(second (storage-manifest name svc)))

(defun sn-copies (names &optional svcs)
	;how many of the members keep each block
	(defq has (map (# (ifn (storage-ask %0 +storage_type_has (apply (const cat) (cat (list "") names))) ""))
		(filter (const identity) (ifn svcs sn_svcs))))
	(map (lambda (at) (length (filter (# (and (< at (length %0)) (eql (elem-get %0 at) "1"))) has)))
		(range 0 (length names))))

(defun sn-bytes (count seed)
	;blocks of 4096, no two the same
	(apply (const cat) (map (# (cat (sha256 (str seed %0)) (pad "" 4064 "."))) (range 0 count))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; three members, and all of it on each
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(each (# (push sn_svcs (sn-start %0))) (slice sn_ids 0 3))
(assert-true "three members are started" (every (const identity) sn_svcs))
(when (every (const identity) sn_svcs)
	(assert-list-eq "each knows only itself, it has met nobody" '("m-b") (storage-members :nil (sn-of "m-b")))
	(sn-settle)
	(assert-list-eq "they meet: each knows the three" '("m-a" "m-b" "m-c") (storage-members :nil (sn-of "m-b")))
	(defq sn_big (sn-bytes 20 "big"))
	(assert-eq "an object of 20 blocks is put at one" :ok (first (storage-put "big" sn_big :nil (sn-of "m-a"))))
	(assert-list-eq "that one member has it, of three" '(1 3) (storage-seen "big" (sn-of "m-a")))
	(assert-eq "another has not heard of it" :nil (storage-stat "big" (sn-of "m-b")))
	(sn-settle)
	(assert-list-eq "they meet: all three have it" '(3 3) (storage-seen "big" (sn-of "m-a")))
	(assert-list-eq "as the others know too" '(3 3) (storage-seen "big" (sn-of "m-c")))
	(defq sn_names (sn-blocks "big" (sn-of "m-b")))
	(assert-list-eq "with three members and three copies, every block is on all three" '(3)
		(reduce (# (if (find %1 %0) %0 (push %0 %1))) (sn-copies sn_names) (list)))
	(assert-eq "it is had from one it was not put at" sn_big (storage-get "big" (sn-of "m-c")))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; five members, and three keep a block
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(each (# (push sn_svcs (sn-start %0))) (slice sn_ids 3 5))
	(assert-true "two more are started" (every (const identity) sn_svcs))
	(sn-settle)
	(assert-list-eq "all five know the five" sn_ids (storage-members :nil (sn-of "m-e")))
	(assert-list-eq "and all five have the path" '(5 5) (storage-seen "big" (sn-of "m-d")))
	(assert-list-eq "every block is kept by three of the five, no more and no fewer" '(3)
		(reduce (# (if (find %1 %0) %0 (push %0 %1))) (sn-copies sn_names) (list)))
	(assert-list-eq "the manifest is kept by all five" '(5)
		(sn-copies (list (elem-get (storage-stat "big" (sn-of "m-a")) +store_entry_hash))))
	(defq sn_held (map (# (length (filter (lambda (n) (= n 1)) (sn-copies sn_names (list %0))))) sn_svcs))
	(assert-eq "60 copies of the 20 blocks in all" 60 (reduce (const +) sn_held 0))
	(assert-true "no member has them all, and none has none" (every (# (< 0 %0 20)) sn_held))
	(assert-eq "the one it was put at has let go of what is others' to keep" :t (< (first sn_held) 20))
	(assert-true "it is had whole from any of them, from the others what one has not"
		(every (# (eql sn_big (storage-get "big" %0))) sn_svcs))
	(assert-eq "and a part of it" (slice sn_big 4000 9000) (storage-read "big" 4000 5000 (sn-of "m-e")))

	;a gopher goes where the blocks are
	(defq sn_sum (reduce (# (+ %0 (code sn_big 1 %1))) (range 0 (length sn_big) 97) 0)
		sn_back (storage-walk "big" 0 '(lambda (sum data at count)
			(list (reduce (# (+ %0 (code data 1 %1))) (range (% (- 97 (% (* at 4096) 97)) 97) (length data) 97) sum) (inc at)))
			:nil 30 (sn-of "m-a")))
	(assert-eq "a gopher sent to an object no member has all of comes back with the answer" sn_sum (first sn_back))
	(defq sn_went (map (const first) (second sn_back)))
	(assert-true "it had to go to more than one member for it" (> (length (reduce (# (if (find %1 %0) %0 (push %0 %1))) sn_went (list))) 1))
	(assert-list-eq "it began at the member it was sent from, at the first block" '("m-a" 0) (first (second sn_back)))
	(assert-true "and went only to members" (every (# (find %0 sn_ids)) sn_went))
	(assert-true "each time it was sent on it was for a block no earlier, and the member it went to has that block"
		(every (lambda ((who at) (was before)) (and (>= at before)
				(eql (storage-ask (sn-of who) +storage_type_has (elem-get sn_names at)) "1")))
			(rest (second sn_back)) (second sn_back)))

	;;;;;;;;;;;;;;;;;;;;
	; one is cut off
	;;;;;;;;;;;;;;;;;;;;

	;m-b is taken away: it can reach none of them, nor they it
	(defq sn_rest (filter (# (nql %0 "m-b")) sn_ids))
	(storage-cut sn_rest (sn-of "m-b"))
	(each (# (storage-cut '("m-b") (sn-of %0))) sn_rest)
	(storage-put "user/me/notes" "written at home" :nil (sn-of "m-b"))
	(storage-put "plan" "the laptop's plan" :nil (sn-of "m-b"))
	(storage-put "plan" "the laptop's plan, again" :nil (sn-of "m-b"))
	(storage-put "plan" "the rack's plan" :nil (sn-of "m-a"))
	(storage-put "rack/only" "written on the rack" :nil (sn-of "m-d"))
	(sn-settle)
	(assert-list-eq "what the one away wrote, it alone has: seen by 1 of 5" '(1 5) (storage-seen "plan" (sn-of "m-b")))
	(assert-eq "a writer there that waits for most to have it is told they have not, and does not wait for ever" :nil
		(storage-wait "plan" 0 :nil (sn-of "m-b")))
	(assert-list-eq "one on the rack that waits is told they have, at once" '(4 5) (storage-wait "plan" 0 :nil (sn-of "m-a")))
	(assert-list-eq "one that asks for only itself has that anywhere" '(1 5) (storage-wait "plan" 0 1 (sn-of "m-b")))
	(assert-eq "and for all five, not while one is away" :nil (storage-wait "plan" 0 5 (sn-of "m-a")))
	(assert-list-eq "what the rack wrote, the four have: 4 of 5" '(4 5) (storage-seen "plan" (sn-of "m-c")))
	(assert-eq "the one away has not heard of what the rack wrote" :nil (storage-stat "rack/only" (sn-of "m-b")))
	(assert-eq "nor the rack of what it wrote" :nil (storage-stat "user/me/notes" (sn-of "m-a")))
	(assert-eq "it still has what it had of the big one, and can not have the rest" :nil (storage-get "big" (sn-of "m-b")))
	;it comes back
	(each (# (storage-cut (list) %0)) sn_svcs)
	(sn-settle)
	(assert-eq "it comes back: what it alone wrote is on the rack, as it wrote it" "written at home"
		(storage-get "user/me/notes" (sn-of "m-e")))
	(assert-eq "what the rack wrote is on it" "written on the rack" (storage-get "rack/only" (sn-of "m-b")))
	(assert-eq "the path both wrote is what most had, though the one away wrote it twice" "the rack's plan"
		(storage-get "plan" (sn-of "m-b")))
	(defq sn_aside (some (# (if (starts-with "plan~" (first %0)) (first %0))) (storage-list "" (sn-of "m-a"))))
	(assert-true "and what it wrote is kept under another name" sn_aside)
	(assert-eq "as it last wrote it, on a member that was never away" "the laptop's plan, again" (storage-get sn_aside (sn-of "m-d")))
	(assert-list-eq "all five have the path as it is now" '(5 5) (storage-seen "plan" (sn-of "m-b")))
	(assert-list-eq "and a wait for all five is answered" '(5 5) (storage-wait "plan" 0 5 (sn-of "m-b")))
	(assert-eq "the five are the same" 1
		(length (reduce (# (if (find %1 %0) %0 (push %0 %1))) (map (# (. (storage-state %0) :find :digest)) sn_svcs) (list))))
	(assert-eq "and it can have the big one again" sn_big (storage-get "big" (sn-of "m-b")))

	;;;;;;;;;;;;;;;;;;;;;;;;;;
	; pinned, and a member gone
	;;;;;;;;;;;;;;;;;;;;;;;;;;

	(storage-pin "big" :nil (sn-of "m-c"))
	(assert-true "a member pins the big one: some of it is not there yet" (nempty? (storage-pins-missing (sn-of "m-c"))))
	(sn-settle)
	(assert-list-eq "its tender looks: all of it is there" '() (storage-pins-missing (sn-of "m-c")))
	(assert-eq "the member has all 20 blocks" 20 (length (filter (# (= %0 1)) (sn-copies sn_names (list (sn-of "m-c"))))))
	(storage-pin "big" :t (sn-of "m-c"))
	(sn-settle)
	(assert-list-eq "the pin off: every block is back to three keepers" '(3)
		(reduce (# (if (find %1 %0) %0 (push %0 %1))) (sn-copies sn_names) (list)))

	;m-e is switched off
	(storage-stop (sn-of "m-e"))
	(defq sn_t0 (pii-time))
	(while (and (storage-service (cat "@SM" sn_pid "m-e")) (< (- (pii-time) sn_t0) (task-timeout 5))) (task-sleep 10000))
	(elem-set sn_svcs 4 :nil)
	(sn-settle)
	(assert-list-eq "a member is gone: every block is kept by three of the four that are left" '(3)
		(reduce (# (if (find %1 %0) %0 (push %0 %1))) (sn-copies sn_names) (list)))
	(assert-eq "it is still a member, of five" 5 (length (storage-members :nil (sn-of "m-a"))))
	(assert-eq "and the object is whole" sn_big (storage-get "big" (sn-of "m-d")))
	;and on again
	(elem-set sn_svcs 4 (sn-start "m-e"))
	(sn-settle)
	(assert-list-eq "it is back: every block is kept by three of the five" '(3)
		(reduce (# (if (find %1 %0) %0 (push %0 %1))) (sn-copies sn_names) (list)))
	(assert-eq "and it has what was written while it was gone" "the rack's plan" (storage-get "plan" (sn-of "m-e"))))

(each (# (if %0 (storage-stop %0))) sn_svcs)
(task-sleep 100000)
(each (# (pii-remove (sn-image %0))) sn_ids)
