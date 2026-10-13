(report-header "System: Storage, a store of five members: notes compared, blocks kept by three, a split, a gopher that goes to the blocks")

(import "service/storage/app.inc")
(import "service/storage/gopher.inc")

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
		sn_visit '(lambda (sum data at count)
			(list (reduce (# (+ %0 (code data 1 %1))) (range (% (- 97 (% (* at 4096) 97)) 97) (length data) 97) sum) (inc at)))
		sn_back (storage-walk "big" 0 sn_visit :nil 30 (sn-of "m-a")))
	(assert-eq "a gopher sent to an object no member has all of comes back with the answer" sn_sum (first sn_back))
	(defq sn_went (map (const first) (second sn_back)))
	(assert-true "it had to go to more than one member for it" (> (length (reduce (# (if (find %1 %0) %0 (push %0 %1))) sn_went (list))) 1))
	(assert-list-eq "it began at the member it was sent from, at the first block" '("m-a" 0) (first (second sn_back)))

	;where it is to go is worked out, and nobody is asked
	(defq sn_peers (map (# (list %0 (sn-of %0))) '("m-b" "m-c" "m-d" "m-e"))
		sn_route (gopher-route sn_names 0 "m-a" sn_peers 3 (list))
		sn_keep (store-rank (first sn_names) sn_ids 3)
		sn_run (lambda (where) (defq who (some (# (if (eql (second %0) where) (first %0))) sn_peers))
			(ifn (some (# (unless (find who (store-rank %0 sn_ids 3)) (!))) sn_names) (length sn_names))))
	(assert-list-eq "a route is the other members that are to keep the block"
		(sort (filter (# (nql %0 "m-a")) sn_keep))
		(sort (map (lambda (where) (some (# (if (eql (second %0) where) (first %0))) sn_peers)) sn_route)))
	(assert-true "the one that is to keep the longest run of the blocks after it first"
		(every (# (>= (sn_run (first sn_route)) (sn_run %0))) sn_route))
	(assert-true "and each of them has it, the store is as it is to be"
		(every (# (eql "1" (storage-ask %0 +storage_type_has (first sn_names)))) sn_route))
	(assert-list-eq "one that did not answer before is left out" (rest sn_route)
		(gopher-route sn_names 0 "m-a" sn_peers 3 (list (hex-encode (first sn_route)))))
	;two at once at the one member, and each is of what the object was when it set out
	(defq sn_two (mail-mbox) sn_len '(lambda (sum data at count) (list (+ sum (length data)) (inc at))))
	(storage-put "ver" (sn-bytes 2 "was") :nil (sn-of "m-a"))
	(defq sn_was (hex-encode (elem-get (storage-stat "ver" (sn-of "m-a")) +store_entry_hash)))
	(storage-put "ver" (sn-bytes 3 "is") :nil (sn-of "m-a"))
	(gopher-pass (list (sn-of "m-a")) sn_two "ver" 0 0 sn_len (list) (list) 5 sn_was)
	(gopher-pass (list (sn-of "m-a")) sn_two "ver" 0 0 sn_len (list) (list) 5)
	(gopher-pass (list (sn-of "m-a")) sn_two "big" 0 0 sn_visit (list) (list) 5)
	(defq sn_said (map (lambda (i) (if (defq said (mail-read-timeout sn_two (task-timeout 30))) (first (first (read (string-stream said))))))
		(range 0 3)))
	(assert-list-eq "three gophers at a member at once all come back, one of what the object was, one of what it is"
		(sort (list (length (sn-bytes 2 "was")) (length (sn-bytes 3 "is")) sn_sum) (const -)) (sort (map (# (ifn %0 0)) sn_said) (const -)))

	;the dial: how many keep the blocks of the one object
	(defq sn_uniq (# (reduce (lambda (out n) (if (find n out) out (push out n))) %0 (list)))
		sn_other (sn-blocks "ver" (sn-of "m-a")))
	(assert-eq "an object is to have as many copies as the store says" 3 (storage-copies "big" :nil (sn-of "m-c")))
	(assert-eq "it is set to five, at one member" :ok (first (storage-copies "big" 5 (sn-of "m-a"))))
	(sn-settle)
	(assert-list-eq "and every member has heard" '(5) (sn_uniq (map (# (storage-copies "big" :nil %0)) sn_svcs)))
	(assert-list-eq "every block of it is kept by all five" '(5) (sn_uniq (sn-copies sn_names)))
	(assert-list-eq "and the blocks of another object by three, as they were" '(3) (sn_uniq (sn-copies sn_other)))
	(assert-eq "a gopher sent to it now has no need to go anywhere" 1
		(length (second (storage-walk "big" 0 sn_visit :nil 30 (sn-of "m-d")))))
	(storage-copies "big" 1 (sn-of "m-e"))
	(sn-settle)
	(assert-list-eq "set to one, at another member: every block of it is kept by the one" '(1) (sn_uniq (sn-copies sn_names)))
	(assert-eq "and it is had whole, from a member that may have none of it" sn_big (storage-get "big" (sn-of "m-b")))
	(assert-eq "a gopher still comes back with the answer" sn_sum (first (storage-walk "big" 0 sn_visit :nil 30 (sn-of "m-a"))))
	(storage-copies "big" 0 (sn-of "m-b"))
	(sn-settle)
	(assert-list-eq "set back to what the store says: three" '(3) (sn_uniq (sn-copies sn_names)))
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
	;every member is told who has it pinned, with the path map, so a gopher
	;is sent to that member and has nobody to ask
	(assert-list-eq "every member knows who has it pinned" '(("m-c") ("m-c") ("m-c") ("m-c") ("m-c"))
		(map (# (storage-pinned-by "big" %0)) sn_svcs))
	(defq sn_pinned_walk (storage-walk "big" 0 sn_visit :nil 30 (sn-of "m-a")))
	(assert-eq "a gopher sent to it comes back with the answer" sn_sum (first sn_pinned_walk))
	(assert-true "by way of the member that has it pinned, at most the one move, and it ends there"
		(and (<= (length (second sn_pinned_walk)) 2) (eql (str (first (last (second sn_pinned_walk)))) "m-c")))
	(storage-pin "big" :t (sn-of "m-c"))
	(sn-settle)
	(assert-list-eq "the pin off: every member knows nobody has it pinned" '(() () () () ())
		(map (# (storage-pinned-by "big" %0)) sn_svcs))
	(assert-list-eq "the pin off: every block is back to three keepers" '(3)
		(reduce (# (if (find %1 %0) %0 (push %0 %1))) (sn-copies sn_names) (list)))

	;m-e is switched off
	(storage-stop (sn-of "m-e"))
	(defq sn_t0 (pii-time))
	(while (and (storage-service (cat "@SM" sn_pid "m-e")) (< (- (pii-time) sn_t0) (task-timeout 5))) (task-sleep 10000))
	(elem-set sn_svcs 4 :nil)
	;before any of them has looked: a gopher goes by those that are there
	(assert-eq "a member is gone, and a gopher comes back with the answer by way of the others" sn_sum
		(first (storage-walk "big" 0 sn_visit :nil 30 (sn-of "m-a"))))
	;sent on to a member whose node does not start it, it goes to the next
	(defq sn_gone (cat (slice (sn-of "m-b") 0 +mailbox_id_size)
			(slice (cat (sha256 "no such node") (sha256 "at all")) 0 (- (length (sn-of "m-b")) +mailbox_id_size)))
		sn_mbox (mail-mbox))
	(assert-eq "a gopher sent on goes past a member that does not take it, to one that does" (sn-of "m-b")
		(gopher-pass (list sn_gone (sn-of "m-b")) sn_mbox "big" 0 0 sn_visit (list) (defq sn_skip (list)) 1))
	(assert-list-eq "the one that did not is not to be asked again" (list (hex-encode sn_gone)) sn_skip)
	(defq sn_said (mail-read-timeout sn_mbox (task-timeout 30)))
	(assert-eq "and comes back with the answer" sn_sum (if sn_said (first (first (read (string-stream sn_said))))))
	(assert-eq "none of them takes it: it is not sent on" :nil
		(gopher-pass (list sn_gone) sn_mbox "big" 0 0 sn_visit (list) (list) 1))
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
	(assert-eq "and it has what was written while it was gone" "the rack's plan" (storage-get "plan" (sn-of "m-e")))

	;an object as a stream, by what knows nothing of a store
	(defq sn_stream (storage-stream "big" (sn-of "m-a")) sn_parts (list ""))
	(assert-true "an object no member has all of is opened as a stream" sn_stream)
	(when sn_stream
		(while (defq sn_part (read-blk sn_stream 10000)) (push sn_parts sn_part))
		(assert-true "read to the end in pieces, it is the object" (eql sn_big (apply (const cat) sn_parts)))
		(assert-eq "sought from the start, that is where it is" 4000 (stream-seek sn_stream 4000 0))
		(assert-true "and read from there" (eql (slice sn_big 4000 9000) (read-blk sn_stream 5000)))
		(assert-eq "sought on from where it is" 19000 (stream-seek sn_stream 10000 1))
		(assert-true "and read from there" (eql (slice sn_big 19000 19100) (read-blk sn_stream 100)))
		(assert-eq "sought back from where it is" 18100 (stream-seek sn_stream -1000 1))
		(assert-true "and read from there" (eql (slice sn_big 18100 18200) (read-blk sn_stream 100)))
		(assert-eq "sought from the end" (- (length sn_big) 50) (stream-seek sn_stream -50 2))
		(assert-true "and read from there, to the end" (eql (slice sn_big -51 -1) (read-blk sn_stream 50)))
		(assert-eq "and there is no more" :nil (read-char sn_stream)))
	(storage-put "text" (apply (const cat) (map (# (cat "line " (str %0) (ascii-char 10))) (range 0 2000))) :nil (sn-of "m-b"))
	(sn-settle)
	(defq sn_stream (storage-stream "text" (sn-of "m-d") 2) sn_count 0 sn_last "")
	(when sn_stream (lines! (lambda (sn_line) (++ sn_count) (setq sn_last sn_line) :nil) sn_stream))
	(assert-eq "an object of lines is read a line at a time, across its blocks" 2000 sn_count)
	(assert-eq "to the last" "line 1999" sn_last)
	(assert-eq "no object of that path, no stream" :nil (storage-stream "no such" (sn-of "m-a")))

	;how often they look is changed as they run
	(storage-put "paced" "one" :nil (sn-of "m-a"))
	(task-sleep 200000)
	(assert-list-eq "told only: a write stays where it was put" '(1 5) (storage-seen "paced" (sn-of "m-a")))
	(each (# (storage-pace 50000 0 %0)) sn_svcs)
	(assert-list-eq "by the clock: all five have it, none told to look" '(5 5) (storage-wait "paced" 10 5 (sn-of "m-a")))
	(each (# (storage-pace 0 0 %0)) sn_svcs)
	(task-sleep 200000)
	(storage-put "paced2" "two" :nil (sn-of "m-a"))
	(task-sleep 300000)
	(assert-list-eq "told only again: a write stays where it was put" '(1 5) (storage-seen "paced2" (sn-of "m-a"))))

(each (# (if %0 (storage-stop %0))) sn_svcs)
(task-sleep 100000)
(each (# (pii-remove (sn-image %0))) sn_ids)

;a member on another node of this machine, the only service of its own
;that node has. Stopped, its name is gone from here: a node whose last @
;service was forgotten did not say so, and the others held it for ever,
;sys/mail/class.vp, 2026-10-12
(defq sn_far (filter (# (nql %0 (task-nodeid))) (lisp-nodes :t)))
(when (nempty? sn_far)
	(defq sn_name (cat "@SMfar" sn_pid)
		sn_svc (storage-start (sn-image "far") :size 0x100000 :id "far" :name sn_name :block 4096
			:group (cat "*SMfar" sn_pid) :member "far" :tick 0 :grace 0 :node (last sn_far)))
	(assert-true "a member is started on another node of this machine" sn_svc)
	(when sn_svc
		(assert-true "and is on that node" (eql (slice sn_svc +mailbox_id_size -1) (last sn_far)))
		(assert-true "its name is known here" (storage-service sn_name))
		(storage-stop sn_svc)
		(defq sn_t0 (pii-time))
		(while (and (storage-service sn_name) (< (- (pii-time) sn_t0) (task-timeout 5))) (task-sleep 10000))
		(assert-eq "stopped, its name is not known here any longer" :nil (storage-service sn_name)))
	(pii-remove (sn-image "far")))

