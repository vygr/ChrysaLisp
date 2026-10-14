(report-header "System: Storage, the recorder: what each member does, a line each, told to a mailbox that asked")

(import "service/storage/app.inc")

;three members of one store on this node, their tenders look only when
;told, as in test_storage_members
(defq sr_pid (str (pii-pid)) sr_group (cat "*StoreRecord" sr_pid)
	sr_ids '("r-a" "r-b" "r-c") sr_svcs (list) sr_box (mail-mbox) sr_lines (list))

(defun sr-image (id) (cat "tests/scratch/record_" sr_pid "_" id ".img"))
(defun sr-of (id) (elem-get sr_svcs (find id sr_ids)))
(defun sr-settle ()
	(defq rounds 0 did 1)
	(while (and (> did 0) (< (++ rounds) 12))
		(setq did 0)
		(each (lambda (svc) (setq did (+ did (reduce (const +) (ifn (storage-settle svc) (list 0)) 0)))) sr_svcs)))
(defun sr-take ()
	;what has come, till nothing more does for a while, a line each
	(while (defq text (mail-read-timeout sr_box 300000))
		(each (lambda (line) (push sr_lines (split line " "))) (split text +storage_lf))))
(defun sr-did (member kind &optional what)
	;the lines of that member and kind, of that path or block if given
	(filter (lambda (line) (and (eql (second line) member) (eql (third line) kind)
			(or (not what) (eql (last line) what))))
		sr_lines))

(each (lambda (id) (push sr_svcs (storage-start (sr-image id) :size 0x100000 :id "shared" :name (cat "@SR" sr_pid id)
	:block 4096 :group sr_group :member id :tick 0 :grace 0))) sr_ids)
(assert-true "three members are started" (every (const identity) sr_svcs))
(when (every (const identity) sr_svcs)
	(sr-settle)
	(assert-eq "the members of the group are found to record" 3 (length (storage-record-members sr_group)))
	;nothing is said till it is asked for
	(storage-put "before" "not recorded" :nil (sr-of "r-a"))
	(sr-take)
	(assert-eq "with no recorder nothing is said" 0 (length sr_lines))
	(assert-true "each is asked to tell this mailbox" (every (lambda (svc) (storage-record-ask svc sr_box)) sr_svcs))
	;a put at one, and the others meet it
	(defq sr_data (apply (const cat) (map (# (cat (sha256 (str "rec" %0)) (pad "" 4064 "."))) (range 0 3))))
	(storage-put "doc/one" sr_data :nil (sr-of "r-a"))
	(sr-settle)
	(sr-take)
	(defq sr_set (sr-did "r-a" "set" "doc/one"))
	(assert-eq "the put is said by the member it was put at" 1 (length sr_set))
	(assert-eq "with the name of the object and its length" (str (length sr_data)) (elem-get (first sr_set) 4))
	(assert-eq "blocks it was sent and kept new, the three, the manifest is of the put" 3 (length (sr-did "r-a" "block+")))
	(assert-true "the others learn of it, from it" (every (lambda (id)
		(some (# (eql (elem-get %0 4) "r-a")) (sr-did id "learnt" "doc/one"))) '("r-b" "r-c")))
	(assert-true "and their tenders say whom they met" (nempty? (sr-did "r-b" "meet")))
	(assert-true "a block one fetched it says it fetched, and from whom" (some (# (eql (last %0) "r-a"))
		(cat (sr-did "r-b" "fetched") (sr-did "r-c" "fetched"))))
	(assert-true "a line begins with a time" (every (# (> (str-as-num (first %0)) 0)) sr_lines))
	;a gopher
	(setq sr_lines (list))
	(storage-walk "doc/one" 0 '(lambda (state data at count) (list (+ state (length data)) (inc at))) :nil 30 (sr-of "r-b"))
	(sr-take)
	(assert-true "a gopher is said where it starts" (nempty? (sr-did "r-b" "gopher" "doc/one")))
	(assert-true "each block it is given is said" (>= (+ (length (sr-did "r-a" "gopher-step")) (length (sr-did "r-b" "gopher-step"))
		(length (sr-did "r-c" "gopher-step"))) 3))
	(assert-true "and where it is done" (some (# (nempty? (sr-did %0 "gopher-done" "doc/one"))) sr_ids))
	;a delete, and then no recorder
	(setq sr_lines (list))
	(storage-delete "doc/one" :nil (sr-of "r-c"))
	(sr-take)
	(assert-eq "a delete is said" 1 (length (sr-did "r-c" "delete" "doc/one")))
	;two recorders at once, each is told all of it
	(defq sr_box2 (mail-mbox))
	(each (lambda (svc) (storage-record-ask svc sr_box2)) sr_svcs)
	(storage-put "both" "told to two" :nil (sr-of "r-a"))
	(sr-take)
	(defq sr_two (list))
	(while (defq text (mail-read-timeout sr_box2 300000))
		(each (lambda (line) (push sr_two (split line " "))) (split text +storage_lf)))
	(assert-true "a second recorder is told, and the first still is"
		(and (nempty? (sr-did "r-a" "set" "both")) (some (# (and (eql (third %0) "set") (eql (last %0) "both"))) sr_two)))
	;the second stops, the first is still told
	(each (lambda (svc) (storage-record-ask svc sr_box2 :t)) sr_svcs)
	(setq sr_lines (list))
	(storage-put "one" "told to one" :nil (sr-of "r-a"))
	(sr-take)
	(assert-true "one that stops is not told, the other still is"
		(and (nempty? (sr-did "r-a" "set" "one")) (not (mail-read-timeout sr_box2 300000))))
	(each (lambda (svc) (storage-record-ask svc :nil)) sr_svcs)
	;what they have now, as a recording from the start would have left it,
	;once they have all met
	(sr-settle)
	(defq sr_seed (list))
	(storage-record-seed (lambda (line) (push sr_seed (split line " "))) sr_group)
	(defq sr_paths (map (const first) (storage-list "" (sr-of "r-a"))))
	(assert-true "a seed says each member has each path"
		(every (lambda (id) (every (lambda (path)
			(some (# (and (eql (second %0) id) (eql (third %0) "learnt") (eql (join (slice %0 5 -1) " ") path))) sr_seed))
				sr_paths)) sr_ids))
	(assert-true "and what each object is made of" (some (# (eql (third %0) "object")) sr_seed))
	(defq sr_one_blocks (second (storage-manifest "doc/one" (sr-of "r-a"))))
	(assert-eq "doc/one is deleted, and is not in it" :nil (find "doc/one" sr_paths))
	(assert-true "the blocks each keeps, as many as it says it has of them"
		(every (lambda (id)
			(defq said (length (filter (# (and (eql (second %0) id) (eql (third %0) "block+"))) sr_seed)))
			(> said 0))
			sr_ids))
	(assert-true "who each sees" (some (# (eql (third %0) "peer+")) sr_seed))
	(setq sr_lines (list))
	(storage-put "after" "not recorded" :nil (sr-of "r-a"))
	(sr-settle)
	(sr-take)
	(assert-eq "told to stop, nothing more is said" 0 (length sr_lines)))

(each (# (if %0 (storage-stop %0))) sr_svcs)
(task-sleep 200000)
(each (# (pii-remove (sr-image %0))) sr_ids)

