;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Storage, scenes: a store of its own, a thing done to it, and what came of it
;
; A scene is a script in tests/storage/, a task of its own. It starts the
; members of a store of its own on the nodes of this machine, does what it
; is for, a member cut off and healed say, says what happens as it goes, and
; checks what came of it. Then it stops its members and its store is gone.
;
; It is started by (scene-run), from `storage -t name` or an app, and is
; mailed, a line each, who to tell and the group its members are to be of,
; so that what started it can record them from the first, `storage -t name
; -r file`. It tells, each a mail: "say " and a line, as it goes, and at the
; end "done " and how many of its checks held and how many did not, and
; "gone" once its members are stopped and their stores removed.
;
; The members' tenders look only when they are told, (scene-settle), so
; that each step of a scene is done when it says it is, and it comes out
; the same every time.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./app.inc")
(import "lib/files/files.inc")

(defq +scene_folder "tests/storage" +scene_ids ''("m-a" "m-b" "m-c" "m-d" "m-e"))

;;;;;;;;;;;;;;;;;;
; in a scene's task
;;;;;;;;;;;;;;;;;;

(defun scene-say (scene &rest text)
	; (scene-say scene text ...) -> scene
	(mail-send (. scene :find :reply) (cat "say " (apply (const cat) (map (const str) text))))
	scene)

(defun scene-begin (what)
	; (scene-begin what) -> scene
	;who to tell and the group, as it was mailed, and what the scene is for,
	;said first
	(bind '(reply group) (split (mail-read (task-mbox)) +storage_lf))
	(defq scene (Emap))
	(scatter scene :reply (hex-decode reply) :group group :tag (cat (str (pii-pid)) "_" (str (random 1000000)))
		:svcs (list) :images (list) :passed 0 :failed 0 :starts 0 :big :nil :near :nil :cut (list))
	(scene-say scene what)
	scene)

(defun scene-check (scene what ok)
	; (scene-check scene what ok) -> ok
	;a thing that is to be so, said with whether it is
	(if ok (. scene :insert :passed (inc (. scene :find :passed)))
		(. scene :insert :failed (inc (. scene :find :failed))))
	(scene-say scene (if ok "  ok    " "  FAIL  ") what)
	ok)

(defun scene-image (scene id)
	(cat "obj/storage_scene_" (. scene :find :tag) "_" id ".img"))

(defun scene-start (scene id)
	; (scene-start scene id) -> :nil | netid
	;a member, on a node of this machine, the next of them in turn. A little
	;store of little blocks, to see an object as many, or with (scene-big)
	;room and blocks of the length a store has when not told
	;a name is of the one start, never used again: the name of one that has
	;gone, on another node, is known here for a moment after
	(. scene :insert :starts (inc (. scene :find :starts)))
	(defq nodes (lisp-nodes :t) big (. scene :find :big) image (scene-image scene id)
		ids (map (const first) (. scene :find :svcs))
		svc (storage-start image :size (if big 0x10000000 0x400000) :block (if big 65536 4096)
			:id "scene" :name (cat "@StorageScene" (. scene :find :tag) "_" (str (. scene :find :starts)) "_" id)
			:node (unless (. scene :find :near) (elem-get nodes (% (ifn (find id +scene_ids) (length ids)) (length nodes))))
			:group (. scene :find :group) :member id :tick 0 :grace 0))
	(unless (find image (. scene :find :images)) (push (. scene :find :images) image))
	(when svc
		(. scene :insert :svcs (cat (filter (# (nql (first %0) id)) (. scene :find :svcs)) (list (list id svc)))))
	svc)

(defun scene-big (scene)
	; (scene-big scene) -> scene
	;the members started from now have room, for a tree of files
	(. scene :insert :big :t)
	scene)

(defun scene-seen-all? (scene)
	; (scene-seen-all? scene) -> :nil | :t
	;each member that is running sees all the others it is not cut off
	;from. One just started is known on another node only when its name
	;has come by in a ping. One cut off sees none, and the rest do not see
	;it: wanting more of them, a settle went round all its rounds
	(defq svcs (. scene :find :svcs) cut (. scene :find :cut)
		away (length (filter (# (find (first %0) cut)) svcs))
		want (- (length svcs) 1 away))
	(every (lambda ((id svc))
			(>= (length (filter (const nempty?) (split (ifn (storage-ask svc +storage_type_peers) "") +storage_lf)))
				(if (find id cut) 0 want)))
		svcs))

(defun scene-settle (scene)
	; (scene-settle scene) -> rounds
	;every tender looks, till none of them does anything and each sees all
	;the others, and not for ever: a round that did nothing while one did
	;not see all waits a little for the names to come round
	(defq rounds 0 did 1)
	(while (and (or (> did 0) (not (scene-seen-all? scene))) (< (++ rounds) 30))
		(if (= did 0) (task-sleep 200000))
		(setq did 0)
		(each (lambda ((id svc)) (setq did (+ did (reduce (const +) (ifn (storage-settle svc) (list 0)) 0))))
			(. scene :find :svcs)))
	rounds)

(defun scene-near (scene)
	; (scene-near scene) -> scene
	;the members started from now are on the node of the scene, so what it
	;asks of them goes by no link: what is timed is the store
	(. scene :insert :near :t)
	scene)

(defun scene-members (scene ids)
	; (scene-members scene ids) -> :nil | :t
	;those members started, and met: each knows all of them
	(each (# (scene-start scene %0)) ids)
	(defq ok (= (length (. scene :find :svcs)) (length ids)))
	(when ok
		;a recorder asks each member every so often, it has them all now
		(task-sleep 300000)
		(scene-settle scene))
	ok)

(defun scene-svc (scene id)
	; (scene-svc scene id) -> :nil | netid
	(second (some (# (if (eql (first %0) id) %0)) (. scene :find :svcs))))

(defun scene-ids (scene)
	; (scene-ids scene) -> (id ...)
	;the members that are running
	(map (const first) (. scene :find :svcs)))

(defun scene-stop (scene id)
	; (scene-stop scene id) -> scene
	;a member is switched off. It is still a member, its store is kept
	(when (defq svc (scene-svc scene id))
		(storage-stop svc)
		(. scene :insert :svcs (filter (# (nql (first %0) id)) (. scene :find :svcs))))
	scene)

(defun scene-cut (scene id)
	; (scene-cut scene id) -> scene
	;a member can reach none of the others, nor they it
	(defq others (filter (# (nql (first %0) id)) (. scene :find :svcs)))
	(storage-cut (map (const first) others) (scene-svc scene id))
	(each (# (storage-cut (list id) (second %0))) others)
	(unless (find id (. scene :find :cut)) (push (. scene :find :cut) id))
	scene)

(defun scene-heal (scene)
	; (scene-heal scene) -> scene
	;all can reach all
	(each (# (storage-cut (list) (second %0))) (. scene :find :svcs))
	(. scene :insert :cut (list))
	scene)

(defun scene-bytes (count seed)
	; (scene-bytes count seed) -> str
	;blocks of 4096, no two the same
	(apply (const cat) (map (# (cat (hex-encode (sha256 (str seed %0))) (pad "" 4032 "."))) (range 0 count))))

(defun scene-keepers (scene path)
	; (scene-keepers scene path) -> (count ...)
	;how many of the members that are running keep each block of an object
	(defq svcs (map (const second) (. scene :find :svcs))
		names (ifn (some (# (second (storage-manifest path %0))) svcs) (list))
		body (apply (const cat) (cat (list "") names))
		has (map (# (ifn (storage-ask %0 +storage_type_has body) "")) svcs))
	(map (lambda (at) (length (filter (# (and (< at (length %0)) (eql (elem-get %0 at) "1"))) has)))
		(range 0 (length names))))

(defun scene-seen? (scene path id seen members)
	; (scene-seen? scene path id seen members) -> :nil | :t
	;that member says the path is seen by so many of so many
	(and (defq said (storage-seen path (scene-svc scene id)))
		(= (first said) seen) (= (second said) members)))

(defun scene-sum (data)
	; (scene-sum data) -> num
	;every 64th byte added up, what the gopher of a scene adds
	(reduce (# (+ %0 (code data 1 %1))) (range 0 (length data) 64) 0))

(defq +scene_gopher '(lambda (sum data at count)
	(list (reduce (# (+ %0 (code data 1 %1))) (range 0 (length data) 64) sum) (inc at))))

(defun scene-end (scene)
	; (scene-end scene)
	;the members settle, so what is left to fetch and let go of is done,
	;and what they say of it goes to a recorder. How it went is told, and
	;then the members are stopped, all at once, and their stores are gone
	(scene-settle scene)
	(task-sleep 500000)
	(mail-send (. scene :find :reply) (cat "done " (str (. scene :find :passed)) " " (str (. scene :find :failed))))
	(each (# (storage-send (second %0) +storage_type_stop)) (. scene :find :svcs))
	(defq t0 (pii-time))
	(while (and (nempty? (mail-enquire (cat (. scene :find :group) ","))) (< (- (pii-time) t0) 3000000))
		(task-sleep 50000))
	(task-sleep 200000)
	(each (const pii-remove) (. scene :find :images))
	;and what started it is told it is all gone, it may end now
	(mail-send (. scene :find :reply) "gone"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; for what starts a scene
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scene-list ()
	; (scene-list) -> ((name what) ...)
	;the scenes there are, and what each is for, its first line
	(map (lambda (file)
			(defq name (slice (last (split file "/")) 0 -6)
				what (ifn (some (lambda (line) (if (starts-with ";" line) (trim-start line ";"))) (split (ifn (load file) "") +storage_lf)) ""))
			(list name what))
		(sort (filter (# (ends-with ".lisp" %0)) (files-all +scene_folder '(".lisp"))))))

(defun scene-sweep ()
	; (scene-sweep) -> count
	;the stores of scenes whose process has gone are removed. A scene cut
	;short, its test given up on or its node gone, never stopped its members
	;nor removed their stores, and its images were left in obj/. Each has
	;the process of its scene in its name, how many were removed
	(defq gone 0)
	(each! (lambda (file type)
			(when (and (eql type "8") (starts-with "storage_scene_" file)
					(or (ends-with ".img" file) (ends-with ".img.member" file))
					(not (pii-alive (str-as-num (first (split (slice file 14 -1) "_"))))))
				(pii-remove (cat "obj/" file))
				(++ gone)))
		(unzip (split (pii-dirlist "obj") ",") 2))
	gone)

(defun scene-run (name say &optional record wait)
	; (scene-run name say [record wait]) -> :nil | (passed failed)
	;the scene of that name is started, and each line it says given to say.
	;With record, each line its members record, given to that, and :nil
	;when nothing has come for a tenth of a second. :nil if
	;there is no such scene, or it is not done in wait seconds, 600. Its
	;names are its own, scene_..., so that a function it is given, which is
	;called here, finds its own and not these
	(defq scene_file (cat +scene_folder "/" name ".lisp"))
	(when (/= (age scene_file) 0)
		(scene-sweep)
		(defq scene_said (mail-mbox) scene_heard (mail-mbox) scene_group (cat "*StorageScene" (str (pii-pid)) "_" (str (random 1000000)))
			scene_child (open-child scene_file +kn_call_run) scene_result :nil scene_going :t scene_next 0
			scene_until (+ (pii-time) (task-timeout (ifn wait 600))) scene_heard_at (pii-time))
		(when (/= 0 (get-long scene_child 0))
			(mail-send scene_child (join (list (hex-encode scene_said) scene_group) +storage_lf))
			;and not for ever: a scene that has gone, it threw, says nothing more,
			;and is given up on when it and its members have been quiet a minute
			(while (and scene_going (< (pii-time) scene_until) (< (- (pii-time) scene_heard_at) (task-timeout 60)))
				;a recorder asks each member again, and asks those started since
				(when (and record (>= (pii-time) scene_next))
					(each (# (storage-send %0 +storage_type_record (hex-encode scene_heard))) (storage-record-members scene_group))
					(setq scene_next (+ (pii-time) 200000)))
				(defq scene_timer (mail-timeout (mail-mbox) 100000 0) scene_idx (mail-select (list scene_said scene_heard scene_timer)))
				(when (< scene_idx 2)
					(mail-timeout scene_timer 0 0)
					(setq scene_heard_at (pii-time)))
				(case scene_idx
					(0 (defq scene_msg (mail-read scene_said))
						(cond
							((starts-with "say " scene_msg) (say (slice scene_msg 4 -1)))
							((starts-with "done " scene_msg)
								(setq scene_going :nil scene_result (map (const str-as-num) (split (slice scene_msg 5 -1) " "))))))
					(1 (each record (split (mail-read scene_heard) +storage_lf)))
					;nothing for a while, which a record that holds lines back is told
					(:t (if record (record :nil))))))
		;one given up on says why, as its last line
		(unless scene_result
			(say (cat "given up on, " (cond
				((>= (pii-time) scene_until) (cat "not done in " (str (ifn wait 600)) "s"))
				((>= (- (pii-time) scene_heard_at) (task-timeout 60)) "nothing said for 60s")
				("it went")))))
		;what the members still have to say, till they have said nothing for
		;half a second. What comes after that is to a mailbox nobody reads
		(and record scene_result
			(while (defq scene_msg (mail-read-timeout scene_heard 500000))
				(each record (split scene_msg +storage_lf))))
		;and that the scene has stopped its members and its store is gone:
		;what started it may end as soon as this comes back, and the scene
		;with it, before it had
		(when scene_result
			(defq scene_gone (+ (pii-time) (task-timeout 10)))
			(while (and (< (pii-time) scene_gone)
					(defq scene_msg (mail-read-timeout scene_said (task-timeout 10)))
					(nql scene_msg "gone"))))
		scene_result))
