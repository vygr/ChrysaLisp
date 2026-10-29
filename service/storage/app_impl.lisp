;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Storage, the service
;
; It keeps one store, lib/storage/store.inc, and does what it is asked of
; it by mail, service/storage/app.inc. One of a name for a system, @Storage
; if it is not given another. It is a member of the store, and is known to
; the other members by the name of their group, *Storage, with the store's
; id and its own after it.
;
; The first mail it has says what it is to keep, a line each, a word and
; what it is: ready, who to tell when it is; image, the file of the host
; the store is in; name; and, if they are given, size, how big to make a
; store where there is none, id, block, force, group, member, tick, grace,
; (storage-start).
;
; It does each thing asked before it looks at the next, so a write is all
; done or not begun as far as any other asker can see. And it only ever
; answers. It asks nothing of another member and waits: two services that
; did, each of the other, would wait for ever. What a member has to ask of
; the others, to compare notes and to fetch its share, its tender asks,
; ./tender.lisp, a task of its own that this starts. Gophers are run by
; another, ./gopher.lisp, that it starts too: a gopher sent to this member,
; by an app or by another member, is passed to that as it comes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./app.inc")

(defq +share_most 64)

(defun names-of (text)
	;names sent one after another, 32 bytes each. Only whole ones: what is
	;left over, as a peer that is not of this build may send, is no name
	(map (# (slice text %0 (+ %0 32))) (range 0 (* (/ (length text) 32) 32) 32)))

(defun said (result)
	;what a write came to, as it is sent back
	(bind '(what val) result)
	(case what
		((:ok :stale) (cat (if (eql what :ok) "ok" "stale") +storage_lf (if val (store-entry-line val) "")))
		;no more of them than a block of a manifest has, so no answer is
		;ever the whole of a big object
		(:missing (join (cat (list "missing") (map! (const hex-encode) (list val) 0 (min (length val) +store_manifest_names))) +storage_lf))
		(:full "full")
		(:t "none")))

(defun wrote (kind result path)
	;a write, recorded, and what it came to as it is sent back
	(bind '(what val) result)
	(storage-record recorder member (if (eql what :ok) kind (cat kind "-" (rest (str what))))
		(if (and (eql what :ok) val)
			(cat (storage-record-name (elem-get val +store_entry_hash)) " " (str (elem-get val +store_entry_bytes)) " " path)
			path))
	(said result))

(defun from-of (text)
	;what a path was said to be: ? not said, - none
	(cond ((eql text "?") :nil) ((eql text "-") "") ((hex-decode text))))

(defun info-text (store)
	(bind '(objects blocks free) (store-info store))
	(join (cat (list (cat "id " (get :id store)) (cat "member " (get :member store))
		(cat "members " (join (store-members store) " "))
		(cat "block_size " (str (get :block_size store))) (cat "copies " (str (get :copies store)))
		(cat "objects " (str objects)) (cat "blocks " (str blocks)) (cat "free " (str free)))
		;a line a prefix this member has pinned, as one may have spaces in it
		(map (# (cat "pin " %0)) (get :pins store)))
		+storage_lf))

;how long what was learnt from another member may wait to be written
(defq +dirty_most 2000000)

;a run of block puts is one writing of the volume, ended by anything else
;asked, or by nothing asked for this long
(defq +batch_quiet 100000)

(defun notes-text (store at)
	;what one member tells another: where its count of changes is now,
	;who the members are, and its path map, or what of it is not as it was
	;at the count it is given, one it gave before
	(join (cat (list (cat "at " (store-at store))
			(cat "members " (join (get :members store) " ")) (cat "retired " (join (get :retired store) " ")))
		;what each member has pinned, all of it each time, it is little
		(map (# (cat "pinned " %0)) (store-pinned-lines store))
		(map (const store-entry-line) (store-since store at))) +storage_lf))

(defun notes-take (store text)
	;what another member told, taken in. How many paths are not as they were
	(defq lines (split text +storage_lf) entries (list) pinned (list))
	(each (lambda (line)
		(cond
			((starts-with "at " line))
			((starts-with "pinned " line) (push pinned (slice line 7 -1)))
			((starts-with "members" line) (store-join store (rest (split line " ")) :nil :t))
			((starts-with "retired" line) (store-join store (list) (rest (split line " ")) :t))
			((defq entry (store-entry-read line)) (push entries entry))))
		lines)
	;not written to the volume now: it is when there is a moment, below
	(defq pins (if (store-pinned-take store pinned :t) 1 0) changed (store-meet store entries :t))
	(each (lambda (entry)
		(storage-record recorder member (if (elem-get entry +store_entry_deleted) "learnt-delete" "learnt")
			(cat (storage-record-name (elem-get entry +store_entry_hash)) " " (elem-get entry +store_entry_writer) " " (first entry))))
		changed)
	(+ pins (length changed)))

(defun main ()
	;a bot: it runs before the apps of its node, so a store at work is not
	;held up by them, (task-band)
	(task-band :bot)
	;what it is to keep
	(defq opts (Emap))
	(each (lambda (line)
		(if (defq at (find " " line)) (. opts :insert (sym (cat ":" (slice line 0 at))) (slice line (inc at) -1))))
		(split (mail-read (task-mbox)) +storage_lf))
	(bind '(ready image name size id block force group member tick grace copies)
		(gather opts :ready :image :name :size :id :block :force :group :member :tick :grace :copies))
	(defq ready (hex-decode ready) size (if size (str-as-num size) 0) block (if block (str-as-num block) 0)
		id (ifn id "store") name (ifn name +storage_name) group (ifn group +storage_group)
		member (ifn member (hex-encode (system-id)))
		;the store that is there. One that can not be read, a file cut
		;short, is no store, and not the end of the service. Nobody knows
		;this mailbox but its starter till it is up, so mail to it while it
		;opens is to stop: a big store's open is a minute or more, given
		;up, not finished, for a store it is to hand over
		store (if (/= (age image) 0) (catch (store-open image member (# (not (mail-poll (list (task-mbox)))))) :bad))
		store (if (find store '(:bad :stopped)) :nil store))
	;or a new one, where there is nothing. Where there is something that
	;is no store, only if it was said to
	(and (not store) (> size 0) (or (= (age image) 0) force)
		;the volume has the store's id as its label, as much of it as a label holds
		(store-make image size id member (if (> block 0) block) (slice id 0 (min 11 (length id)))
			(if copies (str-as-num copies)))
		(setq store (store-open image member)))
	(cond
		((or (not store) (nempty? (mail-enquire (cat name ","))))
			(mail-send ready "no"))
		(:t (defq service (mail-declare (task-mbox) name "Storage Service 0.2")
				;how the members of a store find each other, whatever machine they are on
				known (mail-declare (task-mbox) group (cat (get :id store) " " member))
				running :t cut (list) peers (list) recorder (storage-record-new) batch :nil stopped :nil
				tender (open-child "service/storage/tender.lisp" +kn_call_pin)
				runner (open-child "service/storage/gopher.lisp" +kn_call_pin))
			(if (/= 0 (get-long runner 0)) (mail-send runner (hex-encode (task-mbox))))
			(if (/= 0 (get-long tender 0))
				(mail-send tender (join (list (hex-encode (task-mbox)) group (ifn tick (str +storage_tick))
					(ifn grace (str +storage_grace))) +storage_lf)))
			(mail-send ready "ok")
			(while running
				;a run of block puts is one writing of the volume, so what they
				;are goes to the device together, the blocks next to each other
				;and their folder once: a flash stick is slow at many small
				;writes. Anything else asked ends it first, so a path is set
				;only to blocks that are on the device, and so does nothing
				;asked for a moment
				;waited for, not looked for now and then: the next block is
				;here as soon as it is sent
				(defq msg (and batch (mail-read-timeout (task-mbox) +batch_quiet)))
				(unless msg
					(if batch (setq batch (progn (exfat-end batch) :nil)))
					(setq msg (mail-read (task-mbox))))
				;a stop to its open that came as it was up, (storage-start),
				;is no request and is let go
				(while (< (length msg) +storage_rpc_size) (setq msg (mail-read (task-mbox))))
				(defq reply (getf msg +storage_rpc_reply_id)
					type (getf msg +storage_rpc_type) body (slice msg +storage_rpc_size -1))
				(cond
					((= type +storage_type_block_put)
						(unless batch (setq batch (exfat-begin (get :vol store)))))
					(batch (setq batch (progn (exfat-end batch) :nil))))
				;a thing asked that throws is said to have, and the service goes on
				(defq answer (catch (case type
						(+storage_type_info (info-text store))
						(+storage_type_stat
							;with a ? before the path, what it is though it be deleted
							(defq entry (if (starts-with "?" body) (store-find store (rest body)) (store-stat store body)))
							(if entry (store-entry-line entry) ""))
						(+storage_type_list
							(join (map (const store-entry-line) (store-list store body)) +storage_lf))
						(+storage_type_has
							(apply (const cat) (cat (list "")
								(map (# (if (store-block-has? store %0) "1" "0")) (names-of body)))))
						(+storage_type_block_put
							;new here if there are more kept after: the block is hashed the once
							(defq had (get :blocks store) got (store-block-put store body))
							(and got (> (get :blocks store) had)
								(storage-record recorder member "block+" (storage-record-name got)))
							(ifn got ""))
						(+storage_type_block_get
							(defq got (store-block-get store body))
							(if got (storage-record recorder member "block>" (storage-record-name body)))
							(ifn got ""))
						(+storage_type_block_drop
							(cond
								((store-block-drop store body)
									(storage-record recorder member "block-" (storage-record-name body))
									"1")
								("0")))
						(+storage_type_commit
							;the name of the top of its manifest, whose blocks are
							;all here, as the object's are, (store-commit-root)
							(bind '(from bytes path top) (split body +storage_lf))
							(defq got (store-commit-root store path (hex-decode (ifn top "")) (from-of from)))
							;what an object is made of, so a recording can draw it
							(and (eql (first got) :ok) (second got) (storage-recording? recorder)
								(storage-record recorder member "object" (cat (storage-record-name (elem-get (second got) +store_entry_hash)) " "
									(join (map (const storage-record-name) (second (store-manifest store (elem-get (second got) +store_entry_hash)))) " "))))
							(defq result (wrote "set" got path))
							;its tender is to tell the others, now and not at its next look
							(if (/= 0 (get-long tender 0)) (mail-send tender "kick"))
							result)
						(+storage_type_delete
							(defq at (find +storage_lf body)
								result (wrote "delete" (store-delete store (slice body (inc at) -1) (from-of (slice body 0 at))) (slice body (inc at) -1)))
							(if (/= 0 (get-long tender 0)) (mail-send tender "kick"))
							result)
						(+storage_type_rename
							(bind '(was from to) (split body +storage_lf))
							(defq got (store-rename store from to (from-of was)) result (wrote "set" got to))
							(if (eql (first got) :ok) (storage-record recorder member "delete" from))
							(if (/= 0 (get-long tender 0)) (mail-send tender "kick"))
							result)
						(+storage_type_copies
							(defq at (find +storage_lf body)
								result (wrote "copies" (store-copies store (slice body (inc at) -1) (str-as-num (slice body 0 at))) (slice body (inc at) -1)))
							(if (/= 0 (get-long tender 0)) (mail-send tender "kick"))
							result)
						(+storage_type_pin
							(storage-record recorder member "pin" body)
							(store-pin store (rest body) (eql (first body) "-"))
							(join (get :pins store) +storage_lf))
						(+storage_type_pins_missing (join (store-pins-missing store) +storage_lf))
						(+storage_type_pinned (join (store-pinned-by store body) " "))
						(+storage_type_sweep
							(defq swept (store-sweep store))
							(storage-record recorder member "sweep" (str swept))
							(str swept))
						(+storage_type_check
							(join (map (const hex-encode) (store-check store (eql body "mend"))) +storage_lf))
						(+storage_type_walk
							;a gopher. The task that runs them here is given it, with
							;who is to be answered: who asked, or the member that
							;sent it on, who is told it got here
							(storage-record recorder member "gopher-in")
							(if (/= 0 (get-long runner 0))
								(mail-send runner (cat (hex-encode reply) +storage_lf body)))
							:nil)
						(+storage_type_state
							(join (list (cat "member " member) (cat "store " (get :id store))
								(cat "digest " (hex-encode (store-digest store)))
								(cat "members " (join (store-members store) " "))
								(cat "retired " (join (get :retired store) " "))
								(cat "cut " (join cut " "))
								(cat "tender " (if (/= 0 (get-long tender 0)) (hex-encode tender) "")))
								+storage_lf))
						(+storage_type_entries (notes-text store body))
						(+storage_type_merge (str (notes-take store body)))
						(+storage_type_share
							;its share among the members that are there: what it wants,
							;a line of -, and what it has that is others' to keep, each
							;with who they are. No more than so many at a time
							(bind '(wants extras) (store-share store (split body " ")))
							(join (cat (map! (const hex-encode) (list wants) 0 (min +share_most (length wants)))
								(list "-")
								(map! (lambda ((hash holders)) (join (cat (list (hex-encode hash)) holders) " ")) (list extras) 0 (min +share_most (length extras))))
								+storage_lf))
						(+storage_type_where
							;the other members a block may be had from, in the block's
							;own order: those that are to keep it are the first of
							;them, however many its object says that is
							(defq among (cat (list member) (map (const first) peers))
								order (store-holders store body among (length among)))
							(join (map (const second) (sort (cat peers)
								(# (- (ifn (find (first %0) order) 99) (ifn (find (first %1) order) 99))))) +storage_lf))
						(+storage_type_peers
							;its tender says who is there: member and where, a line each
							(if (nempty? body)
								(setq peers (map (# (split %0 " ")) (filter (const nempty?) (split body +storage_lf)))))
							(if (eql body "-") (setq peers (list)))
							(join (map (# (join %0 " ")) peers) +storage_lf))
						(+storage_type_members
							(if (nempty? body) (storage-record recorder member "members" body))
							(cond
								((starts-with "+" body) (store-join store (list (rest body))))
								((starts-with "-" body) (store-join store (list) (list (rest body)))))
							(join (store-members store) " "))
						(+storage_type_cut
							(storage-record recorder member "cut" body)
							(setq cut (split body " ")) "ok")
						(+storage_type_snapshot
							(defq at (find +storage_lf body) name (slice body 0 at)
								count (store-snapshot store name (slice body (inc at) -1)))
							(when count
								(storage-record recorder member "snapshot" (cat (str count) " " name))
								(if (/= 0 (get-long tender 0)) (mail-send tender "kick")))
							(if count (str count) ""))
						(+storage_type_restore
							(defq at (find +storage_lf body) name (slice body 0 at)
								got (store-restore store name (slice body (inc at) -1)))
							(when got
								(storage-record recorder member "restore" (cat (str (first got)) " " name))
								(if (/= 0 (get-long tender 0)) (mail-send tender "kick")))
							(if got (join (map (const str) got) " ") ""))
						(+storage_type_snapshot_drop
							(defq count (store-snapshot-drop store body))
							(storage-record recorder member "snapshot-drop" (cat (str count) " " body))
							(if (/= 0 (get-long tender 0)) (mail-send tender "kick"))
							(str count))
						(+storage_type_resize
							;a copy of the store on a volume of that size, beside this one,
							;and then that is the store: the old file is gone, and the
							;machine whose member this is starts it from the new
							(defq megabytes (str-as-num body) moved_to (storage-resized-name image megabytes))
							;a copy of all of it is long work, not an answer: done in the
							;apps' band, with the apps of the node, not before them, a
							;bot gives them a moment only every 20ms
							(task-band :app)
							(defq resized (cond
								((or (<= megabytes 0) (eql moved_to image)) "")
								((defq moved (store-resize store moved_to (* megabytes 1048576)))
									(when (defq said (storage-member))
										(when (eql (. said :find :image) image)
											(storage-member-save moved_to megabytes (. said :find :id) (. said :find :copies)
												:nil (. said :find :travels))))
									(storage-unclaim image)
									(pii-remove image)
									(setq store moved image moved_to)
									(storage-claim image)
									(storage-record recorder member "resize" (cat (str megabytes) " " image))
									image)
								(:t (pii-remove moved_to) "")))
							(task-band :bot)
							resized)
						(+storage_type_record
							;told to the tender and the gophers too, each says what it does
							(storage-record-to recorder body)
							(if (/= 0 (get-long tender 0)) (mail-send tender (cat "record " body)))
							(if (/= 0 (get-long runner 0)) (mail-send runner (cat "record " body)))
							"ok")
						(+storage_type_stop
							;answered once the volume is closed, so a caller may
							;remove it then: Windows removes no file that is open
							(setq running :nil stopped reply) :nil)
						(:t "error"))
					(progn (prin "Storage: " (slice (defq said_ (str _)) 0 (min 200 (length said_)))) (print) "error")))
				(if answer (mail-send reply answer))
				(storage-record-send recorder)
				;and the apps of the node have a turn, if it has been at it a while
				(task-breathe)
				;what was learnt from other members and is not written yet, is,
				;when nothing else is asked, or it has waited long enough
				(and (defq dirty (get :dirty store))
					(or (not (mail-poll (list (task-mbox)))) (> (- (pii-time) dirty) +dirty_most))
					(store-save store)))
			(if (/= 0 (get-long tender 0)) (mail-send tender "stop"))
			(if (/= 0 (get-long runner 0)) (mail-send runner "stop"))
			(mail-forget service)
			(mail-forget known)
			;and let go of, its file is closed as the store is freed
			(store-close store)
			(setq store :nil)
			(if stopped (mail-send stopped "ok")))))
