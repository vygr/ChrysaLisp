;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Storage, a recording played back
;
; The lines a recorder is told, service/storage/app.inc, as events, in the
; order they came, and what the store was after each: which blocks each
; member keeps, what each has each path as, who each can see, the blocks
; each object is made of, and where a gopher has been.
;
; Each event, once it has been done, keeps what it changed, so it can be
; undone. To go to any event of a recording is to do, or undo, those in
; between, from where it is now: nothing is copied, and a scrub back and
; forth over a long recording is as quick as there are events to pass.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;an event: when, the member, what, the words of it, the path if it is of
;one, and what it changed, once it has been done
(enums +ev 0
	(enum time member kind words path undo))

;how many words come before the path, of each kind that has one
(defq +replay_path_at (scatter (Fmap 31)
	"set" 2 "delete" 2 "copies" 2 "set-stale" 0 "set-full" 0 "set-missing" 0
	"delete-stale" 0 "delete-none" 0 "copies-none" 0 "learnt" 2 "learnt-delete" 2
	"gopher" 1 "gopher-on" 1 "gopher-step" 3 "gopher-pass" 2 "gopher-done" 1 "gopher-none" 1 "gopher-threw" 1 "gopher-late" 1))

(defun replay-event (line)
	; (replay-event line) -> :nil | event
	(defq parts (split line " "))
	(when (>= (length parts) 3)
		(bind '(time member kind &rest words) parts)
		(defq at (. +replay_path_at :find kind) path :nil)
		(and at (> (length words) at)
			(setq path (join (slice words at -1) " ") words (slice words 0 at)))
		(list (str-as-num time) member kind words path :nil)))

(defun replay-new ()
	; (replay-new) -> replay
	(defq r (Emap))
	(scatter r :events (list) :at 0 :members (list) :held (Fmap 11) :paths (Fmap 11)
		:peers (Fmap 11) :cut (Fmap 11) :objects (Fmap 101) :all_paths (Fset 101)
		;each gopher by its tag, (path steps), a step (member block time), and
		;the tag of the last that set out
		:gophers (Fmap 101) :last_gopher :nil
		;and of each object, the last gopher that set out to it
		:path_gopher (Fmap 101))
	r)

(defun replay-add (r line)
	; (replay-add r line) -> r
	;a line of a recording, at its end
	(when (defq e (replay-event line))
		(push (. r :find :events) e)
		(defq member (elem-get e +ev_member))
		;- is the scene, that says what it does, not a member
		(unless (or (eql member "-") (find member (. r :find :members)))
			(push (. r :find :members) member)
			(. (. r :find :held) :insert member (Fset 101))
			(. (. r :find :paths) :insert member (Fmap 101))
			(. (. r :find :peers) :insert member (list))
			(. (. r :find :cut) :insert member ""))
		(if (elem-get e +ev_path) (. (. r :find :all_paths) :insert (elem-get e +ev_path))))
	r)

(defun replay-do (r e)
	;what an event changes, and what it was, kept in the event
	(defq member (elem-get e +ev_member) words (elem-get e +ev_words) path (elem-get e +ev_path)
		held (. (. r :find :held) :find member) paths (. (. r :find :paths) :find member))
	(elem-set e +ev_undo (case (elem-get e +ev_kind)
		("block+" (cond ((. held :find (first words)) :had) (:t (. held :insert (first words)) :new)))
		("block-" (cond ((not (. held :find (first words))) :new) (:t (. held :erase (first words)) :had)))
		(("set" "learnt") (defq was (. paths :find path)) (. paths :insert path (first words)) (list was))
		(("delete" "learnt-delete") (defq was (. paths :find path)) (. paths :insert path "-") (list was))
		("object" (. (. r :find :objects) :insert (first words) (rest words)) :nil)
		;a member started again says again who it sees
		("peer+" (defq peers (. (. r :find :peers) :find member))
			(. (. r :find :peers) :insert member
				(if (find (first words) peers) peers (cat peers (list (first words))))) peers)
		("peer-" (defq peers (. (. r :find :peers) :find member))
			(. (. r :find :peers) :insert member (filter (# (nql %0 (first words))) peers)) peers)
		("cut" (defq was (. (. r :find :cut) :find member))
			(. (. r :find :cut) :insert member (join words " ")) was)
		(("gopher" "gopher-on")
			(defq tag (first words) had (. (. r :find :gophers) :find tag) last (. r :find :last_gopher)
				of (. (. r :find :path_gopher) :find path))
			(unless had (. (. r :find :gophers) :insert tag (list path (list))))
			(when (eql (elem-get e +ev_kind) "gopher")
				(. r :insert :last_gopher tag)
				(. (. r :find :path_gopher) :insert path tag))
			(list (not had) last of))
		("gopher-step"
			(when (defq walk (. (. r :find :gophers) :find (third words)))
				(. (. r :find :gophers) :insert (third words) (list (first walk)
					(cat (second walk) (list (list member (str-as-num (first words)) (elem-get e +ev_time))))))
				:t))
		(:t :nil))))

(defun replay-undo (r e)
	;an event as if it had not been
	(defq member (elem-get e +ev_member) words (elem-get e +ev_words) path (elem-get e +ev_path)
		held (. (. r :find :held) :find member) paths (. (. r :find :paths) :find member) undo (elem-get e +ev_undo))
	(case (elem-get e +ev_kind)
		("block+" (if (eql undo :new) (. held :erase (first words))))
		("block-" (if (eql undo :had) (. held :insert (first words))))
		(("set" "learnt" "delete" "learnt-delete")
			(if (first undo) (. paths :insert path (first undo)) (. paths :erase path)))
		(("peer+" "peer-") (. (. r :find :peers) :insert member undo))
		("cut" (. (. r :find :cut) :insert member undo))
		(("gopher" "gopher-on")
			(bind '(made last of) undo)
			(if made (. (. r :find :gophers) :erase (first words)))
			(. r :insert :last_gopher last)
			(if of (. (. r :find :path_gopher) :insert path of) (. (. r :find :path_gopher) :erase path)))
		("gopher-step"
			(and undo (defq walk (. (. r :find :gophers) :find (third words)))
				(. (. r :find :gophers) :insert (third words) (list (first walk) (slice (second walk) 0 -2)))))))

(defun replay-seek (r at)
	; (replay-seek r at) -> r
	;the store as it was when that many events had been done
	(defq events (. r :find :events) now (. r :find :at) at (max 0 (min at (length events))))
	(while (< now at) (replay-do r (elem-get events now)) (++ now))
	(while (> now at) (-- now) (replay-undo r (elem-get events now)))
	(. r :insert :at at)
	r)

(defun replay-trim (r keep)
	; (replay-trim r keep) -> r
	;no more than so many events, the oldest of those that have been done
	;let go of: the store is as it was, they can not be gone back to
	(defq events (. r :find :events) at (. r :find :at)
		drop (min at (- (length events) keep)))
	(when (> drop 0)
		(. r :insert :events (slice events drop -1))
		(. r :insert :at (- at drop)))
	r)

(defun replay-held (r member)
	; (replay-held r member) -> fset
	(. (. r :find :held) :find member))

(defun replay-path (r member path)
	; (replay-path r member path) -> :nil | manifest | "-"
	;what the member has the path as, - deleted
	(. (. (. r :find :paths) :find member) :find path))

(defun replay-blocks (r manifest)
	; (replay-blocks r manifest) -> :nil | (block ...)
	(. (. r :find :objects) :find manifest))

(defun replay-walk (r tag)
	; (replay-walk r tag) -> :nil | (path steps)
	;where a gopher has been, at the event the store is at, each step
	;(member block time)
	(. (. r :find :gophers) :find tag))

(defun replay-gopher-tag (e)
	; (replay-gopher-tag e) -> :nil | tag
	;the gopher an event is of, if it is of one
	(when (starts-with "gopher" (elem-get e +ev_kind))
		(defq words (elem-get e +ev_words))
		(case (elem-get e +ev_kind)
			("gopher-in" :nil)
			("gopher-step" (third words))
			("gopher-pass" (second words))
			(:t (first words)))))

(defun replay-about? (e path blocks)
	; (replay-about? e path blocks) -> :nil | :t
	;is an event of that path, or of a block of it, or of its manifest
	(or (eql (elem-get e +ev_path) path)
		(and (nempty? (elem-get e +ev_words)) (find (first (elem-get e +ev_words)) blocks) :t)))
