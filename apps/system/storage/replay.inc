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
	"gopher" 0 "gopher-on" 0 "gopher-step" 2 "gopher-pass" 1 "gopher-done" 0 "gopher-none" 0 "gopher-threw" 0))

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
		:peers (Fmap 11) :cut (Fmap 11) :objects (Fmap 101) :trail (list) :all_paths (Fset 101))
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
		("gopher" (defq was (. r :find :trail)) (. r :insert :trail (list path)) was)
		("gopher-step" (defq was (. r :find :trail))
			(. r :insert :trail (cat was (list (list member (str-as-num (first words)) (elem-get e +ev_time))))) was)
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
		(("gopher" "gopher-step") (. r :insert :trail undo))))

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

(defun replay-about? (e path blocks)
	; (replay-about? e path blocks) -> :nil | :t
	;is an event of that path, or of a block of it, or of its manifest
	(or (eql (elem-get e +ev_path) path)
		(and (nempty? (elem-get e +ev_words)) (find (first (elem-get e +ev_words)) blocks) :t)))
