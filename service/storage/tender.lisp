;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a tender, what a member of a store does about the others
;
; A task of its own, one for a Storage service, started by it. A service
; only answers. This is what asks: it looks for the other members, compares
; notes with each that has something it has not, fetches the blocks that
; are this member's to keep and it has not got, and lets go of those that
; are others' to keep, once every one of them says it has them.
;
; It is the meeting of people with notepads: meet, compare what each has,
; copy out what is missing, and back to what it was doing, till it meets
; another.
;
; It is mailed, by the service: where the service is, the name of the
; group, how often to look, 0 for only when told, and how long a member is
; not heard from before what it kept is kept by others. After that, a mail
; of "kick" is look now, "pass " and a mailbox is look now and say there
; what was done, and "stop" is go.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./app.inc")

(defun others (svc group me)
	; (others svc group me) -> ((member netid state) ...)
	;the other members of my store that are there, and not cut off from
	;me, nor I from them: each with what it says of itself
	(defq out (list) cut (. me :find :cut))
	(each (lambda (entry)
		;a name known over every machine is told with the system it is on:
		;the name, where, the system, and what was said of it, which is last
		(defq parts (split entry ",") about (split (last parts) " ") where (hex-decode (second parts)))
		(and (= (length about) 2) (eql (first about) (. me :find :store))
			(nql (second about) (. me :find :member)) (not (find (second about) cut))
			(defq state (storage-state where))
			(not (find (. me :find :member) (. state :find :cut)))
			(push out (list (second about) where state))))
		(mail-enquire (cat group ",")))
	out)

(defun look (svc group grace heard)
	; (look svc group grace heard) -> (met fetched dropped)
	(defq met 0 fetched 0 dropped 0 me (storage-state svc))
	(when me
		(defq peers (others svc group me) now (pii-time) member (. me :find :member))
		(storage-ask svc +storage_type_peers
			(if (empty? peers) "-" (join (map (lambda ((who where state)) (cat who " " (hex-encode where))) peers) +storage_lf)))
		;compare notes with each that is not as I am. What it has is taken
		;here, and then what I have, with that, is taken there: the two are
		;then the same
		(each (lambda ((who where state))
			(. heard :insert who now)
			(unless (eql (. state :find :digest) (. (ifn (storage-state svc) me) :find :digest))
				(when (defq theirs (storage-ask where +storage_type_entries))
					(storage-ask svc +storage_type_merge theirs)
					(if (defq mine (storage-ask svc +storage_type_entries)) (storage-ask where +storage_type_merge mine))
					(++ met))))
			peers)
		;the members that are there: me, and each heard from of late. One
		;that has gone is still there for a while, a machine that is turned
		;off and on is not a reason to copy what it kept
		(defq there (list member))
		(. heard :each (lambda (who when) (if (<= (- now when) grace) (push there who))))
		(defq there (filter (# (find %0 (. (ifn (storage-state svc) me) :find :members))) there)
			more :t rounds 0)
		;my share. A manifest fetched says what more there is to want, so
		;it is asked again, a few times
		(while (and more (< (++ rounds) 8))
			(setq more :nil)
			(defq lines (split (ifn (storage-ask svc +storage_type_share (join there " ")) "-") +storage_lf)
				at (ifn (find "-" lines) (length lines)))
			(each (lambda (want)
				(when (defq data (some (lambda ((who where state)) (storage-block-at (hex-decode want) where)) peers))
					(when (eql (storage-ask svc +storage_type_block_put data) (hex-decode want))
						(++ fetched)
						(setq more :t))))
				(slice lines 0 at))
			;what is others' to keep is let go of when each of them has it
			(each (lambda (line)
				(bind '(extra &rest holders) (split line " "))
				(when (every (lambda (holder)
						(and (defq peer (some (# (if (eql (first %0) holder) %0)) peers))
							(eql (storage-ask (second peer) +storage_type_has (hex-decode extra)) "1")))
						holders)
					(if (eql (storage-ask svc +storage_type_block_drop (hex-decode extra)) "1") (++ dropped))))
				(slice lines (min (inc at) (length lines)) -1))))
	(list met fetched dropped))

(defun main ()
	(bind '(svc group tick grace) (split (mail-read (task-mbox)) +storage_lf))
	(defq svc (hex-decode svc) tick (str-as-num tick) grace (str-as-num grace)
		heard (Fmap 11) running :t select (list (task-mbox) (mail-mbox)))
	(if (> tick 0) (mail-timeout (second select) tick 0))
	(while running
		(defq idx (mail-select select) msg (mail-read (elem-get select idx)))
		(cond
			((= idx 1)
				;time to look
				(catch (look svc group grace heard) :t)
				(mail-timeout (second select) tick 0))
			((eql msg "stop") (setq running :nil))
			((starts-with "pass " msg)
				(defq did (catch (look svc group grace heard) (list 0 0 0)))
				(mail-send (hex-decode (slice msg 5 -1)) (join (map (const str) (if (list? did) did (list 0 0 0))) " ")))
			((eql msg "kick")
				;a write: the others are told now. Not if it only looks when told
				(if (> tick 0) (catch (look svc group grace heard) :t)))))
	(mail-timeout (second select) 0 0))
