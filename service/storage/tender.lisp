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
; what was done, "pace " and two numbers is how often to look and how long
; a member is not heard from, from now on, and "stop" is go.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./app.inc")

;how many blocks are asked for at once
(defq +tender_window 8)

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

(defun notes-at (notes)
	;where the count of changes was, of the member that gave these notes
	(defq line (slice notes 0 (ifn (find +storage_lf notes) (length notes))))
	(if (starts-with "at " line) (slice line 3 -1) ""))

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
				;only what is new: each says where its count of changes is,
				;and is asked next time for what has changed since. took is
				;what was had from each, gave what each was given
				(when (defq theirs (storage-ask where +storage_type_entries (ifn (. took :find who) "")))
					(if (storage-ask svc +storage_type_merge theirs) (. took :insert who (notes-at theirs)))
					(and (defq mine (storage-ask svc +storage_type_entries (ifn (. gave :find who) "")))
						(storage-ask where +storage_type_merge mine)
						(. gave :insert who (notes-at mine)))
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
			(defq lines (split (ifn (storage-ask svc +storage_type_share (join there " ") 60) "-") +storage_lf)
				at (ifn (find "-" lines) (length lines)))
			;so many at a time. Each peer is asked the once which of them it
			;has, then each block is asked of one that has it, in turn where
			;several do, all of them at once and none waited for: between
			;machines it is the going and coming that takes the time, 17ms
			;of it over a Wi-Fi link, measured, and a block at a time was a
			;third of what the link could carry
			(defq wants (map (const hex-decode) (slice lines 0 at)))
			(each (lambda (start)
				(defq batch (slice wants start (min (length wants) (+ start +tender_window)))
					names (apply (const cat) (cat (list "") batch))
					has (map (lambda ((who where state)) (ifn (storage-ask where +storage_type_has names 5) "")) peers)
					boxes (map (lambda (want)
						(defq at_want (!)
							having (filter (# (and (< at_want (length (elem-get has %0))) (eql (elem-get (elem-get has %0) at_want) "1")))
								(range 0 (length peers))))
						(if (nempty? having)
							(storage-send (second (elem-get peers (elem-get having (% at_want (length having)))))
								+storage_type_block_get want)))
						batch))
				(each (lambda (want mbox)
					(when (and mbox (defq data (mail-read-timeout mbox (task-timeout 10))) (eql (sha256 data) want)
							(eql (storage-ask svc +storage_type_block_put data) want))
						(++ fetched)
						(setq more :t)))
					batch boxes))
				(range 0 (length wants) +tender_window))
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
		heard (Fmap 11) took (Fmap 11) gave (Fmap 11) running :t select (list (task-mbox) (mail-mbox)))
	(if (> tick 0) (mail-timeout (second select) tick 0))
	(while running
		(defq idx (mail-select select) msg (mail-read (elem-get select idx)))
		(cond
			((= idx 1)
				;time to look, if it still looks by the clock
				(when (> tick 0)
					(catch (look svc group grace heard) :t)
					(mail-timeout (second select) tick 0)))
			((eql msg "stop") (setq running :nil))
			((starts-with "pass " msg)
				(defq did (catch (look svc group grace heard) (list 0 0 0)))
				(mail-send (hex-decode (slice msg 5 -1)) (join (map (const str) (if (list? did) did (list 0 0 0))) " ")))
			((starts-with "pace " msg)
				;how often it looks is changed, as it runs
				(bind '(tick grace) (map (const str-as-num) (split (slice msg 5 -1) " ")))
				(mail-timeout (second select) 0 0)
				(if (> tick 0) (mail-timeout (second select) tick 0)))
			((eql msg "kick")
				;a write: the others are told now. Not if it only looks when told
				(if (> tick 0) (catch (look svc group grace heard) :t)))))
	(mail-timeout (second select) 0 0))
