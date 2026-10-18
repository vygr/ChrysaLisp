(import "./app.inc")
(import "lib/audio/voice.inc")

;What goes between the voice services of a call, the one that began it
;keeping it, is text: a letter for what it is, and what it carries:
;	j	inbox,service,rec,user	a member is in it, and says so again each
;					beat, rec 1 if it is recording
;	l	inbox			a member leaves it
;	m	id,members		who is in it, from the one that keeps it, each
;					beat and as it changes
;	e	id			it is ended
;From an app, a capital: S user,name, J user,id, L, M 1|0, W mailbox, U mailbox,
;R 1|0 recording or not, P mailbox the tap, (voice-tap), P alone none, and
;Q, out of any call and the service ends. Who is recording is told to all
;in the call, each sees it
;A member not heard from for +call_lease is gone, and a call whose keeper
;is not heard from for as long is ended: what is on another machine can
;not be asked if it is still there

(defq own (hex-encode (task-mbox)) user "Guest"
	;the call this is in, by the mailbox of its keeper, and if this is it
	call_id :nil call_name "" keeper :nil entry :nil
	;the keeper's: each member by its inbox, (service user due rec)
	members (Fmap 11)
	;who is in it, ((user inbox rec) ...), and when the keeper was last heard
	roster (list) heard 0 beat 0
	;what is said and heard, and the tap of an app that records it, hex
	voice :nil muted :nil recording :nil tap :nil
	;the apps watching, by mailbox, and when their lease is out
	watchers (Fmap 11))

(defun tree-text (data)
	(defq out (string-stream (cat "")))
	(tree-save out data)
	(str out))

(defun state-push (&optional only)
	;the watchers told the state of the call here
	(defq text (tree-text (scatter (Emap) :id call_id :name call_name :muted muted :members roster
		:inbox (if voice (voice-inbox voice)) :recording recording)))
	(if only
		(mail-send (hex-decode only) text)
		(. watchers :each (lambda (who due) (mail-send (hex-decode who) text)))))

(defun targets-set ()
	;what is said here goes to each other in the call
	(when voice
		(defq mine (voice-inbox voice))
		(voice-to voice (filter (# (not (eql %0 mine))) (map (const second) roster)))))

(defun voice-up ()
	;the microphone and the speaker for a call, muted or not as this is, and
	;to the tap if one records
	(setq voice (voice-start))
	(voice-mute voice muted)
	(if tap (voice-tap voice (hex-decode tap))))

(defun roster-send (&optional changed)
	;the keeper tells each member who is in the call, and if that changed
	;its own watchers too
	(setq roster (list (list user (voice-inbox voice) (if recording 1 0))))
	(. members :each (lambda (inbox (svc who due rec)) (push roster (list who inbox rec))))
	(defq text (cat "m" own "," (tree-text (scatter (Emap) :name call_name :members roster))))
	(. members :each (lambda (inbox (svc &ignore)) (mail-send (hex-decode svc) text)))
	(when changed
		(targets-set)
		(state-push)))

(defun call-drop ()
	;in no call, nothing said or heard
	(if entry (mail-forget entry))
	(if voice (voice-stop voice))
	(. members :empty)
	(setq voice :nil call_id :nil call_name "" keeper :nil entry :nil roster (list))
	(state-push))

(defun call-end ()
	;out of the call this is in, the others told
	(when call_id
		(if keeper
			(. members :each (lambda (inbox (svc &ignore)) (mail-send (hex-decode svc) (cat "e" own))))
			(mail-send (hex-decode call_id) (cat "l" (voice-inbox voice))))
		(call-drop)))

(defun join-beat ()
	(mail-send (hex-decode call_id) (cat "j" (voice-inbox voice) "," own "," (if recording "1" "0") "," user)))

(defun call-begin (name)
	;a new call, kept here
	(call-end)
	(voice-up)
	(setq call_id own call_name name keeper :t
		entry (mail-declare (task-mbox) "*Call" name))
	(roster-send :t))

(defun call-enter (id)
	;into the call of that keeper
	(call-end)
	(voice-up)
	(setq call_id id keeper :nil heard (pii-time) roster (list)
		call_name (ifn (some (# (if (eql (first %0) id) (second %0))) (call-list)) ""))
	(join-beat)
	(state-push))

(defun message (msg now)
	(defq kind (first msg) rest (slice msg 1 -1))
	(cond
		;from an app
		((eql kind "S")
			(defq at (find "," rest))
			(setq user (slice rest 0 at))
			(call-begin (slice rest (inc at) -1)))
		((eql kind "J")
			(defq at (find "," rest) id (slice rest (inc at) -1))
			(setq user (slice rest 0 at))
			(unless (eql id call_id) (call-enter id)))
		((eql kind "L") (call-end))
		((eql kind "M")
			(setq muted (eql rest "1"))
			(if voice (voice-mute voice muted))
			(state-push))
		((eql kind "R")
			(setq recording (eql rest "1"))
			(cond
				(keeper (roster-send :t))
				(call_id (join-beat)))
			(state-push))
		((eql kind "P")
			(setq tap (if (nempty? rest) rest))
			(if voice (voice-tap voice (if tap (hex-decode tap)))))
		((eql kind "W")
			(. watchers :insert rest (+ now +call_lease))
			(state-push rest))
		((eql kind "U") (. watchers :erase rest))
		((eql kind "Q") (call-end) (setq running :nil))
		;from the others of a call
		((eql kind "j")
			(when keeper
				(bind '(inbox svc rec who) (split rest ","))
				(defq rec (str-as-num rec) old (. members :find inbox))
				(. members :insert inbox (list svc who (+ now +call_lease) rec))
				(if (or (not old) (/= (elem-get old 3) rec)) (roster-send :t))))
		((eql kind "l")
			(when (and keeper (. members :find rest))
				(. members :erase rest)
				(roster-send :t)))
		((eql kind "m")
			(defq at (find "," rest) id (slice rest 0 at))
			(when (and (eql id call_id) (not keeper)
					(defq data (call-state-read (slice rest (inc at) -1))))
				(setq heard now)
				(defq was roster)
				(setq roster (ifn (. data :find :members) (list)) call_name (ifn (. data :find :name) ""))
				(unless (eql (str was) (str roster))
					(targets-set)
					(state-push))))
		((eql kind "e")
			(and (eql rest call_id) (not keeper) (call-drop)))))

(defun tick (now)
	;each beat: members say they are still there, the keeper says who is,
	;and those not heard from are gone
	(when call_id
		(cond
			(keeper
				(defq gone (list))
				(. members :each (lambda (inbox (svc who due rec)) (if (< due now) (push gone inbox))))
				(each (# (. members :erase %0)) gone)
				(roster-send (nempty? gone)))
			((> (- now heard) +call_lease) (call-drop))
			(:t (join-beat))))
	(defq gone (list))
	(. watchers :each (lambda (who due) (if (< due now) (push gone who))))
	(each (# (. watchers :erase %0)) gone))

(defun main ()
	(defq service (mail-declare (task-mbox) "@Voice" "Voice Service 0.1") running :t)
	(while running
		(defq msg (mail-read-timeout (task-mbox) +call_beat) now (pii-time))
		(and msg (nempty? msg) (message msg now))
		(when (> (- now beat) +call_beat)
			(setq beat now)
			(tick now)))
	(mail-forget service))
