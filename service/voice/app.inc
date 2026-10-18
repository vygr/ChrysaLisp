;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; service/voice/app.inc - calls, for any app
;
; A call is voice between the desktops in it, each heard by all the rest.
; It is its own thing on the mesh, said there as *Call by its name, and
; any app joins it: a Whiteboard its host's, Chat, the Voice app to listen
; in with neither. Each desktop has one voice service, @Voice, which has
; its microphone and its speaker, lib/audio/voice.inc, and is in one call
; at a time: to join another leaves the first, as a phone does. An app
; never has the microphone itself, it asks the service.
;
; Mute is the call's here, not an app's: none of what is said here is
; sent while it is on, whichever app put it on.
;
; An app that shows a call says it is watching, and again within
; +call_lease, and is sent the state of the call here each time it
; changes, (call-state-read): what is on another node can not be asked
; if it is still there, so it has to say so.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;a watcher, or a member of a call, not heard from for so long is gone,
;and each says it is still there twice in that time
(defq +call_lease 6000000 +call_beat 2000000
	;one heard within this is talking, and the service looks this often
	+call_talk_for 600000 +call_talk_look 200000)

(defun call-service ()
	; (call-service) -> :nil | net_id
	;the voice service of this desktop, on its node, or any on the machine
	(mail-service "@Voice"))

(defun call-ask (text)
	(if (defq service (call-service)) (mail-send service text)))

(defun call-ask-up (text)
	;as (call-ask), the service of this desktop started first if it has
	;none: it is there only once a call is started or joined, a desktop
	;that never calls has none
	(unless (mail-here "@Voice")
		(open-child "service/voice/app.lisp" +kn_call_pin)
		(defq until_up (+ (pii-time) 2000000))
		(while (and (not (mail-here "@Voice")) (< (pii-time) until_up)) (task-sleep 20000)))
	(call-ask text))

(defun call-user ()
	; (call-user) -> name
	;who is asking, as the others in a call see them: the user of the app,
	;a service has none
	(ifn (and (defq user_env (def? '*env*)) (get :user user_env)) "Guest"))

(defun call-name (text)
	;a comma is not of a name, the mesh keeps one after one
	(join (split text ",") " "))

(defun call-start (name)
	; (call-start name)
	;a new call, of that name, that this desktop is in
	(call-ask-up (cat "S" (call-name (call-user)) "," (call-name name))))

(defun call-join (id)
	; (call-join id)
	;the call of that id, hex, (call-list)
	(call-ask-up (cat "J" (call-name (call-user)) "," id)))

(defun call-leave ()
	; (call-leave)
	(call-ask "L"))

(defun call-mute (on)
	; (call-mute on)
	(call-ask (if on "M1" "M0")))

(defun call-watch (mbox &optional shows)
	; (call-watch mbox [shows])
	;mbox is sent the state of the call each time it changes, (call-state-read),
	;and now. Again within +call_lease to go on being. shows, if the app shows
	;the call as its own, a Whiteboard in its session's call: then the call
	;is not left when another that showed it is put away, (call-put-away)
	(call-ask (cat (if shows "V" "W") (hex-encode mbox))))

(defun call-put-away ()
	; (call-put-away)
	;the app that showed the call is closed: out of it, unless another app
	;shows it as its own. A live microphone is never with nothing on the
	;screen to say so
	(call-ask "X"))

(defun call-record (on)
	; (call-record on)
	;this desktop is recording the call, or not: all in it are told
	(call-ask (if on "R1" "R0")))

(defun call-tap (mbox)
	; (call-tap mbox)
	;mbox is sent all that is said and heard in the call, to be mixed,
	;lib/audio/voice.inc (voice-mix); :nil for none
	(call-ask (cat "P" (if mbox (hex-encode mbox) ""))))

(defun call-play (path)
	; (call-play path)
	;the sound file at path sent to the call in place of the microphone
	;till it ends, "" to stop it, a note played to the others
	(call-ask (cat "F" path)))

(defun call-headphones (on)
	; (call-headphones on)
	;with headphones what is heard is not heard by the microphone, so all
	;may talk at once: half duplex off
	(call-ask (if on "H1" "H0")))

(defun call-unwatch (mbox)
	; (call-unwatch mbox)
	(call-ask (cat "U" (hex-encode mbox))))

(defun call-list ()
	; (call-list) -> ((id name) ...)
	;the calls of the mesh
	(reduce (lambda (out entry)
			(defq parts (split entry ","))
			(if (= (length parts) 4) (push out (list (elem-get parts 1) (elem-get parts 3))))
			out)
		(mail-enquire "*Call,") (list)))

(defun call-state-read (msg)
	; (call-state-read msg) -> :nil | state
	;what a watcher is sent: an Emap of :id, hex or :nil if in no call,
	;:name, :muted, :recording, :headphones, :inbox, where this one hears,
	;:members, a list of (name inbox rec) of all in it, rec 1 for one that
	;records, and :talking, the inboxes of those talking now
	(and (defq data (catch (tree-load (string-stream msg)) :t)) (Emap? data) data))
