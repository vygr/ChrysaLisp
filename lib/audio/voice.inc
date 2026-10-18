;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/audio/voice.inc - voice between machines, for any app
;
; What the microphone hears is mailed to others as it is heard, and what
; others say is played as it comes, each of them on a stream of its own,
; the Audio service mixing them. Two sound pipelines, lib/audio/stages.inc:
;
;	microphone -> voice-send	each piece mailed to those it is to go to
;	voice-hear			each one's pieces in order to its stream
;
; Those it goes to are mailboxes, the inbox of each voice-hear that is to
; hear it, and are told to voice-send as they change: none is mute, the
; microphone still heard here and sent nowhere. Mail is not kept in
; order, so each piece is numbered, and who it is from is the inbox of
; the one that sent it. An app says who is to hear whom, (voice-to), this
; knows nothing of sessions. The Whiteboard and Chat both use it.
;
; Each start is a new inbox, so what was sent to the last is never heard
; by the next: an app tells the others the new one, (voice-inbox).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./stages.inc")

;a piece as it is mailed
(structure +voice_piece 0
	(netid from)
	(uint seq)
	(offset data))

;one not heard from for so long has its stream closed, and it is begun
;again if it is heard again
(defq +voice_quiet 1000000)

;the stages

(defun voice-stage (fn &rest args)
	; (voice-stage fn [arg ...]) -> stage
	;a stage of this module
	(apply (const stage) (cat (list "lib/audio/voice.inc" fn) args)))

(defun voice-send (in out ctl from)
	; (voice-send in out ctl from)
	;a sink: each piece mailed, numbered, to each that is to hear it. ctl
	;is sent who that is, the mailboxes hex, a comma between, "" for none
	(defq to (list) seq 0)
	(sound-pieces in (lambda (data)
		(while (mail-poll (list ctl))
			(setq to (map (const hex-decode) (filter (const nempty?) (split (mail-read ctl) ",")))))
		(when (nempty? to)
			(defq piece (cat (setf-> (str-alloc +voice_piece_size)
					(+voice_piece_from from) (+voice_piece_seq seq)) data))
			(each (# (mail-send %0 piece)) to))
		(setq seq (inc seq)))))

(defun voice-hear (in out inbox ctl)
	; (voice-hear in out inbox ctl)
	;a stage that is both ends: what comes to inbox played, each that sends
	;on a stream of its own, in the order it was said, till ctl is sent
	;anything. A piece that is not there when two after it are is lost
	(defq select (list inbox ctl) running :t talkers (Fmap 11))
	;a talker, by who it is from, its mailbox hex: (handle order last)
	(while running
		(defq idx (mail-select select) msg (mail-read (elem-get select idx)) now (pii-time))
		(cond
			((= idx 1) (setq running :nil))
			(:t (defq from (hex-encode (getf msg +voice_piece_from)) seq (getf msg +voice_piece_seq)
					talker (. talkers :find from))
				;one quiet a while is begun again, its stream may be gone
				(when (and talker (> (- now (elem-get talker 2)) +voice_quiet))
					(audio-stream-close-rpc (first talker))
					(. talkers :erase from)
					(setq talker :nil))
				(unless talker
					(when (defq handle (audio-stream-open-rpc))
						(. talkers :insert from (setq talker (list handle (sound-order seq) now)))))
				(when talker
					(elem-set talker 2 now)
					(each (# (audio-stream-write-rpc (first talker) %0))
						(sound-take (second talker) seq (slice msg +voice_piece_data -1))))
				;and those quiet a while, closed
				(defq gone (list))
				(. talkers :each (lambda (who (handle &ignore last))
					(when (> (- now last) +voice_quiet)
						(audio-stream-close-rpc handle)
						(push gone who))))
				(each (# (. talkers :erase %0)) gone))))
	(. talkers :each (lambda (who (handle &ignore)) (audio-stream-close-rpc handle))))

;for an app

(defun voice-start ()
	; (voice-start) -> voice
	;the microphone sent to none yet, and what is sent here heard
	(defq inbox (mail-mbox) hear_ctl (mail-mbox) mic_ctl (mail-mbox) send_ctl (mail-mbox))
	(stages-run (list (voice-stage 'voice-hear (stage-mbox inbox) (stage-mbox hear_ctl))))
	(stages-run (list (sound-stage 'sound-mic (stage-mbox mic_ctl))
		(voice-stage 'voice-send (stage-mbox send_ctl) (stage-mbox inbox))))
	(list inbox hear_ctl mic_ctl send_ctl))

(defun voice-inbox (voice)
	; (voice-inbox voice) -> hex
	;where what is to be heard here is sent, to tell the others
	(hex-encode (first voice)))

(defun voice-to (voice to)
	; (voice-to voice to)
	;who hears what is said here now: their inboxes, hex, none for mute
	(mail-send (elem-get voice 3) (join to ",")))

(defun voice-stop (voice)
	; (voice-stop voice)
	;neither said nor heard any more
	(mail-send (elem-get voice 2) "")
	(mail-send (elem-get voice 1) ""))
