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
; hear it, and are told to voice-send as they change. Mute is said to it
; too: the microphone still heard here and sent nowhere. Mail is not kept
; in order, so each piece is numbered, and who it is from is the inbox of
; the one that sent it. An app says who is to hear whom, (voice-to), this
; knows nothing of calls, service/voice does.
;
; Each start is a new inbox, so what was sent to the last is never heard
; by the next: an app tells the others the new one, (voice-inbox).
;
; Only speech is sent: a gate, quiet goes nowhere. And half duplex, one
; heard of late is not spoken over, so a speaker's sound heard again by the
; microphone is not sent back: with headphones that is not needed, and it
; is put off, (voice-duplex).
;
; To record it all, a tap, (voice-tap): a mailbox each stage sends each
; piece to as well, what is said here, nothing of it while muted, and what
; each other says in its order. voice-mix makes one sound of them, a source
; of a pipeline, to a file as any other is.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./stages.inc")

;a piece as it is mailed
(structure +voice_piece 0
	(netid from)
	(uint seq)
	(offset data))

;one not heard from for so long has its stream closed, and it is begun
;again if it is heard again. A mix keeps no more than so many pieces of
;one that is ahead of what is said here
(defq +voice_quiet 1000000 +voice_mix_ahead 10
	;the gate: a piece louder than this, of 32767, is speech, and what is
	;said is sent till so long after the last of it, so the ends of words
	;are not cut. As long after another is heard, this one does not begin:
	;half duplex. And the hearing tells the saying no more often than this
	+voice_gate 1000 +voice_hang 400000 +voice_heard_every 100000
	;who is talking is told no more often than this, of each
	+voice_talk_every 300000)

;the stages

(defun voice-stage (fn &rest args)
	; (voice-stage fn [arg ...]) -> stage
	;a stage of this module
	(apply (const stage) (cat (list "lib/audio/voice.inc" fn) args)))

(defun voice-piece (from seq data)
	; (voice-piece from seq data) -> str
	(cat (setf-> (str-alloc +voice_piece_size) (+voice_piece_from from) (+voice_piece_seq seq)) data))

(defun voice-send (in out ctl from talk)
	; (voice-send in out ctl from talk)
	;a sink: each piece mailed, numbered, to each that is to hear it. ctl
	;is sent t and who that is, the mailboxes hex, a comma between; m1 or
	;m0, muted or not; p and a tap's mailbox hex, or p alone for none; d1
	;or d0, half duplex or not, on to begin with; h, another was heard; f
	;and the path of a sound file, f alone for none: what it has is sent
	;in place of what the microphone hears, a piece of it for each piece
	;of the microphone, so at the pace it is heard, till it ends.
	;
	;Only speech is sent, the gate: quiet, nothing goes, no hiss and no
	;traffic. And half duplex: one heard of late is not spoken over, unless
	;this one was speaking first, so what another says, played from the
	;speaker here and heard by the microphone, is not sent back to them.
	;Headphones have no need of it, d0. talk, if not :nil, is told T and
	;from, hex, while this one is sent, who is talking
	(defq to (list) seq 0 sent_seq 0 muted :nil tap :nil duplex :t file :nil file_left 0
		silence (sound-map (str-alloc +sound_piece) (lambda (_) 0))
		speech_until 0 heard_until 0 sending :nil told 0 from_hex (hex-encode from))
	(sound-pieces in (lambda (data)
		(while (mail-poll (list ctl))
			(defq msg (mail-read ctl) kind (first msg) rest (slice msg 1 -1))
			(cond
				((eql kind "t") (setq to (map (const hex-decode) (filter (const nempty?) (split rest ",")))))
				((eql kind "m") (setq muted (eql rest "1")))
				((eql kind "p") (setq tap (if (nempty? rest) (hex-decode rest))))
				((eql kind "d") (setq duplex (eql rest "1")))
				((eql kind "h") (setq heard_until (+ (pii-time) +voice_hang)))
				((eql kind "f")
					;a file of this rate, mono 16 bit, as the microphone is
					(setq file :nil file_left 0)
					(when (and (nempty? rest) (defq loader (sound-loader-of rest)) (defq stream (file-stream rest)))
						(bind '(rate channels bits bytes) ((second loader) stream))
						(and (= rate +sound_rate) (= channels 1) (= bits 16)
							(setq file stream file_left bytes))))))
		;a file sent in place of the microphone, the last piece made up with
		;silence, and then the microphone again
		(when file
			(defq got (read-blk file (min file_left (length data))))
			(setq file_left (- file_left (length got)))
			(if (or (empty? got) (<= file_left 0)) (setq file :nil))
			(if (nempty? got) (setq data (cat got (slice silence (length got) (length data))))))
		(defq now (pii-time))
		(if (> (sound-peak data) +voice_gate) (setq speech_until (+ now +voice_hang)))
		;speech, not muted, and not over another unless this one was first.
		;What goes to others is numbered without gaps, a gap is a loss to them
		(setq sending (and (< now speech_until) (not muted)
			(or sending (not duplex) (>= now heard_until))))
		(when sending
			(when (and talk (> (- now told) +voice_talk_every))
				(setq told now)
				(mail-send talk (cat "T" from_hex)))
			(defq piece (voice-piece from sent_seq data))
			(each (# (mail-send %0 piece)) to)
			(setq sent_seq (inc sent_seq)))
		;a mix goes by what is said here, so a piece goes to the tap muted or
		;not, as quiet when it is
		(if tap (mail-send tap (voice-piece from seq (if muted (sound-map data (lambda (_) 0)) data))))
		(setq seq (inc seq)))))

(defun voice-hear (in out inbox ctl said talk)
	; (voice-hear in out inbox ctl said talk)
	;a stage that is both ends: what comes to inbox played, each that sends
	;on a stream of its own, in the order it was said, till ctl is sent e.
	;p and a tap's mailbox hex, or p alone, as voice-send. said, the ctl of
	;the voice-send beside it, is told h as another is heard, half duplex.
	;talk, if not :nil, is told T and who, hex, as each is heard
	(defq select (list inbox ctl) running :t talkers (Fmap 11) tap :nil told 0)
	;a talker, by who it is from, its mailbox hex: (handle order last seq told)
	(while running
		(defq idx (mail-select select) msg (mail-read (elem-get select idx)) now (pii-time))
		(cond
			((= idx 1)
				(if (eql (first msg) "p")
					(setq tap (if (> (length msg) 1) (hex-decode (slice msg 1 -1))))
					(setq running :nil)))
			(:t (defq from (hex-encode (getf msg +voice_piece_from)) seq (getf msg +voice_piece_seq)
					talker (. talkers :find from))
				;one quiet a while is begun again, its stream may be gone
				(when (and talker (> (- now (elem-get talker 2)) +voice_quiet))
					(audio-stream-close-rpc (first talker))
					(. talkers :erase from)
					(setq talker :nil))
				(unless talker
					(. talkers :insert from (setq talker (list (audio-stream-open-rpc) (sound-order seq) now 0 0))))
				(when (and talk (> (- now (elem-get talker 4)) +voice_talk_every))
					(elem-set talker 4 now)
					(mail-send talk (cat "T" from)))
				(elem-set talker 2 now)
				(when (> (- now told) +voice_heard_every)
					(setq told now)
					(mail-send said "h"))
				(each (lambda (data)
						(if (first talker) (audio-stream-write-rpc (first talker) data))
						(when tap
							(mail-send tap (voice-piece (getf msg +voice_piece_from) (elem-get talker 3) data))
							(elem-set talker 3 (inc (elem-get talker 3)))))
					(sound-take (second talker) seq (slice msg +voice_piece_data -1)))
				;and those quiet a while, closed
				(defq gone (list))
				(. talkers :each (lambda (who (handle order heard tapped spoke))
					(when (> (- now heard) +voice_quiet)
						(if handle (audio-stream-close-rpc handle))
						(push gone who))))
				(each (# (. talkers :erase %0)) gone))))
	(. talkers :each (lambda (who (handle &ignore)) (if handle (audio-stream-close-rpc handle)))))

(defun voice-mix (in out tap own ctl)
	; (voice-mix in out tap own ctl)
	;a source: what comes to tap made one sound, till ctl is sent anything.
	;What is said here, from own, hex, is the clock: as each piece of it
	;comes, it is written with the next of each other's added in. One
	;further ahead than +voice_mix_ahead has its oldest let go
	(defq select (list tap ctl) running :t queues (Fmap 11))
	(while running
		(defq idx (mail-select select) msg (mail-read (elem-get select idx)))
		(cond
			((= idx 1) (setq running :nil))
			(:t (defq from (hex-encode (getf msg +voice_piece_from)) data (slice msg +voice_piece_data -1))
				(cond
					((eql from own)
						(defq datas (list data) taken (list))
						(. queues :each (lambda (who queue)
							(when (nempty? queue)
								(push datas (first queue))
								(push taken who))))
						(each (# (. queues :insert %0 (rest (. queues :find %0)))) taken)
						(sound-write out (sound-add datas)))
					(:t (defq queue (ifn (. queues :find from) (list)))
						(push queue data)
						(if (> (length queue) +voice_mix_ahead) (setq queue (rest queue)))
						(. queues :insert from queue)))))))

;for an app

(defun voice-start (&optional talk)
	; (voice-start [talk]) -> voice
	;the microphone sent to none yet, and what is sent here heard. talk, a
	;mailbox, is told T and an inbox, hex, as each in it talks, this one too
	(defq inbox (mail-mbox) hear_ctl (mail-mbox) mic_ctl (mail-mbox) send_ctl (mail-mbox)
		talk_form (if talk (stage-mbox talk)))
	(stages-run (list (voice-stage 'voice-hear (stage-mbox inbox) (stage-mbox hear_ctl) (stage-mbox send_ctl) talk_form)))
	(stages-run (list (sound-stage 'sound-mic (stage-mbox mic_ctl))
		(voice-stage 'voice-send (stage-mbox send_ctl) (stage-mbox inbox) talk_form)))
	(list inbox hear_ctl mic_ctl send_ctl))

(defun voice-inbox (voice)
	; (voice-inbox voice) -> hex
	;where what is to be heard here is sent, to tell the others
	(hex-encode (first voice)))

(defun voice-to (voice to)
	; (voice-to voice to)
	;who hears what is said here now: their inboxes, hex
	(mail-send (elem-get voice 3) (cat "t" (join to ","))))

(defun voice-mute (voice on)
	; (voice-mute voice on)
	;none of what is said here sent, while on
	(mail-send (elem-get voice 3) (if on "m1" "m0")))

(defun voice-duplex (voice on)
	; (voice-duplex voice on)
	;half duplex, on to begin with: one heard of late is not spoken over.
	;Off with headphones, where nothing heard is heard by the microphone
	(mail-send (elem-get voice 3) (if on "d1" "d0")))

(defun voice-play (voice path)
	; (voice-play voice path)
	;the sound file at path sent in place of the microphone till it ends,
	;"" to stop it. 16000 a second, mono, 16 bit, as the microphone is
	(mail-send (elem-get voice 3) (cat "f" path)))

(defun voice-tap (voice mbox)
	; (voice-tap voice mbox)
	;mbox sent all that is said and heard, to be mixed, (voice-mix); :nil
	;for none
	(defq text (cat "p" (if mbox (hex-encode mbox) "")))
	(mail-send (elem-get voice 3) text)
	(mail-send (elem-get voice 1) text))

(defun voice-stop (voice)
	; (voice-stop voice)
	;neither said nor heard any more
	(mail-send (elem-get voice 2) "")
	(mail-send (elem-get voice 1) "e"))
