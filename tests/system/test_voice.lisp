(report-header "Voice: what is said mailed to those to hear it, and heard in order")

(import "lib/audio/voice.inc")

;what is played of a talker as its pieces come, in the order they were said
(defq vc_order (sound-order 10))
(assert-list-eq "pieces in order are played as they come" '(("a") ("b"))
	(list (sound-take vc_order 10 "a") (sound-take vc_order 11 "b")))
(assert-list-eq "one early is kept till the one before it comes, then both are played" '(() ("c" "d"))
	(list (sound-take vc_order 13 "d") (sound-take vc_order 12 "c")))
(sound-take vc_order 15 "f") (sound-take vc_order 16 "g")
(assert-list-eq "one lost: with three after it, they are played without it" '("f" "g" "h")
	(sound-take vc_order 17 "h"))
(assert-list-eq "and it is let go if it comes after all" '() (sound-take vc_order 14 "e"))

;a tone in place of the microphone, mailed to two that are to hear it
(defun vc-run (to)
	; (vc-run to) -> ((seq from bytes) ...) of each that was sent to
	(defq done (mail-mbox) ctl (mail-mbox) from (mail-mbox) boxes (map (lambda (_) (mail-mbox)) to))
	(mail-send ctl (join (map (const hex-encode) (filter (const identity) (map (# (if %0 %1)) to boxes))) ","))
	(stages-run (list (sound-stage 'sound-tone 440 200 6000)
		(voice-stage 'voice-send (stage-mbox ctl) (stage-mbox from))) done)
	(mail-read-timeout done (task-timeout 20))
	(map (lambda (box)
			(defq got (list))
			(while (defq msg (mail-read-timeout box 100000))
				(push got (list (getf msg +voice_piece_seq) (eql (hex-encode (getf msg +voice_piece_from)) (hex-encode from))
					(- (length msg) +voice_piece_data))))
			got)
		boxes))
(defq vc_got (vc-run '(:t :t)))
(assert-list-eq "each that is to hear is sent each piece of 20ms, numbered, from who said it"
	(list (map (# (list %0 :t +sound_piece)) (range 0 10)) (map (# (list %0 :t +sound_piece)) (range 0 10)))
	(map (lambda (got) (sort got (# (- (first %0) (first %1))))) vc_got))
(assert-list-eq "muted, sent to none, nothing is sent" '(() ()) (vc-run '(:nil :nil)))

(undef (env) 'vc_order 'vc_got)
