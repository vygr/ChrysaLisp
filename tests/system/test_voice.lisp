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

(defun vc-got (box from)
	(defq got (list))
	(while (defq msg (mail-read-timeout box 100000))
		(push got (list (getf msg +voice_piece_seq) (eql (hex-encode (getf msg +voice_piece_from)) (hex-encode from))
			(- (length msg) +voice_piece_data) (> (sound-peak (slice msg +voice_piece_data -1)) 5000))))
	(sort got (# (- (first %0) (first %1)))))
;a tone in place of the microphone, mailed to two that are to hear it, or
;none, muted or not, and to a tap or not
(defun vc-run (to &optional muted tap level said)
	; (vc-run to [muted tap level said]) -> ((seq from bytes peak) ...) of each that was sent to
	(defq done (mail-mbox) ctl (mail-mbox) from (mail-mbox) boxes (map (lambda (_) (mail-mbox)) to))
	(mail-send ctl (cat "t" (join (map (const hex-encode) (filter (const identity) (map (# (if %0 %1)) to boxes))) ",")))
	(if muted (mail-send ctl "m1"))
	(if tap (mail-send ctl (cat "p" (hex-encode tap))))
	(each (# (mail-send ctl %0)) (ifn said (list)))
	(stages-run (list (sound-stage 'sound-tone 440 200 (ifn level 6000))
		(voice-stage 'voice-send (stage-mbox ctl) (stage-mbox from) :nil)) done)
	(mail-read-timeout done (task-timeout 20))
	(map (lambda (box) (vc-got box from)) (if tap (cat boxes (list tap)) boxes)))
(defun vc-all (peak) (map (# (list %0 :t +sound_piece peak)) (range 0 10)))
(assert-list-eq "each that is to hear is sent each piece of 20ms, numbered, from who said it"
	(list (vc-all :t) (vc-all :t)) (vc-run '(:t :t)))
(assert-list-eq "to none, nothing is sent" '(() ()) (vc-run '(:nil :nil)))
(assert-list-eq "muted, nothing is sent to those to hear it, and the tap has each piece, quiet"
	(list (list) (vc-all :nil)) (vc-run '(:t) :t (mail-mbox)))
(assert-list-eq "not muted, the tap has what is said" (list (vc-all :t) (vc-all :t)) (vc-run '(:t) :nil (mail-mbox)))
;the gate: what is quiet is not sent, the tap has it all
(defun vc-quiet (got) (map (lambda ((seq from bytes peak)) (list seq from bytes)) got))
(assert-list-eq "quiet, below the gate, nothing is sent, and the tap has each piece"
	(list (list) (map (# (list %0 :t +sound_piece)) (range 0 10))) (map (const vc-quiet) (vc-run '(:t) :nil (mail-mbox) 500)))
;half duplex: another heard of late is not spoken over
(assert-list-eq "another just heard, what is said here is not sent over them" '(())
	(vc-run '(:t) :nil :nil :nil '("h")))
(assert-list-eq "half duplex off, as with headphones, it is" (list (vc-all :t))
	(vc-run '(:t) :nil :nil :nil '("d0" "h")))

;pieces added, sample by sample
(defq vc_a (sound-map (slice (str-alloc 8) 0 8) (lambda (_) 20000)) vc_b (sound-map (slice (str-alloc 4) 0 4) (lambda (_) -5000)))
(assert-list-eq "a mix of two is the sum of them, kept in range, as long as the first"
	'(15000 15000 20000 20000) (map (# (sound-sample (sound-add (list vc_a vc_b)) %0)) (range 0 4)))
(assert-list-eq "and loud ones are kept in range" '(32767 32767)
	(map (# (sound-sample (sound-add (list vc_a vc_a)) %0)) (range 0 2)))

;a mix of what is said here and what another says, to a file: what is said
;here is the clock, a piece of the file for each of it, the other's added in
(defq vc_tap (mail-mbox) vc_ctl (mail-mbox) vc_done (mail-mbox) vc_own (mail-mbox) vc_other (mail-mbox)
	vc_file "tests/scratch/voice_mix.wav"
	vc_here (sound-map (str-alloc +sound_piece) (lambda (_) 6000)) vc_there (sound-map (str-alloc +sound_piece) (lambda (_) 4000)))
(each (# (mail-send vc_tap (voice-piece vc_other %0 vc_there))) (range 0 3))
(each (# (mail-send vc_tap (voice-piece vc_own %0 vc_here))) (range 0 5))
(stages-run (list (voice-stage 'voice-mix (stage-mbox vc_tap) (hex-encode vc_own) (stage-mbox vc_ctl))
	(sound-stage 'sound-to-file vc_file)) vc_done)
(task-sleep 500000)
(mail-send vc_ctl "")
(mail-read-timeout vc_done (task-timeout 10))
(defq vc_stream (file-stream vc_file))
(WAV-info vc_stream)
(defq vc_pieces (map (lambda (_) (sound-peak (read-blk vc_stream +sound_piece))) (range 0 5)))
(assert-list-eq "five pieces said here, the first three with the other's three added" '(10000 10000 10000 6000 6000) vc_pieces)
(pii-remove vc_file)

;a file sent in place of the microphone: a loud tone of three pieces and a
;half, the microphone quiet. Its pieces go, the last made up with silence,
;then the microphone's again, sent on as the gate is still open
(defq vc_done (mail-mbox) vc_file "tests/scratch/voice_play.wav")
(stages-run (list (sound-stage 'sound-tone 1000 70 20000) (sound-stage 'sound-to-file vc_file)) vc_done)
(mail-read-timeout vc_done (task-timeout 10))
(defq vc_box (mail-mbox) vc_ctl (mail-mbox) vc_from (mail-mbox) vc_got (list))
(mail-send vc_ctl (cat "t" (hex-encode vc_box)))
(mail-send vc_ctl (cat "f" vc_file))
(stages-run (list (sound-stage 'sound-tone 440 200 500)
	(voice-stage 'voice-send (stage-mbox vc_ctl) (stage-mbox vc_from) :nil)) vc_done)
(mail-read-timeout vc_done (task-timeout 20))
(while (defq msg (mail-read-timeout vc_box 100000))
	(push vc_got (list (getf msg +voice_piece_seq) (slice msg +voice_piece_data -1))))
(sort vc_got (# (- (first %0) (first %1))))
;the tone is made as fast as it can be, not at the pace of a microphone,
;so how many of its pieces come before the gate's hang ends is the load's
(defq vc_half (/ +sound_piece 2) vc_loud (map (# (> (sound-peak (second %0)) 5000)) vc_got))
(assert-list-eq "the file's pieces are sent first"
	'(:t :t :t :t) (slice vc_loud 0 (min 4 (length vc_loud))))
(assert-list-eq "and the microphone's after them, quiet"
	'() (filter! (const identity) vc_loud (min 4 (length vc_loud)) -1))
(assert-list-eq "the last of the file made up with silence"
	'(:t 0) (list (> (sound-peak (slice (second (elem-get vc_got 3)) 0 vc_half)) 5000)
		(sound-peak (slice (second (elem-get vc_got 3)) vc_half -1))))
(pii-remove vc_file)

(undef (env) 'vc_box 'vc_from 'vc_got 'vc_half 'vc_loud 'vc_order 'vc_a 'vc_b 'vc_tap 'vc_ctl 'vc_done 'vc_own 'vc_other 'vc_file 'vc_here 'vc_there 'vc_stream 'vc_pieces)
