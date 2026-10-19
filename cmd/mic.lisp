(import "lib/options/options.inc")
(import "lib/audio/stages.inc")

(defq usage `(
(("-h" "--help")
"Usage: mic [options]

    options:
        -h --help: this help info.
        -s --seconds num: how long to listen, default 3.
        -o --out path: what was heard kept in a sound file, .wav,
            default tests/scratch/mic.wav.
        -p --play: what was heard played back.
        -e --echo file: the sound file played on the speaker while
            the microphone is heard, from its second second, for as
            long as it lasts and 2 more, and how loud each second was
            said: the echo canceller of this machine tried alone. The
            room quiet, what is heard after the first second should
            be near what it was in it.

    Listen to the microphone of the Audio service of this machine, and
    say what was heard: how long, how loud at its loudest and on the
    whole. What is heard is 16 bit mono at 16000 a second. It is a sound
    pipeline, lib/audio/stages.inc, the microphone, how loud, to the
    file; and to play it, the file to the speaker. A desktop that is
    never seen, -hidden, hears a tone of 440 a second, not a device.

    On macOS the program the host runs under, the terminal, must be let
    use the microphone, or it hears nothing.")
(("-s" "--seconds") ,(opt-num 'opt_s))
(("-o" "--out") ,(opt-str 'opt_o))
(("-p" "--play") ,(opt-flag 'opt_p))
(("-e" "--echo") ,(opt-str 'opt_e))
))

(defun main ()
	(and (defq stdio (create-stdio))
		(defq opt_s 3 opt_o "tests/scratch/mic.wav" opt_p :nil opt_e :nil args (options stdio usage))
		(cond
			((not (audio-service)) (print "mic: no Audio service, a desktop has one"))
			((and opt_e (not (sound-info opt_e))) (print "mic: no sound file " opt_e))
			(:t (defq ctl (mail-mbox) who (mail-mbox) done (mail-mbox) t0 (pii-time) levels (list)
					second (list) seconds 0 played :nil)
				(if opt_e (setq opt_s (+ (sound-seconds opt_e) 3)))
				(stages-run (list (sound-stage 'sound-mic (stage-mbox ctl))
						(sound-stage 'sound-level (stage-mbox who)) (sound-stage 'sound-to-file opt_o))
					done)
				;how loud each piece was, till it is time to stop, and with a
				;file played, each second of it said
				(while (< (- (pii-time) t0) (* opt_s 1000000))
					(when (and opt_e (not played) (> (- (pii-time) t0) 1000000))
						(setq played :t)
						(stages-run (list (sound-stage 'sound-from-file opt_e) (sound-stage 'sound-speaker))))
					(when (defq msg (mail-read-timeout who 100000))
						(push levels (str-as-num msg))
						(push second (str-as-num msg)))
					(when (and opt_e (>= (length second) 50))
						(print "mic: second " (setq seconds (inc seconds)) ", loudest " (reduce max second 0)
							", on the whole " (/ (reduce + second 0) (length second)))
						(setq second (list))))
				(mail-send ctl "")
				(mail-read-timeout done (task-timeout 5))
				(while (defq msg (mail-read-timeout who 50000)) (push levels (str-as-num msg)))
				(defq secs (sound-seconds opt_o) bytes (if (defq info (sound-info opt_o)) (elem-get info 3) 0))
				(cond
					((= bytes 0) (print "mic: nothing was heard, no microphone, or it is not allowed"))
					(:t (print "mic: " (/ (* bytes 500) +sound_rate) "ms in " opt_o ", "
							(length levels) " pieces, loudest " (reduce max levels 0)
							", on the whole " (/ (reduce + levels 0) (max 1 (length levels))) " of 32767")
						(when opt_p
							(stages-run (list (sound-stage 'sound-from-file opt_o) (sound-stage 'sound-speaker)) done)
							(mail-read-timeout done (task-timeout (+ secs 5)))
							(print "mic: played back"))))))))
