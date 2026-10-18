(report-header "Sound: pipelines of stages, a source, filters and a sink")

(import "lib/audio/stages.inc")

(defq ss_a "tests/scratch/sound_a.wav" ss_b "tests/scratch/sound_b.wav" ss_c "tests/scratch/sound_c.wav")

(defun ss-run (stages)
	; (ss-run stages) -> :t | :nil
	;the pipeline run to its end, :t if its sink said it was done in time
	(defq done (mail-mbox))
	(stages-run stages done)
	(mail-read-timeout done (task-timeout 20)))

(defun ss-peak (file)
	; (ss-peak file) -> num
	;the loudest sample of a sound file
	(defq stream (file-stream file) peak 0)
	(WAV-info stream)
	(while (and (defq data (read-blk stream 4096)) (nempty? data))
		(setq peak (max peak (sound-peak data))))
	peak)

;a tone to a file, written as it comes
(assert-true "a tone to a file, the sink says when it is done"
	(ss-run (list (sound-stage 'sound-tone 440 500 8000) (sound-stage 'sound-to-file ss_a))))
(assert-list-eq "the file is a sound at the rate, mono, 16 bit, of half a second"
	(list +sound_rate 1 16 (* +sound_rate 1)) (sound-info ss_a))
(assert-true "at the tone's level" (<= 7900 (ss-peak ss_a) 8000))

;the file through a filter to another
(assert-true "a file, made quieter, to another"
	(ss-run (list (sound-stage 'sound-from-file ss_a) (sound-stage 'sound-gain 50) (sound-stage 'sound-to-file ss_b))))
(assert-true "half as loud" (<= 3950 (ss-peak ss_b) 4000))
(assert-eq "and as long" (elem-get (sound-info ss_a) 3) (elem-get (sound-info ss_b) 3))

;a high tone is let through less by a low pass than a low one
(ss-run (list (sound-stage 'sound-tone 4000 300 8000) (sound-stage 'sound-lowpass 300) (sound-stage 'sound-to-file ss_c)))
(defq ss_high (ss-peak ss_c))
(ss-run (list (sound-stage 'sound-tone 100 300 8000) (sound-stage 'sound-lowpass 300) (sound-stage 'sound-to-file ss_c)))
(defq ss_low (ss-peak ss_c))
(assert-true "a low pass lets a low tone through" (> ss_low 6000))
(assert-true "and a high one much less" (< ss_high (/ ss_low 4)))

;a level stage tells a mailbox how loud each piece is, and passes it on
(defq ss_who (mail-mbox) ss_levels (list))
(ss-run (list (sound-stage 'sound-tone 440 200 6000) (sound-stage 'sound-level (stage-mbox ss_who))
	(sound-stage 'sound-to-file ss_c)))
(while (defq ss_msg (mail-read-timeout ss_who 100000)) (push ss_levels (str-as-num ss_msg)))
(assert-eq "a level for each piece of 20ms" 10 (length ss_levels))
(assert-true "each the tone's" (every (# (<= 5800 %0 6000)) ss_levels))
(assert-eq "and the sound passed on" (/ (* +sound_rate 2) 5) (elem-get (sound-info ss_c) 3))

(each (const pii-remove) (list ss_a ss_b ss_c))
(undef (env) 'ss_a 'ss_b 'ss_c 'ss_high 'ss_low 'ss_who 'ss_levels 'ss_msg)
