(import "lib/options/options.inc")
(import "lib/media/mp4.inc")

(defq usage `(
(("-h" "--help")
"Usage: mp4 [options] [path] ...

    options:
        -h --help: this help info.
        -s --samples num: the first num samples of the video track
            too, where each is, how long, when, and which are keys.

    What is in a film of boxes, .mp4 .m4v .mov: each track, its
    kind and codec, its size or its sound, and how many samples
    it has; and of the first video track, the frames a second
    its times come to and how many keys.

    If no paths given on command line
    then paths are read from stdin.")
(("-s" "--samples") ,(opt-num 'opt_s))
))

(defun say-file (file)
	(cond
		((not (defq stream (file-stream file))) (print file ": not there"))
		((not (defq reel (mp4-reel stream))) (print file ": not a film of boxes, or no moov"))
		(:t (print file ":")
			(each (lambda (track)
					(defq kind (. track :find :kind) seconds (if (> (. track :find :timescale) 0)
						(/ (n2f (. track :find :duration)) (n2f (. track :find :timescale))) 0.0))
					(print "  " kind " " (. track :find :codec) " "
						(cond
							((eql kind "vide") (cat (str (. track :find :width)) "x" (str (. track :find :height))))
							((eql kind "soun") (cat (str (. track :find :sample_rate)) "Hz " (str (. track :find :channels)) " channels"))
							(""))
						", " (. track :find :samples) " samples, " (n2i seconds) "s"))
				(. reel :find :tracks))
			(when (. reel :find :frames)
				(defq keys (. reel :find :keys))
				(print "  video: " (. reel :find :frames) " frames, "
					(/ (n2f (n2i (* (. reel :find :rate) 100.0))) 100.0) " a second, "
					(if (eql keys :all) "every one a key" (cat (str (length keys)) " keys")))
				(each (lambda (i)
						(print "    " i " at " (elem-get (. reel :find :offsets) i) " " (elem-get (. reel :find :lengths) i)
							" bytes, time " (elem-get (. reel :find :times) i)
							(if (or (eql keys :all) (find i keys)) " key" "")))
					(range 0 (min opt_s (. reel :find :frames))))))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(and
		(defq stdio (create-stdio))
		(defq opt_s 0 args (options stdio usage))
		(if (<= (length args) 1)
			(lines! (# (say-file %0) :nil) (io-stream 'stdin))
			(each! (const say-file) (list args) 1 -1))))
