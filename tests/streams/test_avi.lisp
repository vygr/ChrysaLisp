(report-header "Streams: AVI, a film of RIFF as a reel, its pictures and its sound")

(import "lib/media/mp4.inc")
(import "apps/media/films/sound.inc")

;the clips are of tests/streams/data/film.mkv, a second or half of it at 160
;by 120 and 25 a second, made by ffmpeg: Motion JPEG with PCM of 16 bits
;stereo at 48000, of 8 bits mono at 22050, and with Microsoft's and IMA's
;ADPCM; H.264 with MP3; Xvid with MP3; DivX 5 with AC-3
(defun av-reel (name) (avi-reel (file-stream (cat "tests/streams/data/avi_" name ".avi"))))
(defun av-sound (reel) (some (# (if (eql (. %0 :find :kind) "soun") %0)) (. reel :find :tracks)))

(defq av_pcm (av-reel "mjpeg_pcm") av_h264 (av-reel "h264_mp3") av_xvid (av-reel "xvid_mp3") av_divx (av-reel "divx_ac3"))
(assert-list-eq "Motion JPEG, its size, 13 frames of half a second at 25"
	'("jpeg" 160 120 13 25) (list (. av_pcm :find :codec) (. av_pcm :find :width) (. av_pcm :find :height)
		(. av_pcm :find :frames) (n2i (+ (. av_pcm :find :rate) 0.5))))
(assert-eq "each a key" 13 (length (. av_pcm :find :keys)))
(assert-list-eq "its sound, PCM of 16 bits, stereo, at 48000" '("pcm" 16 2 48000)
	(list (. (av-sound av_pcm) :find :codec) (. (av-sound av_pcm) :find :bits) (. (av-sound av_pcm) :find :channels)
		(. (av-sound av_pcm) :find :sample_rate)))
(assert-list-eq "H.264, as an mp4 names it, a second, a key the first" '("avc1" 25 (0))
	(list (. av_h264 :find :codec) (. av_h264 :find :frames) (. av_h264 :find :keys)))
(assert-list-eq "Xvid and DivX 5 are MPEG-4 Part 2, mp4v" '("mp4v" "XVID" "mp4v" "DX50")
	(list (. av_xvid :find :codec) (. av_xvid :find :fourcc) (. av_divx :find :codec) (. av_divx :find :fourcc)))
(assert-list-eq "and their sound MP3 and AC-3" '("mp3" "ac-3")
	(list (. (av-sound av_xvid) :find :codec) (. (av-sound av_divx) :find :codec)))

;no index: a file cut short, or whose idx1 is lost, read a chunk at a time,
;the same chunks, and of MPEG-4 the keys from its own bytes
(defun av-cut (name)
	;the file with no idx1, a stream of it with that chunk's four letters made JUNK
	(defq data (load (cat "tests/streams/data/avi_" name ".avi")) at (find-rev-idx1 data))
	(string-stream (cat (slice data 0 at) "JUNK" (slice data (+ at 4) -1))))
(defun find-rev-idx1 (data)
	(defq i (- (length data) 4) at :nil)
	(while (and (not at) (>= i 0)) (if (eql (slice data i (+ i 4)) "idx1") (setq at i) (setq i (dec i))))
	at)
(each (lambda (name)
		(defq whole (av-reel name) cut (avi-reel (av-cut name)))
		(assert-true (cat name " with no idx1 has the same chunks, read one after another")
			(and cut (every (lambda (a b)
					(and (eql (str (. a :find :offsets)) (str (. b :find :offsets)))
						(eql (str (. a :find :sizes)) (str (. b :find :sizes)))))
				(. whole :find :tracks) (. cut :find :tracks))))
		(assert-eq (cat name " and the same keys, from its pictures' own bytes")
			(str (. whole :find :keys)) (str (. cut :find :keys))))
	'("mjpeg_pcm" "xvid_mp3" "h264_mp3"))

;the sound, Films' own: each packet decoded, the channels as reals at the
;scale of 16 bits, or of PCM of 16 bit stereo the bytes as they are
(defun av-left (name)
	;the left channel of all of a clip's sound, and how many packets
	(defq reel (av-reel name) sound (sound-of reel) left (list))
	(. sound :insert :decoder ((. sound :find :make) sound))
	(each (lambda (i)
			(defq chans (sound-packet sound i))
			(if (str? chans)
				(each (# (defq v (code chans 2 (* 4 %0))) (push left (if (>= v 0x8000) (- v 0x10000) v))) (range 0 (/ (length chans) 4)))
				(each (# (push left (n2i (elem-get (first chans) %0)))) (range 0 (length (first chans))))))
		(range 0 (. (. sound :find :track) :find :samples)))
	(list left sound))
(defun av-error (a b)
	;of b to a, as many as both have, the mean square of what differs, by that of a
	(defq n (min (length a) (length b)) sig 0 err 0)
	(each (# (defq x (elem-get a %0) y (elem-get b %0)) (setq sig (+ sig (* x x)) err (+ err (* (- x y) (- x y))))) (range 0 n))
	(/ (* err 10000) (max 1 sig)))
(bind '(av_ref av_ref_sound) (av-left "mjpeg_pcm"))
(assert-eq "PCM of 16 bit stereo, half a second of 48000" 24000 (length av_ref))
(bind '(av_ms _) (av-left "adpcm_ms"))
(assert-true "Microsoft ADPCM is the same sound, its error less than a thousandth" (< (av-error av_ref av_ms) 10))
(bind '(av_ima _) (av-left "adpcm_ima"))
(assert-true "and IMA ADPCM" (< (av-error av_ref av_ima) 10))
(bind '(av_u8 av_u8_sound) (av-left "mjpeg_u8"))
(assert-list-eq "PCM of 8 bits mono, half a second of 22050" '(22050 11025) (list (. av_u8_sound :find :rate) (length av_u8)))
(bind '(av_mp3 av_mp3_sound) (av-left "xvid_mp3"))
(assert-list-eq "MP3, its frames found in the stream of its chunks, a second of 48000 and its delay"
	'(48000 43 49536) (list (. av_mp3_sound :find :rate) (. (. av_mp3_sound :find :track) :find :samples) (length av_mp3)))
(bind '(av_ac3 av_ac3_sound) (av-left "divx_ac3"))
(assert-list-eq "AC-3, of 1536 a frame" '(48000 32 49152) (list (. av_ac3_sound :find :rate) (. (. av_ac3_sound :find :track) :find :samples) (length av_ac3)))

;Motion JPEG by lib/image/jpg.inc, a picture of each frame on the codec's canvas
(defq av_codec (avi-codec av_pcm))
(assert-true "a codec of our own for Motion JPEG" (and av_codec (. av_codec :find :mjpeg)))
(when av_codec
	(defun av-pixels (canvas)
		;its pixels, as test_jpg.lisp reads them
		(defq out (string-stream (cat "")))
		(pixmap-write (getf canvas +canvas_pixmap 0) out 32)
		(str out))
	(defq av_canvas (avi-frame av_codec 0))
	(assert-list-eq "its frame the film's size" '(160 120) (. av_canvas :pref_size))
	(defq av_a (av-pixels av_canvas))
	(assert-true "a picture, not all of one colour" (> (length (reduce (lambda (out i) (. out :insert (slice av_a i (+ i 4))) out)
		(range 0 (min (length av_a) 40000) 400) (Fset 11))) 0))
	(avi-frame av_codec 12)
	(assert-true "and another frame is another picture" (nql av_a (av-pixels av_canvas))))

;H.264 of Annex B: its avcC made of its first frame's SPS and PPS, its
;frames given the host as an mp4's, each NAL after four bytes of its
;length, and the order they are shown in of each one's picture order
;count. ffprobe's order, of where each frame's bytes are in the file,
;as it decodes the clip and shows its pictures: the two B-frames shown
;before the P-frames decoded before them
(defq av_v (. av_h264 :find :video) av_setup (second (first (. av_v :find :config))))
(assert-list-eq "an avcC, its version, profile 100, level 11, four bytes of length"
	'(1 100 11 0xff) (list (code av_setup 1 0) (code av_setup 1 1) (code av_setup 1 3) (code av_setup 1 4)))
(assert-true "shown in another order than decoded" (. av_h264 :find :reordered))
(assert-eq "the order shown, as ffprobe gives it"
	"10004 12526 12870 13388 13904 14200 14732 15262 15586 16092 16626 16940 18062 17452 18284 18770 19292 20182 19586 20604 20892 21412 21908 22206 22700"
	(join (map (# (str (elem-get (. av_h264 :find :offsets) (logand %0 0xfffff))))
		(sort (map (# (+ (<< %0 20) (!))) (. av_h264 :find :shown)) (const -))) " "))
(defq av_packet (mp4-packet av_h264 1))
(assert-true "a frame given the host, NALs each after its length"
	(and av_packet (= (+ 4 (code-be av_packet 4 0)) (min (length av_packet) (+ 4 (code-be av_packet 4 0))))))
;MPEG-4 Part 2: its size and, of the start of its first frame, its VOS, VO and VOL
(defq av_mp4v (second (first (. (. av_divx :find :video) :find :config))))
(assert-list-eq "its setup the size, then its headers from its VOS" '(160 120 0xb0)
	(list (code av_mp4v 4 0) (code av_mp4v 4 4) (code av_mp4v 1 11)))

;PCM to channels, native: each width as 16 bits, low byte first
(defun av-ints (pcm) (str (map (lambda (ch) (map (const n2i) ch)) pcm)))
(assert-eq "PCM of 16 bits, stereo, as it is" (str '((1 300) (-2 -32768)))
	(av-ints (adpcm-pcm (cat (char 1 2) (char -2 2) (char 300 2) (char -32768 2)) 2 16)))
(assert-eq "of 8 bits, offset by 128 and moved up, one channel on both" (str '((0 -32768 32512) (0 -32768 32512)))
	(av-ints (adpcm-pcm (cat (char 128) (char 0) (char 255)) 1 8)))
(assert-eq "of 24 bits, its top two bytes" (str '((4660 -1) (4660 -1)))
	(av-ints (adpcm-pcm (cat (char 0x123456 3) (char -1 3)) 1 24)))
