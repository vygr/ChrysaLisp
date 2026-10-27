(report-header "Streams: mkv, a film of Matroska read as a reel, its tracks as an mp4's")

(import "lib/media/mkv.inc")
(import "lib/media/aac.inc")

;film.mkv is a second of 160 by 120 at 25 a second, H.264 with B-frames,
;a key every 25, and AAC-LC at 48000, stereo, made by ffmpeg. Of its
;packets, as ffprobe gives them: 25 of the picture, 5485 bytes, a key
;the first; 48 of the sound, 11977 bytes
(defq mk_reel (mkv-reel (file-stream "tests/streams/data/film.mkv"))
	mk_video (. mk_reel :find :video)
	mk_sound (some (# (if (eql (. %0 :find :kind) "soun") %0)) (. mk_reel :find :tracks)))
(assert-eq "two tracks" 2 (length (. mk_reel :find :tracks)))
(assert-list-eq "the picture's codec and size, as an mp4 names it" '("avc1" 160 120)
	(list (. mk_reel :find :codec) (. mk_reel :find :width) (. mk_reel :find :height)))
(assert-eq "its setup an avcC, its CodecPrivate" "avcC" (first (first (. mk_video :find :config))))
(assert-eq "25 frames" 25 (. mk_reel :find :frames))
(assert-list-eq "a key the first" '(0) (. mk_reel :find :keys))
(assert-true "B-frames, shown in another order than decoded" (. mk_video :find :reordered))
(assert-eq "the picture's bytes, each frame where it is" 5485 (reduce (const +) (. mk_reel :find :lengths) 0))
(assert-eq "25 a second, by the times" 25 (n2i (+ (. mk_reel :find :rate) 0.5)))
(assert-list-eq "the sound's codec, rate and channels" '("mp4a" 48000 2)
	(list (. mk_sound :find :codec) (. mk_sound :find :sample_rate) (. mk_sound :find :channels)))
(assert-eq "48 packets of sound" 48 (length (. mk_sound :find :offsets)))
(assert-eq "the sound's bytes" 11977 (reduce (const +) (. mk_sound :find :sizes) 0))
(assert-list-eq "AAC-LC by its CodecPrivate, the AudioSpecificConfig alone" '(2 3 48000 2 :nil :nil) (aac-track-config mk_sound))
;each packet of sound read where the track says, and decoded
(defq mk_stream (. mk_reel :find :stream) mk_decoder (aac-decoder (aac-track-config mk_sound)))
(assert-true "each packet of sound decodes"
	(every (lambda (at size)
			(stream-seek mk_stream at 0)
			(list? (catch (aac-decode mk_decoder (read-blk mk_stream size)) :t)))
		(. mk_sound :find :offsets) (. mk_sound :find :sizes)))

;film_unsized.mkv is film.mkv with each Cluster's size all ones, not known,
;as a recording writes them that cannot go back: each ends where the next
;element of the top begins. Its frames are the same, where and when
(defq mk_un (mkv-reel (file-stream "tests/streams/data/film_unsized.mkv")) mk_un_stream (. mk_un :find :stream)
	mk_read (lambda (s at size) (stream-seek s at 0) (read-blk s size)))
(assert-true "Clusters of no size, each track's frames the same bytes and times"
	(every (lambda (ta tb)
			(and (= (. ta :find :samples) (. tb :find :samples)) (eql (str (. ta :find :shown)) (str (. tb :find :shown)))
				(every (lambda (oa za ob zb) (eql (mk_read mk_stream oa za) (mk_read mk_un_stream ob zb)))
					(. ta :find :offsets) (. ta :find :sizes) (. tb :find :offsets) (. tb :find :sizes))))
		(. mk_reel :find :tracks) (. mk_un :find :tracks)))

;with few, of one with Cues it is read as it is played as ever, as cheap,
;and of one with none only its first Cluster, to show its start: a
;preview of a big film reads none of the rest. A copy with no Cues, its
;Cues made Tags, which are stepped over
(defq mk_data (load "tests/streams/data/film.mkv")
	mk_at (first (first (first (substr mk_data (cat (char 0x1c) (char 0x53) (char 0xbb) (char 0x6b)))))))
(defq mk_bare (cat (slice mk_data 0 mk_at) (char 0x12) (char 0x54) (char 0xc3) (char 0x67) (slice mk_data (+ mk_at 4) -1)))
(defq mk_few (mkv-reel (string-stream mk_bare) :t) mk_whole (mkv-reel (string-stream mk_bare)))
(assert-eq "of Cues, with few it is read as it is played, not partial" :nil (. (mkv-reel (file-stream "tests/streams/data/film.mkv") :t) :find :partial))
(assert-eq "of none, read whole it is not read as it is played" :nil (. mk_whole :find :lazy))
(assert-eq "read whole, every frame" 25 (. mk_whole :find :frames))
(assert-eq "with few, read for its start, it says it is partial" :t (. mk_few :find :partial))
(assert-true "with frames, no more than all of them"
	(<= 1 (. mk_few :find :frames) (. mk_whole :find :frames)))
(assert-list-eq "the first frames where they are when all is read"
	(slice (. mk_whole :find :offsets) 0 (. mk_few :find :frames)) (. mk_few :find :offsets))

;its Cues say where its keys are, and its frames are each of one length:
;it is read as it is played, the Cluster of a frame found by its time
(assert-true "read as it is played, it has Cues" (. mk_reel :find :lazy))
(assert-eq "its packets of sound by its Duration, the last cut by it" 47 (. mk_sound :find :samples))
(defq mk_stream (. mk_reel :find :stream))
(assert-eq "a packet of sound had by its time is the one there"
	(progn (stream-seek mk_stream (elem-get (. mk_sound :find :offsets) 10) 0) (read-blk mk_stream (elem-get (. mk_sound :find :sizes) 10)))
	(mkv-track-packet mk_reel mk_sound 10))
(assert-true "a frame's time by its number, 25 a second" (< (abs (- (mkv-frame-time mk_reel 10) 0.4)) 0.002))
(defq mk_at (mkv-locate mk_reel (some (# (if (eql %0 mk_video) (!))) (. mk_reel :find :tracks)) 3))
(assert-eq "a frame is found in its Cluster by its time, 40 ms a frame" 120
	(elem-get (. (elem-get (third (elem-get (first mk_at) (second mk_at))) 0) :find :shown) (third mk_at)))
