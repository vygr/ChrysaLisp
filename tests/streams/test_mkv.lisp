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
(assert-eq "48 packets of sound" 48 (. mk_sound :find :samples))
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

;with few, only its first Cluster is read, to show its start: a preview
;of a big film reads none of the rest
(defq mk_few (mkv-reel (file-stream "tests/streams/data/film.mkv") :t))
(assert-eq "read for its start, the reel says it is partial" :t (. mk_few :find :partial))
(assert-true "with frames, no more than all of them"
	(<= 1 (. mk_few :find :frames) (. mk_reel :find :frames)))
(assert-list-eq "the first frames where they are when all is read"
	(slice (. mk_reel :find :offsets) 0 (. mk_few :find :frames)) (. mk_few :find :offsets))
(assert-eq "read whole, it is not partial" :nil (. mk_reel :find :partial))
