;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; mkv, a film of Matroska or WebM as a reel
;
; A file is EBML: elements, each an id and a size, both numbers of 1 to
; 8 bytes whose first byte says how many, and what it holds, more
; elements or a value. A Segment holds the Info, its time, the Tracks,
; each its number, kind, codec and setup, CodecPrivate, and Clusters,
; each a time and the blocks of its frames, each of a track at a time
; from the cluster's, keys said, and of sound often several frames, laced.
; (mkv-reel stream) reads the elements, not the frames, as an mp4's
; tables are, and gives the reel of the first video track in the shape
; lib/media/mp4.inc's codec takes, so the host's H.264 and H.265 decode
; it as they do an mp4's, and each track in :tracks as an mp4's are,
; the sound ones to Films' sound. mp4.inc imports this and makes .mkv
; and .webm reels of it with its own codec.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./reel.inc")

;module
(env-push)

;the ids, as they are written, the length bits in them
(defq +mkv_ebml 0x1a45dfa3 +mkv_segment 0x18538067 +mkv_info 0x1549a966 +mkv_scale 0x2ad7b1
	+mkv_tracks 0x1654ae6b +mkv_entry 0xae +mkv_number 0xd7 +mkv_type 0x83 +mkv_codec 0x86
	+mkv_private 0x63a2 +mkv_duration 0x23e383 +mkv_video 0xe0 +mkv_width 0xb0 +mkv_height 0xba
	+mkv_audio 0xe1 +mkv_rate 0xb5 +mkv_channels 0x9f +mkv_delay 0x56aa
	+mkv_cluster 0x1f43b675 +mkv_time 0xe7 +mkv_simple 0xa3 +mkv_group 0xa0 +mkv_block 0xa1 +mkv_reference 0xfb
	+mkv_length 0x4489 +mkv_seekhead 0x114d9b74 +mkv_seek 0x4dbb +mkv_seek_id 0x53ab +mkv_seek_pos 0x53ac
	+mkv_cues 0x1c53bb6b +mkv_cue 0xbb +mkv_cue_time 0xb3 +mkv_cue_place 0xb7 +mkv_cue_track 0xf7
	+mkv_cue_cluster 0xf1
	;how many Clusters a reel read as it is played keeps the frames of
	+mkv_kept 16)

;what the functions read as they run, this one map
(defq +mkv (Emap))
(scatter +mkv
	;the ids that are only at the top of a Segment, which end a Cluster of
	;no size
	:tops (list 0x1f43b675 0x1c53bb6b 0x1549a966 0x1654ae6b 0x114d9b74 0x1941a469 0x1043a770 0x1254c367)
	;of each codec of Matroska's, the name of it an mp4 has
	:codecs (list "V_MPEG4/ISO/AVC" "avc1" "V_MPEGH/ISO/HEVC" "hvc1" "A_AAC" "mp4a" "A_AC3" "ac-3"
		"A_EAC3" "ec-3" "A_MPEG/L3" "mp3" "A_FLAC" "flac" "A_OPUS" "opus"))

(defun mkv-vint (s at)
	; (mkv-vint str at) -> (value bytes marked)
	;a number of 1 to 8 bytes at at, its first byte's leading 0s one less
	;than its bytes: its value without the length bit, how long it is, and
	;its value with it, as an id is. All ones is a size not known, -1
	(defq b (code s 1 at) n 1)
	(while (and (< n 8) (= 0 (logand b (>> 0x80 (dec n))))) (setq n (inc n)))
	(defq marked (code-be s n at) v (logand marked (dec (<< 1 (* 7 n)))))
	(list (if (= v (dec (<< 1 (* 7 n)))) -1 v) n marked))

(defun mkv-head (stream at)
	; (mkv-head stream at) -> :nil | (id size data_at)
	;the element at at of a stream: its id, its size, -1 not known, and
	;where what it holds begins
	(stream-seek stream at 0)
	(when (and (defq s (read-blk stream 16)) (>= (length s) 2) (/= (code s 1 0) 0))
		(bind '(_ n id) (mkv-vint s 0))
		(when (< (+ n 1) (length s))
			(bind '(size m _) (mkv-vint s n))
			(list id size (+ at n m)))))

(defun mkv-children (s at end)
	; (mkv-children str at end) -> ((id body end) ...)
	;the elements from at to end of a str
	(defq out (list))
	(while (< at end)
		(bind '(_ n id) (mkv-vint s at))
		(bind '(size m _) (mkv-vint s (+ at n)))
		(defq body (+ at n m) stop (if (< size 0) end (min end (+ body size))))
		(push out (list id body stop))
		(setq at stop))
	out)

(defun mkv-uint (s at end)
	(if (> end at) (code-be s (- end at) at) 0))

(defun mkv-asc (name rate channels)
	; (mkv-asc name rate channels) -> str
	;an AudioSpecificConfig of AAC-LC of a rate and channels, or if the
	;name ends /SBR, of type 5, SBR on a core of half the rate
	(defq rates '(96000 88200 64000 48000 44100 32000 24000 22050 16000 12000 11025 8000 7350)
		index (lambda (r) (ifn (some (# (if (= %0 r) (!))) rates) 4))
		fields (if (ends-with "/SBR" name)
			(list 5 5 (index (/ rate 2)) 4 channels 4 (index rate) 4 2 5 0 3)
			(list 2 5 (index rate) 4 channels 4 0 3))
		bits 0 count 0)
	(each (lambda ((v w)) (setq bits (logior (<< bits w) v) count (+ count w))) (partition fields 2))
	;the bits high first, made whole bytes
	(defq pad (logand (- 8 (logand count 7)) 7) bits (<< bits pad) n (/ (+ count pad) 8))
	(apply (const cat) (map (# (char (logand (>> bits (* 8 (- n 1 %0))) 0xff))) (range 0 n))))

(defun mkv-track (s at end)
	; (mkv-track str at end) -> track
	;a TrackEntry: its number, kind as an mp4's, its codec named as an mp4
	;names it, its setup, size or sound
	(defq track (scatter (Emap) :timescale 1000 :start 0 :delay 0 :offsets (list) :sizes (list)
		:shown (list) :keys (list) :config (list)))
	(each (lambda ((id body stop))
			(cond
				((= id +mkv_number) (. track :insert :number (mkv-uint s body stop)))
				((= id +mkv_type) (. track :insert :kind (case (mkv-uint s body stop) (1 "vide") (2 "soun") (:t "other"))))
				((= id +mkv_codec)
					;of AAC the A_AAC/MPEG4/LC and the like of before as well
					(defq name (slice s body stop) codecs (. +mkv :find :codecs)
						i (some (# (and (= (logand (!) 1) 0) (eql %0 name) (!))) codecs))
					(scatter track :matroska name :codec (cond (i (elem-get codecs (inc i))) ((starts-with "A_AAC" name) "mp4a") (name))))
				((= id +mkv_private) (. track :insert :private (slice s body stop)))
				((= id +mkv_duration) (. track :insert :frame_ns (mkv-uint s body stop)))
				((= id +mkv_delay) (. track :insert :codec_delay (mkv-uint s body stop)))
				((= id +mkv_video)
					(each (lambda ((id body stop))
							(cond
								((= id +mkv_width) (. track :insert :width (mkv-uint s body stop)))
								((= id +mkv_height) (. track :insert :height (mkv-uint s body stop)))))
						(mkv-children s body stop)))
				((= id +mkv_audio)
					(each (lambda ((id body stop))
							(cond
								((= id +mkv_rate) (. track :insert :sample_rate (reel-whole s body (- stop body))))
								((= id +mkv_channels) (. track :insert :channels (mkv-uint s body stop)))))
						(mkv-children s body stop)))))
		(mkv-children s at end))
	;its setup as an mp4's sample entry has it, the codec's box
	(defq private (. track :find :private))
	(when private
		(cond
			((eql (. track :find :codec) "avc1") (. track :insert :config (list (list "avcC" private))))
			((eql (. track :find :codec) "hvc1") (. track :insert :config (list (list "hvcC" private))))
			((eql (. track :find :codec) "mp4a") (. track :insert :asc private))))
	;an AAC of an id of before and no CodecPrivate, its config made of its
	;rate and channels, and if its id says SBR, of a core of half the rate
	(and (eql (. track :find :codec) "mp4a") (not private) (. track :find :sample_rate)
		(. track :insert :asc (mkv-asc (. track :find :matroska) (. track :find :sample_rate) (ifn (. track :find :channels) 2))))
	track)

(defun mkv-laced (s at end lacing)
	; (mkv-laced str at end lacing) -> ((at size) ...)
	;the frames of a block from at, its lacing: none, Xiph's, fixed, or
	;EBML's, each where it begins and how long
	(cond
		((= lacing 0) (list (list at (- end at))))
		(:t (defq count (inc (code s 1 at)) at (inc at) sizes (list))
			(cond
				((= lacing 1)
					;Xiph's: each but the last its 255s and the byte after
					(times (dec count)
						(defq size 0 b 255)
						(while (= b 255) (setq b (code s 1 at) size (+ size b) at (inc at)))
						(push sizes size)))
				((= lacing 3)
					;EBML's: the first a number, each after it how much more, signed
					(bind '(first_size n _) (mkv-vint s at))
					(push sizes first_size)
					(setq at (+ at n))
					(times (max 0 (- count 2))
						(bind '(v n _) (mkv-vint s at))
						(push sizes (+ (last sizes) (- v (dec (<< 1 (dec (* 7 n)))))))
						(setq at (+ at n)))))
			(defq known (reduce (const +) sizes 0))
			(cond
				((= lacing 2) (setq sizes (map (lambda (_) (/ (- end at) count)) (range 0 count))))
				((push sizes (- end at known))))
			(map (lambda (size) (defq frame (list at size)) (setq at (+ at size)) frame) sizes))))

(defun mkv-block (s at end start tracks key)
	; (mkv-block str at end start tracks key)
	;a block: its track, its time from the cluster's start, its keys, and
	;its frames into its track's tables, each frame of a laced block where
	;it is in the file, str of the block from start
	(bind '(number n _) (mkv-vint s at))
	(when (defq track (some (# (if (= (. %0 :find :number) number) %0)) tracks))
		(defq rel (code-be s 2 (+ at n)) flags (code s 1 (+ at n 2)))
		(setq rel (sign-extend rel 16))
		;the frames of a laced block each its frame's time on, the track's
		;DefaultDuration in the cluster's time, :step
		(each (lambda ((frame_at size))
				(if (or (eql key :t) (and (eql key :simple) (/= 0 (logand flags 0x80))))
					(push (. track :find :keys) (length (. track :find :offsets))))
				(push (. track :find :offsets) (+ start frame_at))
				(push (. track :find :sizes) size)
				(push (. track :find :shown) (+ (. track :find :cluster) rel (* (!) (. track :find :step)))))
			(mkv-laced s (+ at n 3) end (logand (>> flags 1) 3)))))

(defun mkv-element (s body stop start tracks id)
	; (mkv-element str body stop start tracks id)
	;an element of a Cluster, of a str from start: its time, or a block
	(cond
		((= id +mkv_time) (each (# (. %0 :insert :cluster (mkv-uint s body stop))) tracks))
		((= id +mkv_simple) (mkv-block s body stop start tracks :simple))
		((= id +mkv_group)
			;a Block with no ReferenceBlock in its group is a key
			(defq kids (mkv-children s body stop))
			(when (defq block (some (# (if (= (first %0) +mkv_block) %0)) kids))
				(mkv-block s (second block) (third block) start tracks
					(if (some (# (= (first %0) +mkv_reference)) kids) :nil :t))))))

(defun mkv-cluster (stream at end tracks)
	; (mkv-cluster stream at end tracks) -> at
	;a Cluster's time and its blocks, read a window of a MB at a time, an
	;element longer than that read whole: a Cluster of no size, of a
	;recording, ends where the next element of the top is, and that may be
	;the file's end. Its end
	(defq tops (. +mkv :find :tops) window 1048576 done :nil)
	(while (and (not done) (< (+ at 2) end))
		(stream-seek stream at 0)
		(defq s (read-blk stream (min (- end at) window)) i 0 refill :nil ends (>= (+ at (length s)) end))
		;each element whole in the window, its header there too unless the
		;window is the last
		(while (and (not done) (not refill) (< (+ i 2) (length s)) (or ends (<= (+ i 16) (length s))))
			(bind '(_ n id) (mkv-vint s i))
			(cond
				((find id tops) (setq done :t))
				(:t (bind '(size m _) (mkv-vint s (+ i n)))
					(defq body (+ i n m) stop (if (< size 0) (length s) (+ body size)))
					(cond
						((<= stop (length s)) (mkv-element s body stop at tracks id) (setq i stop))
						((= i 0)
							(stream-seek stream at 0)
							(defq whole (read-blk stream (min (- end at) stop)))
							(mkv-element whole body (min stop (length whole)) at tracks id)
							(setq i (min stop (length whole)) refill :t))
						((setq refill :t))))))
		;no element begun in it, its end
		(if (= i 0) (setq done :t))
		(setq at (+ at i)))
	at)

(defun mkv-seeks (s at end)
	; (mkv-seeks str at end) -> ((id at) ...)
	;the SeekHead: of each element of the top it names, where it is from the
	;start of the Segment's data
	(map (lambda ((_ body stop))
			(defq id 0 place 0)
			(each (lambda ((id_of b e))
					(cond
						((= id_of +mkv_seek_id) (setq id (mkv-uint s b e)))
						((= id_of +mkv_seek_pos) (setq place (mkv-uint s b e)))))
				(mkv-children s body stop))
			(list id place))
		(filter (# (= (first %0) +mkv_seek)) (mkv-children s at end))))

(defun mkv-cues (s at end number seg)
	; (mkv-cues str at end number seg) -> ((time at) ...)
	;the Cues of a track: each key's time, and where in the file the Cluster
	;it is in begins, seg the start of the Segment's data, in time order
	(defq out (list))
	(each (lambda ((id body stop))
			(when (= id +mkv_cue)
				(defq time -1 place -1)
				(each (lambda ((id_of b e))
						(cond
							((= id_of +mkv_cue_time) (setq time (mkv-uint s b e)))
							((= id_of +mkv_cue_place)
								(defq track -1 cluster -1)
								(each (lambda ((id_in bb ee))
										(cond
											((= id_in +mkv_cue_track) (setq track (mkv-uint s bb ee)))
											((= id_in +mkv_cue_cluster) (setq cluster (mkv-uint s bb ee)))))
									(mkv-children s b e))
								(and (= track number) (>= cluster 0) (setq place (+ seg cluster))))))
					(mkv-children s body stop))
				(and (>= time 0) (>= place 0) (push out (list time place)))))
		(mkv-children s at end))
	(sort out (# (- (first %0) (first %1)))))

(defun mkv-cluster-at (reel at)
	; (mkv-cluster-at reel at) -> :nil | (end tracks)
	;the frames of the Cluster at at, each track's as a reel's tracks have
	;them, and where it ends: read the once and kept, the last few, as a
	;film is played through them. :nil if there is no Cluster there
	(defq lazy (. reel :find :lazy) kept (. lazy :find :kept))
	(cond
		((defq got (some (# (if (= (first %0) at) (second %0))) kept)) got)
		((and (defq el (mkv-head (defq stream (. reel :find :stream)) at)) (= (first el) +mkv_cluster))
			(bind '(_ size body) el)
			(defq stop (. lazy :find :stop)
				tracks (map (# (scatter (Emap) :number (. %0 :find :number) :step (. %0 :find :step) :cluster 0
					:offsets (list) :sizes (list) :shown (list) :keys (list))) (. reel :find :tracks))
				end (mkv-cluster stream body (if (< size 0) stop (min stop (+ body size))) tracks)
				got (list end tracks))
			(push kept (list at got))
			(if (> (length kept) +mkv_kept) (. lazy :insert :kept (setq kept (slice kept 1 -1))))
			got)))

(defun mkv-number (track shown)
	; (mkv-number track shown) -> n
	;the frame of a track shown at a time, of a film of frames of one length
	(defq lazy_scale (. track :find :scale) ns (. track :find :frame_ns))
	(/ (+ (* (- shown (. track :find :first)) lazy_scale) (/ ns 2)) ns))

(defun mkv-locate (reel which n)
	; (mkv-locate reel which n) -> :nil | (chain index place)
	;frame n of track which, as it is shown: the Clusters from the one of
	;the key before it to the one it is in, (at end tracks) each, which of
	;them it is in, and where it is in that one's frames. From the Cues: its
	;time is its number by the length of a frame
	(defq lazy (. reel :find :lazy) track (elem-get (. reel :find :tracks) which)
		time (+ (. track :find :first) (/ (+ (* n (. track :find :frame_ns)) (/ (. track :find :scale) 2)) (. track :find :scale)))
		cues (. lazy :find :cues) at (second (first cues)) chain (list) found :nil)
	(each (# (if (<= (first %0) time) (setq at (second %0)))) cues)
	;the Cluster that has it is this one or after it, a few at most
	(while (and (not found) (>= at 0) (< (length chain) 64) (defq got (mkv-cluster-at reel at)))
		(push chain (list at (first got) (second got)))
		(defq frames (elem-get (second got) which) shown (. frames :find :shown) i -1)
		(each (# (if (= (mkv-number track %0) n) (setq i (!)))) shown)
		(cond
			((>= i 0) (setq found (list chain (dec (length chain)) i)))
			((and (nempty? shown) (> (mkv-number track (reduce (const max) shown -1)) n)) (setq at -1))
			((setq at (first got)))))
	found)

(defun mkv-track-packet (reel track n)
	; (mkv-track-packet reel track n) -> :nil | str
	;packet n of a track of a reel read as it is played, by its time, as a
	;sound is had: :nil if there is none
	(when (defq which (some (# (if (eql %0 track) (!))) (. reel :find :tracks)))
		(when (defq found (mkv-locate reel which n))
			(bind '(chain index place) found)
			(defq frames (elem-get (third (elem-get chain index)) which) stream (. reel :find :stream))
			(stream-seek stream (elem-get (. frames :find :offsets) place) 0)
			(read-blk stream (elem-get (. frames :find :sizes) place)))))

(defun mkv-frame-time (reel n)
	; (mkv-frame-time reel n) -> seconds
	;when frame n is shown, of a reel read as it is played, by its number:
	;its ms made a fixed, a film of more than 2^31 us is 36 minutes
	(/ (n2f (/ (* n (. (. reel :find :video) :find :frame_ns)) 1000000)) (n2f 1000)))

(defun mkv-before? (a b)
	; (mkv-before? a b) -> :nil | :t
	;is one place of a reel read as it is played before another, each
	;(cluster index), a Cluster by where it is in the file
	(or (< (first a) (first b)) (and (= (first a) (first b)) (< (second a) (second b)))))

(defun mkv-reel (stream &optional few)
	; (mkv-reel stream [few]) -> :nil | reel
	;the reel of the first video track of a Matroska file, every track in
	;:tracks: its EBML header, then its Segment's Info, Tracks and
	;Clusters, the rest stepped over. With few, of one that is not read
	;as it is played, the first Cluster and no more, enough to show its
	;start, the reel :partial.
	;
	;Every Cluster read is the whole of a big film, and so a film whose
	;Cues say where its keys are, and whose frames are each of one length,
	;its Duration said, is read as it is played: the first Cluster now, the
	;reel :lazy, and a frame's Cluster found by its time when it is wanted,
	;(mkv-locate). Of any other every Cluster is read now
	(defq end (stream-seek stream 0 2) head (mkv-head stream 0) tracks (list) scale 1000000
		duration :nil seeks (list) cues_place :nil cues :nil tried :nil)
	(when (and head (= (first head) +mkv_ebml) (defq segment (mkv-head stream (+ (third head) (second head))))
			(= (first segment) +mkv_segment))
		(defq at (third segment) seg at stop (if (< (second segment) 0) end (min end (+ at (second segment)))))
		(while (and (< at stop) (defq el (mkv-head stream at)))
			(bind '(id size body) el)
			(defq el_end (if (< size 0) stop (min stop (+ body size))))
			(cond
				((= id +mkv_info)
					(stream-seek stream body 0)
					(defq s (read-blk stream (- el_end body)))
					(each (lambda ((id b e))
							(cond
								((= id +mkv_scale) (setq scale (mkv-uint s b e)))
								((= id +mkv_length) (setq duration (reel-whole s b (- e b))))))
						(mkv-children s 0 (length s))))
				((= id +mkv_seekhead)
					(stream-seek stream body 0)
					(defq s (read-blk stream (- el_end body)))
					(setq seeks (mkv-seeks s 0 (length s))))
				((= id +mkv_cues) (setq cues_place (list body el_end)))
				((= id +mkv_tracks)
					(stream-seek stream body 0)
					(defq s (read-blk stream (- el_end body)))
					(setq tracks (map (lambda ((id b e)) (mkv-track s b e))
						(filter (# (= (first %0) +mkv_entry)) (mkv-children s 0 (length s)))))
					;a frame's length in the clusters' time, of the scale said in the
					;Info before the Tracks
					(each (# (. %0 :insert :step (/ (ifn (. %0 :find :frame_ns) 0) (max 1 scale)))) tracks))
				((= id +mkv_cluster)
					;at the first, are there Cues of the pictures, and are its frames
					;each of one length: then this Cluster is all that is read now
					(unless tried
						(setq tried :t)
						(and duration (defq picture (some (# (and (eql (. %0 :find :kind) "vide") (. %0 :find :frame_ns) %0)) tracks))
							(or cues_place (and (defq seek (some (# (if (= (first %0) +mkv_cues) %0)) seeks))
								(defq cues_el (mkv-head stream (+ seg (second seek)))) (= (first cues_el) +mkv_cues)
								(setq cues_place (list (third cues_el) (+ (third cues_el) (second cues_el))))))
							(progn (stream-seek stream (first cues_place) 0)
								(defq s (read-blk stream (- (second cues_place) (first cues_place)))))
							(setq cues (mkv-cues s 0 (length s) (. picture :find :number) seg))
							(if (empty? cues) (setq cues :nil))))
					(each (# (. %0 :insert :cluster 0)) tracks)
					(setq el_end (mkv-cluster stream body el_end tracks))
					(if (or few cues) (setq el_end stop))
					(task-slice)))
			(setq at el_end))
		(defq timescale (/ 1000000000 (max 1 scale)) video (some (# (if (eql (. %0 :find :kind) "vide") %0)) tracks) reel (Emap))
		(each (lambda (track)
				(defq shown (. track :find :shown))
				(scatter track :timescale timescale :samples (length (. track :find :offsets))
					:times (sort (cat shown) (const -)) :reordered (some (# (and (> (!) 0) (< %0 (elem-get shown (dec (!)))))) shown)))
			tracks)
		(scatter reel :tracks tracks :stream stream :partial (and few (not cues)))
		(when (and cues video)
			;read as it is played: how many frames by its Duration, each track's
			;frames by their times, (mkv-locate)
			(each (lambda (track)
					(defq shown (. track :find :shown))
					(scatter track :scale scale :first (if (nempty? shown) (reduce (const min) shown (first shown)) 0))
					(if (defq ns (. track :find :frame_ns))
						(. track :insert :samples (/ (* duration scale) ns))))
				tracks)
			(scatter reel :lazy (scatter (Emap) :cues cues :kept (list) :stop stop)
				:frame_time (const mkv-frame-time) :track_packet (const mkv-track-packet)))
		(when video
			(defq frames (. video :find :samples) stamps (. video :find :times)
				span (if (> frames 1) (- (last stamps) (first stamps)) 0))
			(scatter reel :frames frames :width (. video :find :width) :height (. video :find :height)
				:codec (. video :find :codec)
				:rate (if (> span 0) (/ (n2f (dec frames)) (/ (n2f span) (n2f timescale))) 0.0)
				:offsets (. video :find :offsets) :lengths (. video :find :sizes)
				:keys (. video :find :keys) :times stamps :shown (. video :find :shown) :video video
				:start (first stamps) :delay 0)
			;of a reel read as it is played, how many and how fast by its Duration
			(if cues (scatter reel :frames (. video :find :samples)
				:rate (/ (n2f 1000) (/ (n2f (/ (. video :find :frame_ns) 1000)) (n2f 1000))))))
		reel))

;module
(export-symbols '(mkv-reel mkv-locate mkv-cluster-at mkv-track-packet mkv-frame-time mkv-number mkv-before?))
(env-pop)
