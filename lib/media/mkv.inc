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

;module
(env-push)

;the ids, as they are written, the length bits in them
(defq +mkv_ebml 0x1a45dfa3 +mkv_segment 0x18538067 +mkv_info 0x1549a966 +mkv_scale 0x2ad7b1
	+mkv_tracks 0x1654ae6b +mkv_entry 0xae +mkv_number 0xd7 +mkv_type 0x83 +mkv_codec 0x86
	+mkv_private 0x63a2 +mkv_duration 0x23e383 +mkv_video 0xe0 +mkv_width 0xb0 +mkv_height 0xba
	+mkv_audio 0xe1 +mkv_rate 0xb5 +mkv_channels 0x9f +mkv_delay 0x56aa
	+mkv_cluster 0x1f43b675 +mkv_time 0xe7 +mkv_simple 0xa3 +mkv_group 0xa0 +mkv_block 0xa1 +mkv_reference 0xfb)

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

(defun mkv-float (s at end)
	; (mkv-float str at end) -> num
	;a whole number held as a float of 4 or 8 bytes, a rate of sound
	(defq bits (code-be s (- end at) at))
	(cond
		((= (- end at) 4)
			(defq power (- (logand (>> bits 23) 0xff) 127))
			(if (< power 0) 0 (>> (logior (logand bits 0x7fffff) 0x800000) (- 23 power))))
		(:t (defq power (- (logand (>> bits 52) 0x7ff) 1023))
			(if (< power 0) 0 (>> (logior (logand bits 0xfffffffffffff) 0x10000000000000) (- 52 power))))))

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
					(defq name (slice s body stop) codecs (. +mkv :find :codecs)
						i (some (# (and (= (logand (!) 1) 0) (eql %0 name) (!))) codecs))
					(scatter track :matroska name :codec (if i (elem-get codecs (inc i)) name)))
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
								((= id +mkv_rate) (. track :insert :sample_rate (mkv-float s body stop)))
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
					(times (- count 2)
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
		(if (>= rel 0x8000) (setq rel (- rel 0x10000)))
		(each (lambda ((frame_at size))
				(if (or (eql key :t) (and (eql key :simple) (/= 0 (logand flags 0x80))))
					(push (. track :find :keys) (length (. track :find :offsets))))
				(push (. track :find :offsets) (+ start frame_at))
				(push (. track :find :sizes) size)
				(push (. track :find :shown) (+ (. track :find :cluster) rel)))
			(mkv-laced s (+ at n 3) end (logand (>> flags 1) 3)))))

(defun mkv-cluster (stream at end tracks)
	; (mkv-cluster stream at end tracks) -> at
	;a Cluster's time and its blocks, read the once; its end, of a Cluster
	;of no size where the next element of the top is
	(stream-seek stream at 0)
	(defq s (read-blk stream (- end at)) tops (. +mkv :find :tops) i 0 done :nil)
	(while (and (not done) (< (+ i 2) (length s)))
		(bind '(_ n id) (mkv-vint s i))
		(cond
			((find id tops) (setq done :t))
			(:t (bind '(size m _) (mkv-vint s (+ i n)))
				(defq body (+ i n m) stop (if (< size 0) (length s) (min (length s) (+ body size))))
				(cond
					((= id +mkv_time) (each (# (. %0 :insert :cluster (mkv-uint s body stop))) tracks))
					((= id +mkv_simple) (mkv-block s body stop at tracks :simple))
					((= id +mkv_group)
						;a Block with no ReferenceBlock in its group is a key
						(defq kids (mkv-children s body stop))
						(when (defq block (some (# (if (= (first %0) +mkv_block) %0)) kids))
							(mkv-block s (second block) (third block) at tracks
								(if (some (# (= (first %0) +mkv_reference)) kids) :nil :t)))))
				(setq i stop))))
	(+ at i))

(defun mkv-reel (stream)
	; (mkv-reel stream) -> :nil | reel
	;the reel of the first video track of a Matroska file, every track in
	;:tracks: its EBML header, then its Segment's Info, Tracks and
	;Clusters, the rest stepped over
	(defq end (stream-seek stream 0 2) head (mkv-head stream 0) tracks (list) scale 1000000)
	(when (and head (= (first head) +mkv_ebml) (defq segment (mkv-head stream (+ (third head) (second head))))
			(= (first segment) +mkv_segment))
		(defq at (third segment) stop (if (< (second segment) 0) end (min end (+ at (second segment)))))
		(while (and (< at stop) (defq el (mkv-head stream at)))
			(bind '(id size body) el)
			(defq el_end (if (< size 0) stop (min stop (+ body size))))
			(cond
				((= id +mkv_info)
					(stream-seek stream body 0)
					(defq s (read-blk stream (- el_end body)))
					(each (lambda ((id b e)) (if (= id +mkv_scale) (setq scale (mkv-uint s b e)))) (mkv-children s 0 (length s))))
				((= id +mkv_tracks)
					(stream-seek stream body 0)
					(defq s (read-blk stream (- el_end body)))
					(setq tracks (map (lambda ((id b e)) (mkv-track s b e))
						(filter (# (= (first %0) +mkv_entry)) (mkv-children s 0 (length s))))))
				((= id +mkv_cluster)
					(each (# (. %0 :insert :cluster 0)) tracks)
					(setq el_end (mkv-cluster stream body el_end tracks))
					(task-slice)))
			(setq at el_end))
		(defq timescale (/ 1000000000 (max 1 scale)) video (some (# (if (eql (. %0 :find :kind) "vide") %0)) tracks) reel (Emap))
		(each (lambda (track)
				(defq shown (. track :find :shown))
				(scatter track :timescale timescale :samples (length (. track :find :offsets))
					:times (sort (cat shown) (const -)) :reordered (some (# (and (> (!) 0) (< %0 (elem-get shown (dec (!)))))) shown)))
			tracks)
		(scatter reel :tracks tracks :stream stream)
		(when video
			(defq frames (. video :find :samples) times (. video :find :times)
				span (if (> frames 1) (- (last times) (first times)) 0))
			(scatter reel :frames frames :width (. video :find :width) :height (. video :find :height)
				:codec (. video :find :codec)
				:rate (if (> span 0) (/ (n2f (dec frames)) (/ (n2f span) (n2f timescale))) 0.0)
				:offsets (. video :find :offsets) :lengths (. video :find :sizes)
				:keys (. video :find :keys) :times times :shown (. video :find :shown) :video video
				:start (first times) :delay 0))
		reel))

;module
(export-symbols '(mkv-reel))
(env-pop)
