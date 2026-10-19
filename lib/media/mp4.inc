;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; mp4, the films of phones and of the web, ISO base media and the
; QuickTime it came of, .mp4 .m4v .mov, as a reel, lib/media/reel.inc
;
; A file is boxes, each its size, 4 bytes high first, and a name of four
; letters, and a box may hold boxes. moov holds what the file is: a trak a
; track, its kind, vide or soun, its codec, avc1 hvc1 mp4a and the like,
; and the tables of its samples, where each is in the file, how long, its
; time, and which can be started from. mdat holds the samples, and may be
; before moov or after it. moov is read whole and the rest stepped over.
;
; (mp4-reel stream) gives the reel of the first video track: its packets
; are its samples in the order they are decoded, its rate the frames a
; second their times come to, and :shown when each is shown, which with
; B-frames is another order. :tracks says what every track is.
;
; The codec is the host's, src/host/video.cpp, for what it has one for,
; avc1, H.264, by VideoToolbox on macOS. (reel-codec) of a film it has
; none for gives :nil. Frame n is the one shown nth: the codec feeds the
; host the packets till that picture is out, and to go anywhere else it
; starts again from the key before it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./reel.inc")

(defun mp4-n (s at bytes)
	;a number of so many bytes, high first
	(code-be s bytes at))

(defun mp4-boxes (s at end)
	; (mp4-boxes str at end) -> ((name body end) ...)
	;the boxes from at to end of a str: each name, where what is in it
	;starts, and where it ends
	(defq out (list))
	(while (<= (+ at 8) end)
		(defq size (mp4-n s at 4) name (slice s (+ at 4) (+ at 8)) head 8)
		(cond
			((= size 1) (setq size (mp4-n s (+ at 8) 8) head 16))
			((= size 0) (setq size (- end at))))
		(cond
			((or (< size head) (> (+ at size) end)) (setq at end))
			(:t (push out (list name (+ at head) (+ at size)))
				(setq at (+ at size)))))
	out)

(defun mp4-in (s box path)
	; (mp4-in str box path) -> :nil | box
	;the first box down a path of names, in a box
	(each (lambda (name)
			(and box (setq box (some (# (if (eql (first %0) name) %0)) (mp4-boxes s (second box) (third box))))))
		path)
	box)

(defun mp4-moov (stream)
	; (mp4-moov stream) -> :nil | str
	;the moov box of a file, what is in it, its other boxes stepped over
	(defq end (stream-seek stream 0 2) at 0 moov :nil)
	(while (and (not moov) (< (+ at 8) end))
		(stream-seek stream at 0)
		(defq head (read-blk stream 16))
		(when (and head (>= (length head) 8))
			(defq size (mp4-n head 0 4) skip 8)
			(cond
				((= size 1) (setq size (mp4-n head 8 8) skip 16))
				((= size 0) (setq size (- end at))))
			(cond
				((< size skip) (setq at end))
				((eql (slice head 4 8) "moov")
					(stream-seek stream (+ at skip) 0)
					(setq moov (read-blk stream (- size skip))))
				(:t (setq at (+ at size))))))
	moov)

(defun mp4-table (s box head width)
	; (mp4-table str box head width) -> ((n ...) ...)
	;the entries of a table box: after its version and flags, a count, and
	;the entries, each so many 4 byte numbers, head more bytes before them
	(when box
		(defq at (+ (second box) 4 head) count (mp4-n s at 4) out (list))
		(setq at (+ at 4))
		(times count
			(push out (map (# (mp4-n s (+ at (* %0 4)) 4)) (range 0 width)))
			(setq at (+ at (* width 4))))
		out))

(defun mp4-config (s at end)
	; (mp4-config str at end) -> ((name str) ...)
	(map (lambda ((name body end)) (list name (slice s body end))) (mp4-boxes s at end)))

(defun mp4-track (s trak)
	; (mp4-track str trak) -> track
	;what a track is, and where each of its samples is, how long, when,
	;and which are keys
	(defq track (Emap) mdhd (mp4-in s trak '("mdia" "mdhd")) hdlr (mp4-in s trak '("mdia" "hdlr"))
		stbl (mp4-in s trak '("mdia" "minf" "stbl")))
	;its time, the units of a second it counts in
	(defq v (code s 1 (second mdhd)))
	(scatter track :kind (slice s (+ (second hdlr) 8) (+ (second hdlr) 12))
		:timescale (mp4-n s (+ (second mdhd) (if (= v 1) 20 12)) 4)
		:duration (mp4-n s (+ (second mdhd) (if (= v 1) 24 16)) (if (= v 1) 8 4)))
	;the first sample entry, its codec, size or sound, and its setup
	(when (defq stsd (mp4-in s stbl '("stsd")))
		(defq entry (+ (second stsd) 8))
		(. track :insert :codec (slice s (+ entry 4) (+ entry 8)))
		(defq entry_end (+ entry (mp4-n s entry 4)))
		(cond
			((eql (. track :find :kind) "vide")
				(scatter track :width (mp4-n s (+ entry 32) 2) :height (mp4-n s (+ entry 34) 2)
					:config (mp4-config s (+ entry 86) entry_end)))
			((eql (. track :find :kind) "soun")
				(scatter track :channels (mp4-n s (+ entry 24) 2) :bits (mp4-n s (+ entry 26) 2)
					:sample_rate (mp4-n s (+ entry 32) 2)
					:config (mp4-config s (+ entry 36) entry_end)))))
	;the samples: their sizes
	(defq stsz (mp4-in s stbl '("stsz")) sizes (list))
	(when stsz
		(defq one (mp4-n s (+ (second stsz) 4) 4) count (mp4-n s (+ (second stsz) 8) 4))
		(setq sizes (if (= one 0)
			(map (# (mp4-n s (+ (second stsz) 12 (* %0 4)) 4)) (range 0 count))
			(map (lambda (_) one) (range 0 count)))))
	;where each chunk is, and how many samples each holds
	(defq chunks (cond
			((defq co (mp4-in s stbl '("stco"))) (map (const first) (mp4-table s co 0 1)))
			((defq co (mp4-in s stbl '("co64")))
				(map (# (mp4-n s (+ (second co) 8 (* %0 8)) 8)) (range 0 (mp4-n s (+ (second co) 4) 4))))
			((list)))
		runs (mp4-table s (mp4-in s stbl '("stsc")) 0 3) offsets (list) at 0)
	(each (lambda (chunk_at)
			(defq chunk (inc (!)) per 0)
			;the run this chunk is in, the last that starts at or before it
			(each (# (if (<= (first %0) chunk) (setq per (second %0)))) runs)
			(times per
				(when (< (length offsets) (length sizes))
					(push offsets chunk_at)
					(setq chunk_at (+ chunk_at (elem-get sizes (dec (length offsets))))))))
		chunks)
	;when each is, and which are keys, all of them if it does not say
	(defq when_at (list) clock 0)
	(each (lambda ((count delta)) (times count (push when_at clock) (setq clock (+ clock delta))))
		(ifn (mp4-table s (mp4-in s stbl '("stts")) 0 2) (list)))
	(defq keys (if (defq stss (mp4-in s stbl '("stss")))
		(map (# (dec (first %0))) (mp4-table s stss 0 1)) :all))
	;when each is shown: its time, and what ctts adds to it where frames
	;are decoded in another order than they are shown, B-frames. Version 1
	;of ctts has the offsets signed, one may be before its time
	(defq shown (cat when_at))
	(when (defq ctts (mp4-in s stbl '("ctts")))
		(defq signed (= (code s 1 (second ctts)) 1) at 0)
		(each (lambda ((count offset))
				(and signed (>= offset 0x80000000) (setq offset (- offset 0x100000000)))
				(times count
					(when (< at (length shown))
						(elem-set shown at (+ (elem-get shown at) offset))
						(setq at (inc at)))))
			(mp4-table s ctts 0 2)))
	(scatter track :sizes sizes :offsets offsets :times when_at :shown shown :keys keys :samples (length sizes))
	track)

(defun mp4-reel (stream)
	; (mp4-reel stream) -> :nil | reel
	;the reel of the first video track of a file, every track in :tracks
	(when (defq moov (mp4-moov stream))
		(defq tracks (map (# (mp4-track moov %0))
				(filter (# (eql (first %0) "trak")) (mp4-boxes moov 0 (length moov))))
			video (some (# (if (eql (. %0 :find :kind) "vide") %0)) tracks)
			reel (Emap))
		(. reel :insert :tracks tracks)
		(when video
			;the rate the times come to, over the whole of it
			(defq frames (. video :find :samples) when_at (. video :find :times)
				span (if (> frames 1) (- (last when_at) (first when_at)) 0))
			(scatter reel :stream stream :frames frames :width (. video :find :width)
				:height (. video :find :height) :codec (. video :find :codec)
				:rate (if (> span 0) (/ (* (n2f (dec frames)) (n2f (. video :find :timescale))) (n2f span)) 0.0)
				:offsets (. video :find :offsets) :lengths (. video :find :sizes)
				:keys (. video :find :keys) :times when_at :shown (. video :find :shown) :video video))
		reel))

(defun mp4-packet (reel index)
	; (mp4-packet reel index) -> :nil | str
	;the bytes of a sample of the video track
	(when (< -1 index (ifn (. reel :find :frames) 0))
		(defq stream (. reel :find :stream))
		(stream-seek stream (elem-get (. reel :find :offsets) index) 0)
		(read-blk stream (elem-get (. reel :find :lengths) index))))

(defun mp4-codec (reel)
	; (mp4-codec reel) -> :nil | codec
	;the host's decoder of the video track's codec, on a canvas of its own,
	;:nil if it has none. Its native is taken as the codec is made, not as
	;this is read, as flm-codec does
	(and (defq video (. reel :find :video) config (. video :find :config)
			setup (some (# (if (eql (first %0) "avcC") (second %0))) config))
		(defq native (ffi "service/gui/lisp_video")
			handle (native 0 (code-be (. reel :find :codec) 4 0) setup 0))
		(/= handle 0)
		(scatter (Emap) :native native :handle handle :setup setup :reel reel :fed 0 :at -1
			:canvas (Canvas (. reel :find :width) (. reel :find :height) 1)
			;frame n is the packet shown nth
			:order (sort (range 0 (. reel :find :frames))
				(lambda (a b) (- (elem-get (. reel :find :shown) a) (elem-get (. reel :find :shown) b)))))))

(defun mp4-feed (codec n)
	;the packets fed to the host till the picture shown nth is out on the
	;canvas. One before the last given, or after the next key, the host
	;starts again from the key before its packet; one after it, before the
	;next key, is fed on to, which is no more work
	(defq reel (. codec :find :reel) native (. codec :find :native) frames (. reel :find :frames)
		packet (elem-get (. codec :find :order) n) shown (elem-get (. reel :find :shown) packet)
		pixmap (getf (. codec :find :canvas) +canvas_pixmap 0) fed (. codec :find :fed)
		keys (. reel :find :keys) key 0)
	(if (eql keys :all) (setq key packet) (each (# (if (<= %0 packet) (setq key %0))) keys))
	(when (or (< n (. codec :find :at)) (> key fed))
		(native 3 (. codec :find :handle) "" 0)
		(. codec :insert :handle (native 0 (code-be (. reel :find :codec) 4 0) (. codec :find :setup) 0))
		(setq fed key))
	;a picture is out a few packets after its own at most, if it is not by
	;then it will not be, its packet would not decode
	(defq last (min frames (+ packet 32)))
	(while (and (< (native 2 (. codec :find :handle) pixmap shown) 0) (< fed last))
		(native 1 (. codec :find :handle) (mp4-packet reel fed) (elem-get (. reel :find :shown) fed))
		(setq fed (inc fed)))
	(scatter codec :fed fed :at n))

(defun mp4-frame (codec n)
	; (mp4-frame codec n) -> :nil | canvas
	;the picture shown nth on the canvas, as it is if it is there already
	(unless (= n (. codec :find :at)) (mp4-feed codec n))
	(. codec :find :canvas))

(defun mp4-close (codec)
	; (mp4-close codec) -> :nil
	((. codec :find :native) 3 (. codec :find :handle) "" 0)
	(. codec :insert :handle 0)
	:nil)

(each (# (reel-kind %0 mp4-reel mp4-packet mp4-codec mp4-frame mp4-close)) '(".mp4" ".m4v" ".mov"))
