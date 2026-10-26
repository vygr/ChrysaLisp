;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; avi, a film of Microsoft's RIFF AVI as a reel
;
; A file is chunks, each four letters, a length of four bytes low first,
; and what it holds, made an even length. RIFF AVI holds LIST hdrl: the
; avih, and a LIST strl for each stream, strh, what kind it is and how
; fast, strf, its picture's or its sound's format. Then LIST movi, the
; frames, each a chunk named by its stream, 00dc 01wb, and idx1, where
; each is and which are keys. A file past 1GB goes on in RIFF AVIX parts,
; a movi each, and is found by the OpenDML index of each stream, the indx
; of its strl, of ix chunks. With neither index, a file cut short, the
; movi are read a chunk at a time.
;
; (avi-reel stream) gives the reel in the shape lib/media/mp4.inc's codec
; takes, as mkv-reel does, and every stream in :tracks. Its pictures go to
; the host's decoder, or of Motion JPEG to ours, (avi-mjpeg-codec). Each
; stream of sound is a stream of its own, (avi-track-stream), of all of its
; chunks one after the other wherever they are: an MP3's frames are found in
; it as in a file of MP3, and the rest are of packets of a size.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui/canvas/lisp.inc")
(import "././streams/lisp_stream.inc")
(import "./bits.inc")

;module
(env-push)

;what the four letters of a picture's compression are, as an mp4 names
;its codec, and the numbers of the formats of sound
(defq +avi (Emap))
(scatter +avi
	:pictures (list
		"MJPG" "jpeg" "AVRN" "jpeg" "LJPG" "jpeg" "JPEG" "jpeg" "DMB1" "jpeg"
		"H264" "avc1" "X264" "avc1" "AVC1" "avc1" "DAVC" "avc1" "VSSH" "avc1"
		"XVID" "mp4v" "DIVX" "mp4v" "DX50" "mp4v" "FMP4" "mp4v" "MP4V" "mp4v" "M4S2" "mp4v"
		"3IV2" "mp4v" "BLZ0" "mp4v" "DXGM" "mp4v"
		"DIV3" "div3" "MP43" "div3" "DIV4" "div3" "MPG3" "div3" "AP41" "div3" "COL1" "div3")
	:sounds (list 0x1 "pcm" 0x2 "adpcm_ms" 0x11 "adpcm_ima" 0x55 "mp3" 0x50 "mp2"
		0x2000 "ac-3" 0x2001 "dts" 0xff "mp4a" 0x1600 "mp4a" 0x706d "mp4a"))

(defun avi-word (s at)
	; (avi-word str at) -> num
	;four bytes, low first
	(if (<= (+ at 4) (length s)) (code s 4 at) 0))

(defun avi-children (s at end)
	; (avi-children str at end) -> ((id body stop kind) ...)
	;the chunks of a str from at to end: its four letters, where what it
	;holds begins and ends, and of a LIST or RIFF the four letters of what
	;kind. A length past the end is the end, a file cut short
	(defq out (list) end (min end (length s)))
	(while (<= (+ at 8) end)
		(defq id (slice s at (+ at 4)) size (avi-word s (+ at 4)) body (+ at 8)
			stop (min end (+ body size)))
		(push out (if (and (find id '("LIST" "RIFF")) (<= (+ body 4) end))
			(list id (+ body 4) stop (slice s body (+ body 4)))
			(list id body stop "")))
		(setq at (+ body size (logand size 1))))
	out)

(defun avi-read (stream at len)
	; (avi-read stream at len) -> str
	;bytes of the file, as many as it has from at
	(stream-seek stream at 0)
	(ifn (read-blk stream len) ""))

(defun avi-tops (stream end)
	; (avi-tops stream end) -> ((id at stop kind) ...)
	;the chunks at the top of each RIFF of the file, where each begins in
	;the file, its hdrl and the movi of each read later as they are wanted
	(defq out (list) at 0)
	(while (<= (+ at 12) end)
		(defq head (avi-read stream at 12) size (avi-word head 4) stop (min end (+ at 8 size)))
		(cond
			((and (eql (slice head 0 4) "RIFF") (find (slice head 8 12) '("AVI " "AVIX")))
				(defq inner (+ at 12))
				(while (<= (+ inner 8) stop)
					(defq h (avi-read stream inner 12) id (slice h 0 4) n (avi-word h 4))
					(push out (list id (+ inner (if (eql id "LIST") 12 8)) (min stop (+ inner 8 n))
						(if (eql id "LIST") (slice h 8 12) "")))
					(setq inner (+ inner 8 n (logand n 1))))
				(setq at (+ at 8 size (logand size 1))))
			((setq at end))))
	out)

(defun avi-number (id)
	; (avi-number id) -> :nil | num
	;the stream a chunk of the movi is of, the two digits it begins with
	(and (find (first id) "0123456789") (find (second id) "0123456789") (str-as-num (slice id 0 2))))

(defun avi-stream (s body stop)
	; (avi-stream str body stop) -> track
	;a strl: its strh, strf and indx, what kind, its codec as an mp4's,
	;its size or its sound
	(defq track (scatter (Emap) :offsets (list) :sizes (list) :keys (list) :shown (list)
		:start 0 :delay 0 :config (list) :avi :t))
	(each (lambda ((id b e kind))
			(cond
				((eql id "strh")
					(scatter track :type (slice s b (+ b 4)) :handler (slice s (+ b 4) (+ b 8))
						:scale (max 1 (avi-word s (+ b 20))) :rate (max 1 (avi-word s (+ b 24)))
						:length (avi-word s (+ b 32)) :sample_size (avi-word s (+ b 44))))
				((eql id "strf") (. track :insert :format (slice s b e)))
				((eql id "indx") (. track :insert :indx (slice s b e)))))
		(avi-children s body stop))
	(defq format (ifn (. track :find :format) "") type (. track :find :type))
	(cond
		((eql type "vids")
			(defq fourcc (to-upper (slice format 16 (min 20 (length format)))) names (. +avi :find :pictures)
				i (some (# (and (= (logand (!) 1) 0) (eql %0 fourcc) (!))) names)
				height (avi-word format 8))
			(scatter track :kind "vide" :fourcc fourcc :codec (if i (elem-get names (inc i)) (to-lower fourcc))
				:width (avi-word format 4) :height (if (>= height 0x80000000) (- 0x100000000 height) height)
				:private (slice format (min 40 (length format)) -1)))
		((eql type "auds")
			(defq tag (if (>= (length format) 2) (code format 2 0) 0) sounds (. +avi :find :sounds))
			;of the extensible format, its own number is the start of its GUID
			(and (= tag 0xfffe) (>= (length format) 26) (setq tag (code format 2 24)))
			(defq i (some (# (and (= (logand (!) 1) 0) (= %0 tag) (!))) sounds))
			(scatter track :kind "soun" :tag tag :codec (if i (elem-get sounds (inc i)) (str "0x" (hex-encode (char tag 2))))
				:channels (if (>= (length format) 4) (code format 2 2) 0)
				:sample_rate (avi-word format 4) :block_align (if (>= (length format) 14) (code format 2 12) 0)
				:bits (if (>= (length format) 16) (code format 2 14) 0)
				:extra (slice format (min 18 (length format)) -1)))
		(:t (. track :insert :kind "other")))
	track)

(defun avi-chunk (track offset size key)
	; (avi-chunk track offset size key)
	;a chunk of a stream's into its tables, where what it holds is in the
	;file, how long, a key or not
	(if key (push (. track :find :keys) (length (. track :find :offsets))))
	(push (. track :find :offsets) offset)
	(push (. track :find :sizes) size))

(defun avi-idx1 (stream at stop movi tracks)
	; (avi-idx1 stream at stop movi tracks) -> :t | :nil
	;the chunks of idx1, each 16 bytes, its id, flags, where it is and how
	;long. Where, from the movi's four letters as it should be, or of some
	;writers from the start of the file: the first is looked at to see
	(defq s (avi-read stream at (- stop at)) n (/ (length s) 16) base movi)
	(when (> n 0)
		(defq first_at (avi-word s 8))
		(unless (eql (avi-read stream (+ movi first_at) 4) (slice s 0 4)) (setq base 0))
		(each (lambda (i)
				(defq e (* i 16) id (slice s e (+ e 4)) number (avi-number id))
				(and number (< number (length tracks)) (not (ends-with "pc" id))
					(avi-chunk (elem-get tracks number) (+ base (avi-word s (+ e 8)) 8) (avi-word s (+ e 12))
						(/= 0 (logand (avi-word s (+ e 4)) 0x10)))))
			(range 0 n))
		:t))

(defun avi-odml? (track)
	; (avi-odml? track) -> :t | :nil
	;has a stream an OpenDML index, an indx of indexes of chunks
	(and (defq indx (. track :find :indx)) (>= (length indx) 24) (= (code indx 1 3) 0) (> (avi-word indx 4) 0)))

(defun avi-odml (stream track)
	; (avi-odml stream track) -> :t | :nil
	;the OpenDML index of a stream: its indx, of the ix chunks each of a
	;part of it, each a base and the chunks from it, a top bit set not a key
	(when (and (defq indx (. track :find :indx)) (>= (length indx) 24) (= (code indx 1 3) 0))
		(defq entries (avi-word indx 4) longs (code indx 2 0) any :nil)
		(each (lambda (i)
				(defq e (+ 24 (* i longs 4)) at (code indx 8 e) size (avi-word indx (+ e 8))
					ix (avi-read stream (+ at 8) (- size 8)))
				(when (and (>= (length ix) 24) (= (code ix 1 3) 1))
					(defq count (avi-word ix 4) per (code ix 2 0) base (code ix 8 12))
					(each (lambda (j)
							(defq f (+ 24 (* j per 4)) size (avi-word ix (+ f 4)))
							(avi-chunk track (+ base (avi-word ix f)) (logand size 0x7fffffff) (= 0 (logand size 0x80000000))))
						(range 0 count))
					(setq any :t)))
			(range 0 entries))
		any))

(defun avi-mp4v-key? (s)
	; (avi-mp4v-key? str) -> :t | :nil
	;has a frame of MPEG-4 Part 2 a VOP coded as I: its start code, 0x1b6,
	;and the two bits after it 0
	(defq i 0 key :nil found :nil)
	(while (and (not found) (<= (+ i 5) (length s)))
		(cond
			((and (= (code s 1 i) 0) (= (code s 1 (+ i 1)) 0) (= (code s 1 (+ i 2)) 1) (= (code s 1 (+ i 3)) 0xb6))
				(setq found :t key (= 0 (logand (code s 1 (+ i 4)) 0xc0))))
			((setq i (inc i)))))
	key)

(defun avi-avc-key? (s)
	; (avi-avc-key? str) -> :t | :nil
	;has a frame of H.264, its NALs each after a start code, an IDR slice
	(defq i 0 key :nil)
	(while (and (not key) (<= (+ i 4) (length s)))
		(if (and (= (code s 1 i) 0) (= (code s 1 (+ i 1)) 0) (= (code s 1 (+ i 2)) 1))
			(setq key (= 5 (logand (code s 1 (+ i 3)) 0x1f)) i (+ i 3))
			(setq i (inc i))))
	key)

(defun avi-start-code (s byte)
	; (avi-start-code str byte) -> :nil | index
	;where a start code, 00 00 01 and that byte, is first in a str
	(defq i 0 at :nil)
	(while (and (not at) (<= (+ i 4) (length s)))
		(if (and (= (code s 1 (+ i 2)) 1) (= (code s 1 (+ i 3)) byte) (= (code s 1 i) 0) (= (code s 1 (inc i)) 0))
			(setq at i) (setq i (inc i))))
	at)

(defun avi-mp4v-headers (stream track)
	; (avi-mp4v-headers stream track) -> str
	;the VOS, VO and VOL of MPEG-4 Part 2: the extra bytes of its format if
	;they have its VOL, else the start of its first frame, all before its
	;first VOP
	(defq private (ifn (. track :find :private) "") offsets (. track :find :offsets))
	(cond
		((and (nempty? private) (avi-start-code private 0x20)) private)
		((nempty? offsets)
			(defq s (avi-read stream (first offsets) (min 4096 (first (. track :find :sizes)))))
			(slice s 0 (ifn (avi-start-code s 0xb6) (length s))))
		("")))

(defun avi-scan-keys (stream track)
	; (avi-scan-keys stream track) -> (n ...)
	;the keys of a stream of pictures from their own bytes: of Motion JPEG
	;each, of MPEG-4 a VOP coded as I, of H.264 an IDR slice, of DivX 3 a
	;frame whose first bit is 0
	(defq offsets (. track :find :offsets) sizes (. track :find :sizes) codec (. track :find :codec) keys (list))
	(each (lambda (at size)
			;of H.264 past what is before its slice, its parameter sets and the
			;encoder's notes, of the rest the first bytes
			(defq s (if (> size 0) (avi-read stream at (min size (if (eql codec "avc1") 4096 64))) ""))
			(if (cond
					((eql codec "jpeg"))
					((eql codec "mp4v") (avi-mp4v-key? s))
					((eql codec "avc1") (avi-avc-key? s))
					;its picture type the first two bits, I 0
					((eql codec "div3") (and (nempty? s) (= 0 (logand (code s 1 0) 0xc0))))
					((= (!) 0)))
				(push keys (!))))
		offsets sizes)
	keys)

(defun avi-scan (stream movis tracks)
	; (avi-scan stream movis tracks)
	;the chunks of each movi read one after another, as there is no index
	;to say: a file cut short, or one whose index is lost. A LIST rec in it
	;is gone in to. Which are keys is not said: of a picture of one frame
	;each, Motion JPEG, all are, of the rest what their own bytes say
	(each (lambda ((at stop))
			(while (<= (+ at 8) stop)
				(defq h (avi-read stream at 12) id (slice h 0 4) size (avi-word h 4))
				(cond
					((eql id "LIST") (setq at (+ at 12)))
					(:t (and (defq number (avi-number id)) (< number (length tracks)) (not (ends-with "pc" id))
							(<= (+ at 8 size) stop)
							(avi-chunk (elem-get tracks number) (+ at 8) size :nil))
						(setq at (+ at 8 size (logand size 1)))))
				(task-slice)))
		movis)
	(each (lambda (track)
			(if (eql (. track :find :kind) "vide")
				(. track :insert :keys (avi-scan-keys stream track))))
		tracks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a stream of the chunks of one stream
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enums +avi_ts 0
	(enum stream offsets sizes starts at bytes))

(defun avi-ts-piece (st at)
	; (avi-ts-piece st at) -> index
	;the chunk that byte at of the stream is in, by halves of the starts
	(defq starts (elem-get st +avi_ts_starts) lo 0 hi (dec (length starts)))
	(while (< lo hi)
		(defq mid (/ (+ lo hi 1) 2))
		(if (<= (elem-get starts mid) at) (setq lo mid) (setq hi (dec mid))))
	lo)

(defun avi-ts-fill (st)
	; (avi-ts-fill st) -> :nil | str
	;what is next, the rest of the chunk it is in, no more than 64KB
	(bind '(stream offsets sizes starts at bytes) st)
	(when (< at bytes)
		(defq i (avi-ts-piece st at) into (- at (elem-get starts i))
			n (min 65536 (- (elem-get sizes i) into))
			got (avi-read stream (+ (elem-get offsets i) into) n))
		(elem-set st +avi_ts_at (+ at (length got)))
		(if (nempty? got) got)))

(defun avi-ts-seek (st offset whence)
	; (avi-ts-seek st offset whence) -> pos
	(defq bytes (elem-get st +avi_ts_bytes) pos (max 0 (min bytes (cond
		((= whence 0) offset) ((= whence 1) (+ offset (elem-get st +avi_ts_at))) ((+ bytes offset))))))
	(elem-set st +avi_ts_at pos)
	pos)

(defun avi-packet (reel n)
	; (avi-packet reel n) -> :nil | str
	;the bytes of frame n of the pictures, as the file has them
	(when (< -1 n (ifn (. reel :find :frames) 0))
		(avi-read (. reel :find :stream) (elem-get (. reel :find :offsets) n) (elem-get (. reel :find :lengths) n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; H.264: an AVI has its frames of Annex B, NALs each after a start code,
; and no times: the order they are shown in is their picture order count,
; of the slice header, by the field widths its SPS says
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun avi-nals (s &optional prefix)
	; (avi-nals str [prefix]) -> ((type at end) ...)
	;the NALs of a frame: each after a start code, Annex B, or with prefix,
	;of an avcC, each after a number of that many bytes high first
	(defq out (list) n (length s))
	(cond
		(prefix
			(defq i 0)
			(while (<= (+ i prefix) n)
				(defq size (code-be s prefix i) at (+ i prefix) end (min n (+ at size)))
				(if (< at end) (push out (list (logand (code s 1 at) 0x1f) at end)))
				(setq i end)))
		(:t (defq i 0 start :nil)
			(while (<= (+ i 3) n)
				(cond
					((and (= (code s 1 (+ i 2)) 1) (= (code s 1 i) 0) (= (code s 1 (inc i)) 0))
						;a start code of 4 bytes, the zero before it is not the NAL's
						(if start (push out (list (logand (code s 1 start) 0x1f) start
							(if (and (> i start) (= (code s 1 (dec i)) 0)) (dec i) i))))
						(setq start (+ i 3) i (+ i 3)))
					((setq i (inc i)))))
			(and start (< start n) (push out (list (logand (code s 1 start) 0x1f) start n)))))
	out)

(defun avi-rbsp (s at end)
	; (avi-rbsp str at end) -> str
	;a NAL's bytes after its header, each 3 that follows two 0s taken out
	(defq parts (list "") from (inc at) i (inc at) zeros 0)
	(while (< i end)
		(defq b (code s 1 i))
		(cond
			((and (>= zeros 2) (= b 3)) (push parts (slice s from i)) (setq from (inc i) zeros 0))
			((= b 0) (setq zeros (inc zeros)))
			((setq zeros 0)))
		(setq i (inc i)))
	(push parts (slice s from end))
	(apply (const cat) parts))

(defun avi-avcc-sps (avcc)
	; (avi-avcc-sps avcc) -> :nil | rbsp
	;the first SPS of an avcC, its bytes after its header
	(when (and (> (length avcc) 8) (> (logand (code avcc 1 5) 0x1f) 0))
		(defq size (code-be avcc 2 6))
		(if (<= (+ 8 size) (length avcc)) (avi-rbsp avcc 8 (+ 8 size)))))

(defun avi-ue (in)
	; (avi-ue reader) -> num
	;a number of Exp-Golomb: so many 0s, a 1, and that many bits
	(defq zeros 0)
	(while (and (< zeros 31) (= (bits-1 in) 0)) (setq zeros (inc zeros)))
	(+ (dec (<< 1 zeros)) (if (> zeros 0) (bits-get in zeros) 0)))

(defun avi-se (in)
	; (avi-se reader) -> num
	(defq v (avi-ue in))
	(if (= (logand v 1) 1) (/ (inc v) 2) (- (/ v 2))))

(defun avi-sps (rbsp)
	; (avi-sps rbsp) -> (frame_bits poc_type poc_bits frame_mbs_only)
	;of an SPS what a slice header's widths are
	(defq in (bits-of rbsp) profile (bits-get in 8))
	(bits-skip in 16)
	(avi-ue in)
	(when (find profile '(100 110 122 244 44 83 86 118 128 138 139 134 135))
		(defq chroma (avi-ue in))
		(if (= chroma 3) (bits-skip in 1))
		(avi-ue in) (avi-ue in) (bits-skip in 1)
		;the scaling lists, each flagged, each of 16 or 64 deltas
		(when (= (bits-1 in) 1)
			(each (lambda (i)
					(when (= (bits-1 in) 1)
						(defq last 8 next 8)
						(times (if (< i 6) 16 64)
							(if (/= next 0) (setq next (logand (+ last (avi-se in) 256) 255)))
							(if (/= next 0) (setq last next)))))
				(range 0 (if (= chroma 3) 12 8)))))
	(defq frame_bits (+ 4 (avi-ue in)) poc_type (avi-ue in) poc_bits 0)
	(cond
		((= poc_type 0) (setq poc_bits (+ 4 (avi-ue in))))
		((= poc_type 1) (bits-skip in 1) (avi-se in) (avi-se in) (times (avi-ue in) (avi-se in))))
	(avi-ue in) (bits-skip in 1) (avi-ue in) (avi-ue in)
	(list frame_bits poc_type poc_bits (bits-1 in)))

(defun avi-avc-poc (s nals sps)
	; (avi-avc-poc str nals sps) -> :nil | (idr ref lsb)
	;of a frame's first slice, is it an IDR, is it a reference, and its
	;picture order count's low bits
	(bind '(frame_bits poc_type poc_bits frame_mbs) sps)
	(when (defq slice (some (# (if (find (first %0) '(1 5)) %0)) nals))
		(bind '(type at end) slice)
		(defq in (bits-of (avi-rbsp s at (min end (+ at 64)))) idr (= type 5)
			ref (/= 0 (logand (code s 1 at) 0x60)))
		(avi-ue in) (avi-ue in) (avi-ue in)
		(bits-skip in frame_bits)
		(and (= frame_mbs 0) (= (bits-1 in) 1) (bits-skip in 1))
		(if idr (avi-ue in))
		(list idr ref (if (= poc_type 0) (bits-get in poc_bits) 0))))

(defun avi-avc (stream video)
	; (avi-avc stream video)
	;of a stream of H.264, its avcC, made of its first SPS and PPS if its
	;format has none, and the order its frames are shown in: by IDR, each
	;frame from one to the next in the order of its picture order count
	(defq private (ifn (. video :find :private) "") offsets (. video :find :offsets) sizes (. video :find :sizes)
		prefix (and (> (length private) 6) (= (code private 1 0) 1) (inc (logand (code private 1 4) 3))))
	(. video :insert :prefix prefix)
	(when (nempty? offsets)
		(defq first_frame (avi-read stream (first offsets) (min 65536 (first sizes))) nals (avi-nals first_frame prefix)
			sps_nal (some (# (if (= (first %0) 7) %0)) nals) pps_nal (some (# (if (= (first %0) 8) %0)) nals))
		;its setup: the format's avcC, or of the first frame's SPS and PPS,
		;each its NAL with its header, after two bytes of its length
		(cond
			(prefix (. video :insert :config (list (list "avcC" private))))
			((and sps_nal pps_nal)
				(defq sps (slice first_frame (second sps_nal) (third sps_nal))
					pps (slice first_frame (second pps_nal) (third pps_nal)))
				(. video :insert :config (list (list "avcC" (cat (char 1) (slice sps 1 4) (char 0xff) (char 0xe1)
					(char-be (length sps) 2) sps (char 1) (char-be (length pps) 2) pps))))))
		;the order shown, of each frame's slice header
		(defq rbsp (cond
				(sps_nal (avi-rbsp first_frame (second sps_nal) (third sps_nal)))
				(prefix (avi-avcc-sps private))))
		(when rbsp
			(defq sps (avi-sps rbsp) max_lsb (<< 1 (third sps)) msb 0 prev 0 period 0 pocs (list) shown (list))
			(defun period-end (upto)
				;the frames since the last IDR, each its place by its count
				(defq order (sort (map (# (+ (<< (+ (elem-get pocs %0) 0x40000000) 20) %0)) (range 0 (length pocs))) (const -)))
				(each (# (elem-set shown (+ period (logand %0 0xfffff)) (+ period (!)))) order)
				(setq period upto pocs (list)))
			(each (lambda (at size)
					(defq s (avi-read stream at (min size (if (= (!) 0) 65536 512))) got (avi-avc-poc s (avi-nals s prefix) sps))
					(push shown (!))
					(cond
						((not got) (push pocs (if (empty? pocs) 0 (inc (last pocs)))))
						(:t (bind '(idr ref lsb) got)
							(and idr (> (!) period) (period-end (!)))
							(if idr (setq msb 0 prev 0))
							(cond
								((and (< lsb prev) (>= (- prev lsb) (/ max_lsb 2))) (setq msb (+ msb max_lsb)))
								((and (> lsb prev) (> (- lsb prev) (/ max_lsb 2))) (setq msb (- msb max_lsb))))
							(push pocs (+ msb lsb))
							(if ref (setq prev lsb)))))
				offsets sizes)
			(period-end (length offsets))
			(defq step (. video :find :scale))
			(. video :insert :shown (map (# (* %0 step)) shown))
			(. video :insert :reordered (some (# (/= %0 (!))) shown)))))

(defun avi-avc-packet (reel n)
	; (avi-avc-packet reel n) -> :nil | str
	;a frame of H.264 of Annex B as the host's decoder takes it, as an mp4
	;has it: each NAL after four bytes of its length, the delimiters left out
	(when (defq s (avi-packet reel n))
		(if (. (. reel :find :video) :find :prefix) s
			(apply (const cat) (cat (list "") (map (lambda ((type at end)) (cat (char-be (- end at) 4) (slice s at end)))
				(filter (# (/= (first %0) 9)) (avi-nals s))))))))

;;;;;;;;;;;;;;;;;;;;;
; Motion JPEG, ours
;;;;;;;;;;;;;;;;;;;;;

(defun avi-mjpeg-codec (reel &optional scale)
	; (avi-mjpeg-codec reel [scale]) -> codec
	;a picture of each frame by lib/image/jpg.inc, put on a canvas of its own
	(defq scale (ifn scale 1))
	(scatter (Emap) :reel reel :at -1 :scale scale :mjpeg :t
		:canvas (Canvas (/ (. reel :find :width) scale) (/ (. reel :find :height) scale) scale)))

(defun avi-mjpeg-frame (codec n)
	; (avi-mjpeg-frame codec n) -> :nil | canvas
	;frame n, a picture of its own, made the size of the canvas and put on
	;it. One that does not decode leaves the one before
	(defq canvas (. codec :find :canvas))
	(unless (= n (. codec :find :at))
		(defq reel (. codec :find :reel) packet (avi-packet reel n)
			picture (and packet (nempty? packet)
				(catch (canvas-load "frame.jpg" +load_flag_noswap :nil (string-stream packet)) :nil)))
		(if picture (. canvas :resize picture))
		(. codec :insert :at n))
	canvas)

;;;;;;;;;;;;;
; the reel
;;;;;;;;;;;;;

(defun avi-reel (stream)
	; (avi-reel stream) -> :nil | reel
	;the reel of the first stream of pictures of an AVI, every stream in
	;:tracks. Its chunks from the OpenDML index if there is one, else idx1,
	;else as the movi are read. Each a time a frame of its own, the rate
	;its strh says, in the order they are in the file
	(defq end (stream-seek stream 0 2) head (avi-read stream 0 12))
	(when (and (eql (slice head 0 4) "RIFF") (eql (slice head 8 12) "AVI "))
		(defq tops (avi-tops stream end) tracks (list) movis (list) idx1 :nil)
		(each (lambda ((id at stop kind))
				(cond
					((and (eql id "LIST") (eql kind "hdrl"))
						(defq s (avi-read stream at (- stop at)))
						(each (lambda ((id b e kind))
								(and (eql id "LIST") (eql kind "strl") (push tracks (avi-stream s b e))))
							(avi-children s 0 (length s))))
					((and (eql id "LIST") (eql kind "movi")) (push movis (list at stop)))
					((and (eql id "idx1") (not idx1)) (setq idx1 (list at stop)))))
			tops)
		(cond
			;each stream's OpenDML index, if each has one, else idx1
			((and (nempty? tracks) (every (const avi-odml?) tracks))
				(each (# (avi-odml stream %0)) tracks))
			((and idx1 (nempty? movis) (avi-idx1 stream (first idx1) (second idx1) (- (first (first movis)) 4) tracks)))
			((avi-scan stream movis tracks)))
		(defq reel (Emap) video (some (# (if (eql (. %0 :find :kind) "vide") %0)) tracks))
		;each chunk a frame a step of the stream's on, a stream of sound its
		;bytes one stream, (avi-track-stream)
		(each (lambda (track)
				(defq step (. track :find :scale) n (length (. track :find :offsets)))
				(scatter track :timescale (. track :find :rate) :samples n
					:shown (map (# (* %0 step)) (range 0 n)) :reordered :nil)
				(. track :insert :times (. track :find :shown)))
			tracks)
		(scatter reel :tracks tracks :stream stream)
		;the setup the host's decoder is given: of MPEG-4 Part 2 and Motion
		;JPEG the size, and of MPEG-4 its headers before its first picture
		;of H.264 its avcC, the order shown, and its packets re-framed
		(when (and video (eql (. video :find :codec) "avc1"))
			(avi-avc stream video)
			(. reel :insert :packet_fn (const avi-avc-packet)))
		(when (and video (find (. video :find :codec) '("mp4v" "jpeg")))
			(defq size (cat (char (. video :find :width) 4) (char (. video :find :height) 4)))
			(. video :insert :config (list (list "setup" (if (eql (. video :find :codec) "mp4v")
				(cat size (avi-mp4v-headers stream video)) size)))))
		(when video
			(defq frames (. video :find :samples))
			(scatter reel :frames frames :width (. video :find :width) :height (. video :find :height)
				:codec (. video :find :codec) :fourcc (. video :find :fourcc)
				:rate (/ (n2f (. video :find :rate)) (n2f (. video :find :scale)))
				:offsets (. video :find :offsets) :lengths (. video :find :sizes)
				:keys (. video :find :keys) :times (. video :find :times) :shown (. video :find :shown)
				:video video :start 0 :delay 0 :reordered (. video :find :reordered)))
		reel))

;module
(export-symbols '(avi-reel avi-packet avi-mjpeg-codec avi-mjpeg-frame avi-ts-fill avi-ts-seek))
(env-pop)

;made after the module: in a function of a module a name put into a
;backquote is looked for as the function runs, and by then the module's
;own names are gone, lib/net/tls.inc says so too
(defun avi-track-stream (reel track)
	; (avi-track-stream reel track) -> stream
	;all of a stream's chunks, one after the other, as a stream of its own
	;that can be seeked, wherever they are in the file: a new one each time
	(defq starts (list) at 0)
	(each (# (push starts at) (setq at (+ at %0))) (. track :find :sizes))
	(defq st (list (. reel :find :stream) (. track :find :offsets) (. track :find :sizes) starts 0 at))
	(lisp-stream `(lambda () (,avi-ts-fill ',st))
		`(lambda (offset whence) (,avi-ts-seek ',st offset whence))))
