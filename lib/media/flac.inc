;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; flac, the Free Lossless Audio Codec, our own decoder
;
; A file is "fLaC", blocks of metadata, STREAMINFO first, then frames.
; A frame is its header, of its block of samples, its rate and how its
; channels are coded, its own CRC-8, then a subframe a channel and the
; CRC-16 of it all. A subframe is one number, the samples as they are,
; or a prediction, of fixed order or by LPC, of the samples before it
; and the residual of it by Rice codes. Lossless, so each sample is as
; it was coded, exactly. (flac-info stream) reads the metadata, and
; (flac-frame info str) gives a frame's samples, nums a channel, and
; how many bytes it was. A file is a reel too, a frame a packet, as an
; MP3 is, and Films plays it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui/canvas/lisp.inc")
(import "./reel.inc")
(import "./bits.inc")

;module
(env-push)

;what the functions read as they run, this one map. Its natives are
;taken as they are first wanted, not as this is read: gui/canvas/lisp.inc
;imports this as a desktop starts, and a system not yet built with them
;would never start to build them, as flm-codec says
;	(media-flac-rice reader out from count k) -> out
;	(media-flac-lpc samples order coeffs shift) -> samples
;	(media-flac-stereo a b mode) -> a
;	(media-flac-ints out ints scale) -> out
(defq +flac (Emap))
(scatter +flac
	;the fixed predictors, of order 0 to 4
	:fixed (list (nums) (nums 1) (nums 2 -1) (nums 3 -3 1) (nums 4 -6 4 -1))
	;the rates and sample sizes of a header's codes, 0 STREAMINFO's
	:rates '(0 88200 176400 192000 8000 16000 22050 24000 32000 44100 48000 96000)
	:sizes '(0 8 12 0 16 20 24 32)
	;the CRC-8 of the header, x^8 + x^2 + x + 1, a byte at a time
	:crc8 (reduce (lambda (out b)
			(defq c b)
			(times 8 (setq c (logand 0xff (if (/= 0 (logand c 0x80)) (logxor (<< c 1) 7) (<< c 1)))))
			(push out c))
		(range 0 256) (cap 256 (nums))))

(defun flac-native (key)
	; (flac-native key) -> function
	(unless (. +flac :find :rice)
		(scatter +flac :rice (ffi "lib/media/media_flac_rice") :lpc (ffi "lib/media/media_flac_lpc")
			:stereo (ffi "lib/media/media_flac_stereo")))
	(. +flac :find key))

(defun flac-bits (in n)
	; (flac-bits reader n) -> num
	;n bits, 0 to 32, high first
	(cond
		((= n 0) 0)
		((> n 24) (+ (<< (bits-get in (- n 16)) 16) (bits-get in 16)))
		((bits-get in n))))

(defun flac-signed (in n)
	; (flac-signed reader n) -> num
	;n bits of two's complement
	(sign-extend (flac-bits in n) n))

(defun flac-unary (in)
	; (flac-unary reader) -> num
	;the 0s before a 1
	(defq n 0)
	(while (= (bits-1 in) 0) (setq n (inc n)))
	n)

(defun flac-crc8 (s from to)
	; (flac-crc8 str from to) -> num
	(defq tab (. +flac :find :crc8) c 0)
	(each! (# (setq c (elem-get tab (logxor c (code %0))))) (list s) from to)
	c)

(defun flac-info (stream)
	; (flac-info stream) -> :nil | info
	;of a file's start, after an ID3v2 tag if it has one: STREAMINFO's block
	;sizes, frame sizes, rate, channels, bits a sample and samples, and the
	;points of a SEEKTABLE, (sample offset) each, the rest stepped over.
	;:start is where the first frame is
	(defq at 0 head (read-blk stream 10))
	(and head (>= (length head) 10) (eql (slice head 0 3) "ID3")
		(setq at (+ 10 (if (/= 0 (logand (code head 1 5) 0x10)) 10 0)
			(reduce (# (+ (<< %0 7) (logand (code head 1 (+ 6 %1)) 127))) (range 0 4) 0))))
	(stream-seek stream at 0)
	(when (eql (read-blk stream 4) "fLaC")
		(defq info (Emap) final 0 ok :t seek (list))
		(setq at (+ at 4))
		(while (and ok (= final 0))
			(defq h (read-blk stream 4))
			(cond
				((or (not h) (< (length h) 4)) (setq ok :nil))
				(:t (setq final (>> (code h 1 0) 7))
					(defq type (logand (code h 1 0) 0x7f) len (code-be h 3 1) body (read-blk stream len))
					(cond
						((or (not body) (< (length body) len)) (setq ok :nil))
						((= type 0)
							(defq in (bits-of body))
							(scatter info :min_block (bits-get in 16) :max_block (bits-get in 16)
								:min_frame (bits-get in 24) :max_frame (bits-get in 24) :rate (bits-get in 20)
								:channels (inc (bits-get in 3)) :bps (inc (bits-get in 5))
								:total (+ (<< (bits-get in 4) 32) (flac-bits in 32))))
						((= type 3)
							(each (lambda (i)
									(defq p (* 18 i) sample (code-be body 8 p))
									;a placeholder point is all ones
									(if (/= sample -1) (push seek (list sample (code-be body 8 (+ p 8))))))
								(range 0 (/ len 18)))))
					(setq at (+ at 4 len)))))
		(and ok (. info :find :rate)
			(scatter info :start at :seek seek))))

(defun flac-residual (in samples order n)
	; (flac-residual reader samples order n)
	;the residual of a subframe of n, from order on: its partitions each
	;of a Rice parameter, of 4 bits or 5, or the escape and its raw bits
	(defq method (bits-get in 2) pbits (if (= method 1) 5 4) escape (if (= method 1) 31 15)
		porder (bits-get in 4) parts (<< 1 porder) at order)
	(each (lambda (p)
			(defq count (if (= p 0) (- (>> n porder) order) (>> n porder)) k (bits-get in pbits))
			(cond
				((= k escape)
					(defq raw (bits-get in 5))
					(each (# (elem-set samples (+ at %0) (flac-signed in raw))) (range 0 count)))
				(((flac-native :rice) in samples at count k)))
			(setq at (+ at count)))
		(range 0 parts)))

(defun flac-subframe (in n bps)
	; (flac-subframe reader n bps) -> nums
	;a channel's n samples of bps bits: one number, the samples as they
	;are, a fixed prediction, or an LPC one, then any bits wasted put back.
	;One the format forbids, an order past its samples, a precision of 16, a
	;shift down by less than 0, a reserved type, is heard as nothing: a
	;release build does not stop at a throw, the error a value that would
	;go on into the natives
	(bits-skip in 1)
	(defq type (bits-get in 6) wasted (if (= (bits-1 in) 1) (inc (flac-unary in)) 0)
		bits (- bps wasted) samples (fill (nums) n 0)
		warm (lambda (order) (each (# (elem-set samples %0 (flac-signed in bits))) (range 0 order))))
	(cond
		((= type 0) (fill samples n (flac-signed in bits)))
		((= type 1) (warm n))
		((and (<= 8 type 12) (<= (- type 8) n))
			(defq order (- type 8))
			(warm order)
			(flac-residual in samples order n)
			((flac-native :lpc) samples order (elem-get (. +flac :find :fixed) order) 0))
		((and (>= type 32) (<= (- type 31) n))
			(defq order (- type 31))
			(warm order)
			(defq precision (inc (bits-get in 4)) shift (flac-signed in 5))
			(when (and (< precision 16) (>= shift 0))
				(defq coeffs (reduce (lambda (out _) (push out (flac-signed in precision))) (range 0 order) (cap order (nums))))
				(flac-residual in samples order n)
				((flac-native :lpc) samples order coeffs shift))))
	(if (> wasted 0) (nums-mul samples (fill (nums) n (<< 1 wasted)) samples) samples))

(defun flac-head (info s)
	; (flac-head info str) -> :nil | head
	;the header of a frame at the start of str, checked by its CRC-8: its
	;block size, sample size, channel assignment, its frame or sample
	;number and which, and the bits of it, by their codes or of STREAMINFO
	;of no more than 16 bytes, the least frame of silence 15, the reader's
	;bytes past them 0 and the CRC-8 to say
	(when (and (>= (length s) 6) (= (code s 1 0) 0xff) (= (logand (code s 1 1) 0xfe) 0xf8))
		(defq in (bits-of (slice s 0 (min 16 (length s)))))
		(bits-skip in 15)
		(defq variable (bits-1 in) bs_code (bits-get in 4) sr_code (bits-get in 4) assign (bits-get in 4) ss_code (bits-get in 3))
		(bits-skip in 1)
		;the frame or sample number, as UTF-8 codes a number
		(defq lead (bits-get in 8) more (cond ((< lead 0x80) 0) ((< lead 0xc0) -1) ((< lead 0xe0) 1) ((< lead 0xf0) 2) ((< lead 0xf8) 3) ((< lead 0xfc) 4) ((< lead 0xfe) 5) (6)))
		(when (>= more 0)
			(defq number (logand lead (if (= more 0) 0x7f (dec (<< 1 (- 6 more))))))
			(times more (setq number (+ (<< number 6) (logand (bits-get in 8) 0x3f))))
			(defq n (cond
					((= bs_code 1) 192)
					((<= 2 bs_code 5) (<< 576 (- bs_code 2)))
					((= bs_code 6) (inc (bits-get in 8)))
					((= bs_code 7) (inc (bits-get in 16)))
					((>= bs_code 8) (<< 256 (- bs_code 8)))
					(0)))
			(cond
				((= sr_code 12) (bits-skip in 8))
				((<= 13 sr_code 14) (bits-skip in 16)))
			(defq bps (if (= ss_code 0) (. info :find :bps) (elem-get (. +flac :find :sizes) ss_code))
				head (>> (bits-at in) 3))
			(and (> n 0) (> bps 0) (< assign 11) (/= sr_code 15) (= (bits-get in 8) (flac-crc8 s 0 head))
				(scatter (Emap) :n n :bps bps :assign assign :number number :variable variable :bits (bits-at in))))))

(defun flac-frame (info s)
	; (flac-frame info str) -> :nil | (chans bytes)
	;the frame at the start of str: its header, a subframe a channel, a
	;side of one bit more, then the channels made left and right; the
	;bytes, the frame aligned to a byte and its CRC-16
	(when (defq h (flac-head info s))
		(defq in (bits-seek (bits-of s) (. h :find :bits)) n (. h :find :n) bps (. h :find :bps) assign (. h :find :assign)
			chans (if (< assign 8)
				(map (lambda (_) (flac-subframe in n bps)) (range 0 (inc assign)))
				(defq a (flac-subframe in n (if (= assign 9) (inc bps) bps)) b (flac-subframe in n (if (= assign 9) bps (inc bps))))
				((flac-native :stereo) a b assign)
				(list a b)))
		(list chans (+ 2 (>> (+ (bits-at in) 7) 3)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a file of FLAC as a reel, a frame a packet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flac-number (reel h)
	; (flac-number reel head) -> num
	;the frame a header is of, or of blocks of sizes that vary its sample,
	;which is what such a frame's header has
	(. h :find :number))

(defun flac-find (reel at)
	; (flac-find reel at) -> :nil | (at frame)
	;the first header from at on, of 2 bytes of sync and its CRC-8 good
	(defq stream (. reel :find :stream) end (. reel :find :end) found :nil)
	(while (and (not found) (< at end))
		(stream-seek stream at 0)
		(defq s (read-blk stream 16384) i 0 ff (char 0xff))
		(cond
			((not s) (setq at end))
			(:t (while (and (not found) (defq j (find ff (slice s i -1))))
					(setq i (+ i j))
					(defq h (flac-head (. reel :find :info) (slice s i (min (length s) (+ i 16)))))
					(if h (setq found (list (+ at i) (flac-number reel h))) (setq i (inc i))))
				(unless found (setq at (+ at (max 1 (- (length s) 16))))))))
	found)

(defun flac-next (reel at n)
	; (flac-next reel at n) -> :nil | (at frame)
	;the header of frame n from at on: the sync and the CRC-8 of a header
	;are in a frame's bytes now and then, those of another number passed
	(defq found (flac-find reel at))
	(while (and found (/= (second found) n)) (setq found (flac-find reel (inc (first found)))))
	found)

(defun flac-index (reel)
	; (flac-index reel) -> reel
	;of a FLAC of blocks of sizes that vary, where each frame is, found the
	;once from the first, each next the one of its sample and its size on
	(defq info (. reel :find :info) stream (. reel :find :stream) at (. reel :find :start) offsets (list) done :nil)
	(until done
		(stream-seek stream at 0)
		(cond
			((defq h (flac-head info (ifn (read-blk stream 16) "")))
				(push offsets at)
				(defq next (flac-next reel (+ at 2) (+ (. h :find :number) (. h :find :n))))
				(if next (setq at (first next)) (setq done :t)))
			((setq done :t))))
	(defq frames (length offsets) total (. info :find :total))
	(scatter reel :offsets offsets :frames frames
		:rate (if (> total 0) (/ (n2f frames) (/ (n2f total) (n2f (. info :find :rate)))) (. reel :find :rate))))

(defun flac-locate (reel n)
	; (flac-locate reel n) -> :nil | at
	;where frame n begins: where one read ended if it was n - 1, else from
	;where the bytes say it should be, back if a later frame is found there,
	;then on header by header to it
	(defq start (. reel :find :start) end (. reel :find :end) frames (. reel :find :frames)
		at (if (defq offsets (. reel :find :offsets)) (elem-get offsets n)
			(some (# (if (= (first %0) n) (second %0))) (. reel :find :known))) tries 0)
	(unless at
		(defq guess (+ start (/ (* n (- end start)) (max 1 frames))) step (/ (- end start) (max 1 frames)))
		(while (and (not at) (< tries 24))
			(defq found (flac-find reel (max start guess)))
			(cond
				((not found) (setq guess (- guess (* 4 step)) tries (inc tries)))
				((> (second found) n) (setq guess (- guess (* (+ 2 (- (second found) n)) step)) tries (inc tries)))
				(:t (while (and found (< (second found) n))
						(setq found (flac-next reel (+ (first found) 2) (inc (second found)))))
					(setq at (and found (= (second found) n) (first found)) tries 24)))))
	at)

(defun flac-packet (reel n)
	; (flac-packet reel n) -> :nil | str
	;frame n, to the header of frame n + 1; the ends of the last four kept
	(when (and (< -1 n (. reel :find :frames)) (defq at (flac-locate reel n)))
		(defq offsets (. reel :find :offsets)
			next (if offsets (if (< (inc n) (length offsets)) (list (elem-get offsets (inc n)))) (flac-next reel (+ at 2) (inc n)))
			to (if next (first next) (. reel :find :end))
			known (. reel :find :known) stream (. reel :find :stream))
		(. reel :insert :known (cat (list (list (inc n) to)) (slice known 0 (min 3 (length known)))))
		(stream-seek stream at 0)
		(read-blk stream (- to at))))

(defun flac-reel (stream)
	; (flac-reel stream) -> :nil | reel
	;a file of FLAC as a reel of its frames, each found as it is wanted, of
	;STREAMINFO's block size, :rate of them a second. Its :tracks the one of
	;sound, of codec "flac", its pictures the samples of each frame, as an
	;MP3's
	(when (defq info (flac-info stream))
		(defq end (stream-seek stream 0 2) block (max 1 (. info :find :max_block)) total (. info :find :total)
			start (. info :find :start)
			;of no count of samples, by the bytes of a frame between the least
			;and the most, else of half the bytes of its samples as they are
			frame_bytes (max 1 (if (> (. info :find :max_frame) 0) (/ (+ (. info :find :min_frame) (. info :find :max_frame)) 2)
				(/ (* block (. info :find :channels) (. info :find :bps)) 16)))
			frames (if (> total 0) (/ (+ total block -1) block) (max 1 (/ (- end start) frame_bytes)))
			reel (scatter (Emap) :stream stream :info info :start start :end end :block block :known (list)
				:frames frames :rate (/ (n2f (. info :find :rate)) (n2f block))
				:width 512 :height 192 :codec "flac" :audio :t))
		;of blocks of sizes that vary, each frame's place found the once
		(stream-seek stream start 0)
		(and (defq h (flac-head info (ifn (read-blk stream 16) ""))) (= (. h :find :variable) 1)
			(flac-index reel))
		(. reel :insert :tracks (list (scatter (Emap) :kind "soun" :codec "flac" :samples (. reel :find :frames) :start 0)))
		reel))

(defun flac-pcm (info chans)
	; (flac-pcm info chans) -> (left right)
	;a frame's channels as reals at the scale of 16 bits, the first two, the
	;one of one on both
	(reel-pcm chans (pow (n2r 2) (n2r (- 16 (. info :find :bps))))))

(defun flac-codec (reel &optional scale)
	; (flac-codec reel [scale]) -> codec
	;what shows a frame: its samples, the left above the right, on a canvas
	;of its own; a scale is not taken
	(scatter (Emap) :canvas (Canvas (. reel :find :width) (. reel :find :height) 1) :scale 1 :reel reel :at -1))

(defun flac-picture (codec n)
	; (flac-picture codec n) -> canvas
	(defq reel (. codec :find :reel) canvas (. codec :find :canvas))
	(unless (= n (. codec :find :at))
		(. codec :insert :at n)
		(defq packet (flac-packet reel n) got (if packet (catch (flac-frame (. reel :find :info) packet) :t)))
		(reel-wave canvas (. reel :find :width) (. reel :find :height)
			(if (list? got) (flac-pcm (. reel :find :info) (first got)))))
	canvas)

(reel-kind ".flac" flac-reel flac-packet flac-codec flac-picture)

;module
(export-symbols '(flac-info flac-frame flac-pcm flac-reel flac-packet flac-codec flac-picture))
(env-pop)
