;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; mp3, MPEG audio layer III, ISO/IEC 11172-3 and 13818-3, our own decoder
;
; A frame is a header of 4 bytes, side information, and main data that
; may begin in the frames before it, the bit reservoir. Its 2 granules
; of MPEG-1, or 1 of MPEG-2, are 576 lines a channel: scale factors, the
; lines by Huffman codes, requantised, the pair's middle and side made
; left and right, short blocks reordered, aliases reduced, the IMDCT of
; 36 or three of 12 a subband, overlapped, and the 32 subbands made
; samples by the polyphase filterbank. (mp3-header str at) reads what a
; frame is, (mp3-decoder) holds what goes from frame to frame, and
; (mp3-frame decoder frame) gives its samples, a reals a channel, at the
; scale of 16 bits.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./bits.inc")
(import "./huff.inc")

;module
(env-push)

(import "./mp3_tables.inc")

;what the functions read as they run, this one map, the tables of
;mp3_tables.inc taken in as this is read
(defq +mp3 (Emap))
(scatter +mp3 :book_codes *mp3_books* :quad_codes *mp3_quads* :half_window *mp3_window*
	;the rates of MPEG-1, MPEG-2 and MPEG-2.5, by the index of the header
	:rates '((44100 48000 32000) (22050 24000 16000) (11025 12000 8000))
	;the bit rates in kbit/s, of MPEG-1 and of the lower rates
	:bitrates '((0 32 40 48 56 64 80 96 112 128 160 192 224 256 320)
		(0 8 16 24 32 40 48 56 64 80 96 112 128 144 160))
	;the widths of the long and short bands, by the rate, 44100 48000
	;32000 22050 24000 16000 11025 12000 8000
	:long '((4 4 4 4 4 4 6 6 8 8 10 12 16 20 24 28 34 42 50 54 76 158)
		(4 4 4 4 4 4 6 6 6 8 10 12 16 18 22 28 34 40 46 54 54 192)
		(4 4 4 4 4 4 6 6 8 10 12 16 20 24 30 38 46 56 68 84 102 26)
		(6 6 6 6 6 6 8 10 12 14 16 20 24 28 32 38 46 52 60 68 58 54)
		(6 6 6 6 6 6 8 10 12 14 16 18 22 26 32 38 46 54 62 70 76 36)
		(6 6 6 6 6 6 8 10 12 14 16 20 24 28 32 38 46 52 60 68 58 54)
		(6 6 6 6 6 6 8 10 12 14 16 20 24 28 32 38 46 52 60 68 58 54)
		(6 6 6 6 6 6 8 10 12 14 16 20 24 28 32 38 46 52 60 68 58 54)
		(12 12 12 12 12 12 16 20 24 28 32 40 48 56 64 76 90 2 2 2 2 2))
	:short '((4 4 4 4 6 8 10 12 14 18 22 30 56) (4 4 4 4 6 6 10 12 14 16 20 26 66)
		(4 4 4 4 6 8 12 16 20 26 34 42 12) (4 4 4 6 6 8 10 14 18 26 32 42 18)
		(4 4 4 6 8 10 12 14 18 24 32 44 12) (4 4 4 6 8 10 12 14 18 24 30 40 18)
		(4 4 4 6 8 10 12 14 18 24 30 40 18) (4 4 4 6 8 10 12 14 18 24 30 40 18)
		(8 8 8 12 16 20 24 28 36 2 2 2 26))
	:pretab '(0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 2 2 3 3 3 2 0)
	;the bits of the scale factors of MPEG-1, slen1 and slen2, by the
	;scalefac_compress
	:slen '((0 0) (0 1) (0 2) (0 3) (3 0) (1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3) (4 2) (4 3))
	;the book and the more bits of each table select, of the books of
	;mp3_tables.inc from 1, 0 none
	:select '((0 0) (1 0) (2 0) (3 0) (0 0) (4 0) (5 0) (6 0) (7 0) (8 0) (9 0) (10 0) (11 0) (12 0) (0 0) (13 0)
		(14 1) (14 2) (14 3) (14 4) (14 6) (14 8) (14 10) (14 13) (15 4) (15 5) (15 6) (15 7) (15 8) (15 9) (15 11) (15 13))
	;the aliases' butterflies, the c of the standard's cs and ca
	:alias '(-0.6 -0.535 -0.33 -0.185 -0.095 -0.041 -0.0142 -0.0037))

(ffi (path-to-absolute "./media_huff") mp3-native-huff)
; (mp3-native-huff data at table) -> num

(defun mp3-pi ()
	; (mp3-pi) -> real
	;pi, by Newton from 3, sin(x) to 0
	(defq x (n2r 3))
	(times 4 (setq x (- x (/ (sin x) (cos x)))))
	x)

(defun mp3-rows (rows cols f)
	; (mp3-rows rows cols f) -> (reals ...)
	;a matrix, each row a reals, of (f row col)
	(map (lambda (i) (reduce (# (push %0 (f i %1))) (range 0 cols) (cap cols (reals)))) (range 0 rows)))

(defun mp3-tables ()
	; (mp3-tables) -> tables
	;made the once, as the first decoder is: the starts of the long bands
	;by the rate, |x|^4/3 to 8206, the butterflies, the cosines of the IMDCT
	;of 36 and of 12, the windows of the 4 block types and of the short, the
	;matrix of the filterbank and its window of 512
	(unless (. +mp3 :find :made)
		(defq pi (mp3-pi) third (/ (n2r 4) (n2r 3)))
		(scatter +mp3 :made :t
			:starts (map (lambda (widths) (reduce (# (push %0 (+ (last %0) %1))) widths (list 0))) (. +mp3 :find :long))
			:pow (reduce (# (push %0 (if (= %1 0) (n2r 0) (pow (n2r %1) third)))) (range 0 8207) (cap 8207 (reals)))
			:cs (map (# (/ (n2r 1) (sqrt (+ (n2r 1) (* (n2r %0) (n2r %0)))))) (. +mp3 :find :alias))
			:ca (map (# (/ (n2r %0) (sqrt (+ (n2r 1) (* (n2r %0) (n2r %0)))))) (. +mp3 :find :alias))
			:c36 (mp3-rows 36 18 (lambda (i k) (cos (/ (* pi (n2r (* (+ (* 2 i) 19) (inc (* 2 k))))) (n2r 72)))))
			:c12 (mp3-rows 12 6 (lambda (i k) (cos (/ (* pi (n2r (* (+ (* 2 i) 7) (inc (* 2 k))))) (n2r 24)))))
			:synth (mp3-rows 64 32 (lambda (i k) (cos (/ (* pi (n2r (* (+ 16 i) (inc (* 2 k))))) (n2r 64))))))
		(defq sin36 (lambda (i) (sin (/ (* pi (+ (n2r i) (n2r 0.5))) (n2r 36))))
			sin12 (lambda (i) (sin (/ (* pi (+ (n2r i) (n2r 0.5))) (n2r 12))))
			zero (n2r 0) one (n2r 1))
		(. +mp3 :insert :windows (list
			(reduce (# (push %0 (sin36 %1))) (range 0 36) (reals))
			(reduce (# (push %0 (cond ((< %1 18) (sin36 %1)) ((< %1 24) one) ((< %1 30) (sin12 (- %1 18))) (zero)))) (range 0 36) (reals))
			(reduce (# (push %0 (sin12 %1))) (range 0 12) (reals))
			(reduce (# (push %0 (cond ((< %1 6) zero) ((< %1 12) (sin12 (- %1 6))) ((< %1 18) one) ((sin36 %1))))) (range 0 36) (reals))))
		;D of the standard, the half given and its mirror
		(defq d (fill (reals) 512 (n2r 0)) k (n2r 65536))
		(each (lambda (v)
				(defq i (!))
				(elem-set d i (/ (n2r v) k))
				(if (< 0 i 256) (elem-set d (- 512 i) (/ (n2r (if (= 0 (logand i 63)) v (neg v))) k))))
			(. +mp3 :find :half_window))
		(. +mp3 :insert :d d))
	+mp3)

(defun mp3-header (s at)
	; (mp3-header str at) -> :nil | header
	;the 4 bytes at at, if they are a header of layer III: MPEG-1, 2 or 2.5,
	;the rate and its index of the band tables, the mode and its extension,
	;the channels, granules and samples, and the bytes of the frame
	(when (<= (+ at 4) (length s))
		(defq h (code-be s 4 at))
		(and (= (>> h 21) 0x7ff) (= (logand (>> h 17) 3) 1)
			(defq ver (logand (>> h 19) 3) br (logand (>> h 12) 15) sr (logand (>> h 10) 3))
			(/= ver 1) (< 0 br 15) (/= sr 3)
			(defq lsf (/= ver 3) kind (case ver (3 0) (2 1) (:t 2)) mode (logand (>> h 6) 3)
				rate (elem-get (elem-get (. +mp3 :find :rates) kind) sr)
				kbps (elem-get (elem-get (. +mp3 :find :bitrates) (if lsf 1 0)) br))
			(scatter (Emap) :lsf lsf :rate rate :sri (+ (* 3 kind) sr) :crc (= 0 (logand (>> h 16) 1))
				:mode mode :ext (logand (>> h 4) 3) :channels (if (= mode 3) 1 2) :granules (if lsf 1 2)
				:samples (if lsf 576 1152) :size (+ (/ (* (if lsf 72 144) kbps 1000) rate) (logand (>> h 9) 1))))))

(defun mp3-frames (s)
	; (mp3-frames str) -> ((at size) ...)
	;the frames of the bytes of a file, after an ID3v2 tag if it has one,
	;each found where the one before ends, or the next header after
	(defq at 0 out (list))
	(and (>= (length s) 10) (eql (slice s 0 3) "ID3")
		(setq at (+ 10 (reduce (# (+ (<< %0 7) (logand (code s 1 (+ 6 %1)) 127))) (range 0 4) 0))))
	(while (< (+ at 4) (length s))
		(cond
			((and (defq h (mp3-header s at)) (<= (+ at (. h :find :size)) (length s)))
				(push out (list at (. h :find :size)))
				(setq at (+ at (. h :find :size))))
			((setq at (inc at)))))
	out)

(defun mp3-granule (in lsf sri)
	; (mp3-granule reader lsf sri) -> granule
	;of a channel of a granule, its side information: the bits of its scale
	;factors and lines, the big values, the gain, the scale factors' code,
	;the block type, mixed or not, the tables of its regions and the pairs
	;of each, the gains of short windows, and the flags after
	(defq part (bits-get in 12) big (bits-get in 9) gain (bits-get in 8) sfc (bits-get in (if lsf 9 4)))
	(defq bt 0 mixed 0 tables :nil sbg (list 0 0 0) regions :nil)
	(cond
		((= (bits-1 in) 1)
			(setq bt (bits-get in 2) mixed (bits-1 in) tables (list (bits-get in 5) (bits-get in 5) 0)
				sbg (list (bits-get in 3) (bits-get in 3) (bits-get in 3))
				regions (list (cond ((= bt 2) (if (= sri 8) 36 18)) ((<= sri 2) 18) ((/= sri 8) 27) (54)) 288)))
		(:t (setq tables (list (bits-get in 5) (bits-get in 5) (bits-get in 5)))
			(defq r1 (bits-get in 4) r2 (bits-get in 3) starts (elem-get (. +mp3 :find :starts) sri))
			(setq regions (list (/ (elem-get starts (inc r1)) 2) (/ (elem-get starts (min (+ r1 r2 2) 22)) 2)))))
	(defq e0 (min (first regions) big) e1 (min (second regions) big))
	(scatter (Emap) :part part :big big :gain gain :sfc sfc :bt bt :mixed mixed :tables tables :sbg sbg
		:pairs (list e0 (- e1 e0) (- big e1))
		:preflag (if lsf (if (>= sfc 500) 1 0) (bits-1 in)) :sfscale (bits-1 in) :count1 (bits-1 in)))

(defun mp3-side (in h)
	; (mp3-side reader header) -> (main_data_begin scfsi granules)
	;the side information: where the main data begins, back in the
	;reservoir, the scale factors each channel shares between its two
	;granules, and each granule's channels
	(defq lsf (. h :find :lsf) nch (. h :find :channels) mdb (bits-get in (if lsf 8 9)))
	(bits-skip in (if lsf nch (if (= nch 1) 5 3)))
	(list mdb (map (lambda (_) (if lsf 0 (bits-get in 4))) (range 0 nch))
		(map (lambda (_) (map (lambda (_) (mp3-granule in lsf (. h :find :sri))) (range 0 nch))) (range 0 (. h :find :granules)))))

(defun mp3-scales (in g gr scfsi prev)
	; (mp3-scales reader granule gr scfsi prev) -> (sf ...)
	;the scale factors of MPEG-1: of short blocks 3 a band, of a mixed one
	;the 8 long first, of long the 4 groups of bands, any the first granule
	;gives the second by the scfsi; each so many bits by its slen
	(bind '(s1 s2) (elem-get (. +mp3 :find :slen) (. g :find :sfc)))
	(defq sfs (lambda (n bits) (map (lambda (_) (if (> bits 0) (bits-get in bits) 0)) (range 0 n))))
	(if (= (. g :find :bt) 2)
		(cat (sfs (if (= 1 (. g :find :mixed)) 17 18) s1) (sfs 18 s2) (list 0 0 0))
		(push (apply (const cat) (map (lambda (n bits k start)
				(if (and (= gr 1) (/= 0 (logand scfsi (>> 8 k)))) (slice prev start (+ start n)) (sfs n bits)))
			'(6 5 5 5) (list s1 s1 s2 s2) '(0 1 2 3) '(0 6 11 16))) 0)))

(defun mp3-huff (in table)
	; (mp3-huff reader table) -> index
	;the code next, what it stands for, by the native reader of a book's table
	(defq got (mp3-native-huff (bits-str in) (bits-at in) table))
	(bits-seek in (logand got 0xffffffff))
	(>> got 32))

(defun mp3-lines (d in g end)
	; (mp3-lines decoder reader granule end) -> nums
	;the 576 lines of a channel's granule, quantised and signed: the big
	;values by pairs, each region by its table, 15 the more bits of its
	;table and then the sign; then quads of 1 and 0 till the end of the
	;part, a quad that goes past it not had
	(defq lines (fill (nums) 576 0) at 0 books (. d :find :books) select (. +mp3 :find :select))
	(each (lambda (pairs table)
			(bind '(book more) (elem-get select table))
			(cond
				((= book 0) (setq at (+ at (* 2 pairs))))
				(:t (bind '(w tab) (elem-get books (dec book)))
					(times pairs
						(when (< (bits-at in) end)
							(defq v (mp3-huff in tab) x (/ v w) y (% v w))
							(and (= x 15) (> more 0) (setq x (+ x (bits-get in more))))
							(and (> x 0) (= 1 (bits-1 in)) (setq x (neg x)))
							(and (= y 15) (> more 0) (setq y (+ y (bits-get in more))))
							(and (> y 0) (= 1 (bits-1 in)) (setq y (neg y)))
							(elem-set lines at x)
							(elem-set lines (inc at) y))
						(setq at (+ at 2))))))
		(. g :find :pairs) (. g :find :tables))
	(defq quad (elem-get (. d :find :quads) (. g :find :count1)) more :t)
	(while (and more (<= at 572) (< (bits-at in) end))
		(defq v (mp3-huff in quad)
			vals (map (# (if (and (/= 0 (logand v %0)) (= 1 (bits-1 in))) -1 (if (/= 0 (logand v %0)) 1 0))) '(8 4 2 1)))
		(cond
			((> (bits-at in) end) (setq more :nil))
			(:t (each (# (elem-set lines (+ at (!)) %0)) vals)
				(setq at (+ at 4)))))
	(bits-seek in end)
	lines)

(defun mp3-band (out lines at width e pow)
	;a band's lines requantised, |x|^4/3 by 2^(e/4), signed
	(defq f (pow (n2r 2) (/ (n2r e) (n2r 4))))
	(each! (lambda (v)
			(elem-set out (!) (if (< v 0) (neg (* (elem-get pow (neg v)) f)) (* (elem-get pow v) f))))
		(list lines) at (+ at width)))

(defun mp3-requant (g lines sf sri)
	; (mp3-requant granule lines sf sri) -> reals
	;each line by the gain of its band: the global gain, less its scale
	;factor and the pretab by the scale, of a short window its gain too
	(defq out (fill (reals) 576 (n2r 0)) gain (- (. g :find :gain) 210) shift (inc (. g :find :sfscale))
		pow (. +mp3 :find :pow) bt (. g :find :bt) mixed (. g :find :mixed) at 0
		long_end (if (= bt 2) (if (= mixed 1) (if (<= sri 2) 8 6) 0) 22)
		short_start (if (= bt 2) (if (= mixed 1) 3 0) 13)
		pre (if (= 1 (. g :find :preflag)) (. +mp3 :find :pretab) (fill (list) 22 0)))
	(each! (lambda (width s p)
			(mp3-band out lines at width (- gain (<< (+ s p) shift)) pow)
			(setq at (+ at width)))
		(list (elem-get (. +mp3 :find :long) sri) sf pre) 0 long_end)
	(defq k long_end)
	(each! (lambda (width)
			(each (lambda (sb)
					(mp3-band out lines at width (- gain (<< (elem-get sf k) shift) (* 8 sb)) pow)
					(setq at (+ at width) k (inc k)))
				(. g :find :sbg)))
		(list (elem-get (. +mp3 :find :short) sri)) short_start 13)
	out)

(defun mp3-reorder (x g sri)
	;a short block's lines from band, window, line to band, line, window
	(when (= (. g :find :bt) 2)
		(defq at (if (= 1 (. g :find :mixed)) 36 0))
		(each! (lambda (len)
				(defq band (slice x at (+ at (* 3 len))))
				(each (lambda (i)
						(each (# (elem-set x (+ at (* 3 i) %0) (elem-get band (+ (* %0 len) i)))) '(0 1 2)))
					(range 0 len))
				(setq at (+ at (* 3 len))))
			(list (elem-get (. +mp3 :find :short) sri)) (if (= 1 (. g :find :mixed)) 3 0) 13)))

(defun mp3-alias (x g)
	;the butterflies between each two subbands of long blocks, of a mixed
	;one the first two alone, of short none
	(unless (and (= (. g :find :bt) 2) (= 0 (. g :find :mixed)))
		(defq cs (. +mp3 :find :cs) ca (. +mp3 :find :ca))
		(each (lambda (sb)
				(each (lambda (c a)
						(defq lo (- (* 18 sb) 1 (!)) hi (+ (* 18 sb) (!)) l (elem-get x lo) h (elem-get x hi))
						(elem-set x lo (- (* l c) (* h a)))
						(elem-set x hi (+ (* h c) (* l a))))
					cs ca))
			(range 1 (if (= (. g :find :bt) 2) 2 32)))))

(defun mp3-imdct (x g overlap)
	; (mp3-imdct lines granule overlap) -> (reals ...)
	;each subband's 18 samples: the IMDCT of 36 of its 18 lines, or of a
	;short block three of 12 of its 6 a window, by the window of its block
	;type, the first half on what the last left, the second half left for
	;the next; of an odd subband the odd samples the other sign
	(defq c36 (. +mp3 :find :c36) c12 (. +mp3 :find :c12) windows (. +mp3 :find :windows)
		bt (. g :find :bt) mixed (. g :find :mixed) zero (n2r 0))
	(map (lambda (sb)
			(defq z (cond
					((and (= bt 2) (not (and (= mixed 1) (< sb 2))))
						(defq z (fill (reals) 36 zero))
						(each (lambda (w)
								(defq xw (reduce (# (push %0 (elem-get x (+ (* 18 sb) (* 3 %1) w)))) (range 0 6) (cap 6 (reals))))
								(each (lambda (row win)
										(defq i (+ 6 (* 6 w) (!)))
										(elem-set z i (+ (elem-get z i) (* win (nums-sum (nums-mul row xw))))))
									c12 (elem-get windows 2)))
							'(0 1 2))
						z)
					(:t (defq xs (slice x (* 18 sb) (+ (* 18 sb) 18)))
						(nums-mul (reduce (# (push %0 (nums-sum (nums-mul %1 xs)))) c36 (cap 36 (reals)))
							(elem-get windows (if (= bt 2) 0 bt)))))
				out (nums-add (slice z 0 18) (elem-get overlap sb)))
			(elem-set overlap sb (slice z 18 36))
			(if (odd? sb) (each (# (elem-set out %0 (neg (elem-get out %0)))) '(1 3 5 7 9 11 13 15 17)))
			out)
		(range 0 32)))

(defun mp3-synth (subbands v pcm)
	; (mp3-synth subbands v pcm) -> v
	;the polyphase filterbank: for each of the 18 times, the 32 subbands'
	;samples into the 64 of the matrix, on the front of the 1024 of v, and
	;of the window D over 16 of them 32 samples, pushed to pcm
	(defq rows (. +mp3 :find :synth) d (. +mp3 :find :d) k (n2r 32768))
	(each (lambda (t)
			(defq s (reduce (# (push %0 (elem-get %1 t))) subbands (cap 32 (reals))))
			(setq v (cat (reduce (# (push %0 (nums-sum (nums-mul %1 s)))) rows (cap 64 (reals))) (slice v 0 960)))
			(defq acc (fill (reals) 32 (n2r 0)))
			(each (lambda (i)
					(setq acc (nums-add acc (nums-mul (slice d (* 64 i) (+ (* 64 i) 32)) (slice v (* 128 i) (+ (* 128 i) 32)))))
					(setq acc (nums-add acc (nums-mul (slice d (+ (* 64 i) 32) (+ (* 64 i) 64)) (slice v (+ (* 128 i) 96) (+ (* 128 i) 128))))))
				(range 0 8))
			(each (# (push pcm (* %0 k))) acc))
		(range 0 18))
	v)

(defun mp3-decoder ()
	; (mp3-decoder) -> decoder
	;what goes from frame to frame: the reservoir of main data, of each
	;channel its scale factors, the halves of its IMDCT and its v, and the
	;books made native tables
	(mp3-tables)
	(scatter (Emap) :reservoir "" :scales (list (list) (list))
		:overlap (map (lambda (_) (map (lambda (_) (fill (reals) 18 (n2r 0))) (range 0 32))) (range 0 2))
		:v (map (lambda (_) (fill (reals) 1024 (n2r 0))) (range 0 2))
		:books (map (lambda ((w codes)) (list w (huff-table codes))) (. +mp3 :find :book_codes))
		:quads (map (const huff-table) (. +mp3 :find :quad_codes))))

(defun mp3-frame (d frame)
	; (mp3-frame decoder frame) -> (left right)
	;a frame's samples, its main data from the reservoir and itself, each
	;granule's channels read, the pair's middle and side made left and
	;right, each to samples; nothing heard of a frame whose main data
	;begins before what the reservoir has
	(defq h (mp3-header frame 0) nch (. h :find :channels) in (bits-of frame))
	(if (. h :find :lsf) (throw "mp3: MPEG-2 and 2.5 scale factors are not here yet" (. h :find :rate)))
	(and (= (. h :find :mode) 1) (/= 0 (logand (. h :find :ext) 1)) (throw "mp3: intensity stereo is not here yet" (. h :find :ext)))
	(bits-skip in (if (. h :find :crc) 48 32))
	(bind '(mdb scfsi grans) (mp3-side in h))
	(defq res (. d :find :reservoir) main (slice frame (>> (bits-at in) 3) (min (length frame) (. h :find :size)))
		all (cat res main) pcm (map (lambda (_) (cap (. h :find :samples) (reals))) (range 0 nch)))
	(. d :insert :reservoir (slice all (max 0 (- (length all) 2048)) -1))
	(cond
		((> mdb (length res))
			(each (# (fill %0 (. h :find :samples) (n2r 0))) pcm))
		(:t (defq md (bits-of (slice all (- (length res) mdb) -1)) sri (. h :find :sri))
			(each (lambda (chans)
					(defq gr (!) xs (map (lambda (g ch scfsi)
								(defq start (bits-at md) sf (mp3-scales md g gr scfsi (elem-get (. d :find :scales) ch)))
								(elem-set (. d :find :scales) ch sf)
								(mp3-requant g (mp3-lines d md g (+ start (. g :find :part))) sf sri))
							chans (range 0 nch) scfsi))
					;the middle and side of a pair, made left and right
					(when (and (= nch 2) (= (. h :find :mode) 1) (/= 0 (logand (. h :find :ext) 2)))
						(defq m (first xs) s (second xs) k (sqrt (/ (n2r 1) (n2r 2))))
						(setq xs (list (nums-scale (nums-add m s) k) (nums-scale (nums-sub m s) k))))
					(each (lambda (x g ch)
							(mp3-reorder x g sri)
							(mp3-alias x g)
							(elem-set (. d :find :v) ch
								(mp3-synth (mp3-imdct x g (elem-get (. d :find :overlap) ch)) (elem-get (. d :find :v) ch) (elem-get pcm ch))))
						xs chans (range 0 nch)))
				grans)))
	(if (= nch 1) (list (first pcm) (first pcm)) pcm))

;module
(export-symbols '(mp3-header mp3-decoder mp3-frame mp3-frames))
(env-pop)
