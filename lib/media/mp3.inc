;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; mp3, MPEG audio layer III, ISO/IEC 11172-3 and 13818-3, our own decoder
;
; A frame is a header of 4 bytes, side information, and main data that
; may begin in the frames before it, the bit reservoir. Its 2 granules
; of MPEG-1, or 1 of MPEG-2 and 2.5, are 576 lines a channel: scale
; factors, the lines by Huffman codes, requantised, a pair's middle and
; side made left and right and its intensity stereo shared, short blocks
; reordered, aliases reduced, the IMDCT of 36 or three of 12 a subband,
; overlapped, and the 32 subbands made samples by the polyphase
; filterbank. (mp3-header str at) reads what a
; frame is, (mp3-decoder) holds what goes from frame to frame, and
; (mp3-frame decoder frame) gives its samples, a reals a channel, at the
; scale of 16 bits.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui/canvas/lisp.inc")
(import "./reel.inc")
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
	;of MPEG-2 and 2.5, the scale factors of each of the 4 parts, by the
	;split of the scalefac_compress and the block, long, short or mixed
	:nsf '(((6 5 5 5) (9 9 9 9) (6 9 9 9)) ((6 5 7 3) (9 9 12 6) (6 9 12 6)) ((11 10 0 0) (18 18 0 0) (15 18 0 0))
		((7 7 7 0) (12 12 12 0) (6 15 12 0)) ((6 6 6 3) (12 9 9 6) (6 12 9 6)) ((8 8 5 0) (15 12 9 0) (6 18 9 0)))
	;the book and the more bits of each table select, of the books of
	;mp3_tables.inc from 1, 0 none
	:select '((0 0) (1 0) (2 0) (3 0) (0 0) (4 0) (5 0) (6 0) (7 0) (8 0) (9 0) (10 0) (11 0) (12 0) (0 0) (13 0)
		(14 1) (14 2) (14 3) (14 4) (14 6) (14 8) (14 10) (14 13) (15 4) (15 5) (15 6) (15 7) (15 8) (15 9) (15 11) (15 13))
	;the aliases' butterflies, the c of the standard's cs and ca, as str,
	;a fixed number is not exact to these places
	:alias '("-0.6" "-0.535" "-0.33" "-0.185" "-0.095" "-0.041" "-0.0142" "-0.0037"))

;its natives are taken as the first decoder is made, mp3-tables, not as this
;is read: gui/canvas/lisp.inc imports this as a desktop starts, and a system
;not yet built with them would never start to build them
;	(media-matvec matrix rows cols x from out) -> out
;	(media-mp3-lines reader lines end regions quad) -> lines
;	(media-dequant out q from to pow gain) -> out
;	(media-mp3-window v d out at scale) -> out
;	(media-mp3-alias x bands cs ca) -> x

(defun mp3-pi ()
	; (mp3-pi) -> real
	;pi, by Newton from 3, sin(x) to 0
	(defq x (n2r 3))
	(times 4 (setq x (- x (/ (sin x) (cos x)))))
	x)

(defun mp3-rows (rows cols f)
	; (mp3-rows rows cols f) -> reals
	;a matrix of (f row col), its rows one after another, as the native
	;media_matvec takes it
	(reduce (lambda (out i) (reduce (# (push %0 (f i %1))) (range 0 cols) out)) (range 0 rows) (cap (* rows cols) (reals))))

(defun mp3-tables ()
	; (mp3-tables) -> tables
	;made the once, as the first decoder is: the starts of the long bands
	;by the rate, |x|^4/3 to 8206, the butterflies, the cosines of the IMDCT
	;of 36 and of 12, the windows of the 4 block types and of the short, the
	;matrix of the filterbank and its window of 512
	(unless (. +mp3 :find :made)
		(defq pi (mp3-pi) third (/ (n2r 4) (n2r 3)))
		(scatter +mp3 :made :t
			:matvec (ffi "lib/media/media_matvec") :lines (ffi "lib/media/media_mp3_lines") :dequant (ffi "lib/media/media_dequant")
			:window (ffi "lib/media/media_window") :alias_native (ffi "lib/media/media_mp3_alias")
			:column (ffi "lib/media/media_gather") :none (fill (reals) 18 (n2r 0))
			;2^(e/4) of the gains of a band, from -1024
			:gains (reduce (# (push %0 (pow (n2r 2) (/ (n2r (- %1 1024)) (n2r 4))))) (range 0 1280) (cap 1280 (reals)))
			:starts (map (lambda (widths) (reduce (# (push %0 (+ (last %0) %1))) widths (list 0))) (. +mp3 :find :long))
			:pow (reduce (# (push %0 (if (= %1 0) (n2r 0) (pow (n2r %1) third)))) (range 0 8207) (cap 8207 (reals)))
			:cs (reduce (# (push %0 (/ (n2r 1) (sqrt (+ (n2r 1) (* (str-to-real %1) (str-to-real %1))))))) (. +mp3 :find :alias) (reals))
			:ca (reduce (# (push %0 (/ (str-to-real %1) (sqrt (+ (n2r 1) (* (str-to-real %1) (str-to-real %1))))))) (. +mp3 :find :alias) (reals))
			:c36 (mp3-rows 36 18 (lambda (i k) (cos (/ (* pi (n2r (* (+ (* 2 i) 19) (inc (* 2 k))))) (n2r 72)))))
			:c12 (mp3-rows 12 6 (lambda (i k) (cos (/ (* pi (n2r (* (+ (* 2 i) 7) (inc (* 2 k))))) (n2r 24)))))
			:synth (mp3-rows 64 32 (lambda (i k) (cos (/ (* pi (n2r (* (+ 16 i) (inc (* 2 k))))) (n2r 64))))))
		(defq sin36 (lambda (i) (sin (/ (* pi (+ (n2r i) (n2r 0.5))) (n2r 36))))
			sin12 (lambda (i) (sin (/ (* pi (+ (n2r i) (n2r 0.5))) (n2r 12))))
			zero (n2r 0) one (n2r 1))
		(. +mp3 :insert :windows (defq windows (list
			(reduce (# (push %0 (sin36 %1))) (range 0 36) (reals))
			(reduce (# (push %0 (cond ((< %1 18) (sin36 %1)) ((< %1 24) one) ((< %1 30) (sin12 (- %1 18))) (zero)))) (range 0 36) (reals))
			(reduce (# (push %0 (sin12 %1))) (range 0 12) (reals))
			(reduce (# (push %0 (cond ((< %1 6) zero) ((< %1 12) (sin12 (- %1 6))) ((< %1 18) one) ((sin36 %1))))) (range 0 36) (reals)))))
		;those of an odd subband, its odd samples of the other sign, theirs
		;and the half left for the next both
		(. +mp3 :insert :odd_windows (map (lambda (w) (reduce (# (push %0 (if (odd? (!)) (neg %1) %1))) w (cap (length w) (reals)))) windows))
		;D of the standard, the half given and its mirror
		(defq d (fill (reals) 512 (n2r 0)) k (n2r 65536))
		(each (lambda (v)
				(defq i (!))
				(elem-set d i (/ (n2r v) k))
				(if (< 0 i 256) (elem-set d (- 512 i) (/ (n2r (if (= 0 (logand i 63)) v (neg v))) k))))
			(. +mp3 :find :half_window))
		(. +mp3 :insert :d d)
		;the books made native tables, read only, so every decoder has these
		(scatter +mp3 :books (map (lambda ((w codes)) (list w (huff-table codes)
				(reduce (# (push %0 (+ (<< (/ %1 w) 4) (% %1 w)))) (range 0 (* w w)) (cap (* w w) (nums)))))
			(. +mp3 :find :book_codes))
			:quads (map (const huff-table) (. +mp3 :find :quad_codes))))
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
				:samples (if lsf 576 1152) :kbps kbps :size (+ (/ (* (if lsf 72 144) kbps 1000) rate) (logand (>> h 9) 1))))))

(defun mp3-id3 (s)
	; (mp3-id3 str) -> bytes
	;the bytes of an ID3v2 tag at the start, and its footer, 0 if none
	(if (and (>= (length s) 10) (eql (slice s 0 3) "ID3"))
		(+ 10 (if (/= 0 (logand (code s 1 5) 0x10)) 10 0)
			(reduce (# (+ (<< %0 7) (logand (code s 1 (+ 6 %1)) 127))) (range 0 4) 0))
		0))

(defun mp3-frames (s)
	; (mp3-frames str) -> ((at size) ...)
	;the frames of the bytes of a file, after an ID3v2 tag if it has one,
	;each found where the one before ends, or the next header after
	(defq at (mp3-id3 s) out (list))
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
	;big values of no more than the 288 pairs of the 576 lines: a frame that
	;says more is wrong, and the native would write past them
	(defq part (bits-get in 12) big (min 288 (bits-get in 9)) gain (bits-get in 8) sfc (bits-get in (if lsf 9 4)))
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
		:preflag (if lsf 0 (bits-1 in)) :sfscale (bits-1 in) :count1 (bits-1 in)))

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

(defun mp3-split (sf n1 n2 n3)
	; (mp3-split sf n1 n2 n3) -> (slen ...)
	;the 4 lengths of MPEG-2's scale factors in one number, the last by n3,
	;then n2 and n1, each of 0 a length of 0
	(defq out (list))
	(each (lambda (n)
			(cond
				((= n 0) (push out 0))
				(:t (push out (% sf n)) (setq sf (/ sf n)))))
		(list n3 n2 n1))
	(push out sf)
	(reverse out))

(defun mp3-scales-lsf (in g h ch)
	; (mp3-scales-lsf reader granule header ch) -> (sf ...)
	;the scale factors of MPEG-2 and 2.5: the 9 bits of the scalefac_compress
	;split to the lengths of 4 parts, of the right of intensity stereo its
	;own way, as many in each part as the table has, 40 in all; of 500 or
	;more the pretab used
	(defq sfc (. g :find :sfc) block (if (= (. g :find :bt) 2) (if (= 1 (. g :find :mixed)) 2 1) 0)
		is (and (= ch 1) (/= 0 (logand (. h :find :ext) 1))) slen :nil part 0)
	(cond
		(is (setq sfc (>> sfc 1))
			(cond
				((< sfc 180) (setq slen (mp3-split sfc 6 6 0) part 3))
				((< sfc 244) (setq slen (mp3-split (- sfc 180) 4 4 0) part 4))
				(:t (setq slen (mp3-split (- sfc 244) 3 0 0) part 5))))
		((< sfc 400) (setq slen (mp3-split sfc 5 4 4) part 0))
		((< sfc 500) (setq slen (mp3-split (- sfc 400) 5 4 0) part 1))
		(:t (setq slen (mp3-split (- sfc 500) 3 0 0) part 2) (. g :insert :preflag 1)))
	(defq out (reduce! (lambda (out n bits) (reduce (lambda (out _) (push out (if (> bits 0) (bits-get in bits) 0))) (range 0 n) out))
		(list (elem-get (elem-get (. +mp3 :find :nsf) part) block) slen) (cap 40 (list))))
	(while (< (length out) 40) (push out 0))
	out)

(defun mp3-positions (lsf sfc)
	; (mp3-positions lsf sfc) -> ((kl kr) ...)
	;the shares of left and right of each position of intensity stereo: of
	;MPEG-1 tan(pos pi/12) to 1, of MPEG-2 by 2^-1/4 or 2^-1/2 a step, the
	;scalefac_compress's last bit, to the left at an odd position, the right
	;at an even
	(defq one (n2r 1))
	(if lsf
		(map (lambda (i)
				(defq f (pow (n2r 2) (/ (n2r (neg (* (inc sfc) (>> (inc i) 1)))) (n2r 4))))
				(if (odd? i) (list f one) (list one f)))
			(range 0 16))
		(map (lambda (i)
				(cond
					((= i 6) (list one (n2r 0)))
					(:t (defq x (/ (* (n2r i) (mp3-pi)) (n2r 12)) t (/ (sin x) (cos x)))
						(list (/ t (+ one t)) (/ one (+ one t))))))
			(range 0 7))))

(defun mp3-intensity (l r g sf h)
	; (mp3-intensity left right granule sf header)
	;of a pair of intensity stereo, from the top band down, each band the
	;right has nothing in shares the left's by the right's scale factor, its
	;position, those below it of middle and side if the frame says; a short
	;block each window its own way, the last band by the scale factor of
	;the band before
	(defq lsf (. h :find :lsf) sri (. h :find :sri) ms (/= 0 (logand (. h :find :ext) 2))
		bt (. g :find :bt) mixed (. g :find :mixed) at 576 k1 (sqrt (/ (n2r 1) (n2r 2)))
		long_end (if (= bt 2) (if (= mixed 1) (if (<= sri 2) 8 6) 0) 22)
		short_start (if (= bt 2) (if (= mixed 1) 3 0) 13)
		sfmax (if lsf 16 7) tab (mp3-positions lsf (logand (. g :find :sfc) 1))
		nonzero (lambda (from len) (some! (# (/= %0 (n2r 0))) (list r) :nil from (+ from len)))
		ms-band (lambda (from len)
			(if ms (each! (lambda (a b)
					(elem-set l (!) (* (+ a b) k1))
					(elem-set r (!) (* (- a b) k1)))
				(list l r) from (+ from len))))
		is-band (lambda (from len pos)
			(bind '(kl kr) (elem-get tab pos))
			(each! (lambda (a)
					(elem-set l (!) (* a kl))
					(elem-set r (!) (* a kr)))
				(list l) from (+ from len)))
		found (list :nil :nil :nil) k (+ (* 3 (- 13 short_start)) long_end -3) i 12
		widths (elem-get (. +mp3 :find :short) sri))
	(while (>= i short_start)
		(if (/= i 11) (setq k (- k 3)))
		(defq len (elem-get widths i))
		(each (lambda (w)
				(setq at (- at len))
				(cond
					((elem-get found w) (ms-band at len))
					((nonzero at len) (elem-set found w :t) (ms-band at len))
					((>= (defq pos (elem-get sf (+ k w))) sfmax) (ms-band at len))
					((is-band at len pos))))
			'(2 1 0))
		(setq i (dec i)))
	(defq any (some (const identity) found) widths (elem-get (. +mp3 :find :long) sri) i (dec long_end))
	(while (>= i 0)
		(defq len (elem-get widths i))
		(setq at (- at len))
		(cond
			(any (ms-band at len))
			((nonzero at len) (setq any :t) (ms-band at len))
			((>= (defq pos (elem-get sf (if (= i 21) 20 i))) sfmax) (ms-band at len))
			((is-band at len pos)))
		(setq i (dec i))))

(defun mp3-lines (d in g end)
	; (mp3-lines decoder reader granule end) -> nums
	;the 576 lines of a channel's granule, quantised and signed, by the
	;native reader, lib/media/lisp.vp media_mp3_lines: the big values by
	;pairs, each region by its book, 15 the more bits of its table and then
	;the sign; then quads of 1 and 0 till the end of the part
	(defq books (. d :find :books) select (. +mp3 :find :select) none (nums))
	((. +mp3 :find :lines) in (fill (nums) 576 0) end
		(reduce! (lambda (out pairs table)
				(bind '(book more) (elem-get select table))
				(if (= book 0) (push out pairs none none 0)
					(bind '(w tab xy) (elem-get books (dec book)))
					(push out pairs tab xy more)))
			(list (. g :find :pairs) (. g :find :tables)) (cap 12 (list)))
		(elem-get (. d :find :quads) (. g :find :count1))))

(defun mp3-band (out lines at width e pow)
	;a band's lines requantised, |x|^4/3 by 2^(e/4), signed, native,
	;lib/media/lisp.vp media_dequant
	((. +mp3 :find :dequant) out lines at (+ at width) pow (elem-get (. +mp3 :find :gains) (+ e 1024))))

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
	;one the first two alone, of short none, native, lib/media/lisp.vp
	;media_mp3_alias
	(unless (and (= (. g :find :bt) 2) (= 0 (. g :find :mixed)))
		((. +mp3 :find :alias_native) x (if (= (. g :find :bt) 2) 1 31) (. +mp3 :find :cs) (. +mp3 :find :ca))))

(defun mp3-imdct (x g overlap)
	; (mp3-imdct lines granule overlap) -> (reals ...)
	;each subband's 18 samples: the IMDCT of 36 of its 18 lines, or of a
	;short block three of 12 of its 6 a window, by the window of its block
	;type, the two lowest of a mixed block of any type the plain window's;
	;the first half on what the last left, the second half left for the
	;next; of an odd subband the odd samples the other sign, by its windows
	(defq c36 (. +mp3 :find :c36) c12 (. +mp3 :find :c12) evens (. +mp3 :find :windows) odds (. +mp3 :find :odd_windows)
		bt (. g :find :bt) mixed (. g :find :mixed) zero (n2r 0) y36 (fill (reals) 36 zero)
		none (. +mp3 :find :none) top 32)
	;the subbands above the highest with a line not 0, often a quarter,
	;their IMDCT nothing: the half the last left heard as it is, none left
	(while (and (> top 0) (= zero (nums-dot (defq q (slice x (* 18 (dec top)) (* 18 top))) q)))
		(setq top (dec top)))
	(map (lambda (sb)
			(cond
				((>= sb top)
					(defq out (elem-get overlap sb))
					(elem-set overlap sb none)
					out)
				(:t (defq windows (if (odd? sb) odds evens) z (cond
							((and (= bt 2) (not (and (= mixed 1) (< sb 2))))
								(defq z (fill (reals) 36 zero))
								(each (lambda (w)
										(defq xw (reduce (# (push %0 (elem-get x (+ (* 18 sb) (* 3 %1) w)))) (range 0 6) (cap 6 (reals))))
										(each (lambda (y win)
												(defq i (+ 6 (* 6 w) (!)))
												(elem-set z i (+ (elem-get z i) (* win y))))
											((. +mp3 :find :matvec) c12 12 6 xw 0 (fill (reals) 12 zero)) (elem-get windows 2)))
									'(0 1 2))
								z)
							(:t (nums-mul ((. +mp3 :find :matvec) c36 36 18 x (* 18 sb) y36)
									(elem-get windows (if (or (= bt 2) (and (= mixed 1) (< sb 2))) 0 bt)) y36)))
						out (slice z 0 18))
					(nums-add out (elem-get overlap sb) out)
					(elem-set overlap sb (slice z 18 36))
					out)))
		(range 0 32)))

(defun mp3-synth (subbands v pcm at)
	; (mp3-synth subbands v pcm at) -> v
	;the polyphase filterbank: for each of the 18 times, the 32 subbands'
	;samples into the 64 of the matrix, on the front of the 1024 of v, and
	;of the window D over 16 of them 32 samples, into pcm from at
	(defq rows (. +mp3 :find :synth) d (. +mp3 :find :d) k (n2r 32768) column (. +mp3 :find :column)
		s (fill (reals) 32 (n2r 0)) m (fill (reals) 64 (n2r 0)))
	(each (lambda (t)
			;the subbands' samples of time t, native, lib/media/lisp.vp media_gather
			(column s subbands t)
			(setq v (cat ((. +mp3 :find :matvec) rows 64 32 s 0 m) (slice v 0 960)))
			((. +mp3 :find :window) v d pcm (+ at (* 32 t)) k 32 8 64 128 32 96))
		(range 0 18))
	v)

(defun mp3-decoder ()
	; (mp3-decoder) -> decoder
	;what goes from frame to frame: the reservoir of main data, of each
	;channel its scale factors, the halves of its IMDCT and its v, and the
	;books made native tables, made the once by (mp3-tables)
	(mp3-tables)
	(scatter (Emap) :reservoir "" :scales (list (list) (list))
		:overlap (map (lambda (_) (map (lambda (_) (fill (reals) 18 (n2r 0))) (range 0 32))) (range 0 2))
		:v (map (lambda (_) (fill (reals) 1024 (n2r 0))) (range 0 2))
		:books (. +mp3 :find :books) :quads (. +mp3 :find :quads)))

(defun mp3-frame (d frame)
	; (mp3-frame decoder frame) -> (left right)
	;a frame's samples, its main data from the reservoir and itself, each
	;granule's channels read, the pair's middle and side made left and
	;right, each to samples; nothing heard of a frame whose main data
	;begins before what the reservoir has
	(defq h (mp3-header frame 0) nch (. h :find :channels) in (bits-of frame))
	(bits-skip in (if (. h :find :crc) 48 32))
	(bind '(mdb scfsi grans) (mp3-side in h))
	(defq res (. d :find :reservoir) main (slice frame (>> (bits-at in) 3) (min (length frame) (. h :find :size)))
		all (cat res main) pcm (map (lambda (_) (fill (reals) (. h :find :samples) (n2r 0))) (range 0 nch)))
	(. d :insert :reservoir (slice all (max 0 (- (length all) 2048)) -1))
	(cond
		;nothing heard of a frame begun before the reservoir has
		((> mdb (length res)))
		(:t (defq md (bits-of (slice all (- (length res) mdb) -1)) sri (. h :find :sri))
			(each (lambda (chans)
					(defq gr (!) xs (map (lambda (g ch scfsi)
								(defq start (bits-at md) sf (if (. h :find :lsf) (mp3-scales-lsf md g h ch)
										(mp3-scales md g gr scfsi (elem-get (. d :find :scales) ch))))
								(elem-set (. d :find :scales) ch sf)
								(mp3-requant g (mp3-lines d md g (min (+ start (. g :find :part)) (* 8 (- (length (bits-str md)) 8)))) sf sri))
							chans (range 0 nch) scfsi))
					;the middle and side of a pair made left and right, and intensity
					;stereo, by the right's scale factors
					(and (= nch 2) (= (. h :find :mode) 1)
						(cond
							((/= 0 (logand (. h :find :ext) 1))
								(mp3-intensity (first xs) (second xs) (second chans) (elem-get (. d :find :scales) 1) h))
							((/= 0 (logand (. h :find :ext) 2))
								(defq m (first xs) s (second xs) k (sqrt (/ (n2r 1) (n2r 2))))
								(setq xs (list (nums-scale (nums-add m s) k) (nums-scale (nums-sub m s) k))))))
					(each (lambda (x g ch)
							(mp3-reorder x g sri)
							(mp3-alias x g)
							(elem-set (. d :find :v) ch
								(mp3-synth (mp3-imdct x g (elem-get (. d :find :overlap) ch)) (elem-get (. d :find :v) ch) (elem-get pcm ch) (* 576 gr))))
						xs chans (range 0 nch)))
				grans)))
	(if (= nch 1) (list (first pcm) (first pcm)) pcm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a file of MP3 as a reel, a frame a packet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mp3-sync (s at)
	; (mp3-sync str at) -> :nil | (at header)
	;the first frame from at on: a header, and one of the same rate where
	;it ends, if that is in the str, a header alone may be of a frame's
	;bytes
	(defq h :nil end (- (length s) 4))
	(while (and (< at end)
			(not (and (setq h (mp3-header s at))
				(or (> (+ at (. h :find :size)) end)
					(and (defq next (mp3-header s (+ at (. h :find :size)))) (= (. next :find :sri) (. h :find :sri)))))))
		(setq at (inc at)))
	(if (< at end) (list at h)))

(defun mp3-xing (s at h)
	; (mp3-xing str at header) -> :nil | (frames bytes toc vbr)
	;what a first frame of Xing or Info, of nothing heard, says of those
	;after it: how many, their bytes, and where each hundredth of them
	;begins, by 1/256 of the bytes, :nil of each it does not say, and if
	;their bit rate varies, Xing's. A table is taken only if it rises,
	;some coders leave it all 0, and not that of Info, of a constant bit
	;rate, the rate is nearer
	(defq o (+ at 4 (if (. h :find :crc) 2 0) (if (. h :find :lsf)
		(if (= (. h :find :channels) 1) 9 17) (if (= (. h :find :channels) 1) 17 32)))
		tag (slice s (min o (length s)) (min (+ o 4) (length s))))
	(when (and (<= (+ o 120) (length s)) (or (eql tag "Xing") (eql tag "Info")))
		(defq flags (code-be s 4 (+ o 4)) p (+ o 8) frames :nil bytes :nil toc :nil)
		(when (/= 0 (logand flags 1)) (setq frames (code-be s 4 p) p (+ p 4)))
		(when (/= 0 (logand flags 2)) (setq bytes (code-be s 4 p) p (+ p 4)))
		(and (/= 0 (logand flags 4)) (eql tag "Xing") (> (code s 1 (+ p 99)) (code s 1 p))
			(setq toc (slice s p (+ p 100))))
		(list frames bytes toc (eql tag "Xing"))))

(defun mp3-unit (h)
	;the bytes of a frame of a constant bit rate, by the rate
	(* (if (. h :find :lsf) 72 144) (. h :find :kbps) 1000))

(defun mp3-reel (stream)
	; (mp3-reel stream) -> :nil | reel
	;a file of MP3 as a reel of its frames, each found as it is wanted, not
	;all now: a book of 40 hours is 2GB. They are counted by a first frame
	;of Xing or Info if it has one, else by the bit rate, :frames, and a
	;frame a packet, :rate of them a second. Its :tracks the one of sound,
	;of codec "mp3", and its pictures those of lib/media/mp3.inc's codec,
	;:width by :height, the samples of each frame
	(defq end (stream-seek stream 0 2) at 0)
	(stream-seek stream 0 0)
	(setq at (mp3-id3 (ifn (read-blk stream 10) "")))
	;before an ID3v1 tag at the end
	(when (> end 128)
		(stream-seek stream (- end 128) 0)
		(if (eql (read-blk stream 3) "TAG") (setq end (- end 128))))
	(stream-seek stream at 0)
	(when (and (defq s (read-blk stream 16384)) (defq found (mp3-sync s 0)))
		(bind '(i h) found)
		(defq frames :nil bytes :nil toc :nil vbr :nil xing (mp3-xing s i h))
		(setq at (+ at i))
		;a Xing table's places are from the start of the Xing frame
		(defq base at)
		(when xing
			(bind '(frames bytes toc vbr) xing)
			(setq at (+ at (. h :find :size))))
		(setq bytes (ifn bytes (- end at))
			frames (ifn frames (/ (+ (* bytes (. h :find :rate)) (/ (mp3-unit h) 2)) (mp3-unit h))))
		(scatter (Emap) :stream stream :head h :start at :base base :end end :bytes bytes :toc toc :vbr vbr :known (list)
			:frames frames :rate (/ (n2f (. h :find :rate)) (n2f (. h :find :samples)))
			:width 512 :height 192 :codec "mp3" :audio :t
			:tracks (list (scatter (Emap) :kind "soun" :codec "mp3" :samples frames :start 0)))))

(defun mp3-guess (reel n)
	;where frame n should begin: by the table of a first frame of Xing,
	;between two of its hundredths, else of a varying bit rate by the
	;bytes a frame of them all, else by the bit rate of the first
	(defq frames (. reel :find :frames) bytes (. reel :find :bytes) h (. reel :find :head))
	(cond
		((defq toc (. reel :find :toc))
			(defq i (min 99 (/ (* n 100) frames)) a (code toc 1 i) b (if (< i 99) (code toc 1 (inc i)) 256))
			(+ (. reel :find :base) (/ (* bytes (+ (* a frames) (* (- b a) (- (* n 100) (* i frames))))) (* 256 frames))))
		((. reel :find :vbr) (+ (. reel :find :start) (/ (* n bytes) frames)))
		((+ (. reel :find :start) (/ (* n (mp3-unit h)) (. h :find :rate))))))

(defun mp3-packet (reel n)
	; (mp3-packet reel n) -> :nil | str
	;frame n: where one read ends if it was n - 1, else the first found
	;from a little before where it should be. The ends of the last four
	;read are kept, the sound and the picture each read one after another
	(when (< -1 n (. reel :find :frames))
		(defq stream (. reel :find :stream) known (. reel :find :known)
			at (some (# (if (= (first %0) n) (second %0))) known)
			;the header there, if it is one in the file
			head-at (lambda (at)
				(stream-seek stream at 0)
				(and (defq frame (read-blk stream 4)) (defq h (mp3-header frame 0))
					(<= (+ at (. h :find :size)) (. reel :find :end)) h)))
		;where one ended, unless what is there is not a header, junk or a tag
		;between frames: then found as a frame not read before is
		(unless (and at (head-at at))
			(stream-seek stream (setq at (max (. reel :find :start) (- (mp3-guess reel n) 2))) 0)
			(setq at (and (defq s (read-blk stream 8192)) (defq found (mp3-sync s 0)) (+ at (first found)))))
		(when (and at (defq h (head-at at)))
			(. reel :insert :known (cat (list (list (inc n) (+ at (. h :find :size)))) (slice known 0 (min 3 (length known)))))
			(stream-seek stream at 0)
			(read-blk stream (. h :find :size)))))

(defun mp3-codec (reel &optional scale)
	; (mp3-codec reel [scale]) -> codec
	;what shows a frame: its samples, the left above the right, on a
	;canvas of its own, by a decoder of its own; a scale is not taken
	(scatter (Emap) :canvas (Canvas (. reel :find :width) (. reel :find :height) 1) :scale 1
		:reel reel :decoder (mp3-decoder) :at -1))

(defun mp3-samples (codec n)
	;frame n's samples, :nil of one not there or not read
	(and (defq frame (mp3-packet (. codec :find :reel) n))
		(list? (defq pcm (catch (mp3-frame (. codec :find :decoder) frame) :t)))
		pcm))

(defun mp3-before (size)
	; (mp3-before size) -> frames
	;how many frames of a size are decoded first, before one, for the
	;reservoir to hold the 511 bytes a frame may begin back
	(inc (/ (+ 511 size -1) size)))

(defun mp3-picture (codec n)
	; (mp3-picture codec n) -> canvas
	;frame n's samples drawn: after the one before, else with those before
	;it decoded first, for the reservoir and the overlap
	(defq canvas (. codec :find :canvas) w (. (. codec :find :reel) :find :width) h (. (. codec :find :reel) :find :height))
	(unless (= n (. codec :find :at))
		(unless (= n (inc (. codec :find :at)))
			(. (. codec :find :decoder) :insert :reservoir "")
			(each (# (mp3-samples codec %0)) (range (max 0 (- n (mp3-before (. (. (. codec :find :reel) :find :head) :find :size)))) n)))
		(. codec :insert :at n)
		(reel-wave canvas w h (mp3-samples codec n)))
	canvas)

(each (# (reel-kind %0 mp3-reel mp3-packet mp3-codec mp3-picture)) '(".mp3"))

;module
(export-symbols '(mp3-header mp3-decoder mp3-frame mp3-frames mp3-before mp3-reel mp3-packet mp3-codec mp3-picture))
(env-pop)
