;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; aac, the sound of films, AAC-LC, ISO/IEC 14496-3, our own decoder
;
; A packet of the sound of an mp4 is a raw data block: elements, each a
; channel, a pair of them, or something to step over, till END. A channel
; is its window, which bands of its spectrum are coded by which Huffman
; book, a scale for each band, and the spectrum, 1024 lines, or 8 short
; windows of 128. (aac-config setup) says what the setup of the track,
; its esds, says: the object type, the rate and the channels.
; (aac-decoder config) holds what a channel carries from one block to
; the next, (aac-decode decoder packet) gives the 1024 samples of each
; channel of a packet. Of more than two channels, 5.1 or 7.1, only the
; centre and the front pair are read, and made the front pair with the
; centre in each at 0.707, a downmix to two.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./huff.inc")
(import "./bits.inc")
(import "./mdct.inc")
(import "./noise.inc")

;module
(env-push)

(import "./aac_tables.inc")

;what the functions read as they run: a module's names are gone by then,
;and a + constant is put in the code as it is read, this one map
(defq +aac (Emap))

(defq *aac_rates* '(96000 88200 64000 48000 44100 32000 24000 22050 16000 12000 11025 8000 7350)
	;the kinds of element
	+aac_sce 0 +aac_cpe 1 +aac_cce 2 +aac_lfe 3 +aac_dse 4 +aac_pce 5 +aac_fil 6 +aac_end 7
	;the windows
	+aac_only_long 0 +aac_long_start 1 +aac_eight_short 2 +aac_long_stop 3
	;the books that are not Huffman books of the spectrum
	+aac_zero 0 +aac_noise 13 +aac_intensity2 14 +aac_intensity 15)

;;;;;;;;;;;;;;;;;
; the Huffman books
;;;;;;;;;;;;;;;;;

(defun aac-book (codes)
	; (aac-book codes) -> (fmap longest)
	;a book to read codes of: each code, its length high, to what it stands for
	(defq lookup (Fmap 101) longest 0)
	(each (lambda ((n v))
			(. lookup :insert (+ (<< n 24) v) (!))
			(setq longest (max longest n)))
		(partition codes 2))
	(list lookup longest))

(defq *aac_sf* (aac-book *aac_sf_book*) *aac_spectral* (map (const aac-book) *aac_books*))

(defun aac-huff (in table)
	; (aac-huff reader table) -> index
	;the code next, what it stands for, by the native reader of a book's
	;table, lib/media/lisp.vp media_huff
	(defq got ((. +aac :find :huff) (bits-str in) (bits-at in) table))
	(bits-seek in (logand got 0xffffffff))
	(>> got 32))

(defun aac-huff-lisp (in book)
	; (aac-huff-lisp reader book) -> index
	;the same in Lisp, the bits of the longest code looked at, one more
	;each time till one is a code: what the native one is checked against
	(bind '(lookup longest) book)
	(defq at (bits-at in) s (bits-str in) byte (>> at 3) shift (logand at 7)
		;the next 32 bits, and those after the first byte
		window (logior (<< (code-be s 4 byte) 8) (code s 1 (+ byte 4)))
		len 0 found :nil)
	(while (not found)
		(setq len (inc len))
		(setq found (. lookup :find (+ (<< len 24)
			(logand (>> window (- 40 shift len)) (dec (<< 1 len))))))
		(if (> len longest) (throw "aac: no such code" len)))
	(bits-seek in (+ at len))
	found)

;;;;;;;;;;;
; the setup
;;;;;;;;;;;

(defun aac-descriptor (s at)
	; (aac-descriptor str at) -> (tag len body_at)
	;a descriptor of an esds, its tag, and its length of 1 to 4 bytes of
	;7 bits each, the top bit set but in the last
	(defq tag (code s 1 at) len 0 more :t)
	(setq at (inc at))
	(while more
		(defq b (code s 1 at))
		(setq len (logior (<< len 7) (logand b 127)) at (inc at) more (/= 0 (logand b 128))))
	(list tag len at))

(defun aac-esds (s)
	; (aac-esds str) -> :nil | str
	;the AudioSpecificConfig of what is in an esds box, past its version
	(defq at 4 end (length s) found :nil)
	(while (and (not found) (< at end))
		(bind '(tag len body) (aac-descriptor s at))
		(case tag
			;ES_Descriptor: its id, flags, and what the flags say is there
			(3 (defq flags (code s 1 (+ body 2)) at (+ body 3))
				(if (/= 0 (logand flags 0x80)) (setq at (+ at 2)))
				(if (/= 0 (logand flags 0x40)) (setq at (+ at 1 (code s 1 at))))
				(if (/= 0 (logand flags 0x20)) (setq at (+ at 2))))
			;DecoderConfigDescriptor: 13 bytes, then what it holds
			(4 (setq at (+ body 13)))
			;DecoderSpecificInfo, the setup
			(5 (setq found (slice s body (+ body len))))
			(:t (setq at (+ body len)))))
	found)

(defun aac-config (setup)
	; (aac-config setup) -> :nil | (type rate_index rate channels)
	;what an AudioSpecificConfig says: the object type, 2 is AAC-LC, the
	;index of its rate, the rate, and the channels
	(when (>= (length setup) 2)
		(defq in (bits-of setup) type (bits-get in 5))
		(if (= type 31) (setq type (+ 32 (bits-get in 6))))
		(defq index (bits-get in 4)
			rate (if (= index 15) (bits-get in 24) (elem-get (. +aac :find :rates) (min index 12))))
		(list type index rate (bits-get in 4))))

(defun aac-track-config (track)
	; (aac-track-config track) -> :nil | config
	;of a sound track of an mp4, from its esds, or one in a QuickTime wave
	(defq config (. track :find :config)
		esds (some (# (if (eql (first %0) "esds") (second %0))) config))
	(unless esds
		(when (defq wave (some (# (if (eql (first %0) "wave") (second %0))) config))
			;the boxes of the wave, each its size, 4 bytes high first, and name
			(defq at 0)
			(while (and (not esds) (<= (+ at 8) (length wave)))
				(defq size (code-be wave 4 at))
				(if (eql (slice wave (+ at 4) (+ at 8)) "esds")
					(setq esds (slice wave (+ at 8) (min (length wave) (+ at size)))))
				(setq at (+ at (max size 8))))))
	(and esds (defq setup (aac-esds esds)) (aac-config setup)))

;;;;;;;;;;;;;;;;;;;;;
; a channel's window
;;;;;;;;;;;;;;;;;;;;;

(defun aac-ics-info (in rate_index)
	; (aac-ics-info reader rate_index) -> info
	;its window, its shape, how many bands are coded, and the groups of
	;short windows, how many windows in each, and the bands' bounds
	(bits-1 in)
	(defq sequence (bits-get in 2) shape (bits-1 in) info (Emap))
	(cond
		((= sequence +aac_eight_short)
			(defq max_sfb (bits-get in 4) grouping (bits-get in 7) lengths (list 1))
			;a 1 of the grouping puts the next window in the group before it
			(each (lambda (i)
					(if (/= 0 (logand grouping (<< 1 (- 6 i))))
						(elem-set lengths -2 (inc (last lengths)))
						(push lengths 1)))
				(range 0 7))
			(scatter info :windows 8 :lengths lengths :bands (elem-get (. +aac :find :bands_short) rate_index)))
		(:t (defq max_sfb (bits-get in 6))
			;no prediction in AAC-LC
			(if (/= 0 (bits-1 in)) (throw "aac: prediction is not of AAC-LC" sequence))
			(scatter info :windows 1 :lengths (list 1) :bands (elem-get (. +aac :find :bands_long) rate_index))))
	(scatter info :sequence sequence :shape shape :max_sfb max_sfb))

(defun aac-sections (in info)
	; (aac-sections reader info) -> ((book ...) ...)
	;the book of each band of each group, in runs
	(defq short (= (. info :find :sequence) +aac_eight_short)
		bits (if short 3 5) escape (dec (<< 1 bits)) max_sfb (. info :find :max_sfb))
	(map (lambda (_)
			(defq books (cap max_sfb (list)))
			(while (< (length books) max_sfb)
				(defq book (bits-get in 4) run 0 more escape)
				(while (= more escape)
					(setq more (bits-get in bits) run (+ run more)))
				(times run (push books book)))
			books)
		(. info :find :lengths)))

(defun aac-scales (in info sections global)
	; (aac-scales reader info sections global) -> ((scale ...) ...)
	;the scale of each band of each group: of a spectral book, from the
	;global gain by the differences; of intensity, its position; of noise,
	;its energy, the first of 9 bits of its own
	(defq scale global position 0 noise (- global 90) first_noise :t)
	(map (lambda (books)
			(map (lambda (book)
					(cond
						((= book +aac_zero) 0)
						((or (= book +aac_intensity) (= book +aac_intensity2))
							(setq position (+ position (- (aac-huff in (. +aac :find :sf)) 60))))
						((= book +aac_noise)
							(if first_noise
								(setq first_noise :nil noise (+ noise (- (bits-get in 9) 256)))
								(setq noise (+ noise (- (aac-huff in (. +aac :find :sf)) 60)))))
						(:t (setq scale (+ scale (- (aac-huff in (. +aac :find :sf)) 60))))))
				books))
		sections))

(defun aac-pulses (in)
	; (aac-pulses reader) -> (start (offset amp) ...)
	(defq count (inc (bits-get in 2)) start (bits-get in 6))
	(cat (list start) (map (lambda (_) (list (bits-get in 5) (bits-get in 4))) (range 0 count))))

(defun aac-tns (in info)
	; (aac-tns reader info) -> (((length direction (coef ...)) ...) ...)
	;the filters of each window, each over so many bands down from the top,
	;its direction and its coefficients, as the indices they were sent as
	(defq short (= (. info :find :sequence) +aac_eight_short))
	(map (lambda (_)
			(defq count (bits-get in (if short 1 2)) filters (list))
			(when (> count 0)
				(defq res (bits-1 in))
				(times count
					(defq len (bits-get in (if short 4 6)) order (bits-get in (if short 3 5)))
					(cond
						((> order 0)
							(defq direction (bits-1 in) compress (bits-1 in)
								bits (- (+ res 3) compress))
							(push filters (list len direction
								(map (lambda (_) (bits-get in bits)) (range 0 order)) res compress)))
						(:t (push filters (list len 0 (list) res 0))))))
			filters)
		(range 0 (. info :find :windows))))

;;;;;;;;;;;;;;;;;;;;;;;;;
; the spectrum of a channel
;;;;;;;;;;;;;;;;;;;;;;;;;

;each spectral book: values a code, signed or not, the largest, and the base
(defq *aac_book_kinds* '(
	(4 :t 1 3) (4 :t 1 3) (4 :nil 2 3) (4 :nil 2 3) (2 :t 4 9) (2 :t 4 9)
	(2 :nil 7 8) (2 :nil 7 8) (2 :nil 12 13) (2 :nil 12 13) (2 :nil 16 17)))

(defun aac-escape (in)
	; (aac-escape reader) -> num
	;a value of 16 or more of book 11: so many 1s and a 0, then 4 bits more
	;than there were 1s
	(defq ones 0)
	(while (= (bits-1 in) 1) (setq ones (inc ones)))
	(+ (<< 1 (+ ones 4)) (bits-get in (+ ones 4))))

(defun aac-values (in book)
	; (aac-values reader book) -> (value ...)
	;the 2 or 4 values of the next code of a book, signed
	(bind '(dims signed top base) (elem-get (. +aac :find :kinds) (dec book)))
	(defq index (aac-huff in (elem-get (. +aac :find :spectral) (dec book)))
		values (if (= dims 4)
			(list (/ index (* base base base)) (% (/ index (* base base)) base) (% (/ index base) base) (% index base))
			(list (/ index base) (% index base))))
	(cond
		(signed (map (# (- %0 top)) values))
		(:t ;a sign bit after the code for each that is not 0, then escapes
			(setq values (map (# (if (and (/= %0 0) (= (bits-1 in) 1)) (neg %0) %0)) values))
			(if (= book 11)
				(map (# (if (= (abs %0) 16) (* (if (< %0 0) -1 1) (aac-escape in)) %0)) values)
				values))))

(defun aac-spectrum (in info sections)
	; (aac-spectrum reader info sections) -> nums
	;the 1024 quantised lines, of a long window, or of 8 short ones one
	;after the other, 128 each. A group's lines are sent band by band, each
	;band of each of its windows in turn
	(defq bands (. info :find :bands) window 0 max_sfb (. info :find :max_sfb)
		size (if (= (. info :find :sequence) +aac_eight_short) 128 1024) out (fill (list) 1024 0))
	(each (lambda (count books)
			(each (lambda (sfb)
					(defq book (elem-get books sfb) from (elem-get bands sfb) to (elem-get bands (inc sfb))
						w window)
					(times count
						(when (<= 1 book 11)
							(defq at (+ (* w size) from) end (+ (* w size) to)
								dims (first (elem-get (. +aac :find :kinds) (dec book))))
							(while (< at end)
								(each (# (elem-set out (+ at (!)) %0)) (aac-values in book))
								(setq at (+ at dims))))
						(setq w (inc w))))
				(range 0 max_sfb))
			(setq window (+ window count)))
		(. info :find :lengths) sections)
	(apply (const nums) out))

(defun aac-channel (in info rate_index &optional common)
	; (aac-channel reader info rate_index [common]) -> channel
	;an individual channel stream: its gain, window, unless it has the one
	;of its pair, sections, scales, pulses, TNS and spectrum
	(defq ch (Emap) global (bits-get in 8))
	(unless common (setq info (aac-ics-info in rate_index)))
	(defq sections (aac-sections in info) scales (aac-scales in info sections global) pulses :nil tns :nil)
	(when (= (bits-1 in) 1)
		(if (= (. info :find :sequence) +aac_eight_short) (throw "aac: pulses in short windows" 0))
		(setq pulses (aac-pulses in)))
	(if (= (bits-1 in) 1) (setq tns (aac-tns in info)))
	(if (= (bits-1 in) 1) (throw "aac: gain control is not of AAC-LC" 0))
	(scatter ch :info info :sections sections :scales scales :pulses pulses :tns tns
		:spectrum (aac-spectrum in info sections)))

;;;;;;;;;;;;;;;;;;;
; a block of a packet
;;;;;;;;;;;;;;;;;;;

(defun aac-block (decoder packet)
	; (aac-block decoder packet) -> (channel ...) | :nil
	;the channels of a raw data block, the elements read till END, and how
	;many bits it took
	(defq in (bits-of packet) channels (list) rate_index (. decoder :find :rate_index)
		kind 0)
	;of more than two channels only the centre and the front pair are wanted,
	;the elements to the first pair: what is after it is not read, its
	;blocks are its length
	(defq front (> (. decoder :find :channels) 2))
	(while (and (/= (setq kind (bits-get in 3)) +aac_end)
			(not (and front (some (# (eql (. %0 :find :pair) :right)) channels))))
		(bits-get in 4)
		(cond
			((or (= kind +aac_sce) (= kind +aac_lfe)) (push channels (aac-channel in :nil rate_index)))
			((= kind +aac_cpe)
				(defq common (bits-1 in) info :nil ms 0 ms_mask :nil)
				(when (= common 1)
					(setq info (aac-ics-info in rate_index) ms (bits-get in 2))
					(if (= ms 1)
						(setq ms_mask (map (lambda (_)
								(map (lambda (_) (bits-1 in)) (range 0 (. info :find :max_sfb))))
							(. info :find :lengths)))))
				(defq left (aac-channel in info rate_index (= common 1))
					right (aac-channel in info rate_index (= common 1)))
				(scatter left :ms ms :ms_mask ms_mask :pair :left)
				(scatter right :ms ms :ms_mask ms_mask :pair :right)
				(push channels left right))
			((= kind +aac_dse)
				;data, stepped over: aligned or not, and its length
				(defq align (bits-1 in) count (bits-get in 8))
				(if (= count 255) (setq count (+ count (bits-get in 8))))
				(if (= align 1) (bits-seek in (* 8 (/ (+ (bits-at in) 7) 8))))
				(bits-skip in (* 8 count)))
			((= kind +aac_fil)
				;fill, stepped over; its 4 bits of count were read as the tag
				(bits-skip in -4)
				(defq count (bits-get in 4))
				(if (= count 15) (setq count (+ count (bits-get in 8) -1)))
				(bits-skip in (* 8 count)))
			(:t (throw "aac: an element not of AAC-LC" kind))))
	(. decoder :insert :bits (bits-at in))
	channels)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the spectrum to samples
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aac-power (x n)
	(defq p (n2r 1))
	(times n (setq p (* p x)))
	p)

(defun aac-root (x n)
	; (aac-root x n) -> real
	;the nth root of a real of 1 or more, by Newton's steps, from above it,
	;so each is less than the one before till it is there: a step that is
	;not less ends it, as the last bit may go to and fro
	(defq r (if (> x (n2r 1)) x (n2r 1)) last (+ r (n2r 1)))
	(while (< r last)
		(setq last r r (/ (+ (* (n2r (dec n)) r) (/ x (aac-power r (dec n)))) (n2r n))))
	last)

(defun aac-windows (n)
	; (aac-windows n) -> (sine kbd)
	;the rising halves of the windows, alpha 4 for the long, 6 for the short
	(list (mdct-sine n) (mdct-kbd n (if (= n 1024) 4 6))))

(defun aac-tables ()
	; (aac-tables) -> tables
	;made the once, as the first decoder is
	(unless (. +aac :find :made)
		(defq gain (cap 512 (reals)) step (sqrt (sqrt (n2r 2))) g (n2r 1) pow (cap 8192 (reals (n2r 0))))
		;2^(0.25 (i - 100)), i from 0 to 255, and 2^(0.25 i) from -100
		(times 100 (setq g (/ g step)))
		(times 512 (push gain g) (setq g (* g step)))
		;|v|^(4/3), to 8191
		(each (# (defq v (n2r %0)) (push pow (* v (aac-root v 3)))) (range 1 8192))
		;the native reader of codes, taken now, not as this is read
		(. +aac :insert :huff (ffi "lib/media/media_huff"))
		(. +aac :insert :made (scatter (Emap) :gain gain :pow pow
			:long (aac-windows 1024) :short (aac-windows 128)
			:dct_long (mdct-tables 1024) :dct_short (mdct-tables 128))))
	(. +aac :find :made))

;the bands TNS may filter at most, of a long window and a short, by rate
(defq *aac_tns_long* '(31 31 34 40 42 51 46 46 42 42 42 39 39)
	*aac_tns_short* '(9 9 10 14 14 14 14 14 14 14 14 14 14))

(defun aac-dequant (ch tables)
	; (aac-dequant channel tables) -> reals
	;the 1024 lines of a channel, each |v|^(4/3) by the gain of its band,
	;the pulses added to them first; noise and intensity bands left 0
	(defq info (. ch :find :info) q (. ch :find :spectrum) bands (. info :find :bands)
		short (= (. info :find :sequence) +aac_eight_short) size (if short 128 1024)
		pow (. tables :find :pow) gain (. tables :find :gain) out (fill (reals) 1024 (n2r 0)) window 0)
	(when (defq pulses (. ch :find :pulses))
		(setq q (cat q))
		(defq k (elem-get bands (first pulses)))
		(each! (lambda ((offset amp))
				(setq k (+ k offset))
				(elem-set q k (if (> (elem-get q k) 0) (+ (elem-get q k) amp) (- (elem-get q k) amp))))
			(list pulses) 1 -1))
	(each (lambda (count books scales)
			(each (lambda (sfb)
					(defq book (elem-get books sfb))
					(when (<= 1 book 11)
						(defq g (elem-get gain (elem-get scales sfb)) w window)
						(times count
							(each (lambda (k)
									(defq v (elem-get q k))
									(if (/= v 0) (elem-set out k
										(if (> v 0) (* (elem-get pow (min v 8191)) g) (neg (* (elem-get pow (min (neg v) 8191)) g))))))
								(range (+ (* w size) (elem-get bands sfb)) (+ (* w size) (elem-get bands (inc sfb)))))
							(setq w (inc w)))))
				(range 0 (. info :find :max_sfb)))
			(setq window (+ window count)))
		(. info :find :lengths) (. ch :find :sections) (. ch :find :scales))
	out)

(defun aac-noise (ch out tables decoder)
	; (aac-noise channel lines tables decoder)
	;each band of noise filled, random lines of the band's energy
	(defq info (. ch :find :info) bands (. info :find :bands) window 0
		size (if (= (. info :find :sequence) +aac_eight_short) 128 1024) seed (. decoder :find :seed))
	(each (lambda (count books scales)
			(each (lambda (sfb)
					(when (= (elem-get books sfb) +aac_noise)
						(defq w window)
						(times count
							;the band's noise made once to know its energy, then again
							;from the same seed into the lines, by the scale it gives
							(defq from (+ (* w size) (elem-get bands sfb)) to (+ (* w size) (elem-get bands (inc sfb)))
								values (fill (reals) (- to from) (n2r 0)))
							(noise-fill values 0 (- to from) seed (n2r 1))
							(defq energy (nums-sum (nums-mul values values)))
							(setq seed (noise-fill out from to seed (if (> energy (n2r 0))
								(/ (elem-get (. tables :find :gain) (+ 100 (elem-get scales sfb))) (sqrt energy)) (n2r 0))))
							(setq w (inc w)))))
				(range 0 (. info :find :max_sfb)))
			(setq window (+ window count)))
		(. info :find :lengths) (. ch :find :sections) (. ch :find :scales))
	(. decoder :insert :seed seed))

(defun aac-stereo (left right l r tables)
	; (aac-stereo left right l_lines r_lines tables)
	;of a pair with a common window: M/S, L = M + S and R = M - S, and
	;intensity, R the L of the band by its position, its sign by its book
	;and by the M/S mask
	(defq info (. left :find :info) bands (. info :find :bands) window 0
		size (if (= (. info :find :sequence) +aac_eight_short) 128 1024)
		ms (. left :find :ms) mask (. left :find :ms_mask) gain (. tables :find :gain))
	(each (lambda (count lbooks rbooks rscales)
			(defq g (!))
			(each (lambda (sfb)
					(defq lbook (elem-get lbooks sfb) rbook (elem-get rbooks sfb)
						used (or (= ms 2) (and (= ms 1) (= 1 (elem-get (elem-get mask g) sfb)))) w window)
					(times count
						(defq from (+ (* w size) (elem-get bands sfb)) to (+ (* w size) (elem-get bands (inc sfb))))
						(cond
							((or (= rbook +aac_intensity) (= rbook +aac_intensity2))
								(defq scale (elem-get gain (- 100 (elem-get rscales sfb))))
								(if (= rbook +aac_intensity2) (setq scale (neg scale)))
								(and (= ms 1) used (setq scale (neg scale)))
								(each (# (elem-set r %0 (* (elem-get l %0) scale))) (range from to)))
							((and used (< lbook +aac_noise) (< rbook +aac_noise))
								(each (lambda (k)
										(defq m (elem-get l k) s (elem-get r k))
										(elem-set l k (+ m s))
										(elem-set r k (- m s)))
									(range from to))))
						(setq w (inc w))))
				(range 0 (. info :find :max_sfb)))
			(setq window (+ window count)))
		(. info :find :lengths) (. left :find :sections) (. right :find :sections) (. right :find :scales)))

(defun aac-tns-apply (ch out rate_index)
	; (aac-tns-apply channel lines rate_index)
	;each filter of each window, its parcor coefficients to those of a
	;filter, run over its bands, up or down
	(when (defq tns (. ch :find :tns))
		(defq info (. ch :find :info) bands (. info :find :bands)
			short (= (. info :find :sequence) +aac_eight_short) size (if short 128 1024)
			top_bands (min (. info :find :max_sfb)
				(elem-get (if short (. +aac :find :tns_short) (. +aac :find :tns_long)) rate_index)
				(dec (length bands))))
		(each (lambda (filters)
				(defq w (!) top (dec (length bands)))
				(each (lambda ((len direction coefs res compress))
						(defq bottom (max 0 (- top len)) order (length coefs))
						(when (> order 0)
							;the coefficients, sign extended, to sines, to a filter
							(defq bits (- (+ res 3) compress) half (<< 1 (- (+ res 3) 1))
								iqm (/ (+ (n2r half) (n2r 0.5)) (/ (. +aac :find :pi) (n2r 2)))
								iqp (/ (- (n2r half) (n2r 0.5)) (/ (. +aac :find :pi) (n2r 2)))
								parcor (map (lambda (c)
										(if (>= c (<< 1 (dec bits))) (setq c (- c (<< 1 bits))))
										(sin (/ (n2r c) (if (>= c 0) iqp iqm))))
									coefs)
								lpc (list (n2r 1)))
							(each (lambda (k)
									(defq b (cat lpc) p (elem-get parcor k))
									(push lpc p)
									(each (# (elem-set lpc %0 (+ (elem-get b %0) (* p (elem-get b (- (inc k) %0)))))) (range 1 (inc k))))
								(range 0 order))
							(defq from (+ (* w size) (elem-get bands (min bottom top_bands)))
								to (+ (* w size) (elem-get bands (min top top_bands))))
							(when (> to from)
								(defq lines (if (= direction 1) (reverse (range from to)) (range from to)) done (list))
								(each (lambda (k)
										(defq y (elem-get out k))
										(each (# (if (<= %0 (length done)) (setq y (- y (* (elem-get lpc %0) (elem-get done (- (length done) %0))))))) (range 1 (inc order)))
										(elem-set out k y)
										(push done y))
									lines)))
						(setq top bottom))
					filters))
			tns)))

(defun aac-decoder (config)
	; (aac-decoder config) -> decoder
	;what a channel carries from one block to the next: the half of its
	;window that overlaps the next, and the shape of its window. The native
	;of the DCT-IV is taken now, not as this is read
	(defq tables (aac-tables) zeros (fill (reals) 1024 (n2r 0)))
	(scatter (Emap) :rate_index (second config) :rate (third config) :channels (elem-get config 3)
		:tables tables :dct4 (ffi "lib/media/media_dct4") :seed 1
		:overlap (map (lambda (_) (cat zeros)) (range 0 8)) :shape (map (lambda (_) 0) (range 0 8))
		:zeros zeros :ones (nums-add (nums-scale zeros (n2r 0)) (apply (const reals) (map (lambda (_) (n2r 1)) (range 0 1024))))))

(defun aac-unfold (decoder lines n)
	; (aac-unfold decoder lines n) -> reals
	(mdct-imdct (. decoder :find :dct4) (. (. decoder :find :tables) :find (if (= n 1024) :dct_long :dct_short)) lines n))

(defun aac-imdct (decoder lines info c)
	; (aac-imdct decoder lines info channel) -> reals
	;the 1024 samples of a channel's lines: the IMDCT, windowed, its first
	;half added to what the block before left, its second half left
	(defq tables (. decoder :find :tables) sequence (. info :find :sequence)
		shape (. info :find :shape) before (elem-get (. decoder :find :shape) c)
		long_rise (elem-get (. tables :find :long) before) long_fall (reverse (elem-get (. tables :find :long) shape))
		short_rise (elem-get (. tables :find :short) before) short_cur (elem-get (. tables :find :short) shape)
		short_fall (reverse short_cur) zeros (. decoder :find :zeros) ones (. decoder :find :ones))
	(defq whole (cond
		((= sequence +aac_eight_short)
			;eight short windows, each at 448 and 128 on, the first rising by
			;the shape before, those after by this one
			(defq buf (cat zeros zeros))
			(each (lambda (w)
					(defq x (nums-mul (aac-unfold decoder (slice lines (* w 128) (* (inc w) 128)) 128)
							(cat (if (= w 0) short_rise short_cur) short_fall))
						at (+ 448 (* w 128)))
					(each (# (elem-set buf (+ at (!)) (+ (elem-get buf (+ at (!))) %0))) x))
				(range 0 8))
			buf)
		(:t (nums-mul (aac-unfold decoder lines 1024)
			(cat (if (= sequence +aac_long_stop) (cat (slice zeros 0 448) short_rise (slice ones 0 448)) long_rise)
				(if (= sequence +aac_long_start) (cat (slice ones 0 448) short_fall (slice zeros 0 448)) long_fall))))))
	(defq overlap (. decoder :find :overlap) out (nums-add (slice whole 0 1024) (elem-get overlap c)))
	(elem-set overlap c (slice whole 1024 2048))
	(elem-set (. decoder :find :shape) c shape)
	out)

(defun aac-decode (decoder packet)
	; (aac-decode decoder packet) -> ((reals) ...)
	;the 1024 samples of each channel of a packet
	(defq tables (. decoder :find :tables) rate_index (. decoder :find :rate_index)
		channels (. decoder :insert :last (aac-block decoder packet))
		channels (. decoder :find :last)
		lines (map (lambda (ch)
				(defq out (aac-dequant ch tables))
				(aac-noise ch out tables decoder)
				out)
			channels))
	;the pairs, M/S and intensity, before TNS
	(each (lambda (ch)
			(and (eql (. ch :find :pair) :left) (. ch :find :ms)
				(aac-stereo ch (elem-get channels (inc (!))) (elem-get lines (!)) (elem-get lines (inc (!))) tables)))
		channels)
	(each (# (aac-tns-apply %0 %1 rate_index)) channels lines)
	(defq out (map (# (aac-imdct decoder %1 (. %0 :find :info) (!))) channels lines))
	;of more than two channels, the front pair with the centre before it in
	;each at 0.707, as a downmix to two has it; a recording in the centre
	;alone is heard on both
	(cond
		((and (> (. decoder :find :channels) 2) (defq i (some (# (if (eql (. %0 :find :pair) :left) (!))) channels)))
			(defq left (elem-get out i) right (elem-get out (inc i)))
			(when (> i 0)
				(defq centre (nums-scale (elem-get out (dec i)) (. +aac :find :centre)))
				(setq left (nums-add left centre) right (nums-add right centre)))
			(list left right))
		(out)))

;what the functions read as they run
(scatter +aac :rates *aac_rates* :sf (huff-table *aac_sf_book*) :spectral (map (const huff-table) *aac_books*)
	:sf_lisp *aac_sf* :spectral_lisp *aac_spectral* :kinds *aac_book_kinds*
	:bands_long *aac_bands_long* :bands_short *aac_bands_short*
	:tns_long *aac_tns_long* :tns_short *aac_tns_short* :made :nil :pi (mdct-pi)
	:centre (sqrt (/ (n2r 1) (n2r 2))))

;module
(export-symbols '(aac-config aac-track-config aac-decoder aac-decode))
(env-pop)
