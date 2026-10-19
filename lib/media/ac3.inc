;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ac3, the sound of films, AC-3 and E-AC-3, ATSC A/52, our own decoder
;
; A frame is a header and blocks of 256 samples a channel, 6 of them in
; AC-3, 1 2 3 or 6 in E-AC-3. Each block has each channel's spectrum as
; exponents, and mantissas of as many bits as an allocation from the
; exponents says, worked out alike by coder and decoder. (ac3-header
; reader) reads what a frame is, (ac3-frame-info reader header) the
; strategies of E-AC-3 for all of its blocks.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./bits.inc")
(import "./mdct.inc")

;module
(env-push)

;the channels of each mode, acmod, the full bandwidth ones, and the
;blocks of the codes of E-AC-3
(defq +ac3 (Emap))
(scatter +ac3 :channels '(2 1 2 3 3 4 4 5) :blocks '(1 2 3 6) :rates '(48000 44100 32000)
	;the words of an AC-3 frame by its size code and rate
	:frame_sizes '((64 69 96) (64 70 96) (80 87 120) (80 88 120) (96 104 144) (96 105 144)
		(112 121 168) (112 122 168) (128 139 192) (128 140 192) (160 174 240) (160 175 240)
		(192 208 288) (192 209 288) (224 243 336) (224 244 336) (256 278 384) (256 279 384)
		(320 348 480) (320 349 480) (384 417 576) (384 418 576) (448 487 672) (448 488 672)
		(512 557 768) (512 558 768) (640 696 960) (640 697 960) (768 835 1152) (768 836 1152)
		(896 975 1344) (896 976 1344) (1024 1114 1536) (1024 1115 1536) (1152 1253 1728) (1152 1254 1728)
		(1280 1393 1920) (1280 1394 1920))
	;E-AC-3's strategies of a channel's exponents in the 6 blocks, by a code
	;of 5 bits: 0 reuse, 1 D15, 2 D25, 3 D45
	:frm_expstr '((1 0 0 0 0 0) (1 0 0 0 0 3) (1 0 0 0 2 0) (1 0 0 0 3 3) (2 0 0 2 0 0) (2 0 0 2 0 3) (2 0 0 3 2 0) (2 0 0 3 3 3)
		(2 0 1 0 0 0) (2 0 2 0 0 3) (2 0 2 0 2 0) (2 0 2 0 3 3) (2 0 3 2 0 0) (2 0 3 2 0 3) (2 0 3 3 2 0) (2 0 3 3 3 3)
		(3 1 0 0 0 0) (3 1 0 0 0 3) (3 2 0 0 2 0) (3 2 0 0 3 3) (3 2 0 2 0 0) (3 2 0 2 0 3) (3 2 0 3 2 0) (3 2 0 3 3 3)
		(3 3 1 0 0 0) (3 3 2 0 0 3) (3 3 2 0 2 0) (3 3 2 0 3 3) (3 3 3 2 0 0) (3 3 3 2 0 3) (3 3 3 3 2 0) (3 3 3 3 3 3)))

;the tables of the bit allocation: the first bin of each of the 50 bands,
;the band of each bin, the log addition, the hearing threshold of each
;band at each rate, and the bits a mantissa has by its address
(scatter +ac3 :band_start '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
		16 17 18 19 20 21 22 23 24 25 26 27 28 31 34 37
		40 43 46 49 55 61 67 73 79 85 97 109 121 133 157 181
		205 229 253)
	:bin_band '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
		24 25 26 27 28 28 28 29 29 29 30 30 30 31 31 31 32 32 32 33 33 33 34 34
		34 35 35 35 35 35 35 36 36 36 36 36 36 37 37 37 37 37 37 38 38 38 38 38
		38 39 39 39 39 39 39 40 40 40 40 40 40 41 41 41 41 41 41 41 41 41 41 41
		41 42 42 42 42 42 42 42 42 42 42 42 42 43 43 43 43 43 43 43 43 43 43 43
		43 44 44 44 44 44 44 44 44 44 44 44 44 45 45 45 45 45 45 45 45 45 45 45
		45 45 45 45 45 45 45 45 45 45 45 45 45 46 46 46 46 46 46 46 46 46 46 46
		46 46 46 46 46 46 46 46 46 46 46 46 46 47 47 47 47 47 47 47 47 47 47 47
		47 47 47 47 47 47 47 47 47 47 47 47 47 48 48 48 48 48 48 48 48 48 48 48
		48 48 48 48 48 48 48 48 48 48 48 48 48 49 49 49 49 49 49 49 49 49 49 49
		49 49 49 49 49 49 49 49 49 49 49 49 49)
	:log_add '(64 63 62 61 60 59 58 57 56 55 54 53 52 52 51 50 49 48 47 47
		46 45 44 44 43 42 41 41 40 39 38 38 37 36 36 35 35 34 33 33
		32 32 31 30 30 29 29 28 28 27 27 26 26 25 25 24 24 23 23 22
		22 21 21 21 20 20 19 19 19 18 18 18 17 17 17 16 16 16 15 15
		15 14 14 14 13 13 13 13 12 12 12 12 11 11 11 11 10 10 10 10
		10 9 9 9 9 9 8 8 8 8 8 8 7 7 7 7 7 7 6 6
		6 6 6 6 6 6 5 5 5 5 5 5 5 5 4 4 4 4 4 4
		4 4 4 4 4 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2
		2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1
		1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
		1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0
		0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
		0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	:threshold '((1232 1264 1408) (1232 1264 1408) (1088 1120 1200) (1024 1040 1104) (992 992 1056) (960 976 1008)
		(944 960 992) (944 944 976) (928 944 960) (928 928 944) (928 928 944) (928 928 944)
		(928 928 928) (912 928 928) (912 912 928) (912 912 928) (896 912 928) (896 896 928)
		(880 896 928) (880 896 928) (864 880 912) (864 880 912) (848 864 912) (848 864 912)
		(832 848 896) (832 848 896) (816 832 896) (800 832 880) (784 800 864) (768 784 848)
		(752 768 832) (752 752 816) (752 752 800) (752 752 784) (768 752 768) (784 768 752)
		(832 800 752) (912 848 752) (992 912 768) (1056 992 784) (1120 1056 816) (1168 1104 848)
		(1184 1184 960) (1120 1168 1040) (1088 1120 1136) (1088 1088 1184) (1312 1152 1120) (2048 1584 1088)
		(2112 2112 1104) (2112 2112 1248))
	:bap '(0 1 1 1 1 1 2 2 3 3 3 4 4 5 5 6
		6 6 6 7 7 7 7 8 8 8 8 9 9 9 9 10
		10 10 10 11 11 11 11 12 12 12 12 13 13 13 13 14
		14 14 14 14 14 14 14 15 15 15 15 15 15 15 15 15)
	:slow_decay '(15 17 19 21) :fast_decay '(63 83 103 123) :slow_gain '(1344 1240 1144 1040)
	:db_per_bit '(0 1792 2304 2816) :floor '(752 688 624 560 496 368 240 -2048)
	:fast_gain '(128 256 384 512 640 768 896 1024)
	;the bits of a mantissa of bap 6 to 15
	:qbits '(0 0 0 0 0 0 5 6 7 8 9 10 11 12 14 16)
	:rematrix '(13 25 37 61 253)
	:cpl_bands '(0 0 0 0 0 0 0 0 1 0 1 1 0 1 1 1 1 1)
	:spx_bands '(0 0 0 0 0 0 0 0 1 0 1 0 1 0 1 0 1))

(defun ac3-bsi (in h)
	;the rest of AC-3's: dialogue level, compression, language, production,
	;time codes or the other syntax of id 6, and more to step over
	(times (if (= (. h :find :mode) 0) 2 1)
		(bits-get in 5)
		(if (= (bits-1 in) 1) (bits-get in 8))
		(if (= (bits-1 in) 1) (bits-get in 8))
		(if (= (bits-1 in) 1) (bits-get in 7)))
	(bits-get in 2)
	(cond
		((/= (. h :find :id) 6)
			(if (= (bits-1 in) 1) (bits-get in 14))
			(if (= (bits-1 in) 1) (bits-get in 14)))
		(:t (if (= (bits-1 in) 1) (bits-get in 14))
			(if (= (bits-1 in) 1) (bits-get in 14))))
	(if (= (bits-1 in) 1) (bits-skip in (* 8 (inc (bits-get in 6)))))
	h)

(defun ac3-eac3-bsi (in h)
	;the rest of E-AC-3's header, its stream id, dialogue level and
	;compression, the channel map of a dependent stream, mixing and
	;information metadata, all stepped over
	(defq mode (. h :find :mode) type (. h :find :type) blocks (. h :find :blocks) lfe (. h :find :lfe))
	(bits-get in 5)
	(times (if (= mode 0) 2 1)
		(bits-get in 5)
		(if (= (bits-1 in) 1) (bits-get in 8)))
	(and (= type 1) (= (bits-1 in) 1) (bits-get in 16))
	;mixing metadata
	(when (= (bits-1 in) 1)
		(when (> mode 2)
			(bits-get in 2)
			(if (/= 0 (logand mode 1)) (bits-get in 6))
			(if (/= 0 (logand mode 4)) (bits-get in 6)))
		(and (= lfe 1) (= (bits-1 in) 1) (bits-get in 5))
		(when (= type 0)
			(times (if (= mode 0) 2 1) (if (= (bits-1 in) 1) (bits-get in 6)))
			(if (= (bits-1 in) 1) (bits-get in 6))
			(case (bits-get in 2)
				(1 (bits-get in 5))
				(2 (bits-get in 12))
				(3 (bits-skip in (* 8 (+ 2 (bits-get in 5))))))
			(if (< mode 2) (times (if (= mode 0) 2 1) (if (= (bits-1 in) 1) (bits-get in 14))))
			(when (= (bits-1 in) 1)
				(times blocks (if (or (= blocks 1) (= (bits-1 in) 1)) (bits-get in 5))))))
	;informational metadata
	(when (= (bits-1 in) 1)
		(bits-get in 5)
		(if (= mode 2) (bits-get in 4))
		(if (>= mode 6) (bits-get in 2))
		(times (if (= mode 0) 2 1) (if (= (bits-1 in) 1) (bits-get in 8)))
		(bits-1 in))
	(and (= type 0) (/= blocks 6) (bits-1 in))
	(and (= type 2) (or (= blocks 6) (= (bits-1 in) 1)) (bits-get in 6))
	(if (= (bits-1 in) 1) (bits-skip in (* 8 (inc (bits-get in 6)))))
	h)

(defun ac3-header (in)
	; (ac3-header reader) -> :nil | header
	;the sync word, and what the frame is: AC-3 if the stream id is 10 or
	;less, E-AC-3 if 11 to 16; the size of the frame in bytes, the rate,
	;the blocks, the channels and if there is an LFE. What is only to
	;inform or to mix is stepped over
	(when (= (bits-get in 16) 0x0b77)
		(defq h (Emap) id (logand (>> (code-be (first in) 4 (>> (bits-at in) 3)) 3) 31))
		(cond
			((<= id 10)
				(bits-get in 16)
				(defq rate (bits-get in 2) size_code (bits-get in 6))
				(bits-get in 5)
				(bits-get in 3)
				(defq mode (bits-get in 3))
				(cond
					((= mode 2) (bits-get in 2))
					(:t (and (/= 0 (logand mode 1)) (/= mode 1) (bits-get in 2))
						(and (/= 0 (logand mode 4)) (bits-get in 2))))
				(defq lfe (bits-1 in))
				(scatter h :eac3 :nil :id id :rate (elem-get (. +ac3 :find :rates) rate) :rate_code rate
					:blocks 6 :mode mode :lfe lfe :type 2
					:size (* 2 (elem-get (elem-get (. +ac3 :find :frame_sizes) size_code) rate)))
				(ac3-bsi in h))
			((<= id 16)
				(defq type (bits-get in 2) sub (bits-get in 3) size (* 2 (inc (bits-get in 11)))
					rate (bits-get in 2) blocks 6)
				(if (= rate 3) (throw "ac3: half rates of E-AC-3 are not here" rate)
					(setq blocks (elem-get (. +ac3 :find :blocks) (bits-get in 2))))
				(defq mode (bits-get in 3) lfe (bits-1 in))
				(scatter h :eac3 :t :id id :type type :substream sub :size size :rate_code rate
					:rate (elem-get (. +ac3 :find :rates) rate) :blocks blocks :mode mode :lfe lfe)
				(ac3-eac3-bsi in h))
			(:t (throw "ac3: no such stream id" id)))
		(scatter h :fbw (elem-get (. +ac3 :find :channels) mode) :channels (+ (elem-get (. +ac3 :find :channels) mode) lfe))))

(defun ac3-frame-info (in h)
	; (ac3-frame-info reader header) -> header
	;of E-AC-3, the strategies for all the blocks of a frame: which syntax
	;each block has, coupling, the exponent strategy of each channel in
	;each block, AHT, SNR offset, and what is stepped over
	(defq blocks (. h :find :blocks) mode (. h :find :mode) fbw (. h :find :fbw)
		lfe (. h :find :lfe) six (= blocks 6)
		expstre (if six (bits-1 in) 1) ahte (if six (bits-1 in) 0)
		snroffststr (bits-get in 2) transproce (bits-1 in) blkswe (bits-1 in) dithflage (bits-1 in)
		bamode (bits-1 in) frmfgaincode (bits-1 in) dbaflde (bits-1 in) skipflde (bits-1 in)
		spxattene (bits-1 in) cplstre (list) cplinu (list))
	;coupling, its strategy each block, and if it is in use
	(each (lambda (b)
			(cond
				((> mode 1)
					(push cplstre (if (or (= b 0) (= (bits-1 in) 1)) 1 0))
					(push cplinu (if (= (last cplstre) 1) (bits-1 in) (if (> b 0) (elem-get cplinu (dec b)) 0))))
				(:t (push cplstre 0) (push cplinu 0))))
		(range 0 blocks))
	(defq ncpl (reduce (const +) cplinu 0)
		;each block's strategy of each channel, coupling 0, then 1 to fbw
		expstr (map (lambda (_) (map (lambda (_) 0) (range 0 (+ fbw 1)))) (range 0 blocks)))
	(cond
		((= expstre 1)
			(each (lambda (b)
					(each (# (elem-set (elem-get expstr b) %0 (bits-get in 2)))
						(range (if (= (elem-get cplinu b) 1) 0 1) (inc fbw))))
				(range 0 blocks)))
		(:t (each (lambda (c)
					(defq frm (elem-get (. +ac3 :find :frm_expstr) (bits-get in 5)))
					(each (# (elem-set (elem-get expstr %0) c (elem-get frm %0))) (range 0 6)))
				(range (if (and (> mode 1) (> ncpl 0)) 0 1) (inc fbw)))))
	(defq lfe_expstr (if (= lfe 1) (map (lambda (_) (bits-1 in)) (range 0 blocks)) (list)))
	;converter strategies of a stream from AC-3, stepped over
	(and (= (. h :find :type) 0) (or six (= (bits-1 in) 1)) (bits-skip in (* 5 fbw)))
	;AHT: which channels have it, of those that reuse their exponents after
	;the first block; the coupling channel and the LFE
	(defq aht (map (lambda (_) 0) (range 0 (+ fbw 2))))
	(when (= ahte 1)
		(each (lambda (c)
				(defq reuse (every (# (= 0 (if (= c (inc fbw)) (elem-get lfe_expstr %0) (elem-get (elem-get expstr %0) c))))
					(range 1 6)))
				(if (= c 0) (setq reuse (and reuse (= ncpl 6) (every (# (= 0 (elem-get cplstre %0))) (range 1 6)))))
				(elem-set aht c (if (and reuse (= (bits-1 in) 1)) 1 0)))
			(range (if (= ncpl 6) 0 1) (+ fbw 1 lfe))))
	;the SNR offset of the frame if it is one for all
	(defq snr :nil)
	(when (= snroffststr 0)
		(defq coarse (bits-get in 6) fine (bits-get in 4))
		(setq snr (<< (+ (<< (- coarse 15) 4) fine) 2)))
	;transient processing and spectral extension attenuation, stepped over
	(if (= transproce 1) (times fbw (if (= (bits-1 in) 1) (bits-get in 18))))
	(defq spxatten (map (lambda (_) (if (and (= spxattene 1) (= (bits-1 in) 1)) (bits-get in 5) -1)) (range 0 fbw)))
	;where each block starts, stepped over
	(when (and (> blocks 1) (= (bits-1 in) 1))
		;floor of log2 of the bytes of the frame less 2
		(defq bits 0)
		(while (<= (<< 1 (inc bits)) (- (. h :find :size) 2)) (setq bits (inc bits)))
		(bits-skip in (* (dec blocks) (+ 4 bits))))
	(scatter h :expstre expstre :ahte ahte :snroffststr snroffststr :blkswe blkswe :dithflage dithflage
		:bamode bamode :frmfgaincode frmfgaincode :dbaflde dbaflde :skipflde skipflde :spxattene spxattene
		:cplstre cplstre :cplinu cplinu :expstr expstr :lfe_expstr lfe_expstr :aht aht :snr snr :spxatten spxatten
		:transproce transproce))

;;;;;;;;;;;;;;;;;;;
; the bit allocation
;;;;;;;;;;;;;;;;;;;

(defun ac3-psd (exps start end)
	; (ac3-psd exps start end) -> (psd band_psd)
	;the power of each bin from its exponent, and of each band, the bins of
	;a band added as logs are, by the table
	(defq psd (map (# (- 3072 (<< %0 7))) exps) band_psd (map (lambda (_) 0) (range 0 50))
		band_start (. +ac3 :find :band_start) log_add (. +ac3 :find :log_add) bin start)
	(while (< bin (min end 28))
		(elem-set band_psd bin (elem-get psd bin))
		(setq bin (inc bin)))
	(when (< bin end)
		(defq band (elem-get (. +ac3 :find :bin_band) bin) more :t)
		(while more
			(defq v (elem-get psd bin) band_end (min (elem-get band_start (inc band)) end))
			(setq bin (inc bin))
			(while (< bin band_end)
				(defq p (elem-get psd bin) top (max v p))
				(setq v (+ top (elem-get log_add (min 255 (- top (>> (+ v p 1) 1))))) bin (inc bin)))
			(elem-set band_psd band v)
			(setq band (inc band) more (> end (elem-get band_start band)))))
	(list psd band_psd))

(defun ac3-lowcomp1 (a b0 b1 c)
	(cond ((= (+ b0 256) b1) c) ((> b0 b1) (max (- a 64) 0)) (a)))

(defun ac3-lowcomp (a b0 b1 bin)
	(cond ((< bin 7) (ac3-lowcomp1 a b0 b1 384)) ((< bin 20) (ac3-lowcomp1 a b0 b1 320)) ((max (- a 128) 0))))

(defun ac3-mask (d band_psd start end fast_gain lfe dba)
	; (ac3-mask decoder band_psd start end fast_gain lfe dba) -> mask
	;the masking curve of each band: the excitation by fast and slow leaks
	;and low frequency compensation, the hearing threshold, and the deltas
	(defq bin_band (. +ac3 :find :bin_band) band_start_ (elem-get bin_band start)
		band_end (inc (elem-get bin_band (dec end))) excite (map (lambda (_) 0) (range 0 50))
		slow_gain (. d :find :slow_gain) fast_decay (. d :find :fast_decay) slow_decay (. d :find :slow_decay)
		lowcomp 0 fastleak 0 slowleak 0 begin 0)
	(cond
		((= band_start_ 0)
			(setq lowcomp (ac3-lowcomp1 0 (elem-get band_psd 0) (elem-get band_psd 1) 384))
			(elem-set excite 0 (- (elem-get band_psd 0) fast_gain lowcomp))
			(setq lowcomp (ac3-lowcomp1 lowcomp (elem-get band_psd 1) (elem-get band_psd 2) 384))
			(elem-set excite 1 (- (elem-get band_psd 1) fast_gain lowcomp))
			(setq begin 7)
			(defq band 2 done :nil)
			(while (and (< band 7) (not done))
				(unless (and lfe (= band 6))
					(setq lowcomp (ac3-lowcomp1 lowcomp (elem-get band_psd band) (elem-get band_psd (inc band)) 384)))
				(setq fastleak (- (elem-get band_psd band) fast_gain) slowleak (- (elem-get band_psd band) slow_gain))
				(elem-set excite band (- fastleak lowcomp))
				(and (not (and lfe (= band 6))) (<= (elem-get band_psd band) (elem-get band_psd (inc band)))
					(setq begin (inc band) done :t))
				(setq band (inc band)))
			(each (lambda (band)
					(unless (and lfe (= band 6))
						(setq lowcomp (ac3-lowcomp lowcomp (elem-get band_psd band) (elem-get band_psd (inc band)) band)))
					(setq fastleak (max (- fastleak fast_decay) (- (elem-get band_psd band) fast_gain))
						slowleak (max (- slowleak slow_decay) (- (elem-get band_psd band) slow_gain)))
					(elem-set excite band (max (- fastleak lowcomp) slowleak)))
				(range begin (min band_end 22)))
			(setq begin 22))
		(:t ;the coupling channel, its leaks from the stream
			(setq begin band_start_ fastleak (+ (<< (. d :find :cpl_fast_leak) 8) 768)
				slowleak (+ (<< (. d :find :cpl_slow_leak) 8) 768))))
	(each (lambda (band)
			(setq fastleak (max (- fastleak fast_decay) (- (elem-get band_psd band) fast_gain))
				slowleak (max (- slowleak slow_decay) (- (elem-get band_psd band) slow_gain)))
			(elem-set excite band (max fastleak slowleak)))
		(range begin band_end))
	;the masking curve, the threshold of hearing at least
	(defq mask (map (lambda (_) 0) (range 0 50)) threshold (. +ac3 :find :threshold)
		db_per_bit (. d :find :db_per_bit) rate_code (. d :find :rate_code))
	(each (lambda (band)
			(defq tmp (- db_per_bit (elem-get band_psd band)))
			(if (> tmp 0) (elem-set excite band (+ (elem-get excite band) (>> tmp 2))))
			(elem-set mask band (max (elem-get (elem-get threshold band) rate_code) (elem-get excite band))))
		(range band_start_ band_end))
	;delta bit allocation, its segments
	(when dba
		(defq band band_start_)
		(each (lambda ((offset len value))
				(setq band (+ band offset))
				(defq delta (* 128 (if (>= value 4) (- value 3) (- value 4))))
				(times len (elem-set mask band (+ (elem-get mask band) delta)) (setq band (inc band))))
			dba))
	mask)

(defun ac3-bap (d psd mask start end snr)
	; (ac3-bap decoder psd mask start end snr) -> bap
	;the bits of each mantissa, the address of the power above the mask
	(defq bap (map (lambda (_) 0) (range 0 256)) tab (. +ac3 :find :bap) floor (. d :find :floor))
	(unless (= snr -960)
		(defq bin_band (. +ac3 :find :bin_band) band_start (. +ac3 :find :band_start) bin start)
		(while (< bin end)
			(defq band (if (< bin 28) bin (elem-get bin_band bin))
				m (+ (logand (max (- (elem-get mask band) snr floor) 0) 0x1fe0) floor)
				to (if (< bin 28) (inc bin) (min (elem-get band_start (inc band)) end)))
			(while (< bin to)
				(elem-set bap bin (elem-get tab (max 0 (min 63 (>>> (- (elem-get psd bin) m) 5)))))
				(setq bin (inc bin)))))
	bap)

;;;;;;;;;;;;;;;;;;;;;;;;;;
; the blocks of a frame
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ac3-decoder (h)
	; (ac3-decoder header) -> decoder
	;what is carried from block to block, and frame to frame, of each
	;channel: 0 the coupling channel, 1 to fbw the full ones, fbw+1 the LFE
	(defq n (+ (. h :find :fbw) 2) zeros (map (lambda (_) 0) (range 0 256)))
	(scatter (Emap) :fbw (. h :find :fbw) :lfe (. h :find :lfe) :mode (. h :find :mode) :rate_code (. h :find :rate_code)
		:exps (map (lambda (_) (cat zeros)) (range 0 n)) :bap (map (lambda (_) (cat zeros)) (range 0 n))
		:psd (map (lambda (_) :nil) (range 0 n)) :band_psd (map (lambda (_) :nil) (range 0 n))
		:mask (map (lambda (_) :nil) (range 0 n))
		:start (map (lambda (_) 0) (range 0 n)) :end (map (lambda (_) 0) (range 0 n))
		:snr (map (lambda (_) 0) (range 0 n)) :fgain (map (lambda (_) 0) (range 0 n))
		:dba (map (lambda (_) :nil) (range 0 n)) :dither (map (lambda (_) 1) (range 0 n))
		:in_cpl (map (lambda (_) 0) (range 0 n)) :first_coords (map (lambda (_) 1) (range 0 n))
		:coords (map (lambda (_) (list)) (range 0 n)) :phase (list) :cpl_sizes (list) :cpl_struct (list)
		:first_cpl_leak 1 :cpl_fast_leak 0 :cpl_slow_leak 0 :seed 1))

(defun ac3-groups (in strategy groups prev)
	; (ac3-groups reader strategy groups prev) -> (exp ...)
	;exponents sent as differences of 5 values, 3 to 7 bits, each from the
	;one before, each so many bins wide by its strategy
	(defq out (list) size (elem-get '(0 1 2 4) strategy))
	(times groups
		(defq v (bits-get in 7))
		(each (lambda (d)
				(setq prev (+ prev d -2))
				(times size (push out prev)))
			(list (/ v 25) (% (/ v 5) 5) (% v 5))))
	out)

(defun ac3-bands (in blk eac3 start_sub end_sub default struct)
	; (ac3-bands reader blk eac3 start end default struct) -> (struct sizes)
	;which subbands of 12 bins are in the band before, of coupling or of
	;spectral extension, read or left as they were, and the bands' sizes
	(if (= blk 0) (setq struct (cat default)))
	(defq n (- end_sub start_sub))
	(if (or (not eac3) (= (bits-1 in) 1))
		(each (# (elem-set struct (+ start_sub 1 %0) (bits-1 in))) (range 0 (dec n))))
	(defq sizes (list 12))
	(each (lambda (sub)
			(if (= (elem-get struct (+ start_sub sub)) 1)
				(elem-set sizes -2 (+ (last sizes) 12))
				(push sizes 12)))
		(range 1 n))
	(list struct sizes))

(defun ac3-random (d)
	;the next of the dither, a real of -0.707 to 0.707
	(defq seed (logand (+ (* (. d :find :seed) 1664525) 1013904223) 0xffffffff))
	(. d :insert :seed seed)
	(* (- (/ (n2r seed) (n2r 0x80000000)) (n2r 1)) (n2r 0.707)))

(defun ac3-mantissas (d in c groups)
	; (ac3-mantissas decoder reader channel groups) -> reals
	;the coefficients of a channel's bins, each its mantissa by 2^-exponent:
	;bap 1 2 and 4 are levels sent three or two to a code, those codes
	;shared by the channels of the block, groups; 3 and 5 levels of 3 and 4
	;bits; 6 to 15 numbers of so many bits; 0 is dither or nothing
	(defq bap (elem-get (. d :find :bap) c) exps (elem-get (. d :find :exps) c)
		start (elem-get (. d :find :start) c) end (elem-get (. d :find :end) c)
		dither (or (= c 0) (= 1 (elem-get (. d :find :dither) c)))
		qbits (. +ac3 :find :qbits) out (reals))
	(times 256 (push out (n2r 0)))
	(each (lambda (bin)
			(defq b (elem-get bap bin) m (n2r 0))
			(cond
				((= b 0) (if dither (setq m (ac3-random d))))
				((= b 1)
					(when (empty? (first groups))
						(defq v (bits-get in 5))
						(elem-set groups 0 (list (% v 3) (% (/ v 3) 3) (/ v 9))))
					(setq m (/ (n2r (- (* 2 (pop (first groups))) 2)) (n2r 3))))
				((= b 2)
					(when (empty? (second groups))
						(defq v (bits-get in 7))
						(elem-set groups 1 (list (% v 5) (% (/ v 5) 5) (/ v 25))))
					(setq m (/ (n2r (- (* 2 (pop (second groups))) 4)) (n2r 5))))
				((= b 3) (setq m (/ (n2r (- (* 2 (bits-get in 3)) 6)) (n2r 7))))
				((= b 4)
					(when (empty? (third groups))
						(defq v (bits-get in 7))
						(elem-set groups 2 (list (% v 11) (/ v 11))))
					(setq m (/ (n2r (- (* 2 (pop (third groups))) 10)) (n2r 11))))
				((= b 5) (setq m (/ (n2r (- (* 2 (bits-get in 4)) 14)) (n2r 15))))
				(:t (defq q (elem-get qbits b) v (bits-get in q))
					(if (>= v (<< 1 (dec q))) (setq v (- v (<< 1 q))))
					(setq m (/ (n2r v) (n2r (<< 1 (dec q)))))))
			(elem-set out bin (/ m (n2r (<< 1 (elem-get exps bin))))))
		(range start end))
	out)

;the default bit allocation of E-AC-3 without its syntax, and its codes
(defun ac3-defaults (d)
	(scatter d :slow_decay (elem-get (. +ac3 :find :slow_decay) 2) :fast_decay (elem-get (. +ac3 :find :fast_decay) 1)
		:slow_gain (elem-get (. +ac3 :find :slow_gain) 1) :db_per_bit (elem-get (. +ac3 :find :db_per_bit) 2)
		:floor (elem-get (. +ac3 :find :floor) 7)))

(defun ac3-block (d in h blk)
	; (ac3-block decoder reader header blk) -> (reals ...)
	;a block: its strategies, coupling, exponents, the bit allocation where
	;it changes, and the mantissas, made coefficients, those of the coupled
	;channels by their coordinates; a reals of 256 a channel, 1 to fbw and
	;the LFE, by 2^-exponent
	(defq fbw (. d :find :fbw) lfe (. d :find :lfe) mode (. d :find :mode) eac3 (. h :find :eac3)
		lfe_ch (inc fbw) channels (+ fbw lfe) stages (map (lambda (_) 0) (range 0 (+ fbw 2)))
		start (. d :find :start) end (. d :find :end) in_cpl (. d :find :in_cpl))
	;block switching, and dither
	(when (= (. h :find :blkswe) 1)
		(each (# (if (= (bits-1 in) 1) (throw "ac3: short blocks are not here yet" %0))) (range 0 fbw)))
	(if (= (. h :find :dithflage) 1)
		(each (# (elem-set (. d :find :dither) %0 (bits-1 in))) (range 1 (inc fbw)))
		(each (# (elem-set (. d :find :dither) %0 1)) (range 1 (inc fbw))))
	;dynamic range, read and let be
	(times (if (= mode 0) 2 1) (if (= (bits-1 in) 1) (bits-get in 8)))
	;spectral extension
	(and eac3 (or (= blk 0) (= (bits-1 in) 1)) (= (bits-1 in) 1)
		(throw "ac3: spectral extension is not here yet" blk))
	;coupling: its strategy, given by the frame of E-AC-3, by each block of
	;AC-3, which keeps that of the block before if it says none
	(unless eac3
		(defq cplstre (bits-1 in))
		(push (. h :find :cplstre) cplstre)
		(push (. h :find :cplinu) (if (= cplstre 1) (bits-1 in) (if (> blk 0) (elem-get (. h :find :cplinu) (dec blk)) 0))))
	(defq cplinu (elem-get (. h :find :cplinu) blk))
	(when (= (elem-get (. h :find :cplstre) blk) 1)
		(each (# (elem-set stages %0 3)) (range 0 (+ fbw 2)))
		(cond
			((= cplinu 1)
				(and eac3 (= (bits-1 in) 1) (throw "ac3: enhanced coupling is not here" blk))
				(if (and eac3 (= mode 2))
					(progn (elem-set in_cpl 1 1) (elem-set in_cpl 2 1))
					(each (# (elem-set in_cpl %0 (bits-1 in))) (range 1 (inc fbw))))
				(. d :insert :phase_in_use (if (= mode 2) (bits-1 in) 0))
				(defq begf (bits-get in 4) endf (+ 3 (bits-get in 4)))
				(elem-set start 0 (+ 37 (* 12 begf)))
				(elem-set end 0 (+ 37 (* 12 endf)))
				(bind '(struct sizes) (ac3-bands in blk eac3 begf endf (. +ac3 :find :cpl_bands) (. d :find :cpl_struct)))
				(scatter d :cpl_struct struct :cpl_sizes sizes))
			(:t (each (# (elem-set in_cpl %0 0) (elem-set (. d :find :first_coords) %0 1)) (range 1 (inc fbw)))
				(scatter d :first_cpl_leak (if eac3 1 0) :phase_in_use 0))))
	;coupling coordinates, and the phase of the right of a pair
	(when (= cplinu 1)
		(defq any :nil)
		(each (lambda (c)
				(cond
					((= 0 (elem-get in_cpl c)) (elem-set (. d :find :first_coords) c 1))
					((or (and eac3 (= 1 (elem-get (. d :find :first_coords) c))) (= (bits-1 in) 1))
						(elem-set (. d :find :first_coords) c 0)
						(setq any :t)
						(defq master (* 3 (bits-get in 2)))
						(elem-set (. d :find :coords) c (map (lambda (_)
								(defq e (bits-get in 4) m (bits-get in 4))
								(/ (n2r (>> (if (= e 15) (<< m 22) (<< (+ m 16) 21)) (+ e master))) (n2r 0x800000)))
							(. d :find :cpl_sizes))))))
			(range 1 (inc fbw)))
		(and (= mode 2) any
			(. d :insert :phase (map (lambda (_) (if (= 1 (. d :find :phase_in_use)) (bits-1 in) 0)) (. d :find :cpl_sizes)))))
	;rematrixing, of a pair alone
	(when (= mode 2)
		(when (or (and eac3 (= blk 0)) (= (bits-1 in) 1))
			(defq bands 4)
			(and (= cplinu 1) (<= (elem-get start 0) 61) (setq bands (- bands 1 (if (= (elem-get start 0) 37) 1 0))))
			(. d :insert :rematrix (map (lambda (_) (bits-1 in)) (range 0 bands)))))
	;the strategies of the exponents, from the frame of E-AC-3, of AC-3 in
	;each block, 2 bits, the LFE's 1
	(defq strategy (if eac3 (cat (elem-get (. h :find :expstr) blk))
		(map (lambda (c) (if (or (> c 0) (= cplinu 1)) (bits-get in 2) 0)) (range 0 (inc fbw)))))
	(push strategy (if (= lfe 1) (if eac3 (elem-get (. h :find :lfe_expstr) blk) (bits-1 in)) 0))
	(each (# (if (/= 0 (elem-get strategy %0)) (elem-set stages %0 3))) (range (if (= cplinu 1) 0 1) (+ fbw 1 lfe)))
	;the bandwidth of each full channel
	(each (lambda (c)
			(elem-set start c 0)
			(unless (= 0 (elem-get strategy c))
				(defq prev (elem-get end c))
				(elem-set end c (if (= 1 (elem-get in_cpl c)) (elem-get start 0) (+ 73 (* 3 (bits-get in 6)))))
				(and (> blk 0) (/= prev (elem-get end c)) (each (# (elem-set stages %0 3)) (range 0 (+ fbw 2))))))
		(range 1 (inc fbw)))
	(when (= lfe 1) (elem-set start lfe_ch 0) (elem-set end lfe_ch 7))
	;the exponents of each channel of a new strategy
	(each (lambda (c)
			(defq st (elem-get strategy c))
			(unless (= st 0)
				(defq size (* 3 (elem-get '(0 1 2 4) st))
					groups (cond
						((= c 0) (/ (- (elem-get end 0) (elem-get start 0)) size))
						((= c lfe_ch) 2)
						((/ (+ (elem-get end c) size -4) size)))
					abs (<< (bits-get in 4) (if (= c 0) 1 0)) exps (elem-get (. d :find :exps) c)
					got (ac3-groups in st groups abs) at (if (= c 0) (elem-get start 0) 1))
				(if (/= c 0) (elem-set exps 0 abs))
				(each (# (if (< (+ at (!)) 256) (elem-set exps (+ at (!)) %0))) got)
				(and (/= c 0) (/= c lfe_ch) (bits-get in 2))))
		(range (if (= cplinu 1) 0 1) (+ fbw 1 lfe)))
	;the bit allocation's parameters
	(cond
		((= (. h :find :bamode) 1)
			(when (= (bits-1 in) 1)
				(scatter d :slow_decay (elem-get (. +ac3 :find :slow_decay) (bits-get in 2))
					:fast_decay (elem-get (. +ac3 :find :fast_decay) (bits-get in 2))
					:slow_gain (elem-get (. +ac3 :find :slow_gain) (bits-get in 2))
					:db_per_bit (elem-get (. +ac3 :find :db_per_bit) (bits-get in 2))
					:floor (elem-get (. +ac3 :find :floor) (bits-get in 3)))
				(each (# (elem-set stages %0 (max (elem-get stages %0) 2))) (range 0 (+ fbw 2)))))
		((and eac3 (= blk 0)) (ac3-defaults d)))
	;the SNR offsets, of the frame or of each channel, of AC-3 each with its
	;fast gain, in any block that says so
	(when (or (not eac3) (= blk 0))
		(cond
			((/= (. h :find :snroffststr) 0)
				(when (= (bits-1 in) 1)
					(defq coarse (<< (- (bits-get in 6) 15) 4) snr 0)
					(each (lambda (c)
							(if (or (= (!) 0) (= (. h :find :snroffststr) 2)) (setq snr (<< (+ coarse (bits-get in 4)) 2)))
							(if (/= snr (elem-get (. d :find :snr) c)) (elem-set stages c (max (elem-get stages c) 1)))
							(elem-set (. d :find :snr) c snr)
							(unless eac3
								(defq g (elem-get (. +ac3 :find :fast_gain) (bits-get in 3)))
								(if (/= g (elem-get (. d :find :fgain) c)) (elem-set stages c (max (elem-get stages c) 2)))
								(elem-set (. d :find :fgain) c g)))
						(range (if (= cplinu 1) 0 1) (+ fbw 1 lfe)))))
			(:t (each (# (elem-set (. d :find :snr) %0 (. h :find :snr))) (range 0 (+ fbw 2))))))
	;the fast gains
	(cond
		((and (= (. h :find :frmfgaincode) 1) (= (bits-1 in) 1))
			(each (lambda (c)
					(defq g (elem-get (. +ac3 :find :fast_gain) (bits-get in 3)))
					(if (/= g (elem-get (. d :find :fgain) c)) (elem-set stages c (max (elem-get stages c) 2)))
					(elem-set (. d :find :fgain) c g))
				(range (if (= cplinu 1) 0 1) (+ fbw 1 lfe))))
		((and eac3 (= blk 0)) (each (# (elem-set (. d :find :fgain) %0 (elem-get (. +ac3 :find :fast_gain) 4))) (range 0 (+ fbw 2)))))
	;converter SNR offset, stepped over
	(and (= (. h :find :type) 0) (= (bits-1 in) 1) (bits-get in 10))
	;the leaks of the coupling channel
	(when (= cplinu 1)
		(when (or (= 1 (. d :find :first_cpl_leak)) (= (bits-1 in) 1))
			(defq fl (bits-get in 3) sl (bits-get in 3))
			(if (or (/= fl (. d :find :cpl_fast_leak)) (/= sl (. d :find :cpl_slow_leak))) (elem-set stages 0 (max (elem-get stages 0) 2)))
			(scatter d :cpl_fast_leak fl :cpl_slow_leak sl))
		(. d :insert :first_cpl_leak 0))
	;delta bit allocation
	(cond
		((and (= (. h :find :dbaflde) 1) (= (bits-1 in) 1))
			(defq modes (map (lambda (_) (bits-get in 2)) (range (if (= cplinu 1) 0 1) (inc fbw))))
			(each (lambda (c)
					(defq m (elem-get modes (!)))
					(cond
						((= m 1) (elem-set (. d :find :dba) c
							(map (lambda (_) (list (bits-get in 5) (bits-get in 4) (bits-get in 3))) (range 0 (inc (bits-get in 3))))))
						((= m 2) (elem-set (. d :find :dba) c :nil)))
					(elem-set stages c (max (elem-get stages c) 2)))
				(range (if (= cplinu 1) 0 1) (inc fbw))))
		((= blk 0) (each (# (elem-set (. d :find :dba) %0 :nil)) (range 0 (+ fbw 2)))))
	;the bit allocation of each channel, as far as it has changed
	(each (lambda (c)
			(defq st (elem-get stages c))
			(when (> st 2)
				(bind '(psd band_psd) (ac3-psd (elem-get (. d :find :exps) c) (elem-get start c) (elem-get end c)))
				(elem-set (. d :find :psd) c psd)
				(elem-set (. d :find :band_psd) c band_psd))
			(if (> st 1)
				(elem-set (. d :find :mask) c (ac3-mask d (elem-get (. d :find :band_psd) c) (elem-get start c) (elem-get end c)
					(elem-get (. d :find :fgain) c) (= c lfe_ch) (elem-get (. d :find :dba) c))))
			(if (> st 0)
				(elem-set (. d :find :bap) c (ac3-bap d (elem-get (. d :find :psd) c) (elem-get (. d :find :mask) c)
					(elem-get start c) (elem-get end c) (elem-get (. d :find :snr) c)))))
		(range (if (= cplinu 1) 0 1) (+ fbw 1 lfe)))
	;dummy data, stepped over
	(and (= (. h :find :skipflde) 1) (= (bits-1 in) 1) (bits-skip in (* 8 (bits-get in 9))))
	;the mantissas, the coupling channel's after the first coupled one's
	(defq groups (list (list) (list) (list)) out (list) cpl :nil)
	(each (lambda (c)
			(defq coeffs (ac3-mantissas d in c groups))
			(when (= 1 (elem-get in_cpl c))
				(unless cpl (setq cpl (ac3-mantissas d in 0 groups)))
				;the coupled bins, the coupling channel's by this one's coordinates
				(defq bin (elem-get start 0) coords (elem-get (. d :find :coords) c))
				(each (lambda (size)
						(defq k (elem-get coords (!)) band (!))
						(and (= c 2) (= mode 2) (= 1 (elem-get (. d :find :phase) band)) (setq k (neg k)))
						(times size
							(elem-set coeffs bin (* (if (and (= 0 (elem-get (elem-get (. d :find :bap) 0) bin))
									(= 0 (elem-get (. d :find :dither) c))) (n2r 0) (elem-get cpl bin)) k))
							(setq bin (inc bin))))
					(. d :find :cpl_sizes)))
			(push out coeffs))
		(range 1 (inc channels)))
	;rematrixing of a pair, the sum and difference
	(when (and (= mode 2) (. d :find :rematrix))
		(defq lo (min (elem-get end 1) (elem-get end 2)) bands (. +ac3 :find :rematrix))
		(each (lambda (flag)
				(when (= flag 1)
					(each (lambda (k)
							(defq a (elem-get (first out) k) b (elem-get (second out) k))
							(elem-set (first out) k (+ a b))
							(elem-set (second out) k (- a b)))
						(range (elem-get bands (!)) (min lo (elem-get bands (inc (!))))))))
			(. d :find :rematrix)))
	out)

;;;;;;;;;;;;;;;;;;;;;;
; a frame to samples
;;;;;;;;;;;;;;;;;;;;;;

(defun ac3-frame (d packet)
	; (ac3-frame decoder packet) -> ((reals ...) ...)
	;the samples of a frame, 256 a block a channel, each channel's blocks
	;one after another: the header, the frame's strategies, and each
	;block's coefficients through the IMDCT of 512, the KBD window of alpha
	;5, and half overlapped with the block before
	(defq in (bits-of packet) h (ac3-header in))
	(if (. h :find :eac3) (ac3-frame-info in h)
		;AC-3: its syntax has all of these in each block, what E-AC-3 sends
		;once for a frame
		(scatter h :blkswe 1 :dithflage 1 :bamode 1 :frmfgaincode 0 :dbaflde 1 :skipflde 1 :snroffststr 2
			:cplstre (list) :cplinu (list) :expstr (list) :lfe_expstr (list)))
	;each frame of E-AC-3 sends its coupling coordinates and leaks afresh
	;in its first block, with no flag before them; AC-3 has the flags
	(. d :insert :first_coords (map (lambda (_) 1) (. d :find :first_coords)))
	(. d :insert :first_cpl_leak (if (. h :find :eac3) 1 0))
	(unless (. d :find :window)
		(scatter d :window (mdct-kbd 256 5) :tables (mdct-tables 256) :dct4 (ffi "lib/media/media_dct4")
			:overlap (map (lambda (_) (reals)) (range 0 8))))
	(defq rise (. d :find :window) fall (reverse rise) window (cat rise fall)
		channels (+ (. d :find :fbw) (. d :find :lfe)) out (map (lambda (_) (list)) (range 0 channels)))
	(each (lambda (blk)
			(each (lambda (coeffs)
					(defq c (!) x (nums-mul (mdct-imdct (. d :find :dct4) (. d :find :tables) coeffs 256) window)
						before (elem-get (. d :find :overlap) c))
					(if (empty? before) (setq before (nums-scale (slice x 0 256) (n2r 0))))
					(push (elem-get out c) (nums-add (slice x 0 256) before))
					(elem-set (. d :find :overlap) c (slice x 256 512)))
				(ac3-block d in h blk)))
		(range 0 (. h :find :blocks)))
	(map (# (apply (const cat) %0)) out))

(defun ac3-stereo (d chans)
	; (ac3-stereo decoder chans) -> (left right)
	;the full channels of a frame made two, as 16 bit samples are: by
	;-2^24, the sign the standard's, the size of mantissas by 2^-exponent
	;through the IMDCT as 16 bit samples, matched to afconvert's decode;
	;and of more than two the centre and the surrounds in
	;each side at 0.707, the sum held to full scale; the LFE left out
	(defq mode (. d :find :mode) k (n2r -16777216) mix (sqrt (/ (n2r 1) (n2r 2))))
	(bind '(l r) (case mode
		(1 (list (first chans) (first chans)))
		((0 2) (list (first chans) (second chans)))
		(:t ;the channels of each mode: front L C R, then the surrounds
			(defq order (elem-get '(:nil :nil :nil (0 1 2) (0 2 3) (0 1 2 3) (0 1 2 3) (0 1 2 3 4)) mode)
				centre (if (find mode '(3 5 7)) (elem-get chans 1) :nil)
				front (if centre (list (elem-get chans 0) (elem-get chans 2)) (list (elem-get chans 0) (elem-get chans 1)))
				surrounds (slice chans (if centre 3 2) (length order))
				l (first front) r (second front) gain (n2r 1))
			(when centre
				(setq l (nums-add l (nums-scale centre mix)) r (nums-add r (nums-scale centre mix)) gain (+ gain mix)))
			(case (length surrounds)
				(1 (setq l (nums-add l (nums-scale (first surrounds) mix)) r (nums-add r (nums-scale (first surrounds) mix))))
				(2 (setq l (nums-add l (nums-scale (first surrounds) mix)) r (nums-add r (nums-scale (second surrounds) mix)))))
			(if (nempty? surrounds) (setq gain (+ gain mix)))
			(list (nums-scale l (/ (n2r 1) gain)) (nums-scale r (/ (n2r 1) gain))))))
	(list (nums-scale l k) (nums-scale r k)))

;module
(export-symbols '(ac3-header ac3-frame-info ac3-decoder ac3-block ac3-frame ac3-stereo))
(env-pop)
