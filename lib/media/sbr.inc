;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sbr, spectral band replication, HE-AAC, ISO/IEC 14496-3 4.6.18, our
; own decoder of what AAC-LC carries in its fill elements
;
; The core is coded at half the rate, and the band above it is made again
; from the band below: the core's samples to 32 bands of a QMF bank, the
; bands above copied up from those below by patches, each filtered to be
; as tonal as the coder said, and made the envelope it sent, with noise
; and sinusoids where it said, then all 64 bands to samples at twice the
; rate. (sbr-new rate) holds what goes from frame to frame for an element,
; a channel or a pair, rate that of the core; (sbr-read sbr reader crc
; bytes kind) reads the data of a fill element; (sbr-apply sbr kind chans)
; gives each channel's 2048 samples of its 1024. With no data yet it is
; the QMF banks alone, the core's band at the new rate.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./huff.inc")
(import "./bits.inc")
(import "./mdct.inc")
(import "./ps.inc")

;module
(env-push)

(import "./sbr_tables.inc")

(ffi (path-to-absolute "./media_matvec") sbr-matvec)
; (sbr-matvec matrix rows cols x from out) -> out
(ffi (path-to-absolute "./media_gather") sbr-gather)
; (sbr-gather out arrays index) -> out
(ffi (path-to-absolute "./media_window") sbr-window)
; (sbr-window v d out at scale n parts dstep vstep doff voff) -> out
(ffi (path-to-absolute "./media_sbr_gains") sbr-envelope-gains)
; (sbr-envelope-gains ins flags lim kx delta consts out) -> out

;what the functions read as they run, this one map, the tables of
;sbr_tables.inc taken in as this is read: a module's names are gone by
;the time they run
(defq +sbr (Emap))
(scatter +sbr :books *sbr_books* :offsets *sbr_offsets* :window_q31 *sbr_window* :noise_q31 *sbr_noise*)

;;;;;;;;;;;
; the tables
;;;;;;;;;;;

(defun sbr-tables ()
	; (sbr-tables)
	;the books made native tables, the window, the noise and the matrices
	;of the banks, made the once. The analysis's 32 bands of 64 sums of
	;the window, each 2 e^(i pi (k + 1/2)(2n - 1/2)/64), its cos and its
	;sin; the synthesis's 128 of the 64 bands, re and im, each e^(i pi
	;(k + 1/2)(2n - 255)/128)/64, its re part
	(unless (. +sbr :find :tables)
		(defq k (/ (n2r 1) (n2r 2147483648)) pi (mdct-pi) books (. +sbr :find :books)
			window (apply (const reals) (map (# (* (n2r %0) k)) (. +sbr :find :window_q31)))
			angle (lambda (k n d off) (/ (* pi (+ (n2r k) (n2r 0.5)) (- (n2r (* 2 n)) off)) (n2r d)))
			two (n2r 2) half (n2r 0.5) at (n2r 255) a64 (/ (n2r 1) (n2r 64)))
		(scatter +sbr :tables (map (# (huff-table (second %0))) books) :least (map (const first) books) :window window
			:window_ds (reduce (# (push %0 (elem-get window (* 2 %1)))) (range 0 320) (cap 320 (reals)))
			:noise (apply (const reals) (map (# (* (n2r %0) k)) (. +sbr :find :noise_q31)))
			;the noise's re and im apart, the first 64 again after the 512, so a
			;run of bands from anywhere is a slice of it
			:noise_re (apply (const reals) (map (# (* (n2r (elem-get (. +sbr :find :noise_q31) (* 2 (% %0 512)))) k)) (range 0 576)))
			:noise_im (apply (const reals) (map (# (* (n2r (elem-get (. +sbr :find :noise_q31) (inc (* 2 (% %0 512))))) k)) (range 0 576)))
			:alt (apply (const reals) (map (# (n2r (if (= (logand %0 1) 0) 1 -1))) (range 0 64)))
			:ones (fill (reals) 64 (n2r 1))
			:ana_re (reduce (lambda (out j) (push out (* two (cos (angle (/ j 64) (% j 64) 64 half))))) (range 0 2048) (cap 2048 (reals)))
			:ana_im (reduce (lambda (out j) (push out (* two (sin (angle (/ j 64) (% j 64) 64 half))))) (range 0 2048) (cap 2048 (reals)))
			;of the synthesis rows 0 to 63, its cos and its sin: row 127 - n is
			;of the cos of the other sign and the same sin
			:syn_c (reduce (lambda (out j) (push out (* a64 (cos (angle (% j 64) (/ j 64) 128 at))))) (range 0 4096) (cap 4096 (reals)))
			:syn_s (reduce (lambda (out j) (push out (* a64 (sin (angle (% j 64) (/ j 64) 128 at))))) (range 0 4096) (cap 4096 (reals)))
			:smooth (map (const str-to-real) '("0.33333333333333" "0.30150283239582" "0.21816949906249" "0.11516383427084" "0.03183050093751"))
			:limgain (map (const str-to-real) '("0.70795" "1.0" "1.41254" "10000000000"))
			:bw (map (const str-to-real) '("0.0" "0.75" "0.9" "0.98"))
			:flt_min (str-to-real "1.17549435e-38") :flt_eps (str-to-real "1.1920929e-7")
			:zeros40 (fill (reals) 40 (n2r 0)) :zeros64 (fill (reals) 64 (n2r 0)))))

;;;;;;;;;;;;;;;;;;;;;;;
; the frequency tables
;;;;;;;;;;;;;;;;;;;;;;;

(defun sbr-log2 (x)
	; (sbr-log2 real) -> real
	;of 1 or more, by halving the step to it, pow being there and log not
	(defq lo (n2r 0) hi (n2r 8))
	(times 48
		(defq mid (/ (+ lo hi) (n2r 2)))
		(if (< (pow (n2r 2) mid) x) (setq lo mid) (setq hi mid)))
	lo)

(defun sbr-round (x)
	; (sbr-round real) -> num
	(n2i (floor (+ x (n2r 0.5)))))

(defun sbr-bands (start stop n)
	; (sbr-bands start stop n) -> nums
	;the widths of n bands from start to stop, each the same ratio wider
	(defq base (pow (/ (n2r stop) (n2r start)) (/ (n2r 1) (n2r n))) prod (n2r start) previous start
		out (cap n (nums)))
	(times (dec n)
		(setq prod (* prod base))
		(defq present (sbr-round prod))
		(push out (- present previous))
		(setq previous present))
	(push out (- stop previous)))

(defun sbr-sorted (ns)
	(apply (const nums) (sort (map (const identity) ns) (const -))))

(defun sbr-sums (start widths)
	; (sbr-sums start widths) -> nums
	;the borders of bands of widths from start
	(reduce (# (push %0 (+ (last %0) %1))) widths (nums start)))

(defun sbr-master (sbr)
	; (sbr-master sbr) -> :nil | :t
	;the master table, the borders of the bands of the high band, k0 to k2
	(defq rate (. sbr :find :rate) offsets (. +sbr :find :offsets)
		row (cond ((= rate 16000) 0) ((= rate 22050) 1) ((= rate 24000) 2) ((= rate 32000) 3)
			((<= 44100 rate 64000) 4) ((> rate 64000) 5) (:t -1)))
	(when (>= row 0)
		(defq temp (cond ((< rate 32000) 3000) ((< rate 64000) 4000) (5000))
			start_min (/ (+ (<< temp 7) (>> rate 1)) rate) stop_min (/ (+ (<< temp 8) (>> rate 1)) rate)
			start_freq (. sbr :find :start_freq) stop_freq (. sbr :find :stop_freq)
			k0 (+ start_min (elem-get (elem-get offsets row) start_freq))
			k2 (cond
				((< stop_freq 14) (+ stop_min (reduce! (const +) (list (sbr-sorted (sbr-bands stop_min 64 13))) 0 0 stop_freq)))
				((= stop_freq 14) (* 2 k0))
				((* 3 k0)))
			k2 (min 64 k2) max_qmf (cond ((<= rate 32000) 48) ((= rate 44100) 35) (32))
			master :nil)
		(when (<= (- k2 k0) max_qmf)
			(cond
				((= (. sbr :find :freq_scale) 0)
					(defq dk (inc (. sbr :find :alter_scale)) n (<< (>> (+ (- k2 k0) (logand dk 2)) dk) 1))
					(when (> n 0)
						(defq widths (apply (const nums) (map (lambda (_) dk) (range 0 n))) diff (- k2 k0 (* n dk)))
						(cond
							((< diff 0)
								(elem-set widths 0 (dec (elem-get widths 0)))
								(if (< diff -1) (elem-set widths 1 (dec (elem-get widths 1)))))
							((> diff 0) (elem-set widths (dec n) (inc (elem-get widths (dec n))))))
						(setq master (sbr-sums k0 widths))))
				(:t (defq half (- 7 (. sbr :find :freq_scale)) two (> (* 49 k2) (* 110 k0))
						k1 (if two (* 2 k0) k2)
						n0 (* 2 (sbr-round (* (n2r half) (sbr-log2 (/ (n2r k1) (n2r k0)))))))
					(when (> n0 0)
						(defq vk0 (sbr-sorted (sbr-bands k0 k1 n0)) top0 (last vk0))
						(when (> (reduce (const min) vk0 64) 0)
							(cond
								(two (defq warp (if (= (. sbr :find :alter_scale) 1) (str-to-real "0.76923076923076923077") (n2r 1))
										n1 (* 2 (sbr-round (* (n2r half) warp (sbr-log2 (/ (n2r k2) (n2r k1))))))
										vk1 (sbr-bands k1 k2 n1))
									(when (< (reduce (const min) vk1 64) top0)
										(setq vk1 (sbr-sorted vk1))
										(defq change (min (- top0 (first vk1)) (>>> (- (last vk1) (first vk1)) 1)))
										(elem-set vk1 0 (+ (first vk1) change))
										(elem-set vk1 (dec (length vk1)) (- (last vk1) change)))
									(setq vk1 (sbr-sorted vk1))
									(if (> (reduce (const min) vk1 64) 0)
										(setq master (cat (sbr-sums k0 vk0) (rest (sbr-sums k1 vk1))))))
								((setq master (sbr-sums k0 vk0)))))))))
		(and master (> (dec (length master)) (. sbr :find :xover))
			(scatter sbr :k0 k0 :k2 k2 :master master))))

(defun sbr-patches (sbr)
	; (sbr-patches sbr) -> :nil | :t
	;the patches, each the bands below it that a run of bands above is
	;copied from, to the top of the high band
	(defq rate (. sbr :find :rate) master (. sbr :find :master) k0 (. sbr :find :k0)
		kx (. sbr :find :kx) m (. sbr :find :m) n_master (dec (length master))
		msb k0 usb kx goal (/ (+ (<< 1000 11) (>> rate 1)) rate) sb 0 last_k -1 last_msb -1 ok :t
		counts (nums) starts (nums)
		k (if (< goal (+ kx m)) (some (# (if (>= %0 goal) (!))) master) n_master))
	(until (or (not ok) (= sb (+ kx m)))
		(defq odd 0 i k)
		(if (or (> (length counts) 5) (and (= k last_k) (= msb last_msb))) (setq ok :nil))
		(setq last_k k last_msb msb)
		(while (and ok (or (= i k) (> sb (- (+ k0 msb -1) odd))))
			(setq sb (elem-get master i) odd (logand (+ sb k0) 1) i (dec i)))
		(when ok
			(defq count (max (- sb usb) 0))
			(cond
				((> count 0) (push counts count) (push starts (- k0 odd count)) (setq usb sb msb sb))
				((setq msb kx)))
			(if (< (- (elem-get master k) sb) 3) (setq k n_master))))
	(when ok
		(and (> (length counts) 1) (< (last counts) 3)
			(setq counts (slice counts 0 -2) starts (slice starts 0 -2)))
		(scatter sbr :patch_counts counts :patch_starts starts)))

(defun sbr-limiter (sbr)
	; (sbr-limiter sbr)
	;the bands of the limiter: of the low table and the borders of the
	;patches, those closer than its bands an octave merged, a patch's
	;border kept over another
	(defq low (. sbr :find :low) kx (. sbr :find :kx) bands (. sbr :find :limiter_bands))
	(cond
		((> bands 0)
			(defq warped (str-to-real (elem-get '("1.32715174233856803909" "1.18509277094158210129" "1.11987160404675912501") (dec bands)))
				borders (sbr-sums kx (. sbr :find :patch_counts))
				table (sbr-sorted (cat low (slice borders 1 -2))) out 0 in 1)
			;out is the last kept, in the next looked at, as ffmpeg walks them
			(defq lim (cap (length table) (nums)) n (dec (length table)))
			(push lim (first table))
			(while (<= in n)
				(defq a (last lim) b (elem-get table in))
				(cond
					((>= (n2r b) (* (n2r a) warped)) (push lim b))
					((or (= b a) (not (find b borders))))
					((not (find a borders)) (elem-set lim (dec (length lim)) b))
					((push lim b)))
				(setq in (inc in)))
			(. sbr :insert :limiter lim))
		((. sbr :insert :limiter (nums (first low) (last low))))))

(defun sbr-derived (sbr)
	; (sbr-derived sbr) -> :nil | :t
	;the high and low tables, the noise floor's and the limiter's, and the
	;patches, of the master table and where the high band begins
	(defq master (. sbr :find :master) high (slice master (. sbr :find :xover) -1)
		n_high (dec (length high)) n_low (>> (inc n_high) 1) kx (first high) m (- (last high) kx))
	(when (and (<= (+ kx m) 64) (<= kx 32))
		(defq odd (logand n_high 1)
			low (reduce (# (push %0 (elem-get high (- (* 2 %1) odd)))) (range 1 (inc n_low)) (nums kx))
			n_q (max 1 (sbr-round (* (n2r (. sbr :find :noise_bands)) (sbr-log2 (/ (n2r (. sbr :find :k2)) (n2r kx)))))))
		(when (<= n_q 5)
			(defq temp 0 noise (nums kx))
			(each (lambda (k)
					(setq temp (+ temp (/ (- n_low temp) (- (inc n_q) k))))
					(push noise (elem-get low temp)))
				(range 1 (inc n_q)))
			(scatter sbr :high high :low low :n (nums n_low n_high) :kx kx :m m :n_q n_q :noise noise)
			(when (sbr-patches sbr)
				(sbr-limiter sbr)
				(each (# (. %0 :insert :index_noise 0)) (. sbr :find :data))
				:t))))

;;;;;;;;;;;;;;;;;;;;;;
; the data of a frame
;;;;;;;;;;;;;;;;;;;;;;

(defun sbr-channel ()
	; (sbr-channel) -> data
	;what a channel keeps: of the frame before its grid and the last of its
	;envelope and noise, its filters and the states of its QMF banks
	(defq zeros (fill (reals) 64 (n2r 0)))
	(scatter (Emap) :num_env 0 :freq_res (fill (nums) 9 0) :t_env (fill (nums) 9 0) :t_q (fill (nums) 3 0)
		:num_noise 0 :frame_class 0 :amp_res 0 :df_env (fill (nums) 9 0) :df_noise (fill (nums) 2 0)
		:invf (list (fill (nums) 5 0) (fill (nums) 5 0)) :harmonic_flag 0 :harmonic (fill (nums) 48 0)
		:env_q (map (lambda (_) (fill (nums) 48 0)) (range 0 9)) :noise_q (map (lambda (_) (fill (nums) 5 0)) (range 0 3))
		:env (map (lambda (_) (fill (reals) 48 (n2r 0))) (range 0 9)) :noise_f (map (lambda (_) (fill (reals) 5 (n2r 0))) (range 0 3))
		:e_a (nums -1 -1) :t_env_old 0 :bw (fill (reals) 5 (n2r 0)) :index_noise 0 :index_sine 0
		:s_index (map (lambda (_) (fill (nums) 48 0)) (range 0 9))
		;the low band of the frame before, by band, and of the gains of its
		;last slots, the 32 bands of the analysis of each slot, and the high
		;band made of it, each slot's 64
		:w (map (lambda (_) (map (lambda (_) (list (fill (reals) 32 (n2r 0)) (fill (reals) 32 (n2r 0)))) (range 0 32))) (range 0 2))
		:y (map (lambda (_) (map (lambda (_) (list (cat zeros) (cat zeros))) (range 0 38))) (range 0 2)) :ypos 0
		:g_temp (map (lambda (_) (fill (reals) 48 (n2r 0))) (range 0 42)) :q_temp (map (lambda (_) (fill (reals) 48 (n2r 0))) (range 0 42))
		:analysis (fill (reals) 288 (n2r 0)) :synthesis (fill (reals) 1280 (n2r 0))))

(defun sbr-new (rate &optional ps)
	; (sbr-new rate [ps]) -> sbr
	;of an element of a core of a rate, the SBR at twice it, till its first
	;header only the banks; with ps, parametric stereo makes two of its one
	(sbr-tables)
	(scatter (Emap) :rate (* 2 rate) :start :nil :reset :nil :ready :nil :id -1 :coupling 0
		:start_freq -1 :stop_freq -1 :xover -1 :freq_scale -1 :alter_scale -1 :noise_bands -1
		:amp_res_header 0 :limiter_bands -1 :limiter_gains 2 :interpol_freq 1 :smoothing_mode 1
		:kx 32 :kx_old 32 :m 0 :m_old 0 :n (nums 0 0) :n_q 0 :pushed :nil
		:ps (if ps (ps-new))
		:data (list (sbr-channel) (sbr-channel))))

(defun sbr-off (sbr)
	; (sbr-off sbr)
	;back to the banks alone, as when a frame's data is wrong
	(scatter sbr :start :nil :ready :nil :kx 32 :m 0 :start_freq -1 :stop_freq -1 :xover -1
		:freq_scale -1 :alter_scale -1 :noise_bands -1)
	(each (# (elem-set (. %0 :find :e_a) 1 -1)) (. sbr :find :data)))

(defun sbr-huff (sbr in book)
	; (sbr-huff sbr reader book) -> value
	;a code of one of the 10 books, what it stands for
	(+ (bits-huff in (elem-get (. +sbr :find :tables) book)) (elem-get (. +sbr :find :least) book)))

(defun sbr-header (sbr in)
	; (sbr-header sbr reader)
	;a header: the spectrum, a reset if that is not as it was, and the
	;limiter, its bands made again if they are another number
	(defq old (map (# (. sbr :find %0)) '(:start_freq :stop_freq :xover :freq_scale :alter_scale :noise_bands))
		old_limiter (. sbr :find :limiter_bands))
	(scatter sbr :start :t :ready :nil :amp_res_header (bits-1 in)
		:start_freq (bits-get in 4) :stop_freq (bits-get in 4) :xover (bits-get in 3))
	(bits-skip in 2)
	(defq extra1 (bits-1 in) extra2 (bits-1 in))
	(if (= extra1 1)
		(scatter sbr :freq_scale (bits-get in 2) :alter_scale (bits-1 in) :noise_bands (bits-get in 2))
		(scatter sbr :freq_scale 2 :alter_scale 1 :noise_bands 2))
	(if (nql (str old) (str (map (# (. sbr :find %0)) '(:start_freq :stop_freq :xover :freq_scale :alter_scale :noise_bands))))
		(. sbr :insert :reset :t))
	(if (= extra2 1)
		(scatter sbr :limiter_bands (bits-get in 2) :limiter_gains (bits-get in 2) :interpol_freq (bits-1 in) :smoothing_mode (bits-1 in))
		(scatter sbr :limiter_bands 2 :limiter_gains 2 :interpol_freq 1 :smoothing_mode 1))
	(and (/= (. sbr :find :limiter_bands) old_limiter) (not (. sbr :find :reset)) (. sbr :find :low)
		(sbr-limiter sbr)))

(defun sbr-grid (sbr in ch)
	; (sbr-grid sbr reader data) -> :nil | :t
	;the envelopes in time of a frame of 16 slots, the borders of each and
	;of the noise floors, and which is the one a transient starts
	(defq freq_res (. ch :find :freq_res) t_env (. ch :find :t_env) old_env (. ch :find :num_env)
		trail 16 pointer 0 frame_class (bits-get in 2) num_env 0 ok :t)
	(elem-set freq_res 0 (elem-get freq_res old_env))
	(scatter ch :amp_res (. sbr :find :amp_res_header) :t_env_old (elem-get t_env old_env))
	(cond
		((= frame_class 0)
			(setq num_env (<< 1 (bits-get in 2)))
			(cond
				((> num_env 5) (setq ok :nil))
				(:t (if (= num_env 1) (. ch :insert :amp_res 0))
					(elem-set t_env 0 0)
					(elem-set t_env num_env trail)
					(defq step (/ (+ trail (>> num_env 1)) num_env))
					(each (# (elem-set t_env %0 (* %0 step))) (range 1 num_env))
					(defq res (bits-1 in))
					(each (# (elem-set freq_res %0 res)) (range 1 (inc num_env))))))
		((= frame_class 1)
			(setq trail (+ trail (bits-get in 2)))
			(defq rel (bits-get in 2))
			(setq num_env (inc rel))
			(elem-set t_env 0 0)
			(elem-set t_env num_env trail)
			(each (# (elem-set t_env (- num_env 1 %0) (- (elem-get t_env (- num_env %0)) (* 2 (bits-get in 2)) 2))) (range 0 rel))
			(setq pointer (bits-get in (elem-get '(0 1 2 2 3 3) num_env)))
			(each (# (elem-set freq_res (- num_env %0) (bits-1 in))) (range 0 num_env)))
		((= frame_class 2)
			(elem-set t_env 0 (bits-get in 2))
			(defq rel (bits-get in 2))
			(setq num_env (inc rel))
			(elem-set t_env num_env trail)
			(each (# (elem-set t_env (inc %0) (+ (elem-get t_env %0) (* 2 (bits-get in 2)) 2))) (range 0 rel))
			(setq pointer (bits-get in (elem-get '(0 1 2 2 3 3) num_env)))
			(each (# (elem-set freq_res %0 (bits-1 in))) (range 1 (inc num_env))))
		(:t (elem-set t_env 0 (bits-get in 2))
			(setq trail (+ trail (bits-get in 2)))
			(defq lead (bits-get in 2) rel (bits-get in 2))
			(setq num_env (+ lead rel 1))
			(cond
				((> num_env 5) (setq ok :nil))
				(:t (elem-set t_env num_env trail)
					(each (# (elem-set t_env (inc %0) (+ (elem-get t_env %0) (* 2 (bits-get in 2)) 2))) (range 0 lead))
					(each (# (elem-set t_env (- num_env 1 %0) (- (elem-get t_env (- num_env %0)) (* 2 (bits-get in 2)) 2))) (range 0 rel))
					(setq pointer (bits-get in (elem-get '(0 1 2 2 3 3) num_env)))
					(each (# (elem-set freq_res %0 (bits-1 in))) (range 1 (inc num_env)))))))
	(and ok (<= pointer (inc num_env))
		(every (# (< (elem-get t_env %0) (elem-get t_env (inc %0)))) (range 0 num_env))
		(progn
			(defq num_noise (if (> num_env 1) 2 1) t_q (. ch :find :t_q) e_a (. ch :find :e_a))
			(scatter ch :num_env num_env :frame_class frame_class :num_noise num_noise)
			(elem-set t_q 0 (elem-get t_env 0))
			(elem-set t_q num_noise (elem-get t_env num_env))
			(when (> num_noise 1)
				(elem-set t_q 1 (elem-get t_env (cond
					((= frame_class 0) (>> num_env 1))
					((= (logand frame_class 1) 1) (- num_env (max (dec pointer) 1)))
					((= pointer 0) 1)
					((= pointer 1) (dec num_env))
					((dec pointer))))))
			(elem-set e_a 0 (if (/= (elem-get e_a 1) old_env) -1 0))
			(elem-set e_a 1 (cond
				((and (= (logand frame_class 1) 1) (> pointer 0)) (- (inc num_env) pointer))
				((and (= frame_class 2) (> pointer 1)) (dec pointer))
				(-1)))
			:t)))

(defun sbr-grid-copy (to from)
	; (sbr-grid-copy to from)
	;the second channel of a coupled pair has the first's grid, its own of
	;the frame before kept
	(defq freq_res (. to :find :freq_res) e_a (. to :find :e_a))
	(elem-set freq_res 0 (elem-get freq_res (. to :find :num_env)))
	(. to :insert :t_env_old (elem-get (. to :find :t_env) (. to :find :num_env)))
	(elem-set e_a 0 (if (/= (elem-get e_a 1) (. to :find :num_env)) -1 0))
	(each (# (elem-set freq_res %0 (elem-get (. from :find :freq_res) %0))) (range 1 9))
	(scatter to :t_env (cat (. from :find :t_env)) :t_q (cat (. from :find :t_q)) :num_env (. from :find :num_env)
		:amp_res (. from :find :amp_res) :num_noise (. from :find :num_noise) :frame_class (. from :find :frame_class))
	(elem-set e_a 1 (elem-get (. from :find :e_a) 1)))

(defun sbr-dtdf (in ch)
	; (sbr-dtdf reader data)
	;each envelope and noise floor coded in time or in frequency
	(defq df_env (. ch :find :df_env) df_noise (. ch :find :df_noise))
	(each (# (elem-set df_env %0 (bits-1 in))) (range 0 (. ch :find :num_env)))
	(each (# (elem-set df_noise %0 (bits-1 in))) (range 0 (. ch :find :num_noise))))

(defun sbr-invf (sbr in ch)
	; (sbr-invf sbr reader data)
	;how hard each noise band is filtered, the last kept
	(defq invf (. ch :find :invf))
	(elem-set invf 1 (cat (first invf)))
	(each (# (elem-set (first invf) %0 (bits-get in 2))) (range 0 (. sbr :find :n_q))))

(defun sbr-envelope (sbr in ch c)
	; (sbr-envelope sbr reader data channel) -> :nil | :t
	;each envelope's scale factors, the first of a channel coded in
	;frequency from a start, else each from the one before it, in time
	;from the last envelope, its table of the other resolution mapped
	(defq coupled (and (= (. sbr :find :coupling) 1) (= c 1)) delta (if coupled 2 1) odd (logand (second (. sbr :find :n)) 1)
		amp (. ch :find :amp_res) bits (if coupled (if (= amp 1) 5 6) (if (= amp 1) 6 7))
		t_book (if coupled (if (= amp 1) 6 2) (if (= amp 1) 4 0)) f_book (inc t_book)
		env_q (. ch :find :env_q) freq_res (. ch :find :freq_res) n (. sbr :find :n) ok :t)
	(each (lambda (i)
			(defq to (elem-get env_q (inc i)) from (elem-get env_q i) res (elem-get freq_res (inc i))
				bands (elem-get n res))
			(cond
				((= (elem-get (. ch :find :df_env) i) 1)
					(defq res_before (elem-get freq_res i))
					(each (lambda (j)
							(defq k (cond
									((= res res_before) j)
									((= res 1) (>> (+ j odd) 1))
									((= j 0) 0)
									((- (* 2 j) odd))))
							(elem-set to j (+ (elem-get from k) (* delta (sbr-huff sbr in t_book)))))
						(range 0 bands)))
				(:t (elem-set to 0 (* delta (bits-get in bits)))
					(each (# (elem-set to %0 (+ (elem-get to (dec %0)) (* delta (sbr-huff sbr in f_book))))) (range 1 bands))))
			(if (some! (# (not (<= 0 %0 127))) (list to) :nil 0 bands) (setq ok :nil)))
		(range 0 (. ch :find :num_env)))
	(elem-set env_q 0 (cat (elem-get env_q (. ch :find :num_env))))
	ok)

(defun sbr-noise (sbr in ch c)
	; (sbr-noise sbr reader data channel) -> :nil | :t
	;each noise floor's levels, as the envelopes are
	(defq coupled (and (= (. sbr :find :coupling) 1) (= c 1)) delta (if coupled 2 1)
		t_book (if coupled 9 8) f_book (if coupled 7 5) noise_q (. ch :find :noise_q) n_q (. sbr :find :n_q) ok :t)
	(each (lambda (i)
			(defq to (elem-get noise_q (inc i)) from (elem-get noise_q i))
			(cond
				((= (elem-get (. ch :find :df_noise) i) 1)
					(each (# (elem-set to %0 (+ (elem-get from %0) (* delta (sbr-huff sbr in t_book))))) (range 0 n_q)))
				(:t (elem-set to 0 (* delta (bits-get in 5)))
					(each (# (elem-set to %0 (+ (elem-get to (dec %0)) (* delta (sbr-huff sbr in f_book))))) (range 1 n_q))))
			(if (some! (# (not (<= 0 %0 30))) (list to) :nil 0 n_q) (setq ok :nil)))
		(range 0 (. ch :find :num_noise)))
	(elem-set noise_q 0 (cat (elem-get noise_q (. ch :find :num_noise))))
	ok)

(defun sbr-harmonics (sbr in ch)
	; (sbr-harmonics sbr reader data)
	;which high bands have a sinusoid added
	(. ch :insert :harmonic_flag (bits-1 in))
	(when (= (. ch :find :harmonic_flag) 1)
		(defq harmonic (. ch :find :harmonic))
		(each (# (elem-set harmonic %0 (bits-1 in))) (range 0 (second (. sbr :find :n))))))

(defun sbr-data (sbr in kind)
	; (sbr-data sbr reader kind) -> :nil | :t
	;a frame's data of a channel or a pair, coupled or not, and its
	;extensions stepped over
	(bind '(left right) (. sbr :find :data))
	(. sbr :insert :id kind)
	(and (cond
			((= kind 0)
				(if (= (bits-1 in) 1) (bits-skip in 4))
				(and (sbr-grid sbr in left)
					(progn (sbr-dtdf in left) (sbr-invf sbr in left) :t)
					(sbr-envelope sbr in left 0) (sbr-noise sbr in left 0)
					(progn (sbr-harmonics sbr in left) :t)))
			((= kind 1)
				(if (= (bits-1 in) 1) (bits-skip in 8))
				(cond
					((progn (. sbr :insert :coupling (bits-1 in)) (= (. sbr :find :coupling) 1))
						(and (sbr-grid sbr in left)
							(progn (sbr-grid-copy right left) (sbr-dtdf in left) (sbr-dtdf in right) (sbr-invf sbr in left)
								(defq invf (. right :find :invf))
								(elem-set invf 1 (cat (first invf)))
								(elem-set invf 0 (cat (first (. left :find :invf))))
								:t)
							(sbr-envelope sbr in left 0) (sbr-noise sbr in left 0)
							(sbr-envelope sbr in right 1) (sbr-noise sbr in right 1)
							(progn (sbr-harmonics sbr in left) (sbr-harmonics sbr in right) :t)))
					((and (sbr-grid sbr in left) (sbr-grid sbr in right)
						(progn (sbr-dtdf in left) (sbr-dtdf in right) (sbr-invf sbr in left) (sbr-invf sbr in right) :t)
						(sbr-envelope sbr in left 0) (sbr-envelope sbr in right 1)
						(sbr-noise sbr in left 0) (sbr-noise sbr in right 1)
						(progn (sbr-harmonics sbr in left) (sbr-harmonics sbr in right) :t))))))
		(progn
			;the extensions, each its id of 2 bits, parametric stereo's 2, any
			;other the rest stepped over
			(when (= (bits-1 in) 1)
				(defq size (bits-get in 4))
				(if (= size 15) (setq size (+ size (bits-get in 8))))
				(defq bits_left (* 8 size))
				(while (> bits_left 7)
					(defq id (bits-get in 2))
					(setq bits_left (- bits_left 2))
					(cond
						((and (= id 2) (. sbr :find :ps)) (setq bits_left (- bits_left (ps-read (. sbr :find :ps) in bits_left))))
						(:t (bits-skip in bits_left) (setq bits_left 0))))
				(if (> bits_left 0) (bits-skip in bits_left)))
			:t)))

(defun sbr-read (sbr in crc bytes kind)
	; (sbr-read sbr reader crc bytes kind) -> bits
	;the SBR of a fill element of bytes, the reader past its 4 bits of
	;type, of the element of a kind before it, 0 a channel, 1 a pair: its
	;header if it has one, the tables made again if that says, and its
	;data. How many bits it took, a wrong frame turns SBR off
	(defq from (bits-at in))
	(. sbr :insert :reset :nil)
	(if crc (bits-skip in 10))
	(scatter sbr :kx_old (. sbr :find :kx) :m_old (. sbr :find :m) :pushed :t)
	(if (= (bits-1 in) 1) (sbr-header sbr in))
	(and (. sbr :find :reset)
		(not (and (sbr-master sbr) (sbr-derived sbr)))
		(sbr-off sbr))
	(when (. sbr :find :start)
		(. sbr :insert :ready :t)
		(unless (sbr-data sbr in kind) (sbr-off sbr)))
	(if (> (- (bits-at in) from) (- (* 8 bytes) 4)) (sbr-off sbr))
	(- (bits-at in) from))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the high band made of the low
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sbr-exp2 (x)
	(pow (n2r 2) (n2r x)))

(defun sbr-dequant (sbr)
	; (sbr-dequant sbr)
	;the envelopes and noise floors as levels: of a coupled pair the first
	;the level and the second the balance, made each side's
	(bind '(left right) (. sbr :find :data))
	(defq n (. sbr :find :n) n_q (. sbr :find :n_q) sqrt2 (sqrt (n2r 2)) one (n2r 1) big (str-to-real "1e20")
		level (lambda (q add amp)
			(if (= amp 1) (sbr-exp2 (+ q add))
				(* (sbr-exp2 (+ (>>> q 1) add)) (if (= (logand q 1) 1) sqrt2 one)))))
	(cond
		((and (= (. sbr :find :id) 1) (= (. sbr :find :coupling) 1))
			(defq amp (. left :find :amp_res) pan (if (= amp 1) 12 24))
			(each (lambda (e)
					(defq q0 (elem-get (. left :find :env_q) e) q1 (elem-get (. right :find :env_q) e)
						f0 (elem-get (. left :find :env) e) f1 (elem-get (. right :find :env) e))
					(each (lambda (k)
							(defq t1 (level (elem-get q0 k) 7 amp) t2 (level (- pan (elem-get q1 k)) 0 amp))
							(if (> t1 big) (setq t1 one))
							(defq fac (/ t1 (+ one t2)))
							(elem-set f0 k fac)
							(elem-set f1 k (* fac t2)))
						(range 0 (elem-get n (elem-get (. left :find :freq_res) e)))))
				(range 1 (inc (. left :find :num_env))))
			(each (lambda (e)
					(defq q0 (elem-get (. left :find :noise_q) e) q1 (elem-get (. right :find :noise_q) e)
						f0 (elem-get (. left :find :noise_f) e) f1 (elem-get (. right :find :noise_f) e))
					(each (lambda (k)
							(defq t1 (sbr-exp2 (- 7 (elem-get q0 k))) t2 (sbr-exp2 (- 12 (elem-get q1 k))) fac (/ t1 (+ one t2)))
							(elem-set f0 k fac)
							(elem-set f1 k (* fac t2)))
						(range 0 n_q)))
				(range 1 (inc (. left :find :num_noise)))))
		(:t (each! (lambda (ch)
					(defq amp (. ch :find :amp_res))
					(each (lambda (e)
							(defq q (elem-get (. ch :find :env_q) e) f (elem-get (. ch :find :env) e))
							(each (lambda (k)
									(defq v (level (elem-get q k) 6 amp))
									(elem-set f k (if (> v big) one v)))
								(range 0 (elem-get n (elem-get (. ch :find :freq_res) e)))))
						(range 1 (inc (. ch :find :num_env))))
					(each (lambda (e)
							(defq q (elem-get (. ch :find :noise_q) e) f (elem-get (. ch :find :noise_f) e))
							(each (# (elem-set f %0 (sbr-exp2 (- 6 (elem-get q %0))))) (range 0 n_q)))
						(range 1 (inc (. ch :find :num_noise)))))
				(list (. sbr :find :data)) 0 (inc (. sbr :find :id))))))

(defun sbr-analysis (ch samples)
	; (sbr-analysis data samples) -> ((re im) ...)
	;the 32 slots of 32 bands of 1024 samples: the newest 320 by the
	;window, its 5 parts of 64 summed, by the cos and sin of each band
	(defq x (cat (. ch :find :analysis) samples) win (. +sbr :find :window_ds)
		ana_re (. +sbr :find :ana_re) ana_im (. +sbr :find :ana_im))
	;the 288 newest kept for the next
	(. ch :insert :analysis (slice x 1024 1312))
	(map (lambda (l)
			;the window by the samples newest first
			(defq z (nums-mul win (slice x (+ (* 32 l) 320) (* 32 l)))
				u (nums-add (nums-add (nums-add (slice z 0 64) (slice z 64 128)) (nums-add (slice z 128 192) (slice z 192 256))) (slice z 256 320)))
			(list (sbr-matvec ana_re 32 64 u 0 (fill (reals) 32 (n2r 0)))
				(sbr-matvec ana_im 32 64 u 0 (fill (reals) 32 (n2r 0)))))
		(range 0 32)))

(defun sbr-low (sbr ch)
	; (sbr-low sbr data) -> ((re im) ...)
	;the low band, each of 32 bands its 40 slots: 8 of the frame before and
	;the 32 of this one, below kx and kx before
	(defq w (. ch :find :w) now (elem-get w (. ch :find :ypos)) before (slice (elem-get w (logxor (. ch :find :ypos) 1)) 24 32)
		kx (. sbr :find :kx) kx_old (. sbr :find :kx_old) zero (n2r 0)
		now_r (map (const first) now) now_i (map (const second) now)
		before_r (map (const first) before) before_i (map (const second) before)
		;band k of the slots, those of the frame before or none
		column (lambda (slots k n on) (if on (sbr-gather (fill (reals) n zero) slots k) (fill (reals) n zero))))
	(map (lambda (k)
			(list (cat (column before_r k 8 (< k kx_old)) (column now_r k 32 (< k kx)))
				(cat (column before_i k 8 (< k kx_old)) (column now_i k 32 (< k kx)))))
		(range 0 32)))

(defun sbr-inverse (sbr low)
	; (sbr-inverse sbr low) -> (alpha0 alpha1)
	;of each band below k0 the two terms of its prediction, by its
	;autocorrelation, each (re im), none of a band too tonal to predict
	(defq zero (n2r 0) sixteen (n2r 16) k (str-to-real "1.000001"))
	(map! (lambda ((r i))
			(defq r0 (slice r 0 38) i0 (slice i 0 38) r1 (slice r 1 39) i1 (slice i 1 39) r2 (slice r 2 40) i2 (slice i 2 40)
				rm (slice r 1 38) im (slice i 1 38)
				;phi of lag 2, of lag 1 from 0 and from 1, of lag 0 from 0 and from 1
				p01r (+ (nums-dot r0 r2) (nums-dot i0 i2)) p01i (- (nums-dot r0 i2) (nums-dot i0 r2))
				p11r (+ (nums-dot r0 r1) (nums-dot i0 i1)) p11i (- (nums-dot r0 i1) (nums-dot i0 r1))
				p00r (+ (nums-dot r1 (slice r 2 40)) (nums-dot i1 (slice i 2 40))) p00i (- (nums-dot r1 (slice i 2 40)) (nums-dot i1 (slice r 2 40)))
				sum0 (+ (nums-dot rm rm) (nums-dot im im))
				p21 (+ sum0 (* (elem-get r 0) (elem-get r 0)) (* (elem-get i 0) (elem-get i 0)))
				p10 (+ sum0 (* (elem-get r 38) (elem-get r 38)) (* (elem-get i 38) (elem-get i 38)))
				dk (- (* p21 p10) (/ (+ (* p11r p11r) (* p11i p11i)) k))
				a1r zero a1i zero a0r zero a0i zero)
			(when (/= dk zero)
				(setq a1r (/ (- (* p00r p11r) (* p00i p11i) (* p01r p10)) dk)
					a1i (/ (- (+ (* p00r p11i) (* p00i p11r)) (* p01i p10)) dk)))
			(when (/= p10 zero)
				(setq a0r (neg (/ (+ p00r (* a1r p11r) (* a1i p11i)) p10))
					a0i (neg (/ (- (+ p00i (* a1i p11r)) (* a1r p11i)) p10))))
			(if (or (>= (+ (* a1r a1r) (* a1i a1i)) sixteen) (>= (+ (* a0r a0r) (* a0i a0i)) sixteen))
				(list zero zero zero zero)
				(list a0r a0i a1r a1i)))
		(list low) 0 (. sbr :find :k0)))

(defun sbr-chirp (sbr ch)
	; (sbr-chirp sbr data)
	;each noise band's chirp, of how hard it is to be filtered, now and in
	;the frame before, smoothed
	(defq bw (. ch :find :bw) invf (. ch :find :invf) tab (. +sbr :find :bw))
	(each (lambda (i)
			(defq mode (elem-get (first invf) i) new (if (= (+ mode (elem-get (second invf) i)) 1) (str-to-real "0.6") (elem-get tab mode))
				was (elem-get bw i))
			(setq new (if (< new was)
				(+ (* (n2r 0.75) new) (* (n2r 0.25) was))
				(+ (* (str-to-real "0.90625") new) (* (str-to-real "0.09375") was))))
			(elem-set bw i (if (< new (str-to-real "0.015625")) (n2r 0) new)))
		(range 0 (. sbr :find :n_q))))

(defun sbr-high (sbr ch low alphas)
	; (sbr-high sbr data low alphas) -> ((re im) ...) | :nil
	;the high band, the bands above kx each a band of a patch filtered by
	;its prediction and its noise band's chirp, over the slots of the
	;envelopes
	(defq kx (. sbr :find :kx) noise (. sbr :find :noise) n_q (. sbr :find :n_q) t_env (. ch :find :t_env)
		from (+ 2 (* 2 (first t_env))) to (+ 2 (* 2 (elem-get t_env (. ch :find :num_env)))) bw (. ch :find :bw)
		high (map (lambda (_) :nil) (range 0 64)) k kx g 0 zeros (. +sbr :find :zeros40) ok :t)
	(each (lambda (count start)
			(each (lambda (x)
					(defq p (+ start x))
					(while (and (<= g n_q) (>= k (elem-get noise g))) (setq g (inc g)))
					(setq g (dec g))
					(if (< g 0) (setq ok :nil))
					(when ok
						(bind '(a0r a0i a1r a1i) (elem-get alphas p))
						(bind '(r i) (elem-get low p))
						(defq b (elem-get bw g) bb (* b b) c0 (* a1r bb) c1 (* a1i bb) c2 (* a0r b) c3 (* a0i b)
							r0 (slice r (- from 2) (- to 2)) i0 (slice i (- from 2) (- to 2))
							r1 (slice r (- from 1) (- to 1)) i1 (slice i (- from 1) (- to 1))
							re (nums-add (slice r from to) (nums-add (nums-sub (nums-scale r0 c0) (nums-scale i0 c1)) (nums-sub (nums-scale r1 c2) (nums-scale i1 c3))))
							im (nums-add (slice i from to) (nums-add (nums-add (nums-scale i0 c0) (nums-scale r0 c1)) (nums-add (nums-scale i1 c2) (nums-scale r1 c3)))))
						(elem-set high k (list (cat (slice zeros 0 from) re (slice zeros to 40)) (cat (slice zeros 0 from) im (slice zeros to 40)))))
					(setq k (inc k)))
				(range 0 count)))
		(. sbr :find :patch_counts) (. sbr :find :patch_starts))
	(when ok
		(each (# (elem-set high %0 (list zeros zeros))) (range k (+ kx (. sbr :find :m))))
		high))

(defun sbr-mapping (sbr ch)
	; (sbr-mapping sbr data) -> :nil | ((orig noise sines) ...)
	;of each envelope, each band of the high band its level, its noise
	;floor's, and if a sinusoid is in its band, of those added; which are
	;added, of the frame before too, kept
	(defq kx (. sbr :find :kx) n (. sbr :find :n) high (. sbr :find :high) low (. sbr :find :low)
		noise (. sbr :find :noise) n_q (. sbr :find :n_q) num_env (. ch :find :num_env)
		freq_res (. ch :find :freq_res) t_env (. ch :find :t_env) t_q (. ch :find :t_q)
		s_index (. ch :find :s_index) e_a (. ch :find :e_a) harmonic (. ch :find :harmonic))
	(each (# (elem-set s_index %0 (fill (nums) 48 0))) (range 1 8))
	(when (and (= (first high) kx) (= (first low) kx))
		(defq maps (map (lambda (e)
				(defq res (elem-get freq_res (inc e)) table (if (= res 1) high low)
					orig (fill (reals) 48 (n2r 0)) q (fill (reals) 48 (n2r 0)) s (fill (nums) 48 0)
					env (elem-get (. ch :find :env) (inc e)) si (elem-get s_index (inc e))
					nf (elem-get (. ch :find :noise_f) (if (and (> (. ch :find :num_noise) 1) (>= (elem-get t_env e) (elem-get t_q 1))) 2 1)))
				(each (lambda (i)
						(defq v (elem-get env i))
						(each (# (elem-set orig (- %0 kx) v)) (range (elem-get table i) (elem-get table (inc i)))))
					(range 0 (elem-get n res)))
				(each (lambda (i)
						(defq v (elem-get nf i))
						(each (# (elem-set q (- %0 kx) v)) (range (elem-get noise i) (elem-get noise (inc i)))))
					(range 0 n_q))
				(when (= (. ch :find :harmonic_flag) 1)
					(defq before (first s_index))
					(each (lambda (i)
							(defq mid (- (>> (+ (elem-get high i) (elem-get high (inc i))) 1) kx))
							(elem-set si mid (if (and (= (elem-get harmonic i) 1) (or (>= e (second e_a)) (= (elem-get before mid) 1))) 1 0)))
						(range 0 (second n))))
				(each (lambda (i)
						(defq a (- (elem-get table i) kx) b (- (elem-get table (inc i)) kx)
							present (if (some! (# (/= %0 0)) (list si) :nil a b) 1 0))
						(each (# (elem-set s %0 present)) (range a b)))
					(range 0 (elem-get n res)))
				(list orig q s))
			(range 0 num_env)))
		(elem-set s_index 0 (cat (elem-get s_index num_env)))
		maps))

(defun sbr-estimate (sbr ch high)
	; (sbr-estimate sbr data high) -> (reals ...)
	;of each envelope the energy of the high band made, each band's, or
	;over each band of its table, by the slots of the envelope
	(defq kx (. sbr :find :kx) m (. sbr :find :m) t_env (. ch :find :t_env) n (. sbr :find :n)
		energy (lambda (k a b) (bind '(r i) (elem-get high k)) (defq r (slice r a b) i (slice i a b)) (+ (nums-dot r r) (nums-dot i i))))
	(map (lambda (e)
			(defq a (+ 2 (* 2 (elem-get t_env e))) b (+ 2 (* 2 (elem-get t_env (inc e)))) out (fill (reals) 48 (n2r 0)))
			(cond
				((= (. sbr :find :interpol_freq) 1)
					(defq k (/ (n2r 1) (n2r (- b a))))
					(each (# (elem-set out %0 (* (energy (+ kx %0) a b) k))) (range 0 m)))
				(:t (defq res (elem-get (. ch :find :freq_res) (inc e)) table (if (= res 1) (. sbr :find :high) (. sbr :find :low)))
					(each (lambda (p)
							(defq lo (elem-get table p) hi (elem-get table (inc p))
								sum (/ (reduce (# (+ %0 (energy %1 a b))) (range lo hi) (n2r 0)) (n2r (* (- b a) (- hi lo)))))
							(each (# (elem-set out (- %0 kx) sum)) (range lo hi)))
						(range 0 (elem-get n res)))))
			out)
		(range 0 (. ch :find :num_env))))

(defun sbr-gains (sbr ch maps curr)
	; (sbr-gains sbr data maps energies) -> ((gain noise sine) ...)
	;of each envelope each band's gain, noise and sinusoid, each limiter
	;band's gains kept to its limit and its energy made what was sent,
	;native, lib/media/lisp.vp media_sbr_gains
	(defq kx (. sbr :find :kx) lim (. sbr :find :limiter) e_a (. ch :find :e_a) s_index (. ch :find :s_index)
		zero (n2r 0) consts (reals (n2r 1) (. +sbr :find :flt_eps) (. +sbr :find :flt_min) (n2r 100000)
			(str-to-real "1.584893192") (elem-get (. +sbr :find :limgain) (. sbr :find :limiter_gains))))
	(map (lambda ((orig q s) e_curr)
			(defq e (!) delta (if (or (= e (second e_a)) (= e (first e_a))) 0 1)
				out (sbr-envelope-gains (cat orig q e_curr) (cat s (elem-get s_index (inc e))) lim kx delta consts
					(fill (reals) 144 zero)))
			(list (slice out 0 48) (slice out 48 96) (slice out 96 144)))
		maps curr))

(defun sbr-assemble (sbr ch high gains)
	; (sbr-assemble sbr data high gains)
	;the high band of each slot, its gains smoothed over the slots before
	;unless at a transient, its noise, or its sinusoid, in the slots of
	;this frame's Y: each slot's bands of the high band made a vector by
	;the gather, and all by vectors of the bands
	(defq kx (. sbr :find :kx) m_max (. sbr :find :m) t_env (. ch :find :t_env) e_a (. ch :find :e_a)
		h_sl (if (= (. sbr :find :smoothing_mode) 0) 4 0) g_temp (. ch :find :g_temp) q_temp (. ch :find :q_temp)
		y (elem-get (. ch :find :y) (. ch :find :ypos)) smooth (. +sbr :find :smooth)
		noise_re (. +sbr :find :noise_re) noise_im (. +sbr :find :noise_im)
		alt (slice (. +sbr :find :alt) 0 m_max) ones (slice (. +sbr :find :ones) 0 m_max)
		high_r (map! (const first) (list high) kx (+ kx m_max)) high_i (map! (const second) (list high) kx (+ kx m_max))
		index_noise (. ch :find :index_noise) index_sine (. ch :find :index_sine) t0 (* 2 (first t_env))
		zero (n2r 0) one (n2r 1) top (+ kx m_max))
	(cond
		((. sbr :find :reset)
			(each (lambda (i)
					(elem-set g_temp (+ i t0) (cat (first (first gains))))
					(elem-set q_temp (+ i t0) (cat (second (first gains)))))
				(range 0 h_sl)))
		((> h_sl 0)
			(defq old (* 2 (. ch :find :t_env_old)))
			(each (lambda (i)
					(elem-set g_temp (+ i t0) (cat (elem-get g_temp (+ i old))))
					(elem-set q_temp (+ i t0) (cat (elem-get q_temp (+ i old)))))
				(range 0 4))))
	(each (lambda ((gain q_m _))
			(defq e (!))
			(each (lambda (i)
					(elem-set g_temp (+ h_sl i) gain)
					(elem-set q_temp (+ h_sl i) q_m))
				(range (* 2 (elem-get t_env e)) (* 2 (elem-get t_env (inc e))))))
		gains)
	(each (lambda ((_ _ s_m))
			(defq e (!) edge (or (= e (first e_a)) (= e (second e_a))) sm (slice s_m 0 m_max)
				;1 where there is no sinusoid, the noise's place
				quiet (reduce! (# (push %0 (if (= %1 zero) one zero))) (list s_m) (cap m_max (reals)) 0 m_max))
			(each (lambda (i)
					(defq g_filt (elem-get g_temp (+ i h_sl)) q_filt (elem-get q_temp i))
					(when (and (> h_sl 0) (not edge))
						(defq at (+ i h_sl))
						(setq g_filt (reduce (# (nums-add %0 (nums-scale (elem-get g_temp (- at %1)) (elem-get smooth %1)))) (range 0 5) (fill (reals) 48 zero))
							q_filt (reduce (# (nums-add %0 (nums-scale (elem-get q_temp (- at %1)) (elem-get smooth %1)))) (range 0 5) (fill (reals) 48 zero))))
					(defq g (slice g_filt 0 m_max)
						yr (nums-mul (sbr-gather (fill (reals) m_max zero) high_r (+ i 2)) g)
						yi (nums-mul (sbr-gather (fill (reals) m_max zero) high_i (+ i 2)) g))
					(cond
						((not edge)
							;noise where there is no sinusoid, which goes round the
							;four phases by the slot, the odd bands' of the other sign
							(defq from (logand (inc index_noise) 0x1ff) q (nums-mul (slice q_filt 0 m_max) quiet)
								sign1 (* (elem-get '(0 1 0 -1) index_sine) (- 1 (* 2 (logand kx 1)))))
							(setq yr (nums-add (nums-add yr (nums-mul q (slice noise_re from (+ from m_max))))
									(nums-scale sm (n2r (elem-get '(1 0 -1 0) index_sine))))
								yi (nums-add (nums-add yi (nums-mul q (slice noise_im from (+ from m_max))))
									(nums-scale (nums-mul sm alt) (n2r sign1)))))
						(:t ;at a transient the sinusoids alone, on re or im by the phase
							(defq a (- 1 (logand (+ index_sine (logand kx 1)) 2))
								add (nums-scale (nums-mul sm (if (= (logand index_sine 1) 1) alt ones)) (n2r a)))
							(if (= (logand index_sine 1) 1) (setq yi (nums-add yi add)) (setq yr (nums-add yr add)))))
					(bind '(old_r old_i) (elem-get y i))
					(elem-set y i (list (cat (slice old_r 0 kx) yr (slice old_r top 64)) (cat (slice old_i 0 kx) yi (slice old_i top 64))))
					(setq index_noise (logand (+ index_noise m_max) 0x1ff) index_sine (logand (inc index_sine) 3)))
				(range (* 2 (elem-get t_env e)) (* 2 (elem-get t_env (inc e))))))
		gains)
	(scatter ch :index_noise index_noise :index_sine index_sine))

(defun sbr-x (sbr ch low)
	; (sbr-x sbr data low) -> ((re im) ...)
	;the 38 slots of 64 bands, 32 to the synthesis and the 6 after to the
	;hybrid filters of PS: the low band, and the high band made, of the
	;frame before for the slots its last envelope runs into this one
	(defq kx (. sbr :find :kx) m (. sbr :find :m) kx_old (. sbr :find :kx_old) m_old (. sbr :find :m_old)
		ypos (. ch :find :ypos) y1 (elem-get (. ch :find :y) ypos) y0 (elem-get (. ch :find :y) (logxor ypos 1))
		i_temp (max (- (* 2 (. ch :find :t_env_old)) 32) 0) zeros (. +sbr :find :zeros64)
		low_r (map (const first) low) low_i (map (const second) low) zero (n2r 0))
	;each slot the low band's slot i + 2 below top, the high band made above
	;it, 0 above that
	(map (lambda (i)
			(defq old (< i i_temp) top (if old kx_old kx) span (if (or old (< i 32)) (if old m_old m) 0)
				from (if old (elem-get y0 (+ i 32)) (elem-get y1 (min i 31))) rest (- 64 top span))
			(list (cat (sbr-gather (fill (reals) top zero) (slice low_r 0 top) (+ i 2)) (slice (first from) top (+ top span)) (slice zeros 0 rest))
				(cat (sbr-gather (fill (reals) top zero) (slice low_i 0 top) (+ i 2)) (slice (second from) top (+ top span)) (slice zeros 0 rest))))
		(range 0 38)))

(defun sbr-synthesis (ch slots)
	; (sbr-synthesis data slots) -> reals
	;the 2048 samples of the 32 slots of 64 bands: each slot's 128 of its
	;bands newest in v, its 10 parts by the window, summed. Of its cos by
	;the re a and its sin by the im b, v n is a - b, v 127 - n is -a - b
	;the window's sum native, lib/media/lisp.vp media_window, as MP3's
	(defq syn_c (. +sbr :find :syn_c) syn_s (. +sbr :find :syn_s) win (. +sbr :find :window)
		v (. ch :find :synthesis) zero (n2r 0) out (fill (reals) 2048 zero) one (n2r 1))
	(each! (lambda ((re im))
			(defq a (sbr-matvec syn_c 64 64 re 0 (fill (reals) 64 zero)) b (sbr-matvec syn_s 64 64 im 0 (fill (reals) 64 zero)))
			(setq v (cat (nums-sub a b) (slice (nums-sub (nums-scale a (n2r -1)) b) 64 0) (slice v 0 1152)))
			(sbr-window v win out (* 64 (!)) one 64 5 128 256 64 192))
		(list slots) 0 32)
	(. ch :insert :synthesis v)
	out)

(defun sbr-apply (sbr kind chans &optional alone)
	; (sbr-apply sbr kind chans [alone]) -> (reals ...)
	;the 2048 samples of each of an element's channels of 1024, its kind
	;0 a channel or 1 a pair: each to the QMF bank's bands, its high band
	;made by the data read, and to samples at twice the rate; with PS, of
	;a channel alone in its block, the two of it
	(if (/= kind (. sbr :find :id)) (sbr-off sbr))
	(and (. sbr :find :start) (not (. sbr :find :ready)) (sbr-off sbr))
	(if (. sbr :find :pushed)
		(. sbr :insert :pushed :nil)
		(scatter sbr :kx_old (. sbr :find :kx) :m_old (. sbr :find :m)))
	(when (. sbr :find :start)
		(sbr-dequant sbr)
		(. sbr :insert :ready :nil))
	(defq data (. sbr :find :data) ps (. sbr :find :ps))
	(defq xs (map (lambda (samples ch)
			(defq ypos (. ch :find :ypos))
			(elem-set (. ch :find :w) ypos (sbr-analysis ch samples))
			(defq low (sbr-low sbr ch))
			(. ch :insert :ypos (logxor ypos 1))
			(when (. sbr :find :start)
				(defq alphas (sbr-inverse sbr low))
				(sbr-chirp sbr ch)
				(cond
					((not (defq high (sbr-high sbr ch low alphas))))
					((defq maps (sbr-mapping sbr ch))
						(sbr-assemble sbr ch high (sbr-gains sbr ch maps (sbr-estimate sbr ch high))))
					;tables not made again for kx: SBR off, as ffmpeg turns it off
					((sbr-off sbr))))
			(sbr-x sbr ch low))
		chans data))
	;of one channel with PS, the left and the right made of it, the one
	;on both till its first header
	(and ps alone (= kind 0)
		(setq xs (if (. ps :find :start) (ps-apply ps (first xs) (+ (. sbr :find :kx) (. sbr :find :m))) (list (first xs) (first xs)))))
	(map (# (sbr-synthesis %1 %0)) xs data))

;module
(export-symbols '(sbr-new sbr-read sbr-apply sbr-tables))
(env-pop)
