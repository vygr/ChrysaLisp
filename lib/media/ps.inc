;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ps, parametric stereo, HE-AAC v2, ISO/IEC 14496-3 8.6.4, our own
; decoder of what SBR carries in its extensions
;
; The core is one channel, and the two are made of it by what is sent of
; them: how much louder one is than the other, how alike they are, and
; their phases, a band at a time. The QMF bands of the one, the lowest
; split finer by the hybrid filters, are made a second unlike the first
; by all-pass filters and delays, then the two mixed by each band's
; matrix, interpolated over the envelopes, and the hybrid bands summed
; back. (ps-new) holds what goes from frame to frame, (ps-read ps reader
; bits) reads the data of an extension, (ps-apply ps slots top) gives the
; QMF slots of the left and the right of those of the one.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./huff.inc")
(import "./bits.inc")

;module
(env-push)

(import "./ps_tables.inc")

(ffi (path-to-absolute "./media_gather") ps-gather)
; (ps-gather out arrays index) -> out
(ffi (path-to-absolute "./media_ps_allpass") ps-allpass)
; (ps-allpass out_r out_i dr di links coeffs gains) -> out_r

;what the functions read as they run, this one map, the tables of
;ps_tables.inc taken in as this is read
(defq +ps (Emap))
(scatter +ps :books *ps_books* :k_to_i (map (# (apply (const nums) %0)) *ps_k_to_i*)
	:q30 (list *ps_ha* *ps_hb* *ps_pd_re* *ps_pd_im* *ps_q20* *ps_phi20* *ps_q34* *ps_phi34*
		*ps_f20_8* *ps_f34_12* *ps_f34_8* *ps_f34_4* *ps_g1_q2*))

(defun ps-tables ()
	; (ps-tables)
	;the books made native tables and the rest reals, made the once
	(unless (. +ps :find :tables)
		(defq k (/ (n2r 1) (n2r 1073741824)) books (. +ps :find :books))
		(bind '(ha hb pd_re pd_im q20 phi20 q34 phi34 f20_8 f34_12 f34_8 f34_4 g1)
			(map (lambda (l) (apply (const reals) (map (# (* (n2r %0) k)) l))) (. +ps :find :q30)))
		(scatter +ps :tables (map (# (huff-table (second %0))) books) :least (map (const first) books)
			:ha ha :hb hb :pd_re pd_re :pd_im pd_im :q (list q20 q34) :phi (list phi20 phi34)
			:f20_8 f20_8 :f34_12 f34_12 :f34_8 f34_8 :f34_4 f34_4 :g1 g1
			:zeros32 (fill (reals) 32 (n2r 0)) :ramp (apply (const reals) (map (# (n2r %0)) (range 1 33))))))

(defun ps-new ()
	; (ps-new) -> ps
	(ps-tables)
	(defq z34 (lambda () (fill (reals) 34 (n2r 0))) h (lambda () (map (lambda (_) (map (lambda (_) (z34)) (range 0 6))) (range 0 4))))
	(scatter (Emap) :start :nil :enable_iid 0 :iid_quant 0 :nr_iid_par 0 :nr_ipdopd_par 0 :enable_icc 0 :icc_mode 0
		:nr_icc_par 0 :enable_ext 0 :frame_class 0 :num_env_old 0 :num_env 0 :enable_ipdopd 0
		:border (fill (nums) 6 0) :is34 0 :is34_old 0 :huff (ffi "lib/media/media_huff")
		:iid (map (lambda (_) (fill (nums) 34 0)) (range 0 6)) :icc (map (lambda (_) (fill (nums) 34 0)) (range 0 6))
		:ipd (map (lambda (_) (fill (nums) 34 0)) (range 0 6)) :opd (map (lambda (_) (fill (nums) 34 0)) (range 0 6))
		;the hybrid filters' 6 slots before of the 5 lowest bands, the delays and
		;the all-pass links of each band, the transient detector's energies,
		;the matrices of the envelope before, re and im, and the phases' history
		:in_buf (map (lambda (_) (list (fill (reals) 6 (n2r 0)) (fill (reals) 6 (n2r 0)))) (range 0 5))
		:delay (map (lambda (_) (list (fill (reals) 46 (n2r 0)) (fill (reals) 46 (n2r 0)))) (range 0 91))
		:ap (map (lambda (_) (map (lambda (_) (list (fill (reals) 37 (n2r 0)) (fill (reals) 37 (n2r 0)))) (range 0 3))) (range 0 50))
		:peak (z34) :power (z34) :diff (z34)
		:h (list (h) (h)) :opd_hist (fill (nums) 34 0) :ipd_hist (fill (nums) 34 0)))

;;;;;;;;;;;;;;;;;;;;;;
; the data of a frame
;;;;;;;;;;;;;;;;;;;;;;

(defun ps-huff (ps in book)
	; (ps-huff ps reader book) -> value
	(defq got ((. ps :find :huff) (bits-str in) (bits-at in) (elem-get (. +ps :find :tables) book)))
	(bits-seek in (logand got 0xffffffff))
	(+ (>> got 32) (elem-get (. +ps :find :least) book)))

(defun ps-par (ps in par num book e dt mask)
	; (ps-par ps reader par num book e dt mask)
	;the parameters of an envelope, from those of the one before in time,
	;or each from the band below in frequency, kept to 3 bits if masked
	(defq to (elem-get par e))
	(cond
		((= dt 1)
			(defq from (elem-get par (max 0 (if (> e 0) (dec e) (dec (. ps :find :num_env_old))))))
			(each (lambda (b)
					(defq v (+ (elem-get from b) (ps-huff ps in book)))
					(elem-set to b (if mask (logand v 7) v)))
				(range 0 num)))
		(:t (defq v 0)
			(each (lambda (b)
					(setq v (+ v (ps-huff ps in book)))
					(if mask (setq v (logand v 7)))
					(elem-set to b v))
				(range 0 num)))))

(defun ps-bad? (ps e)
	; (ps-bad? ps e) -> :nil | :t
	;an intensity or a coherence of an envelope out of its range
	(or (and (= (. ps :find :enable_iid) 1)
			(some! (# (> (abs %0) (+ 7 (* 8 (. ps :find :iid_quant))))) (list (elem-get (. ps :find :iid) e)) :nil 0 (. ps :find :nr_iid_par)))
		(and (= (. ps :find :enable_icc) 1)
			(some! (# (not (<= 0 %0 7))) (list (elem-get (. ps :find :icc) e)) :nil 0 (. ps :find :nr_icc_par)))))

(defun ps-clear (ps)
	(each (lambda (key) (each (# (fill %0 34 0)) (. ps :find key))) '(:iid :icc :ipd :opd)))

(defun ps-read (ps in left)
	; (ps-read ps reader bits) -> bits
	;the PS of an extension of bits, read by a reader of its own: its header
	;if it has one, its envelopes' borders, the intensities and coherences
	;of each, and of its extension the phases. How many bits it took, all
	;of them if it is wrong, and that wrong frame's parameters left none
	(defq from (bits-at in) rd (list (first in) (nums from)) ok :t
		pars (lambda (key num tables)
			(each (lambda (e)
					(defq dt (bits-1 rd))
					(ps-par ps rd (. ps :find key) num (elem-get tables dt) e dt :nil))
				(range 0 (. ps :find :num_env)))))
	(defq header (bits-1 rd))
	(when (= header 1)
		(. ps :insert :enable_iid (bits-1 rd))
		(when (= (. ps :find :enable_iid) 1)
			(defq mode (bits-get rd 3))
			(if (> mode 5) (setq ok :nil)
				(scatter ps :nr_iid_par (elem-get '(10 20 34 10 20 34) mode) :iid_quant (if (> mode 2) 1 0)
					:nr_ipdopd_par (elem-get '(5 11 17 5 11 17) mode))))
		(if ok (. ps :insert :enable_icc (bits-1 rd)))
		(when (and ok (= (. ps :find :enable_icc) 1))
			(defq mode (bits-get rd 3))
			(if (> mode 5) (setq ok :nil)
				(scatter ps :icc_mode mode :nr_icc_par (elem-get '(10 20 34 10 20 34) mode))))
		(if ok (. ps :insert :enable_ext (bits-1 rd))))
	(when ok
		(defq frame_class (bits-1 rd) num_env (elem-get (elem-get '((0 1 2 4) (1 2 3 4)) frame_class) (bits-get rd 2))
			border (. ps :find :border))
		(scatter ps :frame_class frame_class :num_env_old (. ps :find :num_env) :num_env num_env)
		(elem-set border 0 -1)
		(cond
			((= frame_class 1)
				(each (lambda (e)
						(elem-set border e (bits-get rd 5))
						(if (< (elem-get border e) (elem-get border (dec e))) (setq ok :nil)))
					(range 1 (inc num_env))))
			(:t (each (# (elem-set border %0 (dec (>> (* %0 32) (elem-get '(0 0 1 1 2) num_env))))) (range 1 (inc num_env))))))
	(when ok
		(if (= (. ps :find :enable_iid) 1)
			(pars :iid (. ps :find :nr_iid_par) (if (= (. ps :find :iid_quant) 1) '(0 1) '(2 3)))
			(each (# (fill %0 34 0)) (. ps :find :iid)))
		(if (= (. ps :find :enable_icc) 1)
			(pars :icc (. ps :find :nr_icc_par) '(4 5))
			(each (# (fill %0 34 0)) (. ps :find :icc)))
		(setq ok (notany (# (ps-bad? ps %0)) (range 0 (. ps :find :num_env)))))
	(when (and ok (= (. ps :find :enable_ext) 1))
		;the extensions, of id 0 the phases
		(defq cnt (bits-get rd 4))
		(if (= cnt 15) (setq cnt (+ cnt (bits-get rd 8))))
		(setq cnt (* 8 cnt))
		(while (> cnt 7)
			(defq id (bits-get rd 2) at (bits-at rd))
			(when (= id 0)
				(. ps :insert :enable_ipdopd (bits-1 rd))
				(when (= (. ps :find :enable_ipdopd) 1)
					(each (lambda (e)
							(defq dt (bits-1 rd))
							(ps-par ps rd (. ps :find :ipd) (. ps :find :nr_ipdopd_par) (if (= dt 1) 7 6) e dt :t)
							(setq dt (bits-1 rd))
							(ps-par ps rd (. ps :find :opd) (. ps :find :nr_ipdopd_par) (if (= dt 1) 9 8) e dt :t))
						(range 0 (. ps :find :num_env))))
				(bits-skip rd 1))
			(setq cnt (- cnt 2 (- (bits-at rd) at))))
		(if (< cnt 0) (setq ok :nil) (bits-skip rd cnt)))
	(when ok
		;an envelope made to the end of the frame if the last is short of it
		(defq num_env (. ps :find :num_env) border (. ps :find :border))
		(when (or (= num_env 0) (< (elem-get border num_env) 31))
			(defq source (if (> num_env 0) (dec num_env) (dec (. ps :find :num_env_old))))
			(and (>= source 0) (/= source num_env)
				(each (lambda (key flag)
						(if (= (. ps :find flag) 1)
							(elem-set (. ps :find key) num_env (cat (elem-get (. ps :find key) source)))))
					'(:iid :icc :ipd :opd) '(:enable_iid :enable_icc :enable_ipdopd :enable_ipdopd)))
			(if (ps-bad? ps num_env) (setq ok :nil))
			(when ok
				(. ps :insert :num_env (setq num_env (inc num_env)))
				(elem-set border num_env 31))))
	(when ok
		(. ps :insert :is34_old (. ps :find :is34))
		(if (or (= (. ps :find :enable_iid) 1) (= (. ps :find :enable_icc) 1))
			(. ps :insert :is34 (if (or (and (= (. ps :find :enable_iid) 1) (= (. ps :find :nr_iid_par) 34))
					(and (= (. ps :find :enable_icc) 1) (= (. ps :find :nr_icc_par) 34))) 1 0)))
		(when (= (. ps :find :enable_ipdopd) 0)
			(each (# (fill %0 34 0)) (. ps :find :ipd))
			(each (# (fill %0 34 0)) (. ps :find :opd)))
		(if (= header 1) (. ps :insert :start :t)))
	(defq used (- (bits-at rd) from))
	(cond
		((and ok (<= used left)) (bits-skip in used) used)
		(:t (. ps :insert :start :nil)
			(ps-clear ps)
			(bits-skip in left)
			left)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the parameters to other bands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;each band of 34, 20 or 10 as the mean of bands of another number, a
;band again to weigh it twice, those to 20 and 34 of all the bands or of
;the phases' fewer
(defq +ps_20_34 ''((0 0) (1 0 1) (2 1) (3 2) (4 2 3) (5 3) (6 4) (7 4) (8 5) (9 5) (10 6) (11 7) (12 8) (13 8) (14 9) (15 9) (16 10))
	+ps_20_34_full ''((17 11) (18 12) (19 13) (20 14) (21 14) (22 15) (23 15) (24 16) (25 16) (26 17) (27 17) (28 18) (29 18) (30 18) (31 18) (32 19) (33 19))
	+ps_10_34 ''((0 0) (1 0) (2 0) (3 1) (4 1) (5 1) (6 2) (7 2) (8 2) (9 2) (10 3) (11 3) (12 4) (13 4) (14 4) (15 4))
	+ps_10_34_full ''((16 5) (17 5) (18 6) (19 6) (20 7) (21 7) (22 7) (23 7) (24 8) (25 8) (26 8) (27 8) (28 9) (29 9) (30 9) (31 9) (32 9) (33 9))
	+ps_34_20 ''((0 0 0 1) (1 1 2 2) (2 3 3 4) (3 4 5 5) (4 6 7) (5 8 9) (6 10) (7 11) (8 12 13) (9 14 15) (10 16))
	+ps_34_20_full ''((11 17) (12 18) (13 19) (14 20 21) (15 22 23) (16 24 25) (17 26 27) (18 28 29 30 31) (19 32 33))
	+ps_10_20 ''((0 0) (1 0) (2 1) (3 1) (4 2) (5 2) (6 3) (7 3) (8 4) (9 4))
	+ps_10_20_full ''((10 5) (11 5) (12 6) (13 6) (14 7) (15 7) (16 8) (17 8) (18 9) (19 9)))

(defun ps-map (to from spec)
	; (ps-map to from spec) -> to
	;each band of the spec the mean of its bands of from, of numbers as C
	;divides them, to 0, of reals by the count
	(each (lambda ((dst &rest srcs))
			(defq sum (reduce (# (+ %0 (elem-get from %1))) srcs (if (reals?? from) (n2r 0) 0)) n (length srcs))
			(elem-set to dst (cond
				((reals?? from) (/ sum (n2r n)))
				((< sum 0) (neg (/ (neg sum) n)))
				((/ sum n)))))
		spec)
	to)

(defun ps-remap (par num is34 full)
	; (ps-remap par num is34 full) -> nums
	;an envelope's parameters of num bands as those of 34 or 20
	(defq to (fill (nums) 34 0))
	(cond
		(is34 (cond
			((or (= num 20) (= num 11)) (ps-map to par +ps_20_34) (if full (ps-map to par +ps_20_34_full)) to)
			((or (= num 10) (= num 5)) (ps-map to par +ps_10_34) (if full (ps-map to par +ps_10_34_full)) to)
			(par)))
		((or (= num 34) (= num 17)) (ps-map to par +ps_34_20) (if full (ps-map to par +ps_34_20_full)) to)
		((or (= num 10) (= num 5)) (ps-map to par +ps_10_20) (if full (ps-map to par +ps_10_20_full)) to)
		(par)))

;;;;;;;;;;;;;;;;;;;;;;;;;
; the two of the one
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ps-series (slots q n)
	; (ps-series slots q n) -> (re im)
	;band q's first n slots, of slots of 64 bands each
	(list (ps-gather (fill (reals) n (n2r 0)) (map! (const first) (list slots) 0 n) q)
		(ps-gather (fill (reals) n (n2r 0)) (map! (const second) (list slots) 0 n) q)))

(defun ps-filter (xr xi filter n)
	; (ps-filter re im filter n) -> ((re im) ...)
	;n sub-subbands of a band of 44 slots, 6 before, by a complex filter
	;of 13 taps, symmetric, the 7 of each in the table
	(defq tap (lambda (x j) (slice x j (+ j 32)))
		inre0 (map (# (nums-add (tap xr %0) (tap xr (- 12 %0)))) (range 0 6))
		inre1 (map (# (nums-sub (tap xi %0) (tap xi (- 12 %0)))) (range 0 6))
		inim0 (map (# (nums-add (tap xi %0) (tap xi (- 12 %0)))) (range 0 6))
		inim1 (map (# (nums-sub (tap xr %0) (tap xr (- 12 %0)))) (range 0 6))
		in6r (tap xr 6) in6i (tap xi 6))
	(map (lambda (q)
			(defq at (* q 14) f6 (elem-get filter (+ at 12)) re (nums-scale in6r f6) im (nums-scale in6i f6))
			(each (lambda (j)
					(defq fr (elem-get filter (+ at (* 2 j))) fi (elem-get filter (+ at (* 2 j) 1)))
					(setq re (nums-sub (nums-add re (nums-scale (elem-get inre0 j) fr)) (nums-scale (elem-get inre1 j) fi))
						im (nums-add (nums-add im (nums-scale (elem-get inim0 j) fr)) (nums-scale (elem-get inim1 j) fi))))
				(range 0 6))
			(list re im))
		(range 0 n)))

(defun ps-filter2 (xr xi g reverse)
	; (ps-filter2 re im g reverse) -> ((re im) (re im))
	;a band of 44 slots two, by a real filter of 13 taps of which only the
	;centre and the odd are not 0
	(defq tap (lambda (x j) (slice x j (+ j 32))) g6 (elem-get g 6)
		in_r (nums-scale (tap xr 6) g6) in_i (nums-scale (tap xi 6) g6)
		op_r (fill (reals) 32 (n2r 0)) op_i (fill (reals) 32 (n2r 0)))
	(each (lambda (j)
			(defq c (elem-get g (inc j)))
			(setq op_r (nums-add op_r (nums-scale (nums-add (tap xr (inc j)) (tap xr (- 11 j))) c))
				op_i (nums-add op_i (nums-scale (nums-add (tap xi (inc j)) (tap xi (- 11 j))) c))))
		'(0 2 4))
	(defq plus (list (nums-add in_r op_r) (nums-add in_i op_i)) minus (list (nums-sub in_r op_r) (nums-sub in_i op_i)))
	(if (= reverse 1) (list minus plus) (list plus minus)))

(defun ps-analysis (ps slots is34)
	; (ps-analysis ps slots is34) -> ((re im) ...)
	;the 71 or 91 hybrid bands of the 38 slots of a channel: the lowest 3 or
	;5 bands split by the hybrid filters, each with 6 slots before, the rest
	;as they are
	(defq in_buf (. ps :find :in_buf)
		xs (map (lambda (q)
				(bind '(r i) (ps-series slots q 38))
				(bind '(hr hi) (elem-get in_buf q))
				(defq xr (cat hr r) xi (cat hi i))
				(elem-set in_buf q (list (slice xr 32 38) (slice xi 32 38)))
				(list xr xi))
			(range 0 5)))
	(cat (if (= is34 1)
			(cat (ps-filter (first (first xs)) (second (first xs)) (. +ps :find :f34_12) 12)
				(ps-filter (first (second xs)) (second (second xs)) (. +ps :find :f34_8) 8)
				(ps-filter (first (third xs)) (second (third xs)) (. +ps :find :f34_4) 4)
				(ps-filter (first (elem-get xs 3)) (second (elem-get xs 3)) (. +ps :find :f34_4) 4)
				(ps-filter (first (elem-get xs 4)) (second (elem-get xs 4)) (. +ps :find :f34_4) 4))
			(defq t8 (ps-filter (first (first xs)) (second (first xs)) (. +ps :find :f20_8) 8)
				sum (lambda (a b) (list (nums-add (first a) (first b)) (nums-add (second a) (second b)))))
			(cat (list (elem-get t8 6) (elem-get t8 7) (elem-get t8 0) (elem-get t8 1)
					(sum (elem-get t8 2) (elem-get t8 5)) (sum (elem-get t8 3) (elem-get t8 4)))
				(ps-filter2 (first (second xs)) (second (second xs)) (. +ps :find :g1) 1)
				(ps-filter2 (first (third xs)) (second (third xs)) (. +ps :find :g1) 0)))
		(map (# (ps-series slots %0 32)) (range (if (= is34 1) 5 3) 64))))

(defun ps-decorrelate (ps bands is34)
	; (ps-decorrelate ps bands is34) -> ((re im) ...)
	;the second channel's bands, each unlike the first's: by its parameter
	;band's transients a gain, then the lowest by all-pass filters of 3
	;links of their delays, those above by a delay of 14 slots, the top by 1
	(defq nr_par (if (= is34 1) 34 20) nr_bands (length bands) nr_ap (if (= is34 1) 50 30) short (if (= is34 1) 62 42)
		cutoff (if (= is34 1) 32 10) k_to_i (elem-get (. +ps :find :k_to_i) is34)
		zero (n2r 0) one (n2r 1) quarter (n2r 0.25) impact (n2r 1.5) decay (str-to-real "0.76592833836465")
		peak (. ps :find :peak) power_s (. ps :find :power) diff_s (. ps :find :diff)
		power (map (lambda (_) (fill (reals) 32 zero)) (range 0 34)) gains (list))
	(when (/= is34 (. ps :find :is34_old))
		(each (# (fill %0 34 zero)) (list peak power_s diff_s))
		(each (# (fill (first %0) 46 zero) (fill (second %0) 46 zero)) (. ps :find :delay))
		(each (# (each (lambda ((r i)) (fill r 37 zero) (fill i 37 zero)) %0)) (. ps :find :ap)))
	(each (lambda ((r i) p)
			(elem-set power p (nums-add (elem-get power p) (nums-add (nums-mul r r) (nums-mul i i)))))
		bands k_to_i)
	;the transients, each parameter band's gain by its power against its
	;peak decaying
	(each (lambda (i)
			(defq g (cap 32 (reals)) pk (elem-get peak i) ps_ (elem-get power_s i) df (elem-get diff_s i))
			(each (lambda (p)
					(setq pk (max (* decay pk) p) ps_ (+ ps_ (* quarter (- p ps_))) df (+ df (* quarter (- pk p df))))
					(defq denom (* impact df))
					(push g (if (> denom ps_) (/ ps_ denom) one)))
				(elem-get power i))
			(elem-set peak i pk) (elem-set power_s i ps_) (elem-set diff_s i df)
			(push gains g))
		(range 0 nr_par))
	(defq q_tab (elem-get (. +ps :find :q) is34) phi_tab (elem-get (. +ps :find :phi) is34) delays (. ps :find :delay)
		links (list (str-to-real "0.65143905753106") (str-to-real "0.56471812200776") (str-to-real "0.48954165955695")))
	(map (lambda ((sr si))
			(defq k (!) g (elem-get gains (elem-get k_to_i k)) dl (elem-get delays k)
				dr (cat (slice (first dl) 32 46) sr) di (cat (slice (second dl) 32 46) si))
			(elem-set delays k (list dr di))
			(cond
				((< k nr_ap)
					(defq slope (max zero (min one (- one (* (n2r (- k cutoff)) (n2r 0.05)))))
						at (* 6 k) z32 (. +ps :find :zeros32)
						;the phase, then each link's fractional delay and decayed gain
						coeffs (reals (elem-get phi_tab (* 2 k)) (elem-get phi_tab (inc (* 2 k)))
							(elem-get q_tab at) (elem-get q_tab (+ at 1)) (* (first links) slope)
							(elem-get q_tab (+ at 2)) (elem-get q_tab (+ at 3)) (* (second links) slope)
							(elem-get q_tab (+ at 4)) (elem-get q_tab (+ at 5)) (* (third links) slope))
						aps (map (lambda ((r i)) (list (cat (slice r 32 37) z32) (cat (slice i 32 37) z32)))
							(elem-get (. ps :find :ap) k))
						out_r (fill (reals) 32 zero) out_i (fill (reals) 32 zero))
					(ps-allpass out_r out_i dr di (reduce (# (push %0 (first %1) (second %1))) aps (list)) coeffs g)
					(elem-set (. ps :find :ap) k aps)
					(list out_r out_i))
				((< k short) (list (nums-mul (slice dr 0 32) g) (nums-mul (slice di 0 32) g)))
				((list (nums-mul (slice dr 13 45) g) (nums-mul (slice di 13 45) g)))))
		bands))

(defun ps-stereo (ps lbands rbands is34)
	; (ps-stereo ps left right is34)
	;each band's two of the one and the unlike, mixed by its parameter
	;band's matrix of each envelope, interpolated over the envelope's
	;slots from the one before, its phases too if they were sent
	(defq nr_par (if (= is34 1) 34 20) nr_ipd (if (= is34 1) 17 11) k_to_i (elem-get (. +ps :find :k_to_i) is34)
		h (. ps :find :h) num_env (. ps :find :num_env) old (. ps :find :num_env_old) ipdopd (= (. ps :find :enable_ipdopd) 1)
		lut (if (< (. ps :find :icc_mode) 3) (. +ps :find :ha) (. +ps :find :hb)) quant (. ps :find :iid_quant)
		opd_hist (. ps :find :opd_hist) ipd_hist (. ps :find :ipd_hist) ramp (. +ps :find :ramp)
		pd_re (. +ps :find :pd_re) pd_im (. +ps :find :pd_im) border (. ps :find :border) zero (n2r 0))
	;the matrices the frame before ended with, at 34 or 20 as now
	(if (> old 0) (each (lambda (part) (each (# (elem-set %0 0 (cat (elem-get %0 old)))) part)) h))
	(defq iid (map! (# (ps-remap %0 (. ps :find :nr_iid_par) (= is34 1) :t)) (list (. ps :find :iid)) 0 num_env)
		icc (map! (# (ps-remap %0 (. ps :find :nr_icc_par) (= is34 1) :t)) (list (. ps :find :icc)) 0 num_env)
		ipd (map! (# (ps-remap %0 (. ps :find :nr_ipdopd_par) (= is34 1) :nil)) (list (. ps :find :ipd)) 0 num_env)
		opd (map! (# (ps-remap %0 (. ps :find :nr_ipdopd_par) (= is34 1) :nil)) (list (. ps :find :opd)) 0 num_env))
	(cond
		((and (= is34 1) (= (. ps :find :is34_old) 0))
			(each (lambda (part) (each (# (defq v (first %0)) (elem-set %0 0 (ps-map (cat v) v (cat +ps_20_34 +ps_20_34_full)))) part)) h)
			(fill opd_hist 34 0) (fill ipd_hist 34 0))
		((and (= is34 0) (= (. ps :find :is34_old) 1))
			(each (lambda (part) (each (# (defq v (first %0)) (elem-set %0 0 (ps-map (cat v) v (cat +ps_34_20 +ps_34_20_full)))) part)) h)
			(fill opd_hist 34 0) (fill ipd_hist 34 0)))
	(each (lambda (e)
			(defq ie (elem-get iid e) ce (elem-get icc e))
			(each (lambda (b)
					(defq at (* 4 (+ (* 8 (+ (elem-get ie b) 7 (* 23 quant))) (elem-get ce b)))
						hs (map (# (elem-get lut (+ at %0))) (range 0 4)))
					(when (and ipdopd (< b nr_ipd))
						(defq opd_idx (+ (* 8 (elem-get opd_hist b)) (elem-get (elem-get opd e) b))
							ipd_idx (+ (* 8 (elem-get ipd_hist b)) (elem-get (elem-get ipd e) b))
							opd_r (elem-get pd_re opd_idx) opd_i (elem-get pd_im opd_idx)
							ipd_r (elem-get pd_re ipd_idx) ipd_i (elem-get pd_im ipd_idx)
							adj_r (+ (* opd_r ipd_r) (* opd_i ipd_i)) adj_i (- (* opd_i ipd_r) (* opd_r ipd_i)))
						(elem-set opd_hist b (logand opd_idx 0x3f))
						(elem-set ipd_hist b (logand ipd_idx 0x3f))
						(each (lambda (m v) (elem-set (elem-get m (inc e)) b v))
							(second h) (list (* (first hs) opd_i) (* (second hs) adj_i) (* (third hs) opd_i) (* (elem-get hs 3) adj_i)))
						(setq hs (list (* (first hs) opd_r) (* (second hs) adj_r) (* (third hs) opd_r) (* (elem-get hs 3) adj_r))))
					(each (lambda (m v) (elem-set (elem-get m (inc e)) b v)) (first h) hs))
				(range 0 nr_par))
			(defq start (elem-get border e) stop (elem-get border (inc e)) len (- stop start)
				width (/ (n2r 1) (n2r (if (= len 0) 1 len))) r (slice ramp 0 (max len 1)))
			(when (> len 0)
				(each (lambda ((lr li) (rr ri))
						(defq k (!) b (elem-get k_to_i k) a (inc start) z (+ a len)
							neg_im (if (= is34 1) (<= 9 k 13) (<= k 1))
							at (map (# (elem-get (elem-get %0 e) b)) (first h))
							to (map (# (elem-get (elem-get %0 (inc e)) b)) (first h))
							hv (map (# (nums-add (fill (reals) len %0) (nums-scale r (* (- %1 %0) width)))) at to)
							l_r (slice lr a z) l_i (slice li a z) r_r (slice rr a z) r_i (slice ri a z))
						(bind '(h0 h1 h2 h3) hv)
						(defq nl_r (nums-add (nums-mul h0 l_r) (nums-mul h2 r_r)) nl_i (nums-add (nums-mul h0 l_i) (nums-mul h2 r_i))
							nr_r (nums-add (nums-mul h1 l_r) (nums-mul h3 r_r)) nr_i (nums-add (nums-mul h1 l_i) (nums-mul h3 r_i)))
						(when ipdopd
							(defq at_i (map (# (elem-get (elem-get %0 e) b)) (second h))
								at_i (if neg_im (map (const neg) at_i) at_i)
								to_i (map (# (elem-get (elem-get %0 (inc e)) b)) (second h))
								gv (map (# (nums-add (fill (reals) len %0) (nums-scale r (* (- %1 %0) width)))) at_i to_i))
							(bind '(g0 g1 g2 g3) gv)
							(setq nl_r (nums-sub nl_r (nums-add (nums-mul g0 l_i) (nums-mul g2 r_i)))
								nl_i (nums-add nl_i (nums-add (nums-mul g0 l_r) (nums-mul g2 r_r)))
								nr_r (nums-sub nr_r (nums-add (nums-mul g1 l_i) (nums-mul g3 r_i)))
								nr_i (nums-add nr_i (nums-add (nums-mul g1 l_r) (nums-mul g3 r_r)))))
						(elem-set lbands k (list (cat (slice lr 0 a) nl_r (slice lr z 32)) (cat (slice li 0 a) nl_i (slice li z 32))))
						(elem-set rbands k (list (cat (slice rr 0 a) nr_r (slice rr z 32)) (cat (slice ri 0 a) nr_i (slice ri z 32)))))
					lbands rbands)))
		(range 0 num_env)))

(defun ps-synthesis (bands is34)
	; (ps-synthesis bands is34) -> ((re im) ...)
	;the 32 slots of 64 QMF bands of the hybrid bands, those split summed
	(defq sum (lambda (from to)
			(reduce! (# (list (nums-add (first %0) (first %1)) (nums-add (second %0) (second %1))))
				(list bands) (elem-get bands from) (inc from) to))
		qmf (if (= is34 1)
			(cat (list (sum 0 12) (sum 12 20) (sum 20 24) (sum 24 28) (sum 28 32)) (slice bands 32 91))
			(cat (list (sum 0 6) (sum 6 8) (sum 8 10)) (slice bands 10 71))))
	(defq qr (map (const first) qmf) qi (map (const second) qmf) zero (n2r 0))
	(map (# (list (ps-gather (fill (reals) 64 zero) qr %0) (ps-gather (fill (reals) 64 zero) qi %0))) (range 0 32)))

(defun ps-apply (ps slots top)
	; (ps-apply ps slots top) -> (left right)
	;the 32 QMF slots of each side, of the 38 of the one, top the band above
	;the last SBR made, the delays above it let go
	(defq is34 (. ps :find :is34) nr_bands (if (= is34 1) 91 71) nr_ap (if (= is34 1) 50 30)
		top (+ top nr_bands -64) zero (n2r 0))
	(each! (# (fill (first %0) 46 zero) (fill (second %0) 46 zero)) (list (. ps :find :delay)) (max 0 top) nr_bands)
	(if (< top nr_ap)
		(each! (# (each (lambda ((r i)) (fill r 37 zero) (fill i 37 zero)) %0)) (list (. ps :find :ap)) (max 0 top) nr_ap))
	(defq left (ps-analysis ps slots is34) right (ps-decorrelate ps left is34))
	(ps-stereo ps left right is34)
	(list (ps-synthesis left is34) (ps-synthesis right is34)))

;module
(export-symbols '(ps-new ps-read ps-apply ps-tables))
(env-pop)
