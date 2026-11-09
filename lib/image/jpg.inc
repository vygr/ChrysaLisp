;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; jpeg loader
;
; A .jpg is markers, each 0xFF and a byte, most with a length and data after.
; DQT the tables each block's numbers are multiplied by, DHT the codes they
; were packed with, SOF how big it is and what its components are, DRI how
; often the packing starts again, and SOS the scan, the packed data after it
; to the end. Baseline, sequential, of 8 bits, is read: grey, or Y Cb Cr of
; any sampling, 4:4:4, 4:2:2, 4:2:0, and progressive, of the same. A file with
; no DHT has the standard's codes, K.3.
;
; The scan is a run of MCUs, each a block of 8 by 8 of each component, or
; more of one sampled more. A block is its 64 numbers, from the codes, the
; first the difference from the last block of the component, the rest runs
; of zeros and a number, in zig zag order; multiplied by its table, and
; turned back into pixels by the inverse DCT, two passes of dot products of
; fixed point rows, native.
;
; It is read as a PNG is, a stage of a pipeline on this node: what comes
; before the scan is read here, and the scan is decoded by a task of its
; own that writes a row of MCUs at a time, 32 bits a pixel, into the stage
; that reads the pixmap.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui/canvas/lisp.inc")
(import "././math/vector.inc")
(import "././streams/stages.inc")

;module
(env-push)

(ffi (path-to-absolute "./jpg_idct") jpg-idct-put)
(ffi (path-to-absolute "./jpg_line") jpg-line)
(ffi (path-to-absolute "./jpg_block") jpg-block-decode)
(ffi (path-to-absolute "./jpg_prog") jpg-prog-block)

(defq +jpg_zigzag (nums 0 1 8 16 9 2 3 10 17 24 32 25 18 11 4 5 12 19 26 33 40 48 41 34 27 20 13 6 7 14 21 28
		35 42 49 56 57 50 43 36 29 22 15 23 30 37 44 51 58 59 52 45 38 31 39 46 53 60 61 54 47 55 62 63)
	;the rows of the inverse DCT, K x u = C u cos((2x+1)u pi/16)/2, by 1024
	*jpg_k* (map (lambda (x)
			(apply (const nums) (map (lambda (u)
						(n2i (* (n2r 1024) (/ (if (= u 0) (recip (sqrt (n2r 2))) +real_1) (n2r 2))
								(cos (/ (* (n2r (inc (* 2 x))) (n2r u) +real_pi) (n2r 16))))))
					(range 0 8))))
		(range 0 8)))

;K as one nums of 64, its rows one after the other, for the native IDCT
(defq *jpg_kflat* (apply (const cat) *jpg_k*))

(defmacro jpg-be (data at)
	;two bytes high first, as a .jpg has its numbers
	(static-qq (code-be ,data 2 ,at)))

;the codes of the standard's K.3, of luma and of chroma, (counts values), for
;a file that has none of its own, as Motion JPEG's frames have not
(defq +jpg_k3_dc ''(
		((0 1 5 1 1 1 1 1 1 0 0 0 0 0 0 0) (0 1 2 3 4 5 6 7 8 9 10 11))
		((0 3 1 1 1 1 1 1 1 1 1 0 0 0 0 0) (0 1 2 3 4 5 6 7 8 9 10 11)))
	+jpg_k3_ac ''(
		((0 2 1 3 3 2 4 3 5 5 4 4 0 0 1 0x7d) (
				0x01 0x02 0x03 0x00 0x04 0x11 0x05 0x12 0x21 0x31 0x41 0x06 0x13 0x51 0x61 0x07
				0x22 0x71 0x14 0x32 0x81 0x91 0xa1 0x08 0x23 0x42 0xb1 0xc1 0x15 0x52 0xd1 0xf0
				0x24 0x33 0x62 0x72 0x82 0x09 0x0a 0x16 0x17 0x18 0x19 0x1a 0x25 0x26 0x27 0x28
				0x29 0x2a 0x34 0x35 0x36 0x37 0x38 0x39 0x3a 0x43 0x44 0x45 0x46 0x47 0x48 0x49
				0x4a 0x53 0x54 0x55 0x56 0x57 0x58 0x59 0x5a 0x63 0x64 0x65 0x66 0x67 0x68 0x69
				0x6a 0x73 0x74 0x75 0x76 0x77 0x78 0x79 0x7a 0x83 0x84 0x85 0x86 0x87 0x88 0x89
				0x8a 0x92 0x93 0x94 0x95 0x96 0x97 0x98 0x99 0x9a 0xa2 0xa3 0xa4 0xa5 0xa6 0xa7
				0xa8 0xa9 0xaa 0xb2 0xb3 0xb4 0xb5 0xb6 0xb7 0xb8 0xb9 0xba 0xc2 0xc3 0xc4 0xc5
				0xc6 0xc7 0xc8 0xc9 0xca 0xd2 0xd3 0xd4 0xd5 0xd6 0xd7 0xd8 0xd9 0xda 0xe1 0xe2
				0xe3 0xe4 0xe5 0xe6 0xe7 0xe8 0xe9 0xea 0xf1 0xf2 0xf3 0xf4 0xf5 0xf6 0xf7 0xf8
				0xf9 0xfa))
		((0 2 1 2 4 4 3 4 7 5 4 4 0 1 2 0x77) (
				0x00 0x01 0x02 0x03 0x11 0x04 0x05 0x21 0x31 0x06 0x12 0x41 0x51 0x07 0x61 0x71
				0x13 0x22 0x32 0x81 0x08 0x14 0x42 0x91 0xa1 0xb1 0xc1 0x09 0x23 0x33 0x52 0xf0
				0x15 0x62 0x72 0xd1 0x0a 0x16 0x24 0x34 0xe1 0x25 0xf1 0x17 0x18 0x19 0x1a 0x26
				0x27 0x28 0x29 0x2a 0x35 0x36 0x37 0x38 0x39 0x3a 0x43 0x44 0x45 0x46 0x47 0x48
				0x49 0x4a 0x53 0x54 0x55 0x56 0x57 0x58 0x59 0x5a 0x63 0x64 0x65 0x66 0x67 0x68
				0x69 0x6a 0x73 0x74 0x75 0x76 0x77 0x78 0x79 0x7a 0x82 0x83 0x84 0x85 0x86 0x87
				0x88 0x89 0x8a 0x92 0x93 0x94 0x95 0x96 0x97 0x98 0x99 0x9a 0xa2 0xa3 0xa4 0xa5
				0xa6 0xa7 0xa8 0xa9 0xaa 0xb2 0xb3 0xb4 0xb5 0xb6 0xb7 0xb8 0xb9 0xba 0xc2 0xc3
				0xc4 0xc5 0xc6 0xc7 0xc8 0xc9 0xca 0xd2 0xd3 0xd4 0xd5 0xd6 0xd7 0xd8 0xd9 0xda
				0xe2 0xe3 0xe4 0xe5 0xe6 0xe7 0xe8 0xe9 0xea 0xf2 0xf3 0xf4 0xf5 0xf6 0xf7 0xf8
				0xf9 0xfa))))

(defun jpg-segment (out kind data)
	; (jpg-segment out kind data) -> :t | :nil
	;a marker's segment read into out, :nil if it ends the head: a scan, or
	;a kind of frame not read. A scan's components are (id dc ac), and how
	;it is cut, :spectral, (start end high low): the numbers of a block it
	;has, from start to end in zig zag order, and of their bits, high the
	;last bit sent before, low the one now
	(cond
		((= kind 0xdb) ;tables of 64, of 8 bits or 16
			(defq at 0)
			(while (< at (length data))
				(defq pq (>> (code data 1 at) 4) tq (logand (code data 1 at) 3) at (inc at)
					q (apply (const nums) (map (# (if (= pq 0) (code data 1 (+ at %0)) (jpg-be data (+ at (* 2 %0))))) (range 0 64))))
				(elem-set (. out :find :quant) tq q)
				(setq at (+ at (* 64 (if (= pq 0) 1 2)))))
			:t)
		((= kind 0xc4) ;codes, of DC or AC, each 16 counts and the values
			(defq at 0)
			(while (< (+ at 16) (length data))
				(defq tc (>> (code data 1 at) 4) th (logand (code data 1 at) 3)
					lens (map (# (code data 1 (+ at 1 %0))) (range 0 16)) n (reduce (const +) lens 0)
					vals (map (# (code data 1 (+ at 17 %0))) (range 0 (min n (- (length data) at 17)))))
				(elem-set (. out :find (if (= tc 0) :dc :ac)) th (list lens vals))
				(setq at (+ at 17 n)))
			:t)
		((= kind 0xdd) (. out :insert :restart (jpg-be data 0)) :t)
		((<= 0xc0 kind 0xc2) ;baseline, extended or progressive: height width, and
			;each component
			(scatter out :height (jpg-be data 1) :width (jpg-be data 3) :progressive (= kind 0xc2)
				:comps (map (# (list (code data 1 (+ 6 (* 3 %0))) (>> (code data 1 (+ 7 (* 3 %0))) 4)
							(logand (code data 1 (+ 7 (* 3 %0))) 15) (logand (code data 1 (+ 8 (* 3 %0))) 3)))
					(range 0 (min 4 (code data 1 5)))))
			:t)
		((= kind 0xda) ;the scan: which code tables each component of it has
			(defq n (min 4 (code data 1 0)) at (+ 1 (* 2 n)))
			(. out :insert :scan (map (# (list (code data 1 (+ 1 (* 2 %0))) (>> (code data 1 (+ 2 (* 2 %0))) 4)
							(logand (code data 1 (+ 2 (* 2 %0))) 3))) (range 0 n)))
			(. out :insert :spectral (if (<= (+ at 3) (length data))
					(list (code data 1 at) (code data 1 (inc at)) (>> (code data 1 (+ at 2)) 4) (logand (code data 1 (+ at 2)) 15))
					'(0 63 0 0)))
			:nil)
		((find kind '(0xc3 0xc5 0xc6 0xc7 0xc9 0xca 0xcb 0xcd 0xce 0xcf))
			;lossless, hierarchical or arithmetic coded, not read
			(. out :insert :comps :nil)
			:nil)
		(:t :t)))

(defun jpg-segments (stream out)
	; (jpg-segments stream out) -> :nil | out
	;the markers and their segments, from where the stream is, till a scan,
	;the stream left at the start of its packed data. :nil if it ends, or
	;the file does, before
	(defq going :t ok :nil)
	(while going
		(defq m (read-blk stream 2))
		(cond
			((or (not m) (< (length m) 2) (/= (code m 1 0) 0xff)) (setq going :nil))
			((= (code m 1 1) 0xd8))
			((= (code m 1 1) 0xd9) (setq going :nil))
			((<= 0xd0 (code m 1 1) 0xd7))
			(:t (defq kind (code m 1 1) lb (read-blk stream 2))
				(cond
					((or (not lb) (< (length lb) 2)) (setq going :nil))
					(:t (defq len (- (jpg-be lb 0) 2) data (if (> len 0) (read-blk stream len) ""))
						(cond
							((or (not data) (< (length data) len)) (setq going :nil))
							((not (jpg-segment out kind data))
								(setq going :nil ok (and (= kind 0xda) (. out :find :comps))))))))))
	(if ok out))

(defun jpg-head (stream)
	; (jpg-head stream) -> :nil | emap
	;what comes before the first scan: :width :height, :comps ((id h v
	;table) ...), :progressive, :quant (64 nums) by table, :dc :ac (counts
	;values) by table, :restart, :scan and :spectral. The stream is left at
	;the start of the packed data
	(when (and stream (eql (read-blk stream 2) (cat (char 0xff) (char 0xd8))))
		(defq out (Emap))
		(scatter out :restart 0 :quant (list :nil :nil :nil :nil) :dc (cat +jpg_k3_dc '(:nil :nil)) :ac (cat +jpg_k3_ac '(:nil :nil)))
		(jpg-segments stream out)))

(defun JPG-info (stream)
	; (JPG-info stream) -> (width height type) | (-1 -1 -1)
	(if (defq head (jpg-head stream))
		(list (. head :find :width) (. head :find :height) 32)
		'(-1 -1 -1)))

(defun jpg-code (table)
	; (jpg-code table) -> (maxcode valptr mincode values)
	;the canonical code of a table, as the standard's F.2.2.3 has it
	(bind '(lens vals) table)
	(defq maxcode (nums) valptr (nums) mincode (nums) run 0 k 0)
	(each (lambda (n)
			(push valptr k) (push mincode run)
			(setq run (+ run n) k (+ k n))
			(push maxcode (if (= n 0) -1 (dec run)))
			(setq run (<< run 1)))
		lens)
	(list maxcode valptr mincode (apply (const nums) vals)))

(defun jpg-geometry (width height comps)
	; (jpg-geometry width height comps) -> (hmax vmax mcus_x mcus_y)
	(defq hmax (reduce (# (max %0 (second %1))) comps 1) vmax (reduce (# (max %0 (third %1))) comps 1))
	(list hmax vmax (/ (+ width (* 8 hmax) -1) (* 8 hmax)) (/ (+ height (* 8 vmax) -1) (* 8 vmax))))

(defun jpg-restart (reader in)
	;a restart: to the next byte, past the marker, which may not have been
	;come to yet
	(defq st (second reader))
	(elem-set st 1 0)
	(when (= (elem-get st 2) 0) (read-char in) (read-char in))
	(elem-set st 2 0))

(defun jpg-rows (out samplings planes my width height hmax vmax mcus_x)
	; (jpg-rows out samplings planes my width height hmax vmax mcus_x)
	;the rows of pixels of a row of MCUs, those inside the picture, from
	;the plane of each component, (h v) its sampling, of mcus_x h 8 by v 8,
	;each row native
	(defq hs (apply (const nums) (map (const first) samplings)) bases (fill (nums) (length samplings) 0))
	(each (lambda (row)
			(when (< (+ (* my vmax 8) row) height)
				(each (lambda ((h v)) (elem-set bases (!) (* (/ (* row v) vmax) (* mcus_x h 8)))) samplings)
				(write-blk out (jpg-line planes bases hs hmax width))))
		(range 0 (* vmax 8))))

(defun jpg-decode (in out info)
	; (jpg-decode in out info)
	;a baseline scan, from where the stream is, a row of MCUs at a time,
	;each row of pixels written blue green red alpha
	(bind '(width height comps scan quant dc ac restart &ignore) info)
	(bind '(hmax vmax mcus_x mcus_y) (jpg-geometry width height comps))
	;each component: its sampling, its codes and its table
	(defq parts (map (lambda ((id h v tq))
				(defq tables (some (# (if (= (first %0) id) %0)) scan))
				(list h v (list (jpg-code (elem-get dc (second tables))) (jpg-code (elem-get ac (third tables))))
					(apply (const nums) (elem-get quant tq))))
			comps)
		samplings (map (# (list (first %0) (second %0))) parts) scratch (fill (nums) 64 0) c (fill (nums) 64 0)
		lasts (fill (list) (length comps) 0) reader (list in (nums 0 0 0)) done 0 my 0)
	(while (< my mcus_y)
		(task-slice)
		;the planes of this row of MCUs, a nums each, its width in samples
		(defq planes (map (lambda ((h v &rest _)) (str-alloc (* mcus_x h 8 v 8))) parts) mx 0)
		(while (< mx mcus_x)
			(when (and (> restart 0) (> done 0) (= (% done restart) 0))
				(jpg-restart reader in)
				(setq lasts (fill (list) (length comps) 0)))
			(each (lambda ((h v codes q) plane)
					(defq ci (!) pw (* mcus_x h 8))
					(each (lambda (by)
							(each (lambda (bx)
									(elem-set lasts ci (jpg-block-decode in (second reader) codes q +jpg_zigzag (elem-get lasts ci) c))
									(jpg-idct-put c scratch *jpg_kflat* plane (+ (* by 8 pw) (* (+ (* mx h) bx) 8)) pw))
								(range 0 h)))
						(range 0 v)))
				parts planes)
			(setq mx (inc mx) done (inc done)))
		(jpg-rows out samplings planes my width height hmax vmax mcus_x)
		(setq my (inc my))))

;;;;;;;;;;;;;
; progressive
;;;;;;;;;;;;;

;A progressive JPEG sends the numbers of every block in scans, each of
;some of them, and of some of their bits, so they are all kept, by
;component, a grid of blocks the size of the MCUs, till the last scan.
;What each kind of scan does is the standard's G.1.2, as libjpeg has it:
;the first bits of the DCs, the next bit of each, the first bits of a
;band of the ACs, with runs of blocks that have none of them, and the
;next bit of each of those, that has a 1 or a 0 for each that is not 0
;and puts in those that are new. A number is taken as low bits up.

(defun jpg-scan (reader in head grids width height hmax vmax mcus_x mcus_y)
	; (jpg-scan reader in head grids width height hmax vmax mcus_x mcus_y)
	;a scan of a progressive JPEG, its numbers into the grids
	(bind '(start end high low) (. head :find :spectral))
	(defq comps (. head :find :comps) restart (. head :find :restart)
		kind (if (= start 0) (if (= high 0) 0 1) (if (= high 0) 2 3))
		;the components of the scan: which, its sampling, and its code
		these (map (lambda ((id dt at))
				(defq ci (ifn (some (# (if (= (first %0) id) (!))) comps) 0) c (elem-get comps ci)
					table (if (= kind 0) (elem-get (. head :find :dc) dt) (if (> kind 1) (elem-get (. head :find :ac) at))))
				(list ci (second c) (third c) (if table (jpg-code table))))
			(. head :find :scan))
		lasts (fill (list) (length these) 0) eobrun 0 done 0)
	(defun one (i ci code br bc)
		;a block of the scan, at row br and column bc of the grid
		(defq coefs (elem-get grids ci) base (* (+ (* br (* mcus_x (second (elem-get comps ci)))) bc) 64))
		;native, the dc of kind 0 is a difference from the last, the acs of 2
		;and 3 have a run of blocks to pass over
		(defq val (jpg-prog-block kind in (second reader) code coefs base start end low
				(if (= kind 0) (elem-get lasts i) eobrun) +jpg_zigzag))
		(cond
			((= kind 0) (elem-set lasts i val))
			((> kind 1) (setq eobrun val))))
	(defun restarted ()
		(when (and (> restart 0) (> done 0) (= (% done restart) 0))
			(jpg-restart reader in)
			(setq lasts (fill (list) (length these) 0) eobrun 0))
		(++ done))
	(cond
		((= (length these) 1)
			;of one component, its blocks that are in the picture, each an MCU
			(bind '(ci h v code) (first these))
			(defq cols (/ (+ (/ (+ (* width h) hmax -1) hmax) 7) 8)
				rows (/ (+ (/ (+ (* height v) vmax -1) vmax) 7) 8) br 0)
			(while (< br rows)
				(task-slice)
				(defq bc 0)
				(while (< bc cols)
					(restarted)
					(one 0 ci code br bc)
					(++ bc))
				(++ br)))
		(:t ;of several, MCU by MCU, each the blocks of each
			(defq my 0)
			(while (< my mcus_y)
				(task-slice)
				(defq mx 0)
				(while (< mx mcus_x)
					(restarted)
					(each (lambda ((ci h v code))
							(defq i (!))
							(each (lambda (by)
									(each (# (one i ci code (+ (* my v) by) (+ (* mx h) %0))) (range 0 h)))
								(range 0 v)))
						these)
					(++ mx))
				(++ my)))))

(defun jpg-next-marker (reader in)
	; (jpg-next-marker reader in) -> :nil | marker
	;the marker after a scan, which the scan may have come to
	(defq st (second reader))
	(elem-set st 1 0)
	(cond
		((/= (defq m (elem-get st 2)) 0) (elem-set st 2 0) m)
		(:t (defq m :nil b 0)
			(while (and (not m) (setq b (read-char in)))
				(when (= b 0xff)
					(while (= (setq b (ifn (read-char in) -1)) 0xff))
					(cond
						((< b 0) (setq m -1))
						((/= b 0) (setq m b)))))
			(and m (> m 0) m))))

(defun jpg-progressive (in out info)
	; (jpg-progressive in out info)
	;the scans, from where the stream is, then the pixels a row of MCUs at
	;a time, each row of pixels written blue green red alpha
	(bind '(width height comps scan quant dc ac restart spectral) info)
	(bind '(hmax vmax mcus_x mcus_y) (jpg-geometry width height comps))
	(defq grids (map (lambda ((id h v tq)) (fill (nums) (* mcus_x h mcus_y v 64) 0)) comps)
		head (Emap) reader (list in (nums 0 0 0)) going :t)
	(scatter head :restart restart :quant quant :dc dc :ac ac :comps comps :scan scan :spectral spectral)
	(while going
		(jpg-scan reader in head grids width height hmax vmax mcus_x mcus_y)
		;the tables and the head of the next scan, if there is one
		(defq m (jpg-next-marker reader in) more :t)
		(while (and more m)
			(cond
				((= m 0xd9) (setq m :nil))
				((<= 0xd0 m 0xd7) (setq m (jpg-next-marker reader in)))
				(:t (defq lb (read-blk in 2))
					(cond
						((or (not lb) (< (length lb) 2)) (setq m :nil))
						(:t (defq len (- (jpg-be lb 0) 2) data (if (> len 0) (read-blk in len) ""))
							(cond
								((or (not data) (< (length data) len)) (setq m :nil))
								((jpg-segment head m data) (setq m (jpg-next-marker reader in)))
								((= m 0xda) (setq more :nil))
								(:t (setq m :nil))))))))
		(setq going (not more)))
	;each block by its table, as the table's numbers are in zig zag order
	;they are put in rows first, and turned to pixels, a row of MCUs at a time
	(defq qns (map (lambda ((id h v tq))
				(defq q (ifn (elem-get (. head :find :quant) tq) (fill (nums) 64 0)) qn (fill (nums) 64 0))
				(each (# (elem-set qn (elem-get +jpg_zigzag %0) (elem-get q %0))) (range 0 64))
				qn)
			comps)
		samplings (map (# (list (second %0) (third %0))) comps) scratch (fill (nums) 64 0) my 0)
	(while (< my mcus_y)
		(task-slice)
		(defq planes (map (lambda ((id h v tq) coefs qn)
					(defq bw (* mcus_x h) pw (* bw 8) plane (str-alloc (* pw v 8)))
					(each (lambda (by)
							(defq br (+ (* my v) by))
							(each (lambda (bc)
									(defq base (* (+ (* br bw) bc) 64))
									(jpg-idct-put (nums-mul (slice coefs base (+ base 64)) qn) scratch *jpg_kflat*
										plane (+ (* by 8 pw) (* bc 8)) pw))
								(range 0 bw)))
						(range 0 v))
					plane)
				comps grids qns))
		(jpg-rows out samplings planes my width height hmax vmax mcus_x)
		(++ my)))

(defun JPG-load (stream)
	; (JPG-load stream) -> :nil | canvas
	(defq canvas :nil)
	(when (and (defq head (jpg-head stream)) (defq comps (. head :find :comps))
			(or (= (length comps) 1) (= (length comps) 3))
			(every (# (elem-get (. head :find :quant) (elem-get %0 3))) comps))
		(defq w (. head :find :width) h (. head :find :height)
			canvas (Canvas w h 1) pixmap (getf canvas +canvas_pixmap 0)
			;what the decode needs, numbers and lists of them, that go in its form
			info (list w h comps (. head :find :scan) (map (# (if %0 (apply (const list) %0))) (. head :find :quant))
				(. head :find :dc) (. head :find :ac) (. head :find :restart) (. head :find :spectral)))
		(setf pixmap +pixmap_type 32 0)
		;the scans decoded by a stage reading the stream from where the head left it
		(unless (stages-load stream pixmap 32
				(list (stage "lib/image/jpg.inc" (if (. head :find :progressive) 'jpg-progressive 'jpg-decode) (list 'quote info))))
			(setq canvas :nil)))
	canvas)

;the canvas is told what this reads
(canvas-loader ".jpg" JPG-info JPG-load)
(canvas-loader ".jpeg" JPG-info JPG-load)

;module
(export-symbols '(JPG-load JPG-info jpg-decode jpg-progressive *jpg_k* *jpg_kflat* +jpg_zigzag))
(env-pop)
