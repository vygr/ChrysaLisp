;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; jpeg loader
;
; A .jpg is markers, each 0xFF and a byte, most with a length and data after.
; DQT the tables each block's numbers are multiplied by, DHT the codes they
; were packed with, SOF how big it is and what its components are, DRI how
; often the packing starts again, and SOS the scan, the packed data after it
; to the end. Baseline, sequential, of 8 bits, is read: grey, or Y Cb Cr of
; any sampling, 4:4:4, 4:2:2, 4:2:0. Progressive is not, yet.
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
(import "lib/math/vector.inc")

;module
(env-push)

(defq +jpg_zigzag (nums 0 1 8 16 9 2 3 10 17 24 32 25 18 11 4 5 12 19 26 33 40 48 41 34 27 20 13 6 7 14 21 28
		35 42 49 56 57 50 43 36 29 22 15 23 30 37 44 51 58 59 52 45 38 31 39 46 53 60 61 54 47 55 62 63)
	;the rows of the inverse DCT, K x u = C u cos((2x+1)u pi/16)/2, by 1024
	*jpg_k* (map (lambda (x)
			(apply (const nums) (map (lambda (u)
				(n2i (* (n2r 1024) (/ (if (= u 0) (recip (sqrt (n2r 2))) +real_1) (n2r 2))
					(cos (/ (* (n2r (inc (* 2 x))) (n2r u) +real_pi) (n2r 16))))))
				(range 0 8))))
		(range 0 8)))

(defun jpg-be (data at)
	(+ (<< (code data 1 at) 8) (code data 1 (inc at))))

(defun jpg-head (stream)
	; (jpg-head stream) -> :nil | emap
	;what comes before the scan: :width :height, :comps ((id h v table) ...),
	;:quant (64 nums) by table, :dc :ac (bits values) by table, :restart. The
	;stream is left at the start of the packed data
	(when (and stream (eql (read-blk stream 2) (cat (char 0xff) (char 0xd8))))
		(defq out (Emap) quant (list :nil :nil :nil :nil) dc (list :nil :nil :nil :nil) ac (list :nil :nil :nil :nil)
			going :t ok :nil)
		(scatter out :restart 0 :quant quant :dc dc :ac ac)
		(while going
			(defq m (read-blk stream 2))
			(cond
				((or (not m) (< (length m) 2) (/= (code m 1 0) 0xff)) (setq going :nil))
				((= (code m 1 1) 0xd8))
				((= (code m 1 1) 0xd9) (setq going :nil))
				(:t (defq kind (code m 1 1) lb (read-blk stream 2))
					(cond
						((or (not lb) (< (length lb) 2)) (setq going :nil))
						(:t (defq len (- (jpg-be lb 0) 2) data (if (> len 0) (read-blk stream len) ""))
							(case kind
								(0xdb ;tables of 64, of 8 bits or 16
									(defq at 0)
									(while (< at (length data))
										(defq pq (>> (code data 1 at) 4) tq (logand (code data 1 at) 15) at (inc at)
											q (apply (const nums) (map (# (if (= pq 0) (code data 1 (+ at %0)) (jpg-be data (+ at (* 2 %0))))) (range 0 64))))
										(elem-set quant tq q)
										(setq at (+ at (* 64 (if (= pq 0) 1 2))))))
								(0xc4 ;codes, of DC or AC, each 16 counts and the values
									(defq at 0)
									(while (< at (length data))
										(defq tc (>> (code data 1 at) 4) th (logand (code data 1 at) 15)
											lens (map (# (code data 1 (+ at 1 %0))) (range 0 16)) n (reduce (const +) lens 0)
											vals (map (# (code data 1 (+ at 17 %0))) (range 0 n)))
										(elem-set (if (= tc 0) dc ac) th (list lens vals))
										(setq at (+ at 17 n))))
								(0xdd (. out :insert :restart (jpg-be data 0)))
								(0xc0 ;baseline: height width, and each component
									(scatter out :height (jpg-be data 1) :width (jpg-be data 3)
										:comps (map (# (list (code data 1 (+ 6 (* 3 %0))) (>> (code data 1 (+ 7 (* 3 %0))) 4)
												(logand (code data 1 (+ 7 (* 3 %0))) 15) (code data 1 (+ 8 (* 3 %0)))))
											(range 0 (code data 1 5)))))
								(0xc1 (setq going :nil))
								(0xc2 (setq going :nil))
								(0xda ;the scan: which code tables each component of it has
									(defq n (code data 1 0) scan (map (# (list (code data 1 (+ 1 (* 2 %0))) (>> (code data 1 (+ 2 (* 2 %0))) 4)
										(logand (code data 1 (+ 2 (* 2 %0))) 15))) (range 0 n)))
									(. out :insert :scan scan)
									(setq going :nil ok (. out :find :comps)))))))))
		(if ok out)))

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

(defun jpg-bit (reader)
	; (jpg-bit reader) -> 0 | 1
	;the next bit of the packed data, the high bit of each byte first. A
	;0xFF is followed by a 0 that is not data; anything else after it is
	;a marker, and from there on there are only 0 reader
	(when (= (elem-get reader 2) 0)
		(defq b (if (elem-get reader 3) 0 (ifn (read-char (elem-get reader 0)) 0)))
		(when (= b 0xff)
			(defq b2 (ifn (read-char (elem-get reader 0)) 0))
			(when (/= b2 0) (elem-set reader 3 b2) (setq b 0)))
		(elem-set reader 1 b)
		(elem-set reader 2 8))
	(elem-set reader 2 (dec (elem-get reader 2)))
	(logand (>> (elem-get reader 1) (elem-get reader 2)) 1))

(defun jpg-bits (reader n)
	(defq v 0)
	(times n (setq v (logior (<< v 1) (jpg-bit reader))))
	v)

(defun jpg-decode-symbol (reader table)
	; (jpg-decode-symbol reader table) -> value
	(bind '(maxcode valptr mincode vals) table)
	(defq v (jpg-bit reader) len 0 found -1)
	(while (and (< found 0) (< len 16))
		(cond
			((and (>= (elem-get maxcode len) 0) (<= v (elem-get maxcode len)))
				(setq found (elem-get vals (+ (elem-get valptr len) (- v (elem-get mincode len))))))
			(:t (setq v (logior (<< v 1) (jpg-bit reader)) len (inc len)))))
	(max found 0))

(defun jpg-extend (v n)
	;a number of n reader, as the standard has the negative ones
	(if (and (> n 0) (< v (<< 1 (dec n)))) (+ v (- 1 (<< 1 n))) v))

(defun jpg-block (reader dc_code ac_code quant prior)
	; (jpg-block reader dc_code ac_code quant prior) -> (pixels dc)
	;a block: its numbers, by their table, and turned back to 64 pixels
	(defq c (nums 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
		s (jpg-decode-symbol reader dc_code) dc (+ prior (jpg-extend (jpg-bits reader s) s)) k 1)
	(elem-set c 0 (* dc (elem-get quant 0)))
	(while (< k 64)
		(defq rs (jpg-decode-symbol reader ac_code) r (>> rs 4) sz (logand rs 15))
		(cond
			((= sz 0) (setq k (if (= r 15) (+ k 16) 64)))
			(:t (setq k (+ k r))
				(when (< k 64)
					(elem-set c (elem-get +jpg_zigzag k) (* (jpg-extend (jpg-bits reader sz) sz) (elem-get quant k)))
					(setq k (inc k))))))
	;the rows by K, then the columns: t x v is row v of c dot K x
	(defq t (apply (const nums) (map (lambda (i) (nums-dot (slice c (* (% i 8) 8) (+ (* (% i 8) 8) 8)) (elem-get *jpg_k* (/ i 8))))
			(range 0 64))))
	(list (apply (const nums) (map (lambda (i)
			(max 0 (min 255 (+ 128 (>>> (+ (nums-dot (elem-get *jpg_k* (/ i 8)) (slice t (* (% i 8) 8) (+ (* (% i 8) 8) 8))) 524288) 20)))))
		(range 0 64)))
		dc))

(defun jpg-decode (in out info)
	; (jpg-decode in out info)
	;the scan, from where the stream is, a row of MCUs at a time, each
	;row of pixels written blue green red alpha
	(bind '(width height comps scan quant dc ac restart) info)
	(defq hmax (reduce (# (max %0 (second %1))) comps 1) vmax (reduce (# (max %0 (third %1))) comps 1)
		mcu_w (* 8 hmax) mcu_h (* 8 vmax) mcus_x (/ (+ width mcu_w -1) mcu_w) mcus_y (/ (+ height mcu_h -1) mcu_h)
		;each component: its sampling, its codes and its table, and the last dc
		parts (map (lambda ((id h v tq))
				(defq tables (some (# (if (= (first %0) id) %0)) scan))
				(list h v (jpg-code (elem-get dc (second tables))) (jpg-code (elem-get ac (third tables))) (elem-get quant tq)))
			comps)
		lasts (map (lambda (_) 0) comps) reader (list in 0 0 :nil) done 0 my 0)
	(while (< my mcus_y)
		(task-slice)
		;the planes of this row of MCUs, a nums each, its width in samples
		(defq planes (map (lambda ((h v &rest _)) (apply (const nums) (map (lambda (_) 0) (range 0 (* mcus_x h 8 v 8))))) parts)
			mx 0)
		(while (< mx mcus_x)
			(when (and (> restart 0) (> done 0) (= (% done restart) 0))
				;a restart: to the next byte, past the marker, which may not have
				;been come to yet, the dc from 0
				(elem-set reader 2 0)
				(unless (elem-get reader 3) (read-char in) (read-char in))
				(elem-set reader 3 :nil)
				(setq lasts (map (lambda (_) 0) comps)))
			(each (lambda ((h v dc_code ac_code q) plane)
					(defq ci (!) pw (* mcus_x h 8))
					(each (lambda (by)
						(each (lambda (bx)
							(bind '(pixels prior) (jpg-block reader dc_code ac_code q (elem-get lasts ci)))
							(elem-set lasts ci prior)
							(defq x0 (* (+ (* mx h) bx) 8) y0 (* by 8))
							(each (lambda (i) (elem-set plane (+ (* (+ y0 (/ i 8)) pw) x0 (% i 8)) (elem-get pixels i))) (range 0 64)))
							(range 0 h)))
						(range 0 v)))
				parts planes)
			(setq mx (inc mx) done (inc done)))
		;its rows of pixels, those inside the picture
		(each (lambda (row)
				(defq y (+ (* my mcu_h) row) line (cap (* width 4) (list)))
				(when (< y height)
					(each (lambda (x)
							(defq s (map (lambda ((h v &rest _) plane)
									(elem-get plane (+ (* (/ (* row v) vmax) (* mcus_x h 8)) (/ (* x h) hmax))))
								parts planes))
							(cond
								((= (length s) 1) (push line (first s) (first s) (first s) 255))
								(:t (bind '(yy cb cr) s)
									(setq cb (- cb 128) cr (- cr 128))
									(push line (max 0 (min 255 (+ yy (>>> (* 116130 cb) 16))))
										(max 0 (min 255 (- yy (>>> (+ (* 22554 cb) (* 46802 cr)) 16))))
										(max 0 (min 255 (+ yy (>>> (* 91881 cr) 16))))
										255))))
						(range 0 width))
					(write-blk out (apply (const cat) (cat (list "") (map (const char) line))))))
			(range 0 mcu_h))
		(setq my (inc my))))

(defun jpg-stage (src downstream_mbox info)
	;the decode, a stage reading the stream from where the head left it
	(str `(progn (import "lib/image/jpg.inc")
		(jpg-decode (obj-ref ,(weak-ref src)) (out-stream (hex-decode ,(hex-encode downstream_mbox))) ',info))))

(defun jpg-stage-pixmap (pixmap handshake_mbox)
	(str `(progn (import "gui/pixmap/lisp.inc")
		(mail-send (hex-decode ,(hex-encode handshake_mbox))
			(in-mbox (defq pixmap (obj-ref ,(weak-ref pixmap)) in (in-stream))))
		(mail-send (hex-decode ,(hex-encode handshake_mbox))
			(str (weak-ref (pixmap-read pixmap in 32)))))))

(defun JPG-load (stream)
	; (JPG-load stream) -> :nil | canvas
	(defq canvas :nil)
	(when (and (defq head (jpg-head stream)) (defq comps (. head :find :comps))
			(or (= (length comps) 1) (= (length comps) 3)))
		(defq w (. head :find :width) h (. head :find :height)
			canvas (Canvas w h 1) pixmap (getf canvas +canvas_pixmap 0) handshake_mbox (mail-mbox)
			;what the decode needs, numbers and lists of them, that go in its form
			info (list w h comps (. head :find :scan) (map (# (if %0 (apply (const list) %0))) (. head :find :quant))
				(. head :find :dc) (. head :find :ac) (. head :find :restart)))
		(setf pixmap +pixmap_type 32 0)
		(open-child (jpg-stage-pixmap pixmap handshake_mbox) +kn_call_pin)
		(open-child (jpg-stage stream (mail-read handshake_mbox) info) +kn_call_pin)
		(if (eql (mail-read handshake_mbox) (const (str :nil))) (setq canvas :nil)))
	canvas)

;the canvas is told what this reads
(canvas-loader ".jpg" JPG-info JPG-load)
(canvas-loader ".jpeg" JPG-info JPG-load)

;module
(export-symbols '(JPG-load JPG-info jpg-decode *jpg_k* +jpg_zigzag))
(env-pop)
