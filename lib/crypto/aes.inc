;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; AES, the block cipher of FIPS 197, the forward way only, as GCM and
; counter mode use it.
;
; (aes-key key) -> keys, the round keys of a key of 16 or 32 bytes,
; AES-128 or AES-256.
; (aes-block keys block) -> the 16 bytes of a block encrypted.
; (aes-ctr keys counter data) -> the data, each byte xored with the
; next of the stream of blocks that counter, 16 bytes, and the ones
; after it make, the last 4 bytes of it counted up, high first, as
; GCM has it. Done again with the same counter it gives the data back.
;
; The state is four columns, each a number of 32 bits, its first byte
; the low one. A round is four tables of what a byte becomes, looked up
; and xored, the tables made here once. The work of aes-ctr is native
; code, lib/crypto/lisp.vp; aes-block is Lisp, for what is done once a
; key, and to hold the native code to.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ffi (path-to-absolute "./aes_ctr_xor") aes-ctr-xor)
; (aes-ctr-xor keys tables counter data out offset length) -> out

;module
(env-push)

;how many bytes the native code is given at a time, a whole number of
;blocks, between which the other tasks of the node have a turn
(defq +aes_slice 65536)

(defun aes-xtime (b)
	;b times x in the field of AES
	(logand (logxor (<< b 1) (if (> b 127) 0x1b 0)) 0xff))

(defun aes-sbox ()
	; (aes-sbox) -> str
	;what each byte becomes, its inverse in the field turned by the affine
	;map of the standard: 3 is a generator, so its powers and their
	;logarithms give each inverse
	(defq exp (nums) lg (cap 256 (nums)) x 1)
	(each (lambda (&) (push lg 0)) (range 0 256))
	(each (lambda (i)
			(push exp x)
			(elem-set lg x i)
			(setq x (logxor x (aes-xtime x))))
		(range 0 255))
	(apply (const cat) (map (lambda (b)
			(defq inv (if (= b 0) 0 (elem-get exp (% (- 255 (elem-get lg b)) 255)))
				out inv)
			(each (lambda (k)
					(setq inv (logand (logior (<< inv 1) (>> inv 7)) 0xff)
						out (logxor out inv)))
				(range 0 4))
			(char (logxor out 0x63)))
		(range 0 256))))

;the box, a str of 256 bytes
(defq +aes_box (aes-sbox))

(defun aes-tables ()
	; (aes-tables) -> str
	;the four tables of a round, 256 numbers of 4 bytes each, then the box
	;itself, as the native code wants them. A byte of row r of a column
	;goes to the four rows as the column mixing has it
	(defq out (list))
	(each (lambda (rot)
			(each (lambda (b)
					(defq s (code +aes_box 1 b) s2 (aes-xtime s) s3 (logxor s2 s)
						w (list s2 s s s3))
					;the row it is in turns the four round
					(push out (char (logior
						(elem-get w (% (- 4 rot) 4))
						(<< (elem-get w (% (- 5 rot) 4)) 8)
						(<< (elem-get w (% (- 6 rot) 4)) 16)
						(<< (elem-get w (% (- 7 rot) 4)) 24)) 4)))
				(range 0 256)))
		(range 0 4))
	(push out +aes_box)
	(apply (const cat) out))

(defq +aes_tables (aes-tables))

(defun aes-word (data at)
	(code data 4 at))

(defun aes-box (b)
	(code +aes_box 1 b))

(defun aes-sub (w)
	;each byte of a word through the box
	(logior (aes-box (logand w 0xff))
		(<< (aes-box (logand (>> w 8) 0xff)) 8)
		(<< (aes-box (logand (>> w 16) 0xff)) 16)
		(<< (aes-box (logand (>> w 24) 0xff)) 24)))

(defun aes-key (key)
	; (aes-key key) -> keys
	;the round keys, 4 numbers of 32 bits a round, as a str
	(defq nk (/ (length key) 4) rounds (+ nk 6) w (list) rcon 1)
	(each (# (push w (aes-word key (* %0 4)))) (range 0 nk))
	(each (lambda (i)
			(defq tw (elem-get w (dec i)))
			(cond
				((= (% i nk) 0)
					;turned a byte, each through the box, and the round's constant
					(setq tw (logxor (aes-sub (logior (>> tw 8) (<< (logand tw 0xff) 24))) rcon)
						rcon (aes-xtime rcon)))
				((and (> nk 6) (= (% i nk) 4))
					(setq tw (aes-sub tw))))
			(push w (logxor (elem-get w (- i nk)) tw)))
		(range nk (* 4 (inc rounds))))
	(apply (const cat) (map (# (char %0 4)) w)))

(defun aes-table (n b)
	(code +aes_tables 4 (* (+ (* n 256) b) 4)))

(defun aes-block (keys block)
	; (aes-block keys block) -> str
	(defq rounds (dec (/ (length keys) 16))
		s (map (# (logxor (aes-word block (* %0 4)) (aes-word keys (* %0 4)))) (range 0 4)))
	(each (lambda (r)
			(setq s (map (lambda (c)
					(logxor (aes-table 0 (logand (elem-get s c) 0xff))
						(aes-table 1 (logand (>> (elem-get s (% (+ c 1) 4)) 8) 0xff))
						(aes-table 2 (logand (>> (elem-get s (% (+ c 2) 4)) 16) 0xff))
						(aes-table 3 (logand (>> (elem-get s (% (+ c 3) 4)) 24) 0xff))
						(aes-word keys (* (+ (* r 4) c) 4))))
				(range 0 4))))
		(range 1 rounds))
	(apply (const cat) (map (lambda (c)
			(char (logxor (logior (aes-box (logand (elem-get s c) 0xff))
					(<< (aes-box (logand (>> (elem-get s (% (+ c 1) 4)) 8) 0xff)) 8)
					(<< (aes-box (logand (>> (elem-get s (% (+ c 2) 4)) 16) 0xff)) 16)
					(<< (aes-box (logand (>> (elem-get s (% (+ c 3) 4)) 24) 0xff)) 24))
				(aes-word keys (* (+ (* rounds 4) c) 4))) 4))
		(range 0 4))))

(defun aes-count (counter n)
	; (aes-count counter n) -> str
	;the counter n blocks on, its last 4 bytes counted up, high first
	(cat (slice counter 0 12) (char-be (logand (+ (code-be counter 4 12) n) 0xffffffff) 4)))

(defun aes-ctr (keys counter data)
	; (aes-ctr keys counter data) -> str
	(defq size (length data) out (str-alloc size) at 0)
	(while (< at size)
		(defq some_bytes (min (- size at) +aes_slice))
		(aes-ctr-xor keys +aes_tables (aes-count counter (/ at 16)) data out at some_bytes)
		(setq at (+ at some_bytes))
		(if (< at size) (task-slice)))
	out)

;module
(export-symbols '(aes-key aes-block aes-ctr aes-count))
(env-pop)
