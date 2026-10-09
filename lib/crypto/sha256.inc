;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SHA-256, the hash of FIPS 180-4.
;
; (sha256 data) -> the 32 bytes of the hash of a str.
;
; For what does not come all at once, a file read a part at a time,
; (sha256-start), (sha256-add ctx data) as often as wanted, then
; (sha256-end ctx).
;
; The work on each block of 64 bytes is native code,
; lib/crypto/lisp.vp. The padding, and all that is done once, is here.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ffi (path-to-absolute "./sha256_blocks") sha256-blocks)
; (sha256-blocks state data offset count) -> state

;64 bytes of 0, for the padding
(defq +sha256_zeros (apply (const cat) (map (lambda (&) (char 0 8)) (range 0 8))))

;how many blocks the native code is given at a time, 64KB, between which
;the other tasks of the node have a turn
(defq +sha256_slice 1024)

(defun sha256-start ()
	; (sha256-start) -> ctx
	;a hash with nothing in it yet. The state, the first 32 bits of the
	;fractions of the square roots of the first 8 primes, the bytes that
	;do not yet make a block, and how many bytes there have been.
	(list (apply (const cat) (map (# (char %0 4)) (static-q (0x6a09e667 0xbb67ae85 0x3c6ef372 0xa54ff53a
		0x510e527f 0x9b05688c 0x1f83d9ab 0x5be0cd19)))) "" 0))

(defun sha256-add (ctx data)
	; (sha256-add ctx data) -> ctx
	;more of what is being hashed
	(bind '(state pending total) ctx)
	(elem-set ctx 2 (+ total (length data)))
	(if (nempty? pending) (setq data (cat pending data)))
	(defq blocks (/ (length data) 64) at 0)
	(while (> blocks 0)
		(defq some_blocks (min blocks +sha256_slice))
		(sha256-blocks state data at some_blocks)
		(setq at (+ at (* some_blocks 64)) blocks (- blocks some_blocks))
		(if (> blocks 0) (task-slice)))
	(elem-set ctx 1 (slice data at -1))
	ctx)

(defun sha256-end (ctx)
	; (sha256-end ctx) -> str
	;the hash of all that was added, 32 bytes. The ctx is done with.
	(bind '(state pending total) ctx)
	;a bit that is set, zeros till 8 bytes short of a whole block, and
	;how many bits there were, the top byte first
	(defq bits (* total 8) tail (cat pending (ascii-char 0x80)))
	(sha256-blocks state
		(setq tail (cat tail (slice +sha256_zeros 0 (% (- 120 (length tail)) 64))
			(apply (const cat) (map (# (char (logand (>> bits %0) 0xff)))
				(static-q (56 48 40 32 24 16 8 0))))))
		0 (/ (length tail) 64))
	;the state, each number the top byte first
	(apply (const cat) (map (lambda (i) (defq word (get-uint state (* i 4)))
		(apply (const cat) (map (# (char (logand (>> word %0) 0xff))) (static-q (24 16 8 0)))))
		(range 0 8))))

(defun sha256 (data)
	; (sha256 data) -> str
	;the hash of a str, 32 bytes
	(sha256-end (sha256-add (sha256-start) data)))

(defun hmac-sha256 (key data)
	; (hmac-sha256 key data) -> str
	;the hash of a str with a key, HMAC of RFC 2104, 32 bytes. Only one
	;who has the key can make it, or check it. A key longer than a block
	;is hashed first, and one that is shorter is made up with 0.
	(if (> (length key) 64) (setq key (sha256 key)))
	(setq key (cat key (slice +sha256_zeros 0 (- 64 (length key)))))
	(sha256 (cat (apply (const cat) (map (# (char (logxor (code %0) 0x5c))) key))
		(sha256-end (sha256-add (sha256-add (sha256-start)
			(apply (const cat) (map (# (char (logxor (code %0) 0x36))) key))) data)))))
