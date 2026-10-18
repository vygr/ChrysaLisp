;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SHA-512, the hash of FIPS 180-4 with 64 bit words.
;
; (sha512 data) -> the 64 bytes of the hash of a str.
; (sha384 data) -> the 48 of SHA-384, the same but for where it starts.
;
; For what does not come all at once, (sha512-start),
; (sha512-add ctx data) as often as wanted, then (sha512-end ctx).
;
; It is what Ed25519 signs with. The work on each block of 128
; bytes is native code, lib/crypto/lisp.vp. The padding, and all
; that is done once, is here.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ffi (path-to-absolute "./sha512_blocks") sha512-blocks)
; (sha512-blocks state data offset count) -> state

;128 bytes of 0, for the padding
(defq +sha512_zeros (apply (const cat) (map (lambda (&) (char 0 8)) (range 0 16))))

;how many blocks the native code is given at a time, 64KB, between which
;the other tasks of the node have a turn
(defq +sha512_slice 512)

(defun sha512-start ()
	; (sha512-start) -> ctx
	;a hash with nothing in it yet. The state, the first 64 bits of the
	;fractions of the square roots of the first 8 primes, the bytes that
	;do not yet make a block, and how many bytes there have been.
	(list (apply (const cat) (map (# (char %0 8)) (static-q (7640891576956012808 -4942790177534073029 4354685564936845355 -6534734903238641935 5840696475078001361 -7276294671716946913 2270897969802886507 6620516959819538809)))) "" 0))

(defun sha512-add (ctx data)
	; (sha512-add ctx data) -> ctx
	;more of what is being hashed
	(bind '(state pending total) ctx)
	(elem-set ctx 2 (+ total (length data)))
	(if (nempty? pending) (setq data (cat pending data)))
	(defq blocks (/ (length data) 128) at 0)
	(while (> blocks 0)
		(defq some_blocks (min blocks +sha512_slice))
		(sha512-blocks state data at some_blocks)
		(setq at (+ at (* some_blocks 128)) blocks (- blocks some_blocks))
		(if (> blocks 0) (task-slice)))
	(elem-set ctx 1 (slice data at -1))
	ctx)

(defun sha512-end (ctx)
	; (sha512-end ctx) -> str
	;the hash of all that was added, 64 bytes. The ctx is done with.
	(bind '(state pending total) ctx)
	;a bit that is set, zeros till 16 bytes short of a whole block, and
	;how many bits there were, in 16 bytes, the top byte first
	(defq bits (* total 8) tail (cat pending (ascii-char 0x80)))
	(sha512-blocks state
		(setq tail (cat tail (slice +sha512_zeros 0 (% (- 240 (length tail)) 128)) (char 0 8)
			(char-be bits 8)))
		0 (/ (length tail) 128))
	;the state, each number the top byte first
	(apply (const cat) (map (lambda (i) (defq word (get-long state (* i 8)))
		(char-be word 8))
		(range 0 8))))

(defun sha512 (data)
	; (sha512 data) -> str
	;the hash of a str, 64 bytes
	(sha512-end (sha512-add (sha512-start) data)))

(defun sha384-start ()
	; (sha384-start) -> ctx
	;SHA-384 is SHA-512 from another state, the first 64 bits of the
	;fractions of the square roots of the 9th to the 16th primes, added
	;to as SHA-512 is, (sha512-add)
	(list (apply (const cat) (map (# (char %0 8)) (static-q (-3766243637369397544 7105036623409894663 -7973340178411365097 1526699215303891257 7436329637833083697 -8163818279084223215 -2662702644619276377 5167115440072839076)))) "" 0))

(defun sha384-end (ctx)
	; (sha384-end ctx) -> str
	;the hash, the first 48 bytes of what SHA-512 ends with
	(slice (sha512-end ctx) 0 48))

(defun sha384 (data)
	; (sha384 data) -> str
	;the hash of a str, 48 bytes
	(sha384-end (sha512-add (sha384-start) data)))
