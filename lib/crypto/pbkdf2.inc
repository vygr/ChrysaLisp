;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A key from a password, PBKDF2 of RFC 8018, with HMAC SHA-256.
;
; (pbkdf2-sha256 password salt count size) -> a str of size bytes,
; to use as a key. The same four give the same key, on any machine.
;
; A password is short and can be guessed, so the key is made slow
; to work out, count times round, which costs the one who knows
; the password once and one who is guessing it every guess. 100,000
; is a fair count today. The salt is bytes kept with what the key
; is for, not secret, (random-bytes 16) when the password is set,
; so that the same password is not the same key twice.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./sha256.inc")

;what follows a hash of 32 bytes in the block it is the message of, when
;a block of 64 has gone before it. A bit, zeros, and 96 bytes as bits, the
;top byte first
(defq +pbkdf2_tail (cat (ascii-char 0x80) (slice +sha256_zeros 0 29) (ascii-char 3) (ascii-char 0)))

(defun pbkdf2-bytes (state)
	; (pbkdf2-bytes state) -> str
	;the 8 numbers of a state as the 32 bytes of the hash, each number the
	;top byte first
	(defq out (str-alloc 32))
	(each (# (defq word (get-uint state %0))
		(set-int out %0 (logior (<< (logand word 0xff) 24) (<< (logand word 0xff00) 8)
			(logand (>> word 8) 0xff00) (>> word 24))))
		(static-q (0 4 8 12 16 20 24 28)))
	out)

(defun pbkdf2-sha256 (password salt count size)
	; (pbkdf2-sha256 password salt count size) -> str
	(unless (and (str? password) (str? salt) (num? count) (num? size) (> count 0) (>= size 0))
		(throw "pbkdf2-sha256, a password, a salt, a count over 0 and a size !" (list count size)))
	;the password as the key of the HMAC. The hash of the key with each of
	;its two pads is the same every time round, so it is worked out the
	;once, and each time round is two blocks from there
	(if (> (length password) 64) (setq password (sha256 password)))
	(setq password (cat password (slice +sha256_zeros 0 (- 64 (length password)))))
	(defq inner (sha256-blocks (first (sha256-start))
			(apply (const cat) (map (# (char (logxor (code %0) 0x36))) password)) 0 1)
		outer (sha256-blocks (first (sha256-start))
			(apply (const cat) (map (# (char (logxor (code %0) 0x5c))) password)) 0 1)
		out (list) block_num 0 rounds 0)
	(while (< (* block_num 32) size)
		(setq block_num (inc block_num))
		;the first time round is of the salt and the number of the block,
		;which is of any length, so it goes the long way
		(defq u (pbkdf2-bytes (sha256-blocks (cat outer)
				(cat (sha256-end (sha256-add (list (cat inner) "" 64)
					(cat salt (char (>> block_num 24)) (char (logand (>> block_num 16) 0xff))
						(char (logand (>> block_num 8) 0xff)) (char (logand block_num 0xff)))))
					+pbkdf2_tail) 0 1))
			total (cat u))
		;each time after is of the 32 bytes of the time before, and all of
		;them are xored together
		(times (dec count)
			(setq u (pbkdf2-bytes (sha256-blocks (cat outer)
				(cat (pbkdf2-bytes (sha256-blocks (cat inner) (cat u +pbkdf2_tail) 0 1))
					+pbkdf2_tail) 0 1)))
			(each (# (set-long total %0 (logxor (get-long total %0) (get-long u %0))))
				(static-q (0 8 16 24)))
			;a long count gives the other tasks of the node a turn
			(if (= (logand (setq rounds (inc rounds)) 1023) 0) (task-slice)))
		(push out total))
	(slice (apply (const cat) (cat (list "") out)) 0 size))
