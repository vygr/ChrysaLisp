;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Poly1305, the authenticator of RFC 8439.
;
; (poly1305 key data) -> a tag of 16 bytes for a str, that only one
; who has the key could have made.
;
; The key is a str of 32 bytes and is for the one message. A key
; used for two lets it be worked out. lib/crypto/aead.inc makes a
; new one for each message from a cipher key and a nonce, and is
; what to use.
;
; For what does not come all at once, (poly1305-start key),
; (poly1305-add ctx data) as often as wanted, then
; (poly1305-end ctx).
;
; The work on each block of 16 bytes is native code,
; lib/crypto/lisp.vp. What is done once is here.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ffi (path-to-absolute "./poly1305_blocks") poly1305-blocks)
; (poly1305-blocks state data offset count top) -> state

;how many blocks the native code is given at a time, 64KB, between which
;the other tasks of the node have a turn
(defq +poly1305_slice 4096 +poly1305_mask 0x3ffffff
	+poly1305_zeros (cat (char 0 8) (char 0 8)))

(defun poly1305-start (key)
	; (poly1305-start key) -> ctx
	;a tag with nothing in it yet. The state the native code keeps, the
	;bytes that do not yet make a block, and the half of the key that
	;is added at the end
	(unless (and (str? key) (= (length key) 32)) (throw "Poly1305, the key is 32 bytes !" key))
	;r is the first half of the key, with some of its bits cleared, as
	;five numbers of 26 bits. The top four are kept times 5 as well
	(defq lo (logand (get-long key 0) 0x0ffffffc0fffffff)
		hi (logand (get-long key 8) 0x0ffffffc0ffffffc)
		r (list (logand lo +poly1305_mask)
			(logand (>> lo 26) +poly1305_mask)
			(logand (logior (>> lo 52) (<< hi 12)) +poly1305_mask)
			(logand (>> hi 14) +poly1305_mask)
			(>> hi 40))
		state (str-alloc 120))
	(each (# (set-long state (* (!) 8) %0))
		(cat '(0 0 0 0 0) r (map (# (* %0 5)) (rest r)) '(0)))
	(list state "" (slice key 16 32)))

(defun poly1305-add (ctx data)
	; (poly1305-add ctx data) -> ctx
	;more of what the tag is of
	(bind '(state pending &rest _) ctx)
	(if (nempty? pending) (setq data (cat pending data)))
	(defq blocks (/ (length data) 16) at 0)
	(while (> blocks 0)
		(defq some_blocks (min blocks +poly1305_slice))
		(poly1305-blocks state data at some_blocks 1)
		(setq at (+ at (* some_blocks 16)) blocks (- blocks some_blocks))
		(if (> blocks 0) (task-slice)))
	(elem-set ctx 1 (slice data at -1))
	ctx)

(defun poly1305-end (ctx)
	; (poly1305-end ctx) -> str
	;the tag of all that was added, 16 bytes. The ctx is done with.
	(bind '(state pending pad) ctx)
	;what is left is a last block, a bit above its last byte, then 0
	(when (nempty? pending)
		(poly1305-blocks state
			(cat pending (char 1) (slice +poly1305_zeros 0 (- 15 (length pending)))) 0 1 0))
	(bind '(h0 h1 h2 h3 h4) (map (# (get-long state (* %0 8))) (range 0 5)))
	;every part of h down to 26 bits, what is over going on to the next
	(defq c (>> h1 26) h1 (logand h1 +poly1305_mask)
		h2 (+ h2 c) c (>> h2 26) h2 (logand h2 +poly1305_mask)
		h3 (+ h3 c) c (>> h3 26) h3 (logand h3 +poly1305_mask)
		h4 (+ h4 c) c (>> h4 26) h4 (logand h4 +poly1305_mask)
		h0 (+ h0 (* c 5)) c (>> h0 26) h0 (logand h0 +poly1305_mask)
		h1 (+ h1 c)
		;h less the prime, which is h and 5 less 2 to the 130. If that
		;is not below 0 then h was not less than the prime, and it is
		;what is wanted
		g0 (+ h0 5) c (>> g0 26) g0 (logand g0 +poly1305_mask)
		g1 (+ h1 c) c (>> g1 26) g1 (logand g1 +poly1305_mask)
		g2 (+ h2 c) c (>> g2 26) g2 (logand g2 +poly1305_mask)
		g3 (+ h3 c) c (>> g3 26) g3 (logand g3 +poly1305_mask)
		g4 (- (+ h4 c) (<< 1 26)))
	(if (>= g4 0) (setq h0 g0 h1 g1 h2 g2 h3 g3 h4 g4))
	;as four numbers of 32 bits, to which the other half of the key is
	;added, and what is over 128 bits is let go
	(defq carry 0)
	(apply (const cat) (map (lambda (word i)
			(setq carry (+ (logand word 0xffffffff) (get-uint pad (* i 4)) (>> carry 32)))
			(char (logand carry 0xffffffff) 4))
		(list (logior h0 (<< h1 26)) (logior (>> h1 6) (<< h2 20))
			(logior (>> h2 12) (<< h3 14)) (logior (>> h3 18) (<< h4 8)))
		'(0 1 2 3))))

(defun poly1305 (key data)
	; (poly1305 key data) -> str
	;the tag of a str, 16 bytes
	(poly1305-end (poly1305-add (poly1305-start key) data)))
