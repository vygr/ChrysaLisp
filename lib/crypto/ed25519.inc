;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Ed25519, the signature of RFC 8032.
;
; A signature says that whoever holds a secret made it, of this
; message and no other, and anyone can check that with the public
; half of the secret, which gives nothing away. It is what a key
; that is shared can not do, with one of those whoever can check
; can also make.
;
; (ed25519-public seed) -> the public key, 32 bytes, of a secret
; seed of 32 bytes, which is kept, (random-bytes 32) makes one.
; (ed25519-sign seed message) -> the signature, 64 bytes.
; (ed25519-verify public message signature) -> :t | :nil.
;
; The arithmetic is of numbers less than the prime 2 to the 255
; less 19, a field, each kept as 16 numbers of 16 bits in a nums,
; so that a product of two of them, and 16 such added, fits in 64
; bits. It follows TweetNaCl, which is small enough to be checked
; against by eye.
;
; It is Lisp but for the hash and the multiply of two numbers of the
; field, which is nearly all of the work. It does not take the same time
; whatever the secret is, as a careful one in C does, so it is not
; for signing where one who is timing it could be watching.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./sha512.inc")

;the numbers of the curve, from their definitions, by Python. d, twice d,
;the base point, and a square root of -1
(defq *ed_zero* (nums 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	*ed_one* (nums 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	*ed_d* (nums 30883 4953 19914 30187 55467 16705 2637 112 59544 30585 16505 36039 65139 11119 27886 20995)
	*ed_d2* (nums 61785 9906 39828 60374 45398 33411 5274 224 53552 61171 33010 6542 64743 22239 55772 9222)
	*ed_x* (nums 54554 36645 11616 51542 42930 38181 51040 26924 56412 64982 57905 49316 21502 52590 14035 8553)
	*ed_y* (nums 26200 26214 26214 26214 26214 26214 26214 26214 26214 26214 26214 26214 26214 26214 26214 26214)
	*ed_i* (nums 41136 18958 6951 50414 58488 44335 6150 12099 55207 15867 153 11085 57099 20417 9344 11139)
	;the order of the base point, a byte at a time, the low byte first
	*ed_l* (nums 237 211 245 92 26 99 18 88 214 156 247 162 222 249 222 20 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 16)
	*ed_16* (nums 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
	*ed_32* (nums 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31))

(defun ed-copy (a)
	; (ed-copy a) -> a new number of the field, the same as a
	(nums-add a *ed_zero*))

(defun ed-car (o)
	; (ed-car o) -> o
	;each part down to 16 bits, what is over going on to the next, and
	;what is over the top coming back in at the bottom 38 times
	(each (lambda (i)
		(defq v (+ (elem-get o i) 65536) c (>>> v 16))
		(elem-set o i (- v (<< c 16)))
		(if (< i 15)
			(elem-set o (inc i) (+ (elem-get o (inc i)) (dec c)))
			(elem-set o 0 (+ (elem-get o 0) (* 38 (dec c))))))
		*ed_16*)
	o)

(ffi (path-to-absolute "./gf25519_mul") ed-mul)
; (ed-mul o a b) -> o
;o is a times b. o can be a, or b, or both. Native code, lib/crypto/lisp.vp,
;it is nearly all of the work of a signature

(defun ed-mul-ref (o a b)
	; (ed-mul-ref o a b) -> o
	;what (ed-mul) does, in Lisp, to check it by
	(defq acc (nums 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
	(each (lambda (i)
		(defq ai (elem-get a i))
		(each (lambda (j)
			(elem-set acc (+ i j) (+ (elem-get acc (+ i j)) (* ai (elem-get b j)))))
			*ed_16*))
		*ed_16*)
	(each! (lambda (i)
		(elem-set o i (+ (elem-get acc i) (* 38 (elem-get acc (+ i 16))))))
		(list *ed_16*) 0 15)
	(elem-set o 15 (elem-get acc 15))
	(ed-car (ed-car o)))

(defun ed-inv (a)
	; (ed-inv a) -> 1 over a, a new number
	(defq c (ed-copy a) n 254)
	(while (>= (setq n (dec n)) 0)
		(ed-mul c c c)
		(unless (or (= n 2) (= n 4)) (ed-mul c c a)))
	c)

(defun ed-pow2523 (a)
	; (ed-pow2523 a) -> a to the power of (p-5)/8, a new number
	(defq c (ed-copy a) n 251)
	(while (>= (setq n (dec n)) 0)
		(ed-mul c c c)
		(unless (= n 1) (ed-mul c c a)))
	c)

(defun ed-pack (n)
	; (ed-pack n) -> str
	;a number of the field as its 32 bytes, the low byte first, and less
	;than the prime, the prime is taken off twice if it will come off
	(defq u (ed-copy n) m (ed-copy *ed_zero*))
	(ed-car (ed-car (ed-car u)))
	(times 2
		(elem-set m 0 (- (elem-get u 0) 0xffed))
		(each! (lambda (i)
			(elem-set m i (- (elem-get u i) 0xffff (logand (>>> (elem-get m (dec i)) 16) 1)))
			(elem-set m (dec i) (logand (elem-get m (dec i)) 0xffff)))
			(list *ed_16*) 1 15)
		(elem-set m 15 (- (elem-get u 15) 0x7fff (logand (>>> (elem-get m 14) 16) 1)))
		(defq borrow (logand (>>> (elem-get m 15) 16) 1))
		(elem-set m 14 (logand (elem-get m 14) 0xffff))
		;it came off with nothing borrowed, so what is left is the number
		(when (= borrow 0)
			(defq keep u) (setq u m m keep)))
	(apply (const cat) (map (# (cat (char (logand %0 0xff)) (char (logand (>> %0 8) 0xff)))) u)))

(defun ed-unpack (bytes)
	; (ed-unpack bytes) -> a number of the field, from its 32 bytes
	(defq o (ed-copy *ed_zero*))
	(each (lambda (i)
		(elem-set o i (+ (code bytes 1 (* i 2)) (<< (code bytes 1 (inc (* i 2))) 8))))
		*ed_16*)
	(elem-set o 15 (logand (elem-get o 15) 0x7fff))
	o)

(defun ed-odd? (a)
	; (ed-odd? a) -> 0 | 1
	(logand (code (ed-pack a)) 1))

(defun ed-add (p q)
	; (ed-add p q) -> p
	;the point p and the point q, added, into p. A point is 4 numbers
	(bind '(p0 p1 p2 p3) p)
	(bind '(q0 q1 q2 q3) q)
	(defq a (nums-sub p1 p0) u (nums-sub q1 q0) b (nums-add p0 p1) c (ed-copy *ed_zero*) d (ed-copy *ed_zero*))
	(ed-mul a a u)
	(nums-add q0 q1 u)
	(ed-mul b b u)
	(ed-mul c p3 q3)
	(ed-mul c c *ed_d2*)
	(ed-mul d p2 q2)
	(nums-add d d d)
	(defq e (nums-sub b a) f (nums-sub d c) g (nums-add d c) h (nums-add b a))
	(ed-mul p0 e f)
	(ed-mul p1 h g)
	(ed-mul p2 g f)
	(ed-mul p3 e h)
	p)

(defun ed-point (x y)
	; (ed-point x y) -> a point, of its x and y
	(list (ed-copy x) (ed-copy y) (ed-copy *ed_one*) (ed-mul (ed-copy *ed_zero*) x y)))

(defun ed-scalarmult (q scalar)
	; (ed-scalarmult q scalar) -> point
	;the point q added to itself the number of times the 32 bytes of the
	;scalar say, the low byte first. q is not changed
	(defq p (list (ed-copy *ed_zero*) (ed-copy *ed_one*) (ed-copy *ed_one*) (ed-copy *ed_zero*))
		q (map (const ed-copy) q) n 256)
	(while (>= (setq n (dec n)) 0)
		(if (= 0 (logand (>> (code scalar 1 (>> n 3)) (logand n 7)) 1))
			(progn (ed-add q p) (ed-add p p))
			(progn (ed-add p q) (ed-add q q))))
	p)

(defun ed-encode (p)
	; (ed-encode p) -> str
	;a point as 32 bytes, its y, with whether its x is odd in the top bit
	(defq zi (ed-inv (third p)) x (ed-mul (ed-copy *ed_zero*) (first p) zi)
		y (ed-pack (ed-mul (ed-copy *ed_zero*) (second p) zi)))
	(cat (slice y 0 31) (char (logxor (code y 1 31) (<< (ed-odd? x) 7)))))

(defun ed-decode-neg (bytes)
	; (ed-decode-neg bytes) -> :nil | point
	;the point of 32 bytes, with its x the other way, which is what a
	;check wants. :nil if they are not a point of the curve
	(defq y (ed-unpack bytes) z (ed-copy *ed_one*) y2 (ed-mul (ed-copy *ed_zero*) y y)
		den (ed-mul (ed-copy *ed_zero*) y2 *ed_d*) num (nums-sub y2 z))
	(nums-add z den den)
	(defq den2 (ed-mul (ed-copy *ed_zero*) den den) den4 (ed-mul (ed-copy *ed_zero*) den2 den2)
		den6 (ed-mul (ed-copy *ed_zero*) den4 den2) x (ed-mul (ed-copy *ed_zero*) den6 num))
	(ed-mul x x den)
	(setq x (ed-pow2523 x))
	(ed-mul x x num) (ed-mul x x den) (ed-mul x x den) (ed-mul x x den)
	(defq chk (ed-mul (ed-copy *ed_zero*) x x))
	(ed-mul chk chk den)
	(when (nql (ed-pack chk) (ed-pack num))
		(ed-mul x x *ed_i*)
		(ed-mul chk x x)
		(ed-mul chk chk den))
	(when (eql (ed-pack chk) (ed-pack num))
		(if (= (ed-odd? x) (>> (code bytes 1 31) 7)) (setq x (nums-sub *ed_zero* x)))
		(list x y z (ed-mul (ed-copy *ed_zero*) x y))))

(defun ed-mod-l (x)
	; (ed-mod-l x) -> str
	;a list of 64 numbers, the bytes of a number, the low one first, less
	;every multiple of the order of the base point that will come off it,
	;as 32 bytes. The list is changed
	(defq i 64 carry 0)
	(while (>= (setq i (dec i)) 32)
		(setq carry 0)
		(defq xi (elem-get x i) j (- i 33) end (- i 12))
		(while (< (setq j (inc j)) end)
			(defq v (+ (elem-get x j) (- carry (* 16 xi (elem-get *ed_l* (- j (- i 32)))))))
			(setq carry (>>> (+ v 128) 8))
			(elem-set x j (- v (<< carry 8))))
		(elem-set x j (+ (elem-get x j) carry))
		(elem-set x i 0))
	(setq carry 0)
	(each (lambda (j)
		(defq v (+ (elem-get x j) (- carry (* (>>> (elem-get x 31) 4) (elem-get *ed_l* j)))))
		(setq carry (>>> v 8))
		(elem-set x j (logand v 255)))
		*ed_32*)
	(each (lambda (j) (elem-set x j (- (elem-get x j) (* carry (elem-get *ed_l* j))))) *ed_32*)
	(apply (const cat) (map (lambda (j)
		(defq v (elem-get x j))
		(elem-set x (inc j) (+ (elem-get x (inc j)) (>>> v 8)))
		(char (logand v 255))) *ed_32*)))

(defun ed-reduce (hash)
	; (ed-reduce hash) -> str
	;the 64 bytes of a hash as a number, less the order of the base point
	;as often as it will come off, 32 bytes
	(ed-mod-l (map (const code) hash)))

(defun ed-secret (seed)
	; (ed-secret seed) -> (scalar prefix)
	;the number a seed stands for, and the other half of its hash
	(defq h (sha512 seed))
	(list (cat (char (logand (code h) 248)) (slice h 1 31)
			(char (logior (logand (code h 1 31) 127) 64)))
		(slice h 32 64)))

(defun ed25519-public (seed)
	; (ed25519-public seed) -> str
	;the public key of a secret seed of 32 bytes, 32 bytes
	(unless (and (str? seed) (= (length seed) 32)) (throw "ed25519-public, a seed of 32 bytes !" seed))
	(ed-encode (ed-scalarmult (ed-point *ed_x* *ed_y*) (first (ed-secret seed)))))

(defun ed25519-sign (seed message)
	; (ed25519-sign seed message) -> str
	;the signature of a message under a secret seed, 64 bytes
	(unless (and (str? seed) (= (length seed) 32) (str? message))
		(throw "ed25519-sign, a seed of 32 bytes and a message !" seed))
	(bind '(scalar prefix) (ed-secret seed))
	(defq base (ed-point *ed_x* *ed_y*) public (ed-encode (ed-scalarmult base scalar))
		r (ed-reduce (sha512 (cat prefix message)))
		big_r (ed-encode (ed-scalarmult base r))
		h (ed-reduce (sha512 (cat big_r public message)))
		x (cat (map (const code) r) (map (lambda (_) 0) *ed_32*)))
	;r, and h times the secret number, less the order
	(each (lambda (i)
		(defq hi (code h 1 i))
		(each (lambda (j)
			(elem-set x (+ i j) (+ (elem-get x (+ i j)) (* hi (code scalar 1 j)))))
			*ed_32*))
		*ed_32*)
	(cat big_r (ed-mod-l x)))

(defun ed25519-verify (public message signature)
	; (ed25519-verify public message signature) -> :nil | :t
	;is it the signature of this message, by the holder of the secret
	;that this is the public key of ?
	(when (and (str? public) (= (length public) 32) (str? message)
			(str? signature) (= (length signature) 64)
			;the second half has to be less than the order, or one
			;signature could be written many ways
			(eql :less (some! (lambda (j)
				(defq s (code signature 1 (+ 32 j)) l (elem-get *ed_l* j))
				(cond ((< s l) :less) ((> s l) :more))) (list *ed_32*) :nil 32 0))
			(defq q (ed-decode-neg public)))
		(defq h (ed-reduce (sha512 (cat (slice signature 0 32) public message)))
			p (ed-scalarmult q h))
		(ed-add p (ed-scalarmult (ed-point *ed_x* *ed_y*) (slice signature 32 64)))
		(eql (ed-encode p) (slice signature 0 32))))
