;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Big numbers, of as many bits as wanted, not less than 0
;
; What a certificate is checked with: RSA is a number to a power, mod a
; number of 2048 or 4096 bits, and the curves of ECDSA are worked mod
; primes of 256 and 384.
;
; A big number is a nums of limbs of 26 bits, the lowest first. A product
; of two limbs is 52 bits, and 2048 of those added fit in 63, so a whole
; product is made with nums-dot, a limb of it at a time, against the other
; number turned round, and the carries put right after.
;
; A number is taken mod another by a digit of the quotient at a time,
; worked out from the top two limbs, and never too big, so what is taken
; away never takes it under 0. The modulus is first moved up so its top
; limb has its top bit set, (bn-modulus), which keeps those digits close.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defq +bn_bits 26 +bn_base (<< 1 26) +bn_mask (dec (<< 1 26)))

(defun bn-zeros (n)
	; (bn-zeros n) -> nums
	;n limbs of 0
	(fill (nums) n 0))

(defun bn-trim (a)
	; (bn-trim a) -> nums
	;without the limbs of 0 at the top, one left at least
	(defq top (length a))
	(while (and (> top 1) (= (elem-get a (dec top)) 0)) (-- top))
	(if (= top (length a)) a (slice a 0 top)))

(defun bn-norm (a &optional from)
	; (bn-norm a [from]) -> a
	;each limb back to 26 bits, the carry, or the borrow, taken on up from
	;limb from, and new limbs at the top for what is over. A number that
	;has gone under 0 is an error
	(setd from 0)
	(defq at from c 0 len (length a))
	(while (< at len)
		(defq v (+ (elem-get a at) c))
		(elem-set a at (logand v +bn_mask))
		(setq c (>>> v +bn_bits))
		(++ at))
	(when (/= c 0)
		(if (< c 0) (throw "bn-norm, under 0" c))
		(while (> c 0)
			(push a (logand c +bn_mask))
			(setq c (>> c +bn_bits))))
	a)

(defun bn-from-num (n)
	; (bn-from-num n) -> nums
	(bn-norm (nums n)))

(defun bn-from-bytes (data)
	; (bn-from-bytes data) -> nums
	;a number from its bytes, the top byte first, as DER and TLS have them
	(defq out (nums) acc 0 have 0 at (length data))
	(while (> at 0)
		(-- at)
		(setq acc (logior acc (<< (code data 1 at) have)) have (+ have 8))
		(when (>= have +bn_bits)
			(push out (logand acc +bn_mask))
			(setq acc (>> acc +bn_bits) have (- have +bn_bits))))
	(if (or (> have 0) (= (length out) 0)) (push out acc))
	(bn-trim out))

(defun bn-bytes (a len)
	; (bn-bytes a len) -> str
	;len bytes of a, the top byte first, 0s in front as need be
	(defq out (list) acc 0 have 0)
	(each (lambda (limb)
			(setq acc (logior acc (<< limb have)) have (+ have +bn_bits))
			(while (>= have 8)
				(push out (logand acc 0xff))
				(setq acc (>> acc 8) have (- have 8))))
		a)
	(if (> have 0) (push out acc))
	(while (< (length out) len) (push out 0))
	(apply (const cat) (map (const char) (reverse (slice out 0 len)))))

(defun bn-bits (a)
	; (bn-bits a) -> num
	;how many bits a has, to its top bit that is set
	(setq a (bn-trim a))
	(defq top (elem-get a (dec (length a))) n 0)
	(while (> top 0) (setq top (>> top 1) n (inc n)))
	(+ (* (dec (length a)) +bn_bits) n))

(defun bn-bit (a i)
	; (bn-bit a i) -> 0 | 1
	(defq limb (/ i +bn_bits))
	(if (< limb (length a)) (logand (>> (elem-get a limb) (% i +bn_bits)) 1) 0))

(defun bn-zero? (a)
	; (bn-zero? a) -> :t | :nil
	(every (# (= %0 0)) a))

(defun bn-cmp (a b)
	; (bn-cmp a b) -> -1 | 0 | 1
	(setq a (bn-trim a) b (bn-trim b))
	(cond
		((< (length a) (length b)) -1)
		((> (length a) (length b)) 1)
		(:t (defq at (length a) out 0)
			(while (and (= out 0) (> at 0))
				(-- at)
				(defq x (elem-get a at) y (elem-get b at))
				(cond ((< x y) (setq out -1)) ((> x y) (setq out 1))))
			out)))

(defun bn-pad (a n)
	; (bn-pad a n) -> nums
	;a with limbs of 0 at the top, to n limbs
	(if (< (length a) n) (cat a (bn-zeros (- n (length a)))) a))

(defun bn-add (a b)
	; (bn-add a b) -> nums
	(defq n (max (length a) (length b)))
	(bn-norm (nums-add (bn-pad a n) (bn-pad b n))))

(defun bn-sub (a b)
	; (bn-sub a b) -> nums
	;a less b, b not more than a
	(defq n (max (length a) (length b)))
	(bn-trim (bn-norm (nums-sub (bn-pad a n) (bn-pad b n)))))

(defun bn-mul (a b)
	; (bn-mul a b) -> nums
	;each limb of the product a dot of a part of a and a part of b turned
	;round, then the carries
	(defq la (length a) lb (length b) rb (reverse b) out (nums) k 0 most (+ la lb -1))
	(while (< k most)
		(defq lo (max 0 (- k lb -1)) hi (min k (dec la)) at (- lb 1 k))
		(push out (nums-dot (slice a lo (inc hi)) (slice rb (+ at lo) (+ at hi 1))))
		(++ k))
	(push out 0)
	(bn-norm out))

(defun bn-shl (a s)
	; (bn-shl a s) -> nums
	;a moved up s bits, s less than 26
	(if (= s 0) a (bn-norm (nums-scale a (<< 1 s)))))

(defun bn-shr (a s)
	; (bn-shr a s) -> nums
	;a moved down s bits, s less than 26, what falls off gone
	(cond
		((= s 0) a)
		(:t (defq out (nums) at 0 len (length a) low (dec (<< 1 s)) up (- +bn_bits s))
			(while (< at len)
				(push out (logior (>> (elem-get a at) s)
					(if (< (inc at) len) (<< (logand (elem-get a (inc at)) low) up) 0)))
				(++ at))
			(bn-trim out))))

(defun bn-modulus (m)
	; (bn-modulus m) -> modulus
	;what (bn-mod) takes a number mod: m, and m moved up till the top bit
	;of its top limb is set, and by how much
	(setq m (bn-trim m))
	(defq k (length m) top (elem-get m (dec k)) s 0)
	(while (< top (<< 1 (dec +bn_bits))) (setq top (<< top 1) s (inc s)))
	(list m (bn-shl m s) s k))

(defun bn-reduce (x up)
	; (bn-reduce x up) -> nums
	;x mod up, the top bit of up's top limb set. A digit of the quotient
	;at a time, from x's top two limbs and up's top limb and 1, so never
	;too big, and what it times up is taken from x
	(defq k (length up) top1 (inc (elem-get up (dec k))))
	(setq x (bn-trim x))
	(while (> (length x) k)
		(defq j (dec (length x))
			q (/ (+ (<< (elem-get x j) +bn_bits) (elem-get x (dec j))) top1)
			at (- j k))
		(setq x (bn-trim (bn-norm
			(nums-sub x (cat (bn-zeros at) (nums-scale up q) (bn-zeros (- (length x) at k)))) at))))
	(while (>= (bn-cmp x up) 0) (setq x (bn-sub x up)))
	x)

(defun bn-mod (x modulus)
	; (bn-mod x modulus) -> nums
	(bind '(m up shift &ignore) modulus)
	(bn-shr (bn-reduce (bn-shl x shift) up) shift))

(defun bn-mulmod (a b modulus)
	; (bn-mulmod a b modulus) -> nums
	(bn-mod (bn-mul a b) modulus))

(defun bn-addmod (a b modulus)
	; (bn-addmod a b modulus) -> nums
	;a and b each less than the modulus
	(defq m (first modulus) s (bn-add a b))
	(if (>= (bn-cmp s m) 0) (bn-sub s m) (bn-trim s)))

(defun bn-submod (a b modulus)
	; (bn-submod a b modulus) -> nums
	;a and b each less than the modulus
	(if (>= (bn-cmp a b) 0) (bn-sub a b) (bn-sub (bn-add a (first modulus)) b)))

(defun bn-powmod (a e modulus)
	; (bn-powmod a e modulus) -> nums
	;a to the power e, mod. e a big number, from its top bit down
	(defq out (bn-from-num 1) i (bn-bits e) a (bn-mod a modulus))
	;each bit gives way to the other tasks of the node, a power of a
	;prime of 384 bits holds it a second or more on a slow machine
	(while (> i 0)
		(task-slice)
		(-- i)
		(setq out (bn-mulmod out out modulus))
		(if (= (bn-bit e i) 1) (setq out (bn-mulmod out a modulus))))
	out)

(defun bn-invmod (a modulus)
	; (bn-invmod a modulus) -> nums
	;1 over a, mod a prime: a to the prime less 2
	(bn-powmod a (bn-sub (first modulus) (bn-from-num 2)) modulus))

;module
(export-symbols '(bn-zeros bn-trim bn-norm bn-from-num bn-from-bytes bn-bytes bn-bits bn-bit
	bn-zero? bn-cmp bn-pad bn-add bn-sub bn-mul bn-shl bn-shr bn-modulus bn-reduce bn-mod
	bn-mulmod bn-addmod bn-submod bn-powmod bn-invmod))
(env-pop)
