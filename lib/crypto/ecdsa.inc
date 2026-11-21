;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ECDSA, the check of a signature, on the curves P-256 and P-384, and
; ECDH, a secret two sides agree on, as TLS 1.2 has it of a server with no
; X25519
;
; (ecdsa-verify curve public digest r s) -> :t | :nil
; (ecdh-public curve secret) -> the point of a secret, 4 then x then y
; (ecdh-shared curve secret public) -> :nil | x of the secret times the
; other side's point, :nil if that point is not on the curve
;
; curve *ecdsa_p256* or *ecdsa_p384*. public the point of the key, as a
; certificate has it, 4 then x then y. digest what was signed, hashed. r and
; s the signature's two numbers, as bytes, the top first.
;
; Only the check, not the signing: what TLS and a certificate need. The
; points are kept as Jacobian X Y Z, x = X/Z^2 and y = Y/Z^3, so no
; division is done till the end. The curves are y^2 = x^3 - 3x + b, and
; the doubling is the one for that -3, dbl-2001-b of the Explicit-Formulas
; Database, the adding add-2007-bl.
;
; It is the big numbers of lib/math/bignum.inc, the points worked in
; Montgomery's form, lib/math/montgomery.inc, whose multiply is native and
; does no division: the coordinates of a point are in the form while it is
; doubled and added, and taken out of it at the end.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././math/bignum.inc")
(import "././math/montgomery.inc")

;module
(env-push)

(defun ec-to-mont (pt fm)
	; (ec-to-mont pt fm) -> pt
	;a point's coordinates in Montgomery's form
	(map (# (bn-to-mont %0 fm)) pt))

(defun ecdsa-curve (p n b x y len)
	; (ecdsa-curve p n b x y len) -> curve
	;a curve from its numbers in hex: the prime, the order of the base
	;point, b, the base point, and how many bytes a number of it is, and
	;the prime and the order for Montgomery's forms, and the base point in
	;them
	(defq fm (bn-modulus (bn-from-bytes (hex-decode p))) nm (bn-modulus (bn-from-bytes (hex-decode n)))
		g (list (bn-from-bytes (hex-decode x)) (bn-from-bytes (hex-decode y)) (bn-from-num 1))
		fmont (bn-mont (first fm)))
	(list fm nm (bn-from-bytes (hex-decode b)) g len fmont (bn-mont (first nm)) (ec-to-mont g fmont)))

;from SEC 2 and FIPS 186-4, checked on the curve and of order n by Python
(defq *ecdsa_p256* (ecdsa-curve
		"FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF"
		"FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551"
		"5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B"
		"6B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C296"
		"4FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5" 32)
	*ecdsa_p384* (ecdsa-curve
		"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFF0000000000000000FFFFFFFF"
		"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC7634D81F4372DDF581A0DB248B0A77AECEC196ACCC52973"
		"B3312FA7E23EE7E4988E056BE3F82D19181D9C6EFE8141120314088F5013875AC656398D8A2ED19D2A85C8EDD3EC2AEF"
		"AA87CA22BE8B05378EB1C71EF320AD746E1D3B628BA79B9859F741E082542A385502F25DBF55296C3A545E3872760AB7"
		"3617DE4A96262C6F5D9E98BF9292DC29F8F41DBD289A147CE9DA3113B5F0B8C00A60B1CE1D7E819D7A431D7C90EA0E5F" 48))

(defun ec-small (a k fm)
	; (ec-small a k fm) -> nums
	;a times a small number, mod, by doubling and adding
	(defq out a n 1)
	(while (< n k)
		(if (<= (* n 2) k)
			(setq out (bn-addmod out out fm) n (* n 2))
			(setq out (bn-addmod out a fm) n (inc n))))
	out)

(defun ec-dbl (pt fm)
	; (ec-dbl pt fm) -> :nil | pt
	;twice a point, :nil the point at infinity, in Montgomery's form
	(when pt
		(bind '(x y z) pt)
		(unless (bn-zero? y)
			(defq delta (bn-mul-mont z z fm) gamma (bn-mul-mont y y fm) beta (bn-mul-mont x gamma fm)
				alpha (ec-small (bn-mul-mont (bn-submod x delta fm) (bn-addmod x delta fm) fm) 3 fm)
				x3 (bn-submod (bn-mul-mont alpha alpha fm) (ec-small beta 8 fm) fm)
				yz (bn-addmod y z fm)
				z3 (bn-submod (bn-submod (bn-mul-mont yz yz fm) gamma fm) delta fm)
				y3 (bn-submod (bn-mul-mont alpha (bn-submod (ec-small beta 4 fm) x3 fm) fm)
					(ec-small (bn-mul-mont gamma gamma fm) 8 fm) fm))
			(list x3 y3 z3))))

(defun ec-add (p1 p2 fm)
	; (ec-add p1 p2 fm) -> :nil | pt
	(cond
		((not p1) p2)
		((not p2) p1)
		(:t (bind '(x1 y1 z1) p1)
			(bind '(x2 y2 z2) p2)
			(defq z1z1 (bn-mul-mont z1 z1 fm) z2z2 (bn-mul-mont z2 z2 fm)
				u1 (bn-mul-mont x1 z2z2 fm) u2 (bn-mul-mont x2 z1z1 fm)
				s1 (bn-mul-mont y1 (bn-mul-mont z2 z2z2 fm) fm) s2 (bn-mul-mont y2 (bn-mul-mont z1 z1z1 fm) fm)
				h (bn-submod u2 u1 fm) r (ec-small (bn-submod s2 s1 fm) 2 fm))
			(cond
				((not (bn-zero? h))
					(defq h2 (ec-small h 2 fm) i (bn-mul-mont h2 h2 fm) j (bn-mul-mont h i fm) v (bn-mul-mont u1 i fm)
						x3 (bn-submod (bn-submod (bn-mul-mont r r fm) j fm) (ec-small v 2 fm) fm)
						y3 (bn-submod (bn-mul-mont r (bn-submod v x3 fm) fm) (ec-small (bn-mul-mont s1 j fm) 2 fm) fm)
						zz (bn-addmod z1 z2 fm)
						z3 (bn-mul-mont (bn-submod (bn-submod (bn-mul-mont zz zz fm) z1z1 fm) z2z2 fm) h fm))
					(list x3 y3 z3))
				;the same x: the same point, or one the other's negative
				((bn-zero? r) (ec-dbl p1 fm))))))

(defun ec-on-curve? (x y curve)
	; (ec-on-curve? x y curve) -> :t | :nil
	;y^2 = x^3 - 3x + b, and each less than the prime
	(defq fm (first curve) b (third curve))
	(and (< (bn-cmp x (first fm)) 0) (< (bn-cmp y (first fm)) 0)
		(= (bn-cmp (bn-mulmod y y fm)
				(bn-addmod (bn-submod (bn-mulmod (bn-mulmod x x fm) x fm) (bn-mod (bn-norm (nums-scale x 3)) fm) fm) b fm)) 0)))

(defun ecdsa-verify (curve public digest r s)
	; (ecdsa-verify curve public digest r s) -> :t | :nil
	(bind '(& nm & & len fm nmont g) curve)
	(defq r (bn-from-bytes r) s (bn-from-bytes s) n (first nm))
	(when (and (= (length public) (inc (* 2 len))) (= (code public 1 0) 4)
			(not (bn-zero? r)) (< (bn-cmp r n) 0) (not (bn-zero? s)) (< (bn-cmp s n) 0))
		(defq qx (bn-from-bytes (slice public 1 (inc len))) qy (bn-from-bytes (slice public (inc len) -1)))
		(when (ec-on-curve? qx qy curve)
			;the digest, its leftmost bits as many as n has
			(defq e (bn-mod (bn-from-bytes (slice digest 0 (min (length digest) len))) nm)
				w (bn-from-mont (bn-inv-mont (bn-to-mont s nmont) nmont) nmont)
				u1 (bn-mulmod e w nm) u2 (bn-mulmod r w nm)
				q (ec-to-mont (list qx qy (bn-from-num 1)) fm) gq (ec-add g q fm) acc :nil
				i (max (bn-bits u1) (bn-bits u2)))
			;u1 G + u2 Q, both at once, a bit of each at a time, each giving
			;way to the other tasks of the node, a verify of P-384 on a Pi
			;is seconds
			(while (> i 0)
				(task-slice)
				(-- i)
				(setq acc (ec-dbl acc fm))
				(defq b1 (bn-bit u1 i) b2 (bn-bit u2 i))
				(cond
					((and (= b1 1) (= b2 1)) (setq acc (ec-add acc gq fm)))
					((= b1 1) (setq acc (ec-add acc g fm)))
					((= b2 1) (setq acc (ec-add acc q fm)))))
			(when acc
				(bind '(x & z) acc)
				(unless (bn-zero? z)
					(defq zi (bn-inv-mont z fm) x (bn-from-mont (bn-mul-mont x (bn-mul-mont zi zi fm) fm) fm))
					(= (bn-cmp (bn-mod x nm) r) 0))))))

(defun ec-affine (pt fm)
	; (ec-affine pt fm) -> :nil | (x y)
	;a point of Montgomery's forms as x and y
	(when pt
		(bind '(x y z) pt)
		(unless (bn-zero? z)
			(defq zi (bn-inv-mont z fm) zi2 (bn-mul-mont zi zi fm))
			(list (bn-from-mont (bn-mul-mont x zi2 fm) fm)
				(bn-from-mont (bn-mul-mont y (bn-mul-mont zi2 zi fm) fm) fm)))))

(defun ec-mul (k pt fm)
	; (ec-mul k pt fm) -> :nil | pt
	;a point times a number, a bit at a time from the top, giving way to
	;the other tasks of the node
	(defq acc :nil i (bn-bits k))
	(while (> i 0)
		(task-slice)
		(-- i)
		(setq acc (ec-dbl acc fm))
		(if (= (bn-bit k i) 1) (setq acc (ec-add acc pt fm))))
	acc)

(defun ec-secret (curve secret)
	; (ec-secret curve secret) -> nums
	;a secret of random bytes as a number from 1 to n - 1, mod n, and 1
	;for the one in 2 to the 256 that is 0
	(defq k (bn-mod (bn-from-bytes secret) (second curve)))
	(if (bn-zero? k) (bn-from-num 1) k))

(defun ecdh-public (curve secret)
	; (ecdh-public curve secret) -> str
	(bind '(& & & & len fm & g) curve)
	(bind '(x y) (ec-affine (ec-mul (ec-secret curve secret) g fm) fm))
	(cat (char 4) (bn-bytes x len) (bn-bytes y len)))

(defun ecdh-shared (curve secret public)
	; (ecdh-shared curve secret public) -> :nil | str
	(bind '(& & & & len fm &ignore) curve)
	(when (and (= (length public) (inc (* 2 len))) (= (code public 1 0) 4))
		(defq qx (bn-from-bytes (slice public 1 (inc len))) qy (bn-from-bytes (slice public (inc len) -1)))
		(and (ec-on-curve? qx qy curve)
			(defq xy (ec-affine (ec-mul (ec-secret curve secret) (ec-to-mont (list qx qy (bn-from-num 1)) fm) fm) fm))
			(bn-bytes (first xy) len))))

;module
(export-symbols '(ecdsa-verify ecdh-public ecdh-shared *ecdsa_p256* *ecdsa_p384*))
(env-pop)
