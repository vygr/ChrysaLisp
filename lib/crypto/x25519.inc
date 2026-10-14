;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; X25519, the key exchange of RFC 7748.
;
; Each side makes a secret of 32 bytes, (random-bytes 32), and sends
; the other its public half, (x25519-public secret). Each then has
; the same 32 bytes, (x25519 secret their_public), and nobody who saw
; only the two public halves has them. It is how TLS 1.3 agrees its
; keys.
;
; It is the Montgomery ladder of TweetNaCl, on the numbers of the
; field of Ed25519, lib/crypto/ed25519.inc, its native multiply.
; Like that, it does not take the same time whatever the secret is.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./ed25519.inc")

(defq *x25519_121665* (nums 0xdb41 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	*x25519_base* (cat (char 9) (apply (const cat) (map (lambda (&) (char 0)) (range 0 31)))))

(defun x25519-swap (p q bit)
	;p and q swapped if bit is 1
	(when (= bit 1)
		(each (lambda (i)
				(defq t (elem-get p i))
				(elem-set p i (elem-get q i))
				(elem-set q i t))
			*ed_16*)))

(defun x25519 (secret point)
	; (x25519 secret point) -> str
	;the secret, 32 bytes, times the point, 32 bytes, the u of a point
	;of the curve, the low byte first
	(defq z (map (# (code secret 1 %0)) *ed_32*))
	(elem-set z 31 (logior (logand (elem-get z 31) 127) 64))
	(elem-set z 0 (logand (elem-get z 0) 248))
	(defq x (ed-unpack point) a (ed-copy *ed_one*) b (ed-copy x) c (ed-copy *ed_zero*) d (ed-copy *ed_one*)
		e (ed-copy *ed_zero*) f (ed-copy *ed_zero*) i 254)
	(while (>= i 0)
		(task-slice)
		(defq bit (logand (>> (elem-get z (>> i 3)) (logand i 7)) 1))
		(x25519-swap a b bit) (x25519-swap c d bit)
		(setq e (nums-add a c (cat e)) a (nums-sub a c (cat a)) c (nums-add b d (cat c)) b (nums-sub b d (cat b)))
		(ed-mul d e e) (ed-mul f a a) (ed-mul a c a) (ed-mul c b e)
		(setq e (nums-add a c (cat e)) a (nums-sub a c (cat a)))
		(ed-mul b a a)
		(setq c (nums-sub d f (cat c)))
		(ed-mul a c *x25519_121665*)
		(setq a (nums-add a d (cat a)))
		(ed-mul c c a) (ed-mul a d f) (ed-mul d b x) (ed-mul b e e)
		(x25519-swap a b bit) (x25519-swap c d bit)
		(setq i (dec i)))
	(ed-pack (ed-mul a a (ed-inv c))))

(defun x25519-public (secret)
	; (x25519-public secret) -> str
	;the public half of a secret, to give the other side
	(x25519 secret *x25519_base*))
