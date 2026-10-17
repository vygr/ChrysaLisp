;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RSA, the check of a signature, RFC 8017
;
; (rsa-verify-pkcs1 n e hash_name digest signature) -> :t | :nil
; (rsa-verify-pss n e hash_name digest signature) -> :t | :nil
;
; n and e the key, as bytes, the top first, as a certificate has them.
; hash_name :sha256, :sha384 or :sha512, digest what was signed, hashed.
;
; The signature to the power e mod n gives back what was signed, laid out
; one of two ways. PKCS #1 v1.5: 0 1, bytes of ff, 0, and the digest with
; the name of its kind in front, which is how most certificates are
; signed. PSS: the digest and a salt, mixed so that it can only be undone
; from the digest, which is what TLS 1.3 signs its handshake with. The salt
; is as long as the digest, as TLS 1.3 has it.
;
; Only the check: e is small, 65537 mostly, so it is 17 products.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././math/bignum.inc")
(import "./sha256.inc")
(import "./sha512.inc")

;module
(env-push)

(defun rsa-hash (hash_name data)
	; (rsa-hash hash_name data) -> :nil | str
	(case hash_name
		(:sha256 (sha256 data))
		(:sha384 (sha384 data))
		(:sha512 (sha512 data))))

(defun rsa-prefix (hash_name)
	; (rsa-prefix hash_name) -> :nil | str
	;the DER of the name of a hash, the DigestInfo the digest goes after
	(case hash_name
		(:sha256 (hex-decode "3031300D060960864801650304020105000420"))
		(:sha384 (hex-decode "3041300D060960864801650304020205000430"))
		(:sha512 (hex-decode "3051300D060960864801650304020305000440"))))

(defun rsa-open (n e signature)
	; (rsa-open n e signature) -> :nil | (bits em)
	;the signature to the power e mod n, as bytes as long as n, and how
	;many bits n has. :nil if the signature is not less than n
	(defq nb (bn-from-bytes n) k (/ (+ (bn-bits nb) 7) 8) s (bn-from-bytes signature))
	(and (= (length signature) k) (< (bn-cmp s nb) 0)
		(list (bn-bits nb) (bn-bytes (bn-powmod s (bn-from-bytes e) (bn-modulus nb)) k))))

(defun rsa-verify-pkcs1 (n e hash_name digest signature)
	; (rsa-verify-pkcs1 n e hash_name digest signature) -> :t | :nil
	(when (and (defq prefix (rsa-prefix hash_name)) (defq opened (rsa-open n e signature)))
		(defq em (second opened) info (cat prefix digest) pad (- (length em) (length info) 3))
		(and (>= pad 8)
			(eql em (cat (char 0) (char 1) (apply (const cat) (map (lambda (&) (char 0xff)) (range 0 pad)))
				(char 0) info)))))

(defun rsa-xor (a b)
	; (rsa-xor a b) -> str
	(apply (const cat) (map (# (char (logxor (code a 1 %0) (code b 1 %0)))) (range 0 (length a)))))

(defun rsa-mgf1 (hash_name seed len)
	; (rsa-mgf1 hash_name seed len) -> str
	;the hash of the seed and a count, again and again, len bytes of it
	(defq out "" count 0)
	(while (< (length out) len)
		(setq out (cat out (rsa-hash hash_name (cat seed (char (logand (>> count 24) 0xff))
			(char (logand (>> count 16) 0xff)) (char (logand (>> count 8) 0xff)) (char (logand count 0xff))))) count (inc count)))
	(slice out 0 len))

(defun rsa-verify-pss (n e hash_name digest signature)
	; (rsa-verify-pss n e hash_name digest signature) -> :t | :nil
	(when (and (rsa-prefix hash_name) (defq opened (rsa-open n e signature)))
		(bind '(bits em) opened)
		;the encoded message is the top bits less 1 of n, so a byte less
		;when n's bits are a multiple of 8
		(defq em_bits (dec bits) em_len (/ (+ em_bits 7) 8) h_len (length digest))
		(when (and (or (= em_len (length em)) (= (code em 1 0) 0))
				(setq em (slice em (- (length em) em_len) -1))
				(>= em_len (+ h_len h_len 2))
				(= (code em 1 (dec em_len)) 0xbc))
			(defq db_len (- em_len h_len 1) masked (slice em 0 db_len) h (slice em db_len (dec em_len))
				top_mask (>> 0xff (- (* 8 em_len) em_bits)))
			(when (= (logand (code masked 1 0) (lognot top_mask)) 0)
				(defq db (rsa-xor masked (rsa-mgf1 hash_name h db_len))
					db (cat (char (logand (code db 1 0) top_mask)) (slice db 1 -1))
					zeros (- em_len h_len h_len 2))
				(and (some! (# (= (code %0) 0)) (list db) :t 0 zeros)
					(= (code db 1 zeros) 1)
					(eql h (rsa-hash hash_name (cat (char 0 8) digest (slice db (inc zeros) -1)))))))))

;module
(export-symbols '(rsa-verify-pkcs1 rsa-verify-pss rsa-hash))
(env-pop)
