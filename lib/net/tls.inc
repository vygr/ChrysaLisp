;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TLS 1.3, RFC 8446, and TLS 1.2, RFC 5246, a client: what https is over
;
; (tls-open host port) -> :nil | (in out raw), a stream to read what the
; server says and one to write to it, and the connection under them, which
; (tls-close) lets go of. What is written goes as records, sealed, what is
; read comes from records opened the same way.
;
; It offers TLS 1.3 with an X25519 key share and the suite
; TLS_CHACHA20_POLY1305_SHA256, and for a server that has only 1.2, an
; X25519 exchange, ECDHE, with ChaCha20-Poly1305 or AES-128-GCM, the
; server's key RSA or ECDSA. A server that takes none of those is not
; talked to. The keys are made as RFC 8446 7.1 has them, or for 1.2 by
; its PRF of SHA-256, of the whole handshake as RFC 7627 has it if the
; server will. The server's Finished is checked, which says the two sides
; have the same keys and saw the same handshake. A server of 1.2 that
; says it has 1.3, the last bytes of its random, is not talked to: what is
; between may have taken the 1.3 away.
;
; Who the server is is checked: the certificates it sends must go up to a
; root the host keeps, the first of them be for the host asked for, and the
; server sign the handshake with that certificate's key, which says it
; holds the secret half, lib/crypto/x509.inc. One that fails is not
; talked to, and (tls-why) says why. Not checked: whether a certificate
; has been revoked.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././crypto/x25519.inc")
(import "././crypto/x509.inc")
(import "././crypto/hkdf.inc")
(import "././crypto/aead.inc")
(import "././crypto/gcm.inc")
(import "././crypto/random.inc")
(import "././streams/lisp_stream.inc")
(import "service/net/app.inc")

;module
(env-push)

(defq +tls_record_most 16384
	;the suites offered, TLS_CHACHA20_POLY1305_SHA256 of 1.3, then of 1.2
	;ECDHE with ChaCha20-Poly1305, ECDSA and RSA, and with AES-128-GCM
	+tls_suites ''(0x1303 0xcca9 0xcca8 0xc02b 0xc02f)
	+tls_zero32 (apply (const cat) (map (lambda (&) (char 0)) (range 0 32)))
	+tls_empty_hash (sha256 ""))

(defmacro tls-be (n bytes)
	; (tls-be n bytes) -> str
	;a number as so many bytes, the high first, as TLS has its numbers
	(static-qq (char-be ,n ,bytes)))

(defmacro tls-num (data at bytes)
	; (tls-num data at bytes) -> num
	(static-qq (code-be ,data ,bytes ,at)))

(defun tls-vec (len_bytes data)
	; (tls-vec len_bytes data) -> str
	;data after its length, as a vector of TLS is
	(cat (tls-be (length data) len_bytes) data))

(defun tls-label (secret label context len)
	; (tls-label secret label context len) -> str
	;HKDF-Expand-Label of RFC 8446 7.1
	(hkdf-expand secret (cat (tls-be len 2) (tls-vec 1 (cat "tls13 " label)) (tls-vec 1 context)) len))

(defun tls-prf (secret label seed len)
	; (tls-prf secret label seed len) -> str
	;the PRF of TLS 1.2, RFC 5246 5, P_SHA256
	(defq seed (cat label seed) a seed out "")
	(while (< (length out) len)
		(setq a (hmac-sha256 secret a) out (cat out (hmac-sha256 secret (cat a seed)))))
	(slice out 0 len))

(defun tls-keys (secret)
	; (tls-keys secret) -> (key iv)
	(list (tls-label secret "key" "" 32) (tls-label secret "iv" "" 12)))

(defun tls-nonce (iv seq)
	; (tls-nonce iv seq) -> str
	;the iv with the number of the record in its last 8 bytes, xor
	(defq seq_bytes (cat (char 0 4) (tls-be seq 8)))
	(apply (const cat) (map (# (char (logxor (code iv 1 %0) (code seq_bytes 1 %0)))) (range 0 12))))

(defun tls-record-read (state)
	; (tls-record-read state) -> :nil | (type body)
	(defq in (. state :find :rin))
	(and (defq head (read-blk in 5)) (= (length head) 5)
		(defq len (tls-num head 3 2) body (if (> len 0) (read-blk in len) ""))
		(= (length body) len)
		(list (code head 1 0) body)))

(defun tls-record-write (state type body)
	(defq out (. state :find :rout))
	(write-blk out (cat (char type) (char 3) (char 3) (tls-be (length body) 2) body))
	(stream-flush out))

(defun tls-seal (state type data)
	; (tls-seal state type data)
	;data of a type, sealed with the keys of this side, as a record
	(bind '(key iv) (. state :find :wkeys))
	(defq seq (. state :find :wseq))
	(. state :insert :wseq (inc seq))
	(cond
		((. state :find :v12)
			;the record's number, type, version and length are what is
			;sealed with it, and of AES-GCM the 8 bytes of the nonce that
			;change go before it, the record's number
			(defq aad (cat (tls-be seq 8) (char type) (char 3) (char 3) (tls-be (length data) 2)))
			(tls-record-write state type
				(if (eql (. state :find :aead) :gcm)
					(cat (tls-be seq 8) (gcm-seal key (cat iv (tls-be seq 8)) aad data))
					(aead-seal key (tls-nonce iv seq) aad data))))
		(:t (defq inner (cat data (char type))
				aad (cat (char 23) (char 3) (char 3) (tls-be (+ (length inner) 16) 2)))
			(tls-record-write state 23 (aead-seal key (tls-nonce iv seq) aad inner)))))

(defun tls-open12 (state type body)
	; (tls-open12 state type body) -> :nil | data
	;a record of TLS 1.2 opened with the keys of the other side
	(bind '(key iv) (. state :find :rkeys))
	(defq seq (. state :find :rseq))
	(. state :insert :rseq (inc seq))
	(cond
		((eql (. state :find :aead) :gcm)
			(when (>= (length body) 24)
				(defq sealed (slice body 8 -1))
				(gcm-open key (cat iv (slice body 0 8))
					(cat (tls-be seq 8) (char type) (char 3) (char 3) (tls-be (- (length sealed) 16) 2)) sealed)))
		((>= (length body) 16)
			(aead-open key (tls-nonce iv seq)
				(cat (tls-be seq 8) (char type) (char 3) (char 3) (tls-be (- (length body) 16) 2)) body))))

(defun tls-open-record (state)
	; (tls-open-record state) -> :nil | (type data)
	;the next record that is not a change of cipher spec, opened with
	;the keys of the other side if it is sealed
	(defq got :nil)
	(while (and (not got) (defq rec (tls-record-read state)))
		(bind '(type body) rec)
		(cond
			((= type 20)
				;of 1.2, what the other side sends from now is sealed
				(when (defq keys (. state :find :rnext))
					(. state :insert :rkeys keys)
					(. state :insert :rnext :nil)
					(. state :insert :rseq 0)))
			((and (. state :find :v12) (. state :find :rkeys))
				(setq got (if (defq data (tls-open12 state type body)) (list type data) (list -1 ""))))
			((and (= type 23) (. state :find :rkeys))
				(bind '(key iv) (. state :find :rkeys))
				(defq seq (. state :find :rseq)
					aad (cat (char 23) (char 3) (char 3) (tls-be (length body) 2))
					inner (aead-open key (tls-nonce iv seq) aad body))
				(. state :insert :rseq (inc seq))
				(cond
					((not inner) (setq got (list -1 "")))
					(:t ;the zeros after the type are padding
						(defq end (length inner))
						(while (and (> end 0) (= (code inner 1 (dec end)) 0)) (-- end))
						(setq got (if (> end 0) (list (code inner 1 (dec end)) (slice inner 0 (dec end))) (list -1 ""))))))
			((setq got (list type body)))))
	got)

(defun tls-message (state)
	; (tls-message state) -> :nil | (type whole)
	;the next handshake message, from as many records as it takes, the
	;whole of it with its head, as the transcript has it
	(defq buf (. state :find :hbuf) ok :t)
	(while (and ok (or (< (length buf) 4) (< (length buf) (+ 4 (tls-num buf 1 3)))))
		(defq rec (tls-open-record state))
		(if (and rec (= (first rec) 22)) (setq buf (cat buf (second rec))) (setq ok :nil)))
	(cond
		(ok (defq whole (slice buf 0 (+ 4 (tls-num buf 1 3))))
			(. state :insert :hbuf (slice buf (length whole) -1))
			(list (code whole 1 0) whole))
		(:t (. state :insert :hbuf buf) :nil)))

(defun tls-client-hello (host public random)
	; (tls-client-hello host public random) -> str
	(defq ext (cat
			;the server's name, so one of many on an address knows which
			(tls-be 0 2) (tls-vec 2 (tls-vec 2 (cat (char 0) (tls-vec 2 host))))
			;X25519, and for a server of 1.2 that has not got it P-256, its
			;points as they are
			(tls-be 10 2) (tls-vec 2 (tls-vec 2 (cat (tls-be 0x001d 2) (tls-be 0x0017 2))))
			(tls-be 11 2) (tls-vec 2 (tls-vec 1 (char 0)))
			;of 1.2, the keys made of the whole handshake, RFC 7627, and
			;no renegotiation, RFC 5746
			(tls-be 23 2) (tls-vec 2 "")
			(tls-be 0xff01 2) (tls-vec 2 (tls-vec 1 ""))
			;the signatures it would take, when it checks them
			(tls-be 13 2) (tls-vec 2 (tls-vec 2 (apply (const cat) (map (# (tls-be %0 2))
				'(0x0403 0x0503 0x0804 0x0805 0x0806 0x0807 0x0401 0x0501 0x0601)))))
			;TLS 1.3, then 1.2
			(tls-be 43 2) (tls-vec 2 (tls-vec 1 (cat (tls-be 0x0304 2) (tls-be 0x0303 2))))
			;its key share
			(tls-be 51 2) (tls-vec 2 (tls-vec 2 (cat (tls-be 0x001d 2) (tls-vec 2 public)))))
		body (cat (tls-be 0x0303 2) random (tls-vec 1 (random-bytes 32))
			(tls-vec 2 (apply (const cat) (map (# (tls-be %0 2)) +tls_suites))) (tls-vec 1 (char 0)) (tls-vec 2 ext)))
	(cat (char 1) (tls-vec 3 body)))

(defun tls-server-hello (hello)
	; (tls-server-hello hello) -> :nil | (version suite random share ems)
	;what the server took of what was offered: TLS 1.3 and the one suite,
	;its X25519 share, or TLS 1.2 and a suite of it, and whether it makes
	;the keys of the whole handshake, ems. A HelloRetryRequest, a random of
	;a set value, is not taken: there is no other group to offer it. Nor a
	;server of 1.2 whose random says it has 1.3
	(when (and (>= (length hello) 42) (= (code hello 1 0) 2))
		;what the server sent, so no index past what is there. Of 1.2 the
		;extensions may not be there at all
		(defq at 4 random (slice hello (+ at 2) (+ at 34)) at (+ at 34) sid (code hello 1 at)
			at (+ at 1 sid) suite (tls-num hello at 2) at (+ at 3)
			ext_end (if (< (+ at 2) (length hello)) (min (length hello) (+ at 2 (tls-num hello at 2))) at) at (+ at 2)
			share :nil version 0x0303 ems :nil)
		(while (<= (+ at 4) ext_end)
			(defq kind (tls-num hello at 2) len (tls-num hello (+ at 2) 2) data (slice hello (+ at 4) (min ext_end (+ at 4 len))))
			(case kind
				(23 (setq ems :t))
				(43 (if (>= (length data) 2) (setq version (tls-num data 0 2))))
				(51 (and (>= (length data) 36) (= (tls-num data 0 2) 0x001d) (= (tls-num data 2 2) 32) (setq share (slice data 4 36)))))
			(setq at (+ at 4 len)))
		(cond
			((= version 0x0304)
				(and (= suite 0x1303) share
					(nql random (hex-decode "CF21AD74E59A6111BE1D8C021E65B891C2A211167ABB8C5E079E09E2C8A8339C"))
					(list version suite random share ems)))
			((= version 0x0303)
				(and (find suite '(0xcca9 0xcca8 0xc02b 0xc02f))
					(not (find (slice random 24 32) (list (cat "DOWNGRD" (char 1)) (cat "DOWNGRD" (char 0)))))
					(list version suite random :nil ems))))))

(defun tls-scheme (scheme)
	; (tls-scheme scheme) -> :nil | (way hash_name)
	;a signature scheme of TLS 1.3 as lib/crypto/x509.inc has it
	(case scheme
		(0x0403 '(:ecdsa :sha256))
		(0x0503 '(:ecdsa :sha384))
		(0x0603 '(:ecdsa :sha512))
		(0x0804 '(:pss :sha256))
		(0x0805 '(:pss :sha384))
		(0x0806 '(:pss :sha512))
		(0x0807 '(:ed25519 :nil))
		;of 1.2 only, a ServerKeyExchange
		(0x0401 '(:pkcs1 :sha256))
		(0x0501 '(:pkcs1 :sha384))
		(0x0601 '(:pkcs1 :sha512))))

(defun tls-certificates (msg &optional v12)
	; (tls-certificates msg [v12]) -> (cert ...)
	;the certificates of a Certificate message, read, :nil for one that
	;does not read. Of 1.2 there is no context before them and nothing
	;after each
	;what the server sent, so no index past what is there
	(defq out (list) most (length msg) after (if v12 0 2))
	(when (> most 4)
		(defq at (if v12 4 (+ 5 (code msg 1 4))))
		(when (<= (+ at 3) most)
			(defq end (min most (+ at 3 (tls-num msg at 3))) at (+ at 3))
			(while (<= (+ at 3) end)
				(defq next (+ at 3 (tls-num msg at 3)))
				(cond
					((> (+ next after) end) (setq at end))
					(:t (push out (x509-parse (slice msg (+ at 3) next)))
						(setq at (if v12 next (+ next 2 (tls-num msg next 2)))))))))
	out)

(defun tls-verify (cert msg transcript)
	; (tls-verify cert msg transcript) -> :t | :nil
	;the server's CertificateVerify, a signature of the handshake so far by
	;the key of its certificate, RFC 8446 4.4.3
	(and cert (>= (length msg) 8)
		(defq way (tls-scheme (tls-num msg 4 2)) len (tls-num msg 6 2))
		;PKCS #1 v1.5 is of 1.2 only
		(not (eql (first way) :pkcs1))
		(= (length msg) (+ 8 len))
		(x509-sig-check (. cert :find :key_kind) (. cert :find :key) (first way) (second way)
			(cat (apply (const cat) (map (lambda (&) " ") (range 0 64)))
				"TLS 1.3, server CertificateVerify" (char 0) (sha256 transcript))
			(slice msg 8 -1))))

(defun tls-server-kx (msg cert crandom srandom)
	; (tls-server-kx msg cert crandom srandom) -> :nil | (group public)
	;the server's share of a ServerKeyExchange of 1.2, X25519 or P-256, if
	;it is signed, with both randoms, by the key of its certificate. cert
	;:nil, and it is not checked
	(when (and (>= (length msg) 8) (= (code msg 1 4) 3))
		(defq group (tls-num msg 5 2) plen (code msg 1 7) at (+ 8 plen))
		(when (and (<= at (length msg))
				(or (and (= group 0x001d) (= plen 32)) (and (= group 0x0017) (= plen 65))))
			(defq params (slice msg 4 at) share (list group (slice params 4 -1)))
			(cond
				((not cert) share)
				((and (>= (length msg) (+ at 4))
						(defq way (tls-scheme (tls-num msg at 2)) len (tls-num msg (+ at 2) 2))
						(= (length msg) (+ at 4 len))
						(x509-sig-check (. cert :find :key_kind) (. cert :find :key) (first way) (second way)
							(cat crandom srandom params) (slice msg (+ at 4) -1)))
					share)))))

(defun tls-handshake12 (state host transcript secret crandom sh)
	; (tls-handshake12 state host transcript secret crandom sh) -> :nil | :t
	;the rest of a handshake of TLS 1.2, RFC 5246 7.3, after the
	;ServerHello: the server's Certificate, its share signed, perhaps a
	;request for one of this side, which is sent none, and its done. Then
	;this side's share, the keys, a change of cipher spec and this side's
	;Finished, and the server's two, checked
	(bind '(version suite srandom share ems) sh)
	(defq certs :nil spub :nil asked :nil done :nil ok :t
		check (. state :find :check) gcm (find suite '(0xc02b 0xc02f)))
	(. state :insert :v12 :t)
	(. state :insert :aead (if gcm :gcm :chacha))
	(while (and ok (not done))
		(cond
			((not (defq got (tls-message state))) (setq ok :nil))
			(:t (setq transcript (cat transcript (second got)))
				(case (first got)
					(11 (setq certs (tls-certificates (second got) :t))
						(when (defq why (if check (x509-chain-check certs host)))
							(. state :insert :why why)
							(setq ok :nil)))
					(12 (unless (setq spub (tls-server-kx (second got) (if check (first certs)) crandom srandom))
							(. state :insert :why "the server did not sign its share with its certificate's key")
							(setq ok :nil)))
					(13 (setq asked :t))
					(14 (setq done :t))))))
	(when (and ok (not spub))
		(. state :insert :why "the server sent no share")
		(setq ok :nil))
	(when ok
		(bind '(group point) spub)
		(defq p256 (= group 0x0017)
			kx (cat (char 16) (tls-vec 3 (tls-vec 1 (if p256 (ecdh-public *ecdsa_p256* secret) (x25519-public secret)))))
			pms (if p256 (ecdh-shared *ecdsa_p256* secret point) (x25519 secret point)))
		(unless pms
			(. state :insert :why "the server's share is not on its curve")
			(setq ok :nil)))
	(when ok
		(defq mine (if asked (cat (cat (char 11) (tls-vec 3 (tls-vec 3 ""))) kx) kx)
			transcript (cat transcript mine)
			master (if ems (tls-prf pms "extended master secret" (sha256 transcript) 48)
				(tls-prf pms "master secret" (cat crandom srandom) 48))
			klen (if gcm 16 32) ilen (if gcm 4 12)
			block (tls-prf master "key expansion" (cat srandom crandom) (* 2 (+ klen ilen)))
			ckey (slice block 0 klen) skey (slice block klen (* 2 klen))
			civ (slice block (* 2 klen) (+ (* 2 klen) ilen)) siv (slice block (+ (* 2 klen) ilen) -1)
			verify (tls-prf master "client finished" (sha256 transcript) 12)
			fin (cat (char 20) (tls-vec 3 verify)))
		(tls-record-write state 22 mine)
		(tls-record-write state 20 (char 1))
		(. state :insert :wkeys (list (if gcm (gcm-key ckey) ckey) civ))
		(. state :insert :wseq 0)
		(tls-seal state 22 fin)
		(setq transcript (cat transcript fin))
		;the server's change of cipher spec, which the reading of records
		;acts on, then its Finished
		(. state :insert :rnext (list (if gcm (gcm-key skey) skey) siv))
		(cond
			((not (and (defq got (tls-message state)) (= (first got) 20) (. state :find :rkeys)))
				(. state :insert :why "the handshake did not finish")
				(setq ok :nil))
			((not (eql (slice (second got) 4 -1) (tls-prf master "server finished" (sha256 transcript) 12)))
				(. state :insert :why "the server's Finished is not of this handshake")
				(setq ok :nil))))
	ok)

(defun tls-handshake13 (state host transcript secret share)
	; (tls-handshake13 state host transcript secret share) -> :nil | :t
	;the rest of a handshake of TLS 1.3, after the ServerHello
	(defq early (hkdf-extract "" +tls_zero32)
		hs (hkdf-extract (tls-label early "derived" +tls_empty_hash 32) (x25519 secret share))
		c_hs (tls-label hs "c hs traffic" (sha256 transcript) 32)
		s_hs (tls-label hs "s hs traffic" (sha256 transcript) 32)
		finished :nil ok :t)
	(. state :insert :rkeys (tls-keys s_hs))
	(. state :insert :rseq 0)
	;EncryptedExtensions, Certificate, CertificateVerify, into the
	;transcript, the certificates checked, and the handshake so far
	;signed by the first one's key. Then the server's Finished
	(defq certs :nil checked :nil)
	(while (and ok (not finished))
		(cond
			((not (defq got (tls-message state))) (setq ok :nil))
			((= (first got) 11)
				(setq certs (tls-certificates (second got)) transcript (cat transcript (second got)))
				(when (defq why (if (. state :find :check) (x509-chain-check certs host)))
					(. state :insert :why why)
					(setq ok :nil)))
			((= (first got) 15)
				(cond
					((or (not (. state :find :check)) (tls-verify (first certs) (second got) transcript))
						(setq checked :t transcript (cat transcript (second got))))
					(:t (. state :insert :why "the server did not sign the handshake with its certificate's key")
						(setq ok :nil))))
			((= (first got) 20)
				(defq want (hmac-sha256 (tls-label s_hs "finished" "" 32) (sha256 transcript)))
				(cond
					((not (eql (slice (second got) 4 -1) want))
						(. state :insert :why "the server's Finished is not of this handshake")
						(setq ok :nil))
					((not checked)
						(. state :insert :why "the server sent no certificate it signed with")
						(setq ok :nil))
					(:t (setq finished :t transcript (cat transcript (second got))))))
			(:t (setq transcript (cat transcript (second got))))))
	(when ok
		(defq master (hkdf-extract (tls-label hs "derived" +tls_empty_hash 32) +tls_zero32)
			th (sha256 transcript)
			c_ap (tls-label master "c ap traffic" th 32) s_ap (tls-label master "s ap traffic" th 32)
			verify (hmac-sha256 (tls-label c_hs "finished" "" 32) th))
		;a change of cipher spec, as a client of 1.2 would, for boxes in the
		;way that look, and this side's Finished
		(tls-record-write state 20 (char 1))
		(. state :insert :wkeys (tls-keys c_hs))
		(. state :insert :wseq 0)
		(tls-seal state 22 (cat (char 20) (tls-vec 3 verify)))
		(. state :insert :wkeys (tls-keys c_ap))
		(. state :insert :wseq 0)
		(. state :insert :rkeys (tls-keys s_ap))
		(. state :insert :rseq 0)
		:t))

(defun tls-handshake (state host)
	; (tls-handshake state host) -> :nil | :t
	(defq secret (random-bytes 32) crandom (random-bytes 32)
		hello (tls-client-hello host (x25519-public secret) crandom))
	(tls-record-write state 22 hello)
	(cond
		((not (and (defq got (tls-message state)) (defq sh (tls-server-hello (second got))))) :nil)
		((= (first sh) 0x0303)
			(tls-handshake12 state host (cat hello (second got)) secret crandom sh))
		((tls-handshake13 state host (cat hello (second got)) secret (elem-get sh 3)))))

(defun tls-fill (state)
	; (tls-fill state) -> :nil | str
	;what the server says next, a record of it. What it says about
	;itself after the handshake, a ticket, is read past
	(defq out :nil)
	(until (or out (. state :find :closed))
		(defq rec (tls-open-record state))
		(cond
			((not rec) (. state :insert :closed :t))
			((= (first rec) 23) (setq out (second rec)))
			((= (first rec) 21) (. state :insert :closed :t))
			((= (first rec) -1) (. state :insert :closed :t))))
	out)

(defun tls-write (state data flush)
	; (tls-write state data flush)
	(defq at 0)
	(while (< at (length data))
		(defq part (slice data at (min (length data) (+ at +tls_record_most))))
		(tls-seal state 23 part)
		(setq at (+ at (length part)))))

;module
(export-symbols '(tls-handshake tls-fill tls-write tls-seal tls-label tls-keys tls-nonce tls-be tls-vec
	tls-scheme tls-certificates tls-verify tls-prf tls-client-hello tls-server-hello tls-server-kx tls-open12))
(env-pop)

;made after the module. In a function of a module a name put into a
;backquote, ,tls-fill, even ,(const tls-fill), is looked for as the
;function runs, and by then the module's own names are gone: the error
;that gives is put in the form, and a stream that calls it says nothing

;why the last (tls-open) gave :nil, and whether to ask who answers, :nil
;to ask, as the default
(defq *tls_why* :nil *tls_unchecked* :nil)

(defun tls-open (host port &optional unchecked)
	; (tls-open host port [unchecked]) -> :nil | (in out state)
	;a connection that is private between here and the host, which has
	;shown it is the host. unchecked :t, and who answers is not asked to
	;show it. :nil if not, (tls-why) says why
	(setd unchecked *tls_unchecked*)
	(setq *tls_why* "no connection")
	(when (defq raw (net-open-rpc host port))
		(defq state (Emap))
		(scatter state :raw raw :rin (first raw) :rout (second raw) :hbuf "" :rkeys :nil :wkeys :nil :closed :nil
			:check (not unchecked) :why "the handshake did not finish")
		(cond
			((tls-handshake state host)
				(setq *tls_why* :nil)
				(list (lisp-stream `(lambda () (,tls-fill ',state)))
					(lisp-stream '(lambda () :nil) :nil `(lambda (data flush) (,tls-write ',state data flush)))
					state))
			(:t (setq *tls_why* (. state :find :why))
				(net-close-rpc raw) :nil))))

(defun tls-why ()
	; (tls-why) -> :nil | str
	;why the last (tls-open) of this task gave :nil
	*tls_why*)

(defun tls-close (conn)
	; (tls-close conn)
	;a close told to the server, and the connection let go of
	(defq state (third conn))
	(unless (. state :find :closed)
		(catch (tls-seal state 21 (cat (char 1) (char 0))) :t)
		(. state :insert :closed :t))
	(net-close-rpc (. state :find :raw)))
