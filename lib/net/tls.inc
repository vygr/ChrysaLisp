;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TLS 1.3, RFC 8446, a client: what https is over
;
; (tls-open host port) -> :nil | (in out raw), a stream to read what the
; server says and one to write to it, and the connection under them, which
; (tls-close) lets go of. What is written goes as records, sealed with
; ChaCha20-Poly1305, what is read comes from records opened the same way.
;
; It offers the one way it has: TLS 1.3, an X25519 key share, and the
; suite TLS_CHACHA20_POLY1305_SHA256. A server that takes none of those is
; not talked to. The keys are made as RFC 8446 7.1 has them, and the
; server's Finished is checked, which says the two sides have the same
; keys and saw the same handshake.
;
; NOT YET: who the server is is not checked. Its certificate is read past,
; not looked at, so another that put itself in the way would be talked to
; as if it were the server. Until the certificate and its signature are
; checked, what is fetched so is to be trusted no more than over http.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/crypto/x25519.inc")
(import "lib/crypto/hkdf.inc")
(import "lib/crypto/aead.inc")
(import "lib/crypto/random.inc")
(import "lib/streams/lisp_stream.inc")
(import "service/net/app.inc")

;module
(env-push)

(defq +tls_record_most 16384
	+tls_zero32 (apply (const cat) (map (lambda (&) (char 0)) (range 0 32)))
	+tls_empty_hash (sha256 ""))

(defun tls-be (n bytes)
	; (tls-be n bytes) -> str
	;a number as so many bytes, the high first, as TLS has its numbers
	(apply (const cat) (map (# (char (logand (>> n (* 8 (- bytes 1 %0))) 0xff))) (range 0 bytes))))

(defun tls-num (data at bytes)
	; (tls-num data at bytes) -> num
	(reduce (# (+ (<< %0 8) (code data 1 (+ at %1)))) (range 0 bytes) 0))

(defun tls-vec (len_bytes data)
	; (tls-vec len_bytes data) -> str
	;data after its length, as a vector of TLS is
	(cat (tls-be (length data) len_bytes) data))

(defun tls-label (secret label context len)
	; (tls-label secret label context len) -> str
	;HKDF-Expand-Label of RFC 8446 7.1
	(hkdf-expand secret (cat (tls-be len 2) (tls-vec 1 (cat "tls13 " label)) (tls-vec 1 context)) len))

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
	(defq seq (. state :find :wseq) inner (cat data (char type))
		aad (cat (char 23) (char 3) (char 3) (tls-be (+ (length inner) 16) 2)))
	(. state :insert :wseq (inc seq))
	(tls-record-write state 23 (aead-seal key (tls-nonce iv seq) aad inner)))

(defun tls-open-record (state)
	; (tls-open-record state) -> :nil | (type data)
	;the next record that is not a change of cipher spec, opened with
	;the keys of the other side if it is sealed
	(defq got :nil)
	(while (and (not got) (defq rec (tls-record-read state)))
		(bind '(type body) rec)
		(cond
			((= type 20))
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

(defun tls-client-hello (host public)
	; (tls-client-hello host public) -> str
	(defq ext (cat
			;the server's name, so one of many on an address knows which
			(tls-be 0 2) (tls-vec 2 (tls-vec 2 (cat (char 0) (tls-vec 2 host))))
			;X25519 the one group
			(tls-be 10 2) (tls-vec 2 (tls-vec 2 (tls-be 0x001d 2)))
			;the signatures it would take, when it checks them
			(tls-be 13 2) (tls-vec 2 (tls-vec 2 (cat (tls-be 0x0804 2) (tls-be 0x0403 2) (tls-be 0x0805 2)
				(tls-be 0x0806 2) (tls-be 0x0401 2) (tls-be 0x0807 2))))
			;TLS 1.3 the one version
			(tls-be 43 2) (tls-vec 2 (tls-vec 1 (tls-be 0x0304 2)))
			;its key share
			(tls-be 51 2) (tls-vec 2 (tls-vec 2 (cat (tls-be 0x001d 2) (tls-vec 2 public)))))
		body (cat (tls-be 0x0303 2) (random-bytes 32) (tls-vec 1 (random-bytes 32))
			(tls-vec 2 (tls-be 0x1303 2)) (tls-vec 1 (char 0)) (tls-vec 2 ext)))
	(cat (char 1) (tls-vec 3 body)))

(defun tls-server-share (hello)
	; (tls-server-share hello) -> :nil | public
	;the server's X25519 share of its ServerHello, if it took TLS 1.3 and
	;the one suite. A HelloRetryRequest, a random of a set value, is not
	;taken: there is no other group to offer it
	(when (and (> (length hello) 44) (= (code hello 1 0) 2))
		(defq at 4 random (slice hello (+ at 2) (+ at 34)) at (+ at 34) sid (code hello 1 at)
			at (+ at 1 sid) suite (tls-num hello at 2) at (+ at 3) ext_end (+ at 2 (tls-num hello at 2)) at (+ at 2)
			share :nil version :nil)
		(while (< (+ at 4) ext_end)
			(defq kind (tls-num hello at 2) len (tls-num hello (+ at 2) 2) data (slice hello (+ at 4) (+ at 4 len)))
			(case kind
				(43 (setq version (tls-num data 0 2)))
				(51 (and (= (tls-num data 0 2) 0x001d) (= (tls-num data 2 2) 32) (setq share (slice data 4 36)))))
			(setq at (+ at 4 len)))
		(and (= suite 0x1303) (= version 0x0304)
			(nql random (hex-decode "CF21AD74E59A6111BE1D8C021E65B891C2A211167ABB8C5E079E09E2C8A8339C"))
			share)))

(defun tls-handshake (state host)
	; (tls-handshake state host) -> :nil | :t
	(defq secret (random-bytes 32) hello (tls-client-hello host (x25519-public secret)))
	(tls-record-write state 22 hello)
	(when (and (defq got (tls-message state)) (defq share (tls-server-share (second got))))
		(defq transcript (cat hello (second got))
			early (hkdf-extract "" +tls_zero32)
			hs (hkdf-extract (tls-label early "derived" +tls_empty_hash 32) (x25519 secret share))
			c_hs (tls-label hs "c hs traffic" (sha256 transcript) 32)
			s_hs (tls-label hs "s hs traffic" (sha256 transcript) 32)
			finished :nil ok :t)
		(. state :insert :rkeys (tls-keys s_hs))
		(. state :insert :rseq 0)
		;EncryptedExtensions, Certificate, CertificateVerify: into the
		;transcript, NOT YET looked at. Then the server's Finished
		(while (and ok (not finished))
			(cond
				((not (defq got (tls-message state))) (setq ok :nil))
				((= (first got) 20)
					(defq want (hmac-sha256 (tls-label s_hs "finished" "" 32) (sha256 transcript)))
					(if (eql (slice (second got) 4 -1) want)
						(setq finished :t transcript (cat transcript (second got)))
						(setq ok :nil)))
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
			:t)))

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
(export-symbols '(tls-handshake tls-fill tls-write tls-seal tls-label tls-keys tls-nonce tls-be))
(env-pop)

;made after the module: a (lisp-stream) made in a function of a module
;never called its functions, the fill and the write, found and not yet
;understood. Made out here, it does

(defun tls-open (host port)
	; (tls-open host port) -> :nil | (in out state)
	;a connection that is private between here and whoever answers at the
	;address. NOT YET is it checked who that is, see the top of this file
	(when (defq raw (net-open-rpc host port))
		(defq state (Emap))
		(scatter state :raw raw :rin (first raw) :rout (second raw) :hbuf "" :rkeys :nil :wkeys :nil :closed :nil)
		(cond
			((tls-handshake state host)
				(list (lisp-stream `(lambda () (,tls-fill ',state)))
					(lisp-stream '(lambda () :nil) :nil `(lambda (data flush) (,tls-write ',state data flush)))
					state))
			(:t (net-close-rpc raw) :nil))))

(defun tls-close (conn)
	; (tls-close conn)
	;a close told to the server, and the connection let go of
	(defq state (third conn))
	(unless (. state :find :closed)
		(catch (tls-seal state 21 (cat (char 1) (char 0))) :t)
		(. state :insert :closed :t))
	(net-close-rpc (. state :find :raw)))
