;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; X.509 certificates, RFC 5280, read and checked
;
; A certificate says that a key is the key of a name, and is signed by the
; one that says so, its issuer, whose own certificate is signed by its
; issuer, up to a root, a certificate kept by the machine as one to trust.
; A server sends its own and those between it and a root.
;
; (x509-parse der) -> :nil | cert, an Emap of what is wanted of it
; (x509-chain-check certs host [roots now]) -> :nil | why not
; (x509-sig-check kind key scheme hash_name data signature) -> :t | :nil
; (x509-roots) -> the roots the host keeps, read once a task
;
; A certificate is DER: each thing a tag, a length, and what is in it,
; which may be more such things. Only what is needed is read: the names,
; the dates, the key, the names it is for, whether it may issue others,
; and the signature, which is checked with lib/crypto/ecdsa.inc,
; lib/crypto/rsa.inc or lib/crypto/ed25519.inc. What a server sends is
; not to be trusted till it is checked, so no index is taken past what is
; there: a release build would not stop it.
;
; NOT DONE: whether a certificate has been revoked, the constraints on the
; names an issuer may issue for, the uses a key is limited to, and the
; length of path an issuer allows below it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/crypto/ecdsa.inc")
(import "lib/crypto/rsa.inc")
(import "lib/crypto/ed25519.inc")
(import "lib/text/base64.inc")
(import "lib/date/date.inc")

;module
(env-push)

;where the hosts keep their roots, the first found is read
(defq +x509_root_files ''("/etc/ssl/cert.pem" "/etc/ssl/certs/ca-certificates.crt"
		"/etc/pki/tls/certs/ca-bundle.crt")
	*x509_roots* :nil)

(defun der-at (data at)
	; (der-at data at) -> :nil | (tag start end head)
	;the thing of DER at a place: its tag, where what is in it starts and
	;ends, and where its head starts, at
	(when (< (inc at) (length data))
		(defq tag (code data 1 at) len (code data 1 (inc at)) start (+ at 2))
		(when (>= len 0x80)
			(defq n (logand len 0x7f))
			(setq len (if (and (<= 1 n 4) (<= (+ start n) (length data)))
				(reduce (# (+ (<< %0 8) (code data 1 (+ start %1)))) (range 0 n) 0)
				-1) start (+ start n)))
		(and (>= len 0) (<= (+ start len) (length data)) (list tag start (+ start len) at))))

(defun der-kids (data from to)
	; (der-kids data from to) -> ((tag start end head) ...)
	;the things one after the other from one place to another
	(defq out (list) at from)
	(while (and (< at to) (defq item (der-at data at)) (<= (third item) to))
		(push out item)
		(setq at (third item)))
	out)

(defun der-in (data item)
	; (der-in data item) -> ((tag start end head) ...)
	;the things inside a thing
	(der-kids data (second item) (third item)))

(defun der-bytes (data item)
	; (der-bytes data item) -> str
	;what is in a thing, without its head
	(slice data (second item) (third item)))

(defun der-whole (data item)
	; (der-whole data item) -> str
	;a thing with its head, as it was signed
	(slice data (elem-get item 3) (third item)))

(defun der-oid (bytes)
	; (der-oid bytes) -> str
	;an object identifier as it is written, 1.2.840.10045.2.1
	(cond
		((empty? bytes) "")
		(:t (defq top (code bytes 1 0) parts (list (str (/ top 40)) (str (% top 40))) v 0)
			(each (lambda (c)
					(setq v (logior (<< v 7) (logand (code c) 0x7f)))
					(when (< (code c) 0x80)
						(push parts (str v))
						(setq v 0)))
				(slice bytes 1 -1))
			(join parts "."))))

(defun x509-digits (text from to)
	; (x509-digits text from to) -> num
	(reduce (# (+ (* %0 10) (- (code %1) 48))) (slice text from to) 0))

(defun x509-time (data item)
	; (x509-time data item) -> seconds
	;a time of UTCTime, YYMMDDHHMMSSZ, the years 1950 to 2049, or of
	;GeneralizedTime, YYYYMMDDHHMMSSZ, as seconds from 1970
	;0, long ago, if it is too short to be a time
	(defq text (der-bytes data item) at (if (= (first item) 0x18) 4 2))
	(cond
		((< (length text) (+ at 10)) 0)
		(:t (defq year (x509-digits text 0 at))
			(if (= at 2) (setq year (+ year (if (< year 50) 2000 1900))))
			(+ (* (days-from-civil year (dec (x509-digits text at (+ at 2))) (x509-digits text (+ at 2) (+ at 4))) 86400)
				(* (x509-digits text (+ at 4) (+ at 6)) 3600) (* (x509-digits text (+ at 6) (+ at 8)) 60)
				(x509-digits text (+ at 8) (+ at 10))))))

(defun x509-bits (data item)
	; (x509-bits data item) -> str
	;what a BIT STRING has, after the byte of how many bits are not used
	(if (< (second item) (third item)) (slice data (inc (second item)) (third item)) ""))

(defun x509-key (cert data spki)
	; (x509-key cert data spki) -> cert
	;the kind of key, :rsa :p256 :p384 :ed25519, and the key. Of RSA, its
	;n and e
	(defq parts (der-in data spki))
	(when (and (= (length parts) 2) (= (first (second parts)) 0x03) (nempty? (der-in data (first parts))))
		(defq alg (der-in data (first parts)) bits (x509-bits data (second parts))
			oid (der-oid (der-bytes data (first alg)))
			param (and (> (length alg) 1) (= (first (second alg)) 0x06) (der-oid (der-bytes data (second alg)))))
		(cond
			((eql oid "1.2.840.113549.1.1.1")
				(when (and (defq seq (der-at bits 0)) (= (length (defq ints (der-in bits seq))) 2))
					(. cert :insert :key_kind :rsa)
					(. cert :insert :key (list (der-bytes bits (first ints)) (der-bytes bits (second ints))))))
			((eql oid "1.2.840.10045.2.1")
				(. cert :insert :key_kind (cond ((eql param "1.2.840.10045.3.1.7") :p256) ((eql param "1.3.132.0.34") :p384)))
				(. cert :insert :key bits))
			((eql oid "1.3.101.112")
				(. cert :insert :key_kind :ed25519)
				(. cert :insert :key bits))))
	cert)

(defun x509-extensions (cert data item)
	; (x509-extensions cert data item) -> cert
	;the names it is for, and whether it may issue others
	(when (defq seq (first (der-in data item)))
		(each (lambda (ext)
				;its name, critical or not, and the value, an OCTET STRING of
				;the DER of what it says
				(defq kids (der-in data ext) oid (der-oid (der-bytes data (first kids)))
					value (der-bytes data (elem-get kids (dec (length kids)))) inner (der-at value 0))
				(cond
					((or (< (length kids) 2) (not inner)))
					((eql oid "2.5.29.17")
						(. cert :insert :names (map (# (der-bytes value %0))
							(filter (# (= (first %0) 0x82)) (der-in value inner)))))
					((eql oid "2.5.29.19")
						(defq flags (der-in value inner))
						(. cert :insert :ca (and (nempty? flags) (= (first (first flags)) 0x01)
							(< (second (first flags)) (third (first flags)))
							(/= (code value 1 (second (first flags))) 0))))))
			(der-in data seq)))
	cert)

(defun x509-parse (der)
	; (x509-parse der) -> :nil | cert
	;:der :tbs, what was signed, :sig_alg :sig :issuer :subject, as
	;their DER, :from :until in seconds, :key_kind :key :names :ca
	(when (and (defq top (der-at der 0)) (= (first top) 0x30)
			(= (length (defq parts (der-in der top))) 3)
			(= (first (first parts)) 0x30) (= (first (third parts)) 0x03))
		(defq fields (der-in der (first parts)) alg (der-in der (second parts)))
		;the version, [0], is there in a version 3
		(and (nempty? fields) (= (first (first fields)) 0xa0) (setq fields (slice fields 1 -1)))
		(when (and (>= (length fields) 6) (nempty? alg))
			(bind '(& & issuer validity subject spki) (slice fields 0 6))
			(defq cert (Emap) times (der-in der validity))
			(when (= (length times) 2)
				(scatter cert :der der :tbs (der-whole der (first parts))
					:sig_alg (der-oid (der-bytes der (first alg))) :sig (x509-bits der (third parts))
					:issuer (der-whole der issuer) :subject (der-whole der subject)
					:from (x509-time der (first times)) :until (x509-time der (second times))
					:key_kind :nil :key :nil :names (list) :ca :nil)
				(x509-key cert der spki)
				(each (# (if (= (first %0) 0xa3) (x509-extensions cert der %0))) (slice fields 6 -1))
				cert))))

(defun x509-pem (stream)
	; (x509-pem stream) -> (der ...)
	;the certificates of a PEM file, each between its BEGIN and END lines
	(defq out (list) lines :nil)
	(lines! (lambda (line)
			(cond
				((starts-with "-----BEGIN CERTIFICATE" line) (setq lines (list)))
				((starts-with "-----END CERTIFICATE" line)
					(task-slice)
					(and lines (defq der (base64-decode (apply (const cat) lines))) (push out der))
					(setq lines :nil))
				(lines (push lines line)))
			:nil)
		stream)
	out)

(defun x509-roots ()
	; (x509-roots) -> (cert ...)
	;the roots the host keeps, read the first time they are wanted
	(unless *x509_roots*
		(setq *x509_roots* (list))
		(when (defq file (some (# (and (/= (age %0) 0) %0)) +x509_root_files))
			(each (# (task-slice) (if (defq cert (x509-parse %0)) (push *x509_roots* cert)))
				(x509-pem (file-stream file)))))
	*x509_roots*)

(defun x509-alg (oid)
	; (x509-alg oid) -> :nil | (scheme hash_name)
	;how a certificate is signed, from the identifier of the way
	(cond
		((eql oid "1.2.840.113549.1.1.11") '(:pkcs1 :sha256))
		((eql oid "1.2.840.113549.1.1.12") '(:pkcs1 :sha384))
		((eql oid "1.2.840.113549.1.1.13") '(:pkcs1 :sha512))
		((eql oid "1.2.840.10045.4.3.2") '(:ecdsa :sha256))
		((eql oid "1.2.840.10045.4.3.3") '(:ecdsa :sha384))
		((eql oid "1.2.840.10045.4.3.4") '(:ecdsa :sha512))
		((eql oid "1.3.101.112") '(:ed25519 :nil))))

(defun x509-sig-check (kind key scheme hash_name data signature)
	; (x509-sig-check kind key scheme hash_name data signature) -> :t | :nil
	;a signature of data by a key, signed one of the ways, scheme :pkcs1,
	;:pss, :ecdsa, :ed25519, with a hash, as a certificate or TLS has it
	(cond
		((eql scheme :ed25519) (and (eql kind :ed25519) (ed25519-verify key data signature)))
		((not (defq digest (rsa-hash hash_name data))) :nil)
		((eql scheme :pkcs1) (and (eql kind :rsa) (rsa-verify-pkcs1 (first key) (second key) hash_name digest signature)))
		((eql scheme :pss) (and (eql kind :rsa) (rsa-verify-pss (first key) (second key) hash_name digest signature)))
		((eql scheme :ecdsa)
			;two INTEGERs, r and s, in a SEQUENCE
			(and (defq curve (cond ((eql kind :p256) *ecdsa_p256*) ((eql kind :p384) *ecdsa_p384*)))
				(defq seq (der-at signature 0)) (= (first seq) 0x30)
				(= (length (defq ints (der-in signature seq))) 2)
				(ecdsa-verify curve key digest (der-bytes signature (first ints)) (der-bytes signature (second ints)))))))

(defun x509-signed? (cert issuer)
	; (x509-signed? cert issuer) -> :t | :nil
	;is cert issued by issuer: named so, and its signature that of the
	;issuer's key
	(and (eql (. cert :find :issuer) (. issuer :find :subject))
		(defq alg (x509-alg (. cert :find :sig_alg)))
		(x509-sig-check (. issuer :find :key_kind) (. issuer :find :key) (first alg) (second alg)
			(. cert :find :tbs) (. cert :find :sig))))

(defun x509-host? (cert host)
	; (x509-host? cert host) -> :t | :nil
	;is the certificate for the host, by one of its names, a name that
	;starts *. for any one name more in front of the rest
	(setq host (to-lower host))
	(some (lambda (name)
			(setq name (to-lower name))
			(or (eql name host)
				(and (starts-with "*." name) (defq dot (find "." host))
					(eql (slice host dot -1) (slice name 1 -1)))))
		(. cert :find :names)))

(defun x509-chain-check (certs host &optional roots now)
	; (x509-chain-check certs host [roots now]) -> :nil | why not
	;:nil if the first of certs is for the host, each is issued by the
	;next, up to one issued by a root, or a root, and each is in date
	(setd roots (x509-roots) now (/ (pii-time) 1000000))
	(defq why :nil done :nil at 0)
	(cond
		((or (empty? certs) (some (# (not %0)) certs)) (setq why "a certificate that does not read"))
		((empty? roots) (setq why "no roots, the host keeps none where they are looked for"))
		((not (x509-host? (first certs) host)) (setq why (cat "the certificate is not for " host))))
	(while (and (not why) (not done))
		(defq cert (elem-get certs at))
		(cond
			((> at 8) (setq why "too many between it and a root"))
			((not (<= (. cert :find :from) now (. cert :find :until)))
				(setq why (cat "out of date, " (str at) " up from the server's")))
			((and (> at 0) (not (. cert :find :ca)))
				(setq why "issued by one that may not issue"))
			((some (# (eql (. %0 :find :der) (. cert :find :der))) roots)
				(setq done :t))
			((some (# (and (<= (. %0 :find :from) now (. %0 :find :until)) (x509-signed? cert %0))) roots)
				(setq done :t))
			((and (< (inc at) (length certs)) (x509-signed? cert (elem-get certs (inc at))))
				(++ at))
			(:t (setq why "not issued by a root, nor by the next sent"))))
	why)

;module
(export-symbols '(x509-parse x509-pem x509-roots x509-sig-check x509-signed? x509-host?
	x509-chain-check der-at der-in der-bytes der-oid *x509_roots*))
(env-pop)
