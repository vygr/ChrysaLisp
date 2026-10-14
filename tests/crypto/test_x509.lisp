(report-header "Crypto: X.509, certificates read and a chain of them checked, made by OpenSSL")

(import "lib/crypto/x509.inc")

;a root of P-384 that signs with SHA-384 an issuer of RSA 2048, that signs
;with SHA-256 a certificate of P-256 for test.example and *.wild.example,
;and one for the same that was in date only in 2020
(defun tx-cert (name) (x509-parse (first (x509-pem (file-stream (cat "tests/crypto/data/x509_" name ".pem"))))))
(defq tx_root (tx-cert "root") tx_issuer (tx-cert "issuer") tx_leaf (tx-cert "leaf") tx_old (tx-cert "old")
	tx_2030 1893456000)

(assert-eq "base64 there and back" "the quick brown fox" (base64-decode (base64-encode "the quick brown fox")))
(assert-eq "base64 of RFC 4648" "Zm9vYmFy" (base64-encode "foobar"))
(assert-eq "and padded" "Zm9vYg==" (base64-encode "foob"))
(assert-eq "and back past the end of a line" "foob" (base64-decode (cat "Zm9v" (ascii-char 10) "Yg==")))

(assert-true "each reads" (and tx_root tx_issuer tx_leaf tx_old))
(assert-eq "the kinds of key" "(:p384 :rsa :p256)" (str (map (# (. %0 :find :key_kind)) (list tx_root tx_issuer tx_leaf))))
(assert-eq "which may issue" "(:t :t :nil)" (str (map (# (if (. %0 :find :ca) :t :nil)) (list tx_root tx_issuer tx_leaf))))
(assert-eq "the names it is for" "(\qtest.example\q \q*.wild.example\q)" (str (. tx_leaf :find :names)))
(assert-eq "from, GeneralizedTime past 2049" 1792011960 (. tx_leaf :find :from))
(assert-eq "until" 4816011960 (. tx_leaf :find :until))
(assert-eq "UTCTime" 1577836800 (. tx_old :find :from))
(assert-eq "how it is signed" "1.2.840.113549.1.1.11" (. tx_leaf :find :sig_alg))
(assert-true "the root signed itself" (x509-signed? tx_root tx_root))
(assert-true "RSA by P-384 and SHA-384" (x509-signed? tx_issuer tx_root))
(assert-true "P-256 by RSA and SHA-256" (x509-signed? tx_leaf tx_issuer))
(assert-true "not by the root" (not (x509-signed? tx_leaf tx_root)))

(defq tx_roots (list tx_root))
(assert-eq "a chain up to the root" :nil (x509-chain-check (list tx_leaf tx_issuer) "test.example" tx_roots tx_2030))
(assert-eq "and with the root sent too" :nil (x509-chain-check (list tx_leaf tx_issuer tx_root) "test.example" tx_roots tx_2030))
(assert-eq "a name of the wildcard" :nil (x509-chain-check (list tx_leaf tx_issuer) "Any.Wild.Example" tx_roots tx_2030))
(assert-true "but one name, not two" (x509-chain-check (list tx_leaf tx_issuer) "a.b.wild.example" tx_roots tx_2030))
(assert-true "nor none" (x509-chain-check (list tx_leaf tx_issuer) "wild.example" tx_roots tx_2030))
(assert-true "nor another host" (x509-chain-check (list tx_leaf tx_issuer) "example.com" tx_roots tx_2030))
(assert-true "not without the one between" (x509-chain-check (list tx_leaf) "test.example" tx_roots tx_2030))
(assert-true "nor with no root" (x509-chain-check (list tx_leaf tx_issuer) "test.example" (list tx_old) tx_2030))
(assert-true "nor before it was made" (x509-chain-check (list tx_leaf tx_issuer) "test.example" tx_roots 1700000000))
(assert-true "nor out of date" (x509-chain-check (list tx_old tx_issuer) "test.example" tx_roots tx_2030))
(assert-true "nor in 2020 when it was, its issuer was not yet" (x509-chain-check (list tx_old tx_issuer) "test.example" tx_roots 1590000000))
(defq tx_noca (tx-cert "issuer"))
(. tx_noca :insert :ca :nil)
(assert-eq "nor one whose issuer may not issue" "issued by one that may not issue"
	(x509-chain-check (list tx_leaf tx_noca) "test.example" tx_roots tx_2030))

;a byte of what was signed changed, and it is no longer signed
(defq tx_der (. tx_leaf :find :der) tx_at (- (length tx_der) 300)
	tx_bad (x509-parse (cat (slice tx_der 0 tx_at) (char (logxor (code tx_der 1 tx_at) 1)) (slice tx_der (inc tx_at) -1))))
(assert-true "a byte changed, it still reads" tx_bad)
(assert-true "and is not signed" (not (x509-signed? tx_bad tx_issuer)))
(assert-true "nor does what is not DER read" (not (x509-parse "not a certificate")))

;the host's own roots, where it keeps them
(when (some (# (/= (age %0) 0)) '("/etc/ssl/cert.pem" "/etc/ssl/certs/ca-certificates.crt" "/etc/pki/tls/certs/ca-bundle.crt"))
	(assert-true "the host's roots read" (> (length (x509-roots)) 50))
	(assert-true "and nearly all of a kind of key known here" (> (length (filter (# (. %0 :find :key_kind)) (x509-roots))) 50)))
