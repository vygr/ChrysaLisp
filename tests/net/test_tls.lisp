(report-header "Net: TLS 1.3, the key schedule and the nonces, against a second working of RFC 8446")

(import "lib/net/tls.inc")

;the values were worked out by Python's hmac, from RFC 8446 7.1 and 5.3,
;not by this code
(defq tt_secret (apply (const cat) (map (const char) (range 0 32))))
(assert-eq "HKDF-Expand-Label, a key" "2FFBC449E87844051C7768F61FFD8AD070830F01EC2C520EF65F08E8296DFFA0"
	(hex-encode (first (tls-keys tt_secret))))
(assert-eq "and an iv" "2F41C846A431A163814BCD71" (hex-encode (second (tls-keys tt_secret))))
(assert-eq "with a context, derived" "A5B1CAA258481FDF573AC069F281E534E4A2379EC9E457E0C8494C227EFB40E6"
	(hex-encode (tls-label tt_secret "derived" (sha256 "") 32)))
(assert-eq "the nonce of record 5, the iv xor its number" "2F41C846A431A163814BCD74"
	(hex-encode (tls-nonce (second (tls-keys tt_secret)) 5)))
(assert-eq "a number high byte first" "000102" (hex-encode (tls-be 258 3)))
