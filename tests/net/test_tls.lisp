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

;the server's CertificateVerify, signed by OpenSSL: 64 spaces, the words,
;a 0 and the hash of the handshake so far, by P-256 and by RSA PSS
(defun tt-cv (scheme sig)
	(defq body (cat (tls-be scheme 2) (tls-be (length sig) 2) sig))
	(cat (char 15) (tls-be (length body) 3) body))
(defq tt_handshake "a handshake, as far as the Certificate"
	tt_ec (scatter (Emap) :key_kind :p256 :key (hex-decode "0421E961B995369C767E93869B516209AFF15A030C3498AD03475DBDCC53E58945F6ED589B44EFB50BE876C8BBDC1D12495A613BCB2456E84BFE9DC6B812AD13F3"))
	tt_rsa (scatter (Emap) :key_kind :rsa :key (list (hex-decode "DBC35178CF134DE4BD108AE4368725EC05EBCDFE4AC1C892ADFCD0FCB662072822F116F06F7D8425660AE64C3D351EB9B8F4C0FEB67516A89C1BC93483806BC05D77EACE78E83B76F67E8C4AB79E0E3B340BBFDE2EFBFC7F14C6FE5A4CEF93108A954A7213A7B58DF378C77F85AE9CE01A4B7C55602CAC4376EC03EB4FAF181250744844DDDE60BAC907FCA99E5ECA1F278DF21FCE76E8D21A34750D8311F0824FD0C332E8BC481665DC3628DC54E4CFCD2948389A0D27813E20397C12125BEF646FFCD4D8C4C43ABD00CA8BF567A8B362C088002A6E7908B761C67DD827270907BADAADDA69B3C1920C6C82AF89DA06DF484BF6BEC481797140B95D818C0B77") (hex-decode "010001")))
	tt_ec_sig (hex-decode "3045022037B8B5CF5A74616A02F4404DAEA4D7AC1FA5A3D0ACA09FBBADF8D7941F1056C4022100A2C72E5D60E4CEFF47F7548F0B40E031EC68BC6E9ADE1BDEEBFE7217EEF24CCA")
	tt_pss_sig (hex-decode "A69D5372FE868F996FF22C9B3F42615D00ACB7A0CEB0AFE251F9F750488DCD7A38A36A241EC8DC975EF3D2CB83F13B0CD5B2098BB23ED454D06F9AA582D801BBE9CAE989BF37D2467C61EB8630FC50170A483A7EF67E752212B182EF2997B5A135535181532A155A3CBD78BF3E587FF863FD40E9A67B6B22D14B0377B222486CC24F33408629C234C01EABB6D3137A22B6D4A64C2470EA745004DA5794828ED8D1843D1569DBB9DEC1EFBE21E72A4DA5CE583F94A30F8D68A7C028D3DB58FB1ABDA96C613BA8213B0D12EA54776707C20180E542C8BD70A0F2BB017B836A36BDCE9B881796BD245948F74143441BE70BF25BA8B1024A533EC4F50430042D8B95"))
(assert-true "CertificateVerify, ecdsa_secp256r1_sha256" (tls-verify tt_ec (tt-cv 0x0403 tt_ec_sig) tt_handshake))
(assert-true "rsa_pss_rsae_sha256" (tls-verify tt_rsa (tt-cv 0x0804 tt_pss_sig) tt_handshake))
(assert-true "not of another handshake" (not (tls-verify tt_ec (tt-cv 0x0403 tt_ec_sig) "another")))
(assert-true "nor by another key" (not (tls-verify tt_rsa (tt-cv 0x0403 tt_ec_sig) tt_handshake)))
(assert-true "nor said to be a scheme it is not" (not (tls-verify tt_rsa (tt-cv 0x0805 tt_pss_sig) tt_handshake)))
(assert-true "nor of a scheme not known" (not (tls-verify tt_ec (tt-cv 0x0203 tt_ec_sig) tt_handshake)))
(assert-true "nor with no certificate" (not (tls-verify :nil (tt-cv 0x0403 tt_ec_sig) tt_handshake)))

;a Certificate message of the test chain, as a server sends it
(defun tt-der (name) (first (x509-pem (file-stream (cat "tests/crypto/data/x509_" name ".pem")))))
(defq tt_certs (tls-certificates (cat (char 11) (tls-be 0 3) (char 0)
	(tls-vec 3 (apply (const cat) (map (# (cat (tls-vec 3 (tt-der %0)) (tls-be 0 2))) '("leaf" "issuer")))))))
(assert-eq "the certificates of a Certificate" 2 (length tt_certs))
(assert-eq "each read" "(\qtest.example\q \q*.wild.example\q)" (str (. (first tt_certs) :find :names)))
(assert-eq "and they go up to the root" :nil
	(x509-chain-check tt_certs "test.example" (list (x509-parse (tt-der "root"))) 1893456000))
