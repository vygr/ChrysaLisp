(report-header "Net: TLS 1.3 and 1.2, the key schedules, the nonces and the records, against second workings of the RFCs")

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

;TLS 1.2. The PRF of RFC 5246 5, worked out by Python's hmac
(assert-eq "the PRF of 1.2, a key block"
	"5D8C56F8CBD44E18D7CB6EEFA3A53771CB6068399BFC8B1A2D27CF5DE985DA84282BD93F1D192C44EE175E6AA5A5E01C087F216D64841250D265030AD9ADB1F615A2B72B4FA7DB0A"
	(hex-encode (tls-prf tt_secret "key expansion" (apply (const cat) (map (const char) (range 0 64))) 72)))
(assert-eq "and a Finished" "4B62D88C0931E331E8514CF2"
	(hex-encode (tls-prf (apply (const cat) (map (const char) (range 0 48))) "client finished" (sha256 "a handshake") 12)))

;a ServerHello of 1.2: what it took, and whether it makes the keys of the
;whole handshake
(defun tt-sh (random suite ext)
	;ext :nil, and there is no block of them at all, as 1.2 allows
	(defq body (cat (tls-be 0x0303 2) random (char 0) (tls-be suite 2) (char 0) (if ext (tls-vec 2 ext) "")))
	(cat (char 2) (tls-be (length body) 3) body))
(defq tt_random (apply (const cat) (map (# (char (+ %0 1))) (range 0 32))))
(assert-eq "1.2, AES-128-GCM, the keys of the whole handshake"
	(str (list 0x0303 0xc02f tt_random :nil :t)) (str (tls-server-hello (tt-sh tt_random 0xc02f (cat (tls-be 23 2) (tls-be 0 2))))))
(assert-eq "1.2, ChaCha20, not, no extensions at all" (str (list 0x0303 0xcca8 tt_random :nil :nil)) (str (tls-server-hello (tt-sh tt_random 0xcca8 :nil))))
(assert-eq "a suite not offered is not taken" :nil (tls-server-hello (tt-sh tt_random 0x009c "")))
(assert-eq "nor a server of 1.2 whose random says it has 1.3" :nil
	(tls-server-hello (tt-sh (cat (slice tt_random 0 24) "DOWNGRD" (char 1)) 0xc02f "")))

;a ServerKeyExchange: its X25519 share, both randoms and it signed by the
;key of the certificate, by OpenSSL with a key made for this, RSA by
;PKCS #1 v1.5 and P-256
(defun tt-ske (scheme sig)
	(defq body (cat (char 3) (tls-be 0x001d 2) (char 32) (apply (const cat) (map (const char) (range 100 132)))
		(tls-be scheme 2) (tls-vec 2 sig)))
	(cat (char 12) (tls-be (length body) 3) body))
(defq tt_cr (apply (const cat) (map (lambda (&) (char 1)) (range 0 32)))
	tt_sr (apply (const cat) (map (lambda (&) (char 2)) (range 0 32)))
	tt_point (apply (const cat) (map (const char) (range 100 132)))
	tt_rsa_ske (scatter (Emap) :key_kind :rsa :key (list (hex-decode "AB8B6EDAA07D807339187ECCCD3A0FD9788AA01854E315296E520758440E3E76F210C621FEBC1BF3C5A8709167C4565902C85F4DE88EE1BE8A17F227865F6BF657641B3AC02818BEEC0AF5A7C98F7BA17A3DE2E51596792C976900018E1BB3563E6F9CC405B238F225B25B676E513532C2FF4428E235518CE52A195A6120B417E423D60825F258850F3A1BA674C3EA082FCAAA701EE5CD3D43968285D9E6FA7C75359A6458801CE70E3C64E334CE8C45F49D2521B435B5519D23A2BD1E47371FDA84F78979AC22952EE714982E7EDBDE293670524217421E757021E3C816BC376BA34E86BD7884C6D40A354C406FC751228F36F72BA669F8953C649F2850F28F") (hex-decode "010001")))
	tt_rsa_sig (hex-decode "816C6685C036EDCCD0723A99A79429AEEAE8E1AA61722E6089FCB2C1DD0B6CCFA932D98D46CCD6E8D5D86FA819D527ACC50A72601AAEDD8150A09540F754EA0B1B35726D1BD8B0985381101C4CE2648ED042C4D0A7F9ACA5DF1316D27EBB41F6704BF8A175C37CDB631CFD9B2BC08EBA49E9DF88485C28AAE4C7DB7EA40FA1443EE31AADDFB700F2F82CA276300C5B7C1CC43DBCDD968CC790A03180AFC60BCA9CB3BB918F2F219A79B84DACF0E44DA004A1B1864E6F3A90E2AD976DF49B6558D0A44DBD1F1C6A3EABFF3E13F84F7FE57E35CAC3CF57E3EE36B6FFB59FB60F607D8906155B2BD74F192778663FBCA7E32F3CF6B3A26A377591CCC32967BE868E")
	tt_ec_ske (scatter (Emap) :key_kind :p256 :key (hex-decode "048D4BE9BA4C21DFD2E27DAA0301F00F5D20D6DB62A64904E91A6E936C570C1D7EDA52DD8C7C3318879736E331C3EC3681865A66F19D88A10C1C625063D3BF5431"))
	tt_ec_ske_sig (hex-decode "30460221009001D863284DE799E0BF64A624B7C22A555B3656A55408B33F76C460853CD45002210098D95D7A0192900BA7C610EBCDFB3D360A38643E06D01ED1E49F982827407EC7"))
(assert-eq "ServerKeyExchange, rsa_pkcs1_sha256" (str (list 0x001d tt_point))
	(str (tls-server-kx (tt-ske 0x0401 tt_rsa_sig) tt_rsa_ske tt_cr tt_sr)))
(assert-eq "ecdsa_secp256r1_sha256" (str (list 0x001d tt_point))
	(str (tls-server-kx (tt-ske 0x0403 tt_ec_ske_sig) tt_ec_ske tt_cr tt_sr)))
(assert-eq "not of other randoms" :nil (tls-server-kx (tt-ske 0x0401 tt_rsa_sig) tt_rsa_ske tt_sr tt_cr))
(assert-eq "nor by another key" :nil (tls-server-kx (tt-ske 0x0401 tt_rsa_sig) tt_ec_ske tt_cr tt_sr))
(assert-true "and PKCS #1 v1.5 is not taken of a CertificateVerify of 1.3, though it signs it"
	(not (tls-verify tt_rsa_ske (tt-cv 0x0401 (hex-decode "213458C7568B1A9F2BF64BB7D5BB2047B66F2E9744ED542BC2089E478B3690F0EE4CCA4B41EAEC6B60FFF7C77FDA460BE9147D4D00B5AAE40228B6926D1E7F122D27D066CF1806649798BBBD3058DDEF8883645701C5C17E43DDF81BEE47B2B3BB57AF4649E424B15A5590D1FB7B0E816B082E1F514986329C0EAC1109E6B69371A89E52AC29ACC0B91EF12827A12ADD3E5EC0FA22FE66BD663C6D45169CCB1E1B48556DDD1C838E485C3714F2FE9CAC223D785711EC7017F906A49E31650F00E03545D63105CA837406D4D0DEB47C09738504EAB45C1184B2BA8AD539E74D1CC49FD260BD101D69321F165555CE4B6150883AA01013BE70C72F7B1BAB5F7DE3")) tt_handshake)))

;a record of 1.2 sealed by one side and opened by the other, AES-GCM, its
;number in front, and ChaCha20-Poly1305
(defq tt_written (list))
(defun tt-record (aead keys data)
	;what is written kept, as a connection would send it
	(clear tt_written)
	(defq out (lisp-stream '(lambda () :nil) :nil '(lambda (data flush) (push tt_written data)))
		state (scatter (Emap) :v12 :t :aead aead :rout out :wkeys keys :wseq 5 :rkeys keys :rseq 5))
	(tls-seal state 23 data)
	(defq rec (apply (const cat) tt_written))
	(list (code rec 1 0) (length rec) (tls-open12 state 23 (slice rec 5 -1))))
(assert-eq "a record of 1.2, AES-128-GCM, sealed and opened" (str (list 23 (+ 5 8 11 16) "hello there"))
	(str (tt-record :gcm (list (gcm-key (slice tt_secret 0 16)) (slice tt_secret 16 20)) "hello there")))
(assert-eq "ChaCha20-Poly1305" (str (list 23 (+ 5 11 16) "hello there"))
	(str (tt-record :chacha (list tt_secret (slice tt_secret 0 12)) "hello there")))
