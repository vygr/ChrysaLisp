(report-header "Crypto: AES of FIPS 197, and AES-GCM of the GCM paper's test cases")
(import "lib/crypto/gcm.inc")

;the examples of FIPS 197, C.1 and C.3
(defq tg_plain (hex-decode "00112233445566778899AABBCCDDEEFF")
	tg_k128 (aes-key (hex-decode "000102030405060708090A0B0C0D0E0F"))
	tg_k256 (aes-key (hex-decode "000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F")))
(assert-eq "AES-128, FIPS 197 C.1" "69C4E0D86A7B0430D8CDB78070B4C55A" (hex-encode (aes-block tg_k128 tg_plain)))
(assert-eq "AES-256, FIPS 197 C.3" "8EA2B7CA516745BFEAFC49904B496089" (hex-encode (aes-block tg_k256 tg_plain)))
;the native counter mode against the Lisp block, across the counter going
;round past its top, and a part block at the end
(defq tg_ctr (hex-decode "00112233445566778899AABBFFFFFFFF")
	tg_zeros (cat (char 0 8) (char 0 8) (char 0 8) (char 0 8) (char 0 8)))
(assert-eq "counter mode, native, is the blocks of the counters"
	(slice (cat (aes-block tg_k256 tg_ctr) (aes-block tg_k256 (aes-count tg_ctr 1)) (aes-block tg_k256 (aes-count tg_ctr 2))) 0 40)
	(aes-ctr tg_k256 tg_ctr tg_zeros))
(assert-eq "the counter goes round in its last 4 bytes" "00112233445566778899AABB00000001" (hex-encode (aes-count tg_ctr 2)))

;test cases 2 and 4 of McGrew and Viega, The Galois/Counter Mode of Operation
(defq tg_k2 (gcm-key (hex-decode "00000000000000000000000000000000")))
(assert-eq "GCM test case 2, a block of zeros" "0388DACE60B6A392F328C2B971B2FE78AB6E47D42CEC13BDF53A67B21257BDDF"
	(hex-encode (gcm-seal tg_k2 (hex-decode "000000000000000000000000") "" (hex-decode "00000000000000000000000000000000"))))
(defq tg_k4 (gcm-key (hex-decode "FEFFE9928665731C6D6A8F9467308308"))
	tg_iv (hex-decode "CAFEBABEFACEDBADDECAF888")
	tg_aad (hex-decode "FEEDFACEDEADBEEFFEEDFACEDEADBEEFABADDAD2")
	tg_p4 (hex-decode "D9313225F88406E5A55909C5AFF5269A86A7A9531534F7DA2E4C303D8A318A721C3C0C95956809532FCF0E2449A6B525B16AEDF5AA0DE657BA637B39")
	tg_s4 (gcm-seal tg_k4 tg_iv tg_aad tg_p4))
(assert-eq "GCM test case 4, what is hidden"
	"42831EC2217774244B7221B784D0D49CE3AA212F2C02A4E035C17E2329ACA12E21D514B25466931C7D8F6A5AAC84AA051BA30B396A0AAC973D58E091"
	(hex-encode (slice tg_s4 0 -17)))
(assert-eq "and its tag" "5BC94FBC3221A5DB94FAE95AE7121A47" (hex-encode (slice tg_s4 -17 -1)))
(assert-eq "opened" tg_p4 (gcm-open tg_k4 tg_iv tg_aad tg_s4))
(assert-eq "not opened with other aad" :nil (gcm-open tg_k4 tg_iv (cat tg_aad "x") tg_s4))
(assert-eq "nor with a byte changed" :nil
	(gcm-open tg_k4 tg_iv tg_aad (cat (char (logxor (code tg_s4) 1)) (slice tg_s4 1 -1))))
(assert-eq "nor too short to have a tag" :nil (gcm-open tg_k4 tg_iv tg_aad "short"))
;sizes about the edges of the blocks, with and without aad
(defq tg_text (apply (const cat) (map (# (char (logand (+ (* %0 17) 1) 0xff))) (range 0 70))))
(assert-true "each size, sealed and opened, 0 to 69, aad or none"
	(every (lambda (n)
			(defq data (slice tg_text 0 n) aad (slice tg_text 0 (% n 23)))
			(eql data (gcm-open tg_k4 tg_iv aad (gcm-seal tg_k4 tg_iv aad data))))
		(range 0 70)))
