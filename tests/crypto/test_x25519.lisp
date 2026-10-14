(report-header "Crypto: X25519 of RFC 7748, and HKDF of RFC 5869, by their own test vectors")

(import "lib/crypto/x25519.inc")
(import "lib/crypto/hkdf.inc")

(defun tx-h (s) (hex-decode (to-upper s)))
(defun tx-x (s) (hex-encode s))

;RFC 7748 5.2, one multiply
(assert-eq "a scalar times a point" (to-upper "c3da55379de9c6908e94ea4df28d084f32eccf03491c71f754b4075577a28552")
	(tx-x (x25519 (tx-h "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4")
		(tx-h "e6db6867583030db3594c1a424b15f7c726624ec26b3353b10a903a6d0ab1c4c"))))

;RFC 7748 6.1, Alice and Bob
(defq tx_alice (tx-h "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a")
	tx_bob (tx-h "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb"))
(assert-eq "Alice's public half" (to-upper "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a")
	(tx-x (x25519-public tx_alice)))
(assert-eq "Bob's public half" (to-upper "de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f")
	(tx-x (x25519-public tx_bob)))
(assert-eq "the secret they share, as Alice has it" (to-upper "4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742")
	(tx-x (x25519 tx_alice (x25519-public tx_bob))))
(assert-eq "and as Bob has it" (to-upper "4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742")
	(tx-x (x25519 tx_bob (x25519-public tx_alice))))

;RFC 5869 A.1
(defq tx_prk (hkdf-extract (tx-h "000102030405060708090a0b0c") (tx-h "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b")))
(assert-eq "HKDF extract" (to-upper "077709362c2e32df0ddc3f0dc47bba6390b6c73bb50f9c3122ec844ad7c2b3e5") (tx-x tx_prk))
(assert-eq "HKDF expand" (to-upper "3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865")
	(tx-x (hkdf-expand tx_prk (tx-h "f0f1f2f3f4f5f6f7f8f9") 42)))
