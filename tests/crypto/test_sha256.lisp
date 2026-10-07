(report-header "Crypto: SHA-256")
(import "lib/crypto/sha256.inc")

(defun sha-hex (bytes)
	;the bytes of a hash as hex
	(to-lower (apply (const cat) (map (# (byte-to-hex-str (code %0))) bytes))))

;the answers of FIPS 180-4 and its examples
(assert-eq "nothing" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
	(sha-hex (sha256 "")))
(assert-eq "abc" "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
	(sha-hex (sha256 "abc")))
(assert-eq "two blocks of padding, 56 bytes" "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
	(sha-hex (sha256 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")))
(assert-eq "112 bytes" "cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1"
	(sha-hex (sha256 "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu")))

;every length about the edges of a block, where the padding changes, of
; bytes that are not all the same
(defq sha_text (apply (const cat) (map (# (char (logand (+ (* %0 7) 3) 0xff))) (range 0 200))))
(assert-eq "1 bytes" "084fed08b978af4d7d196a7446a86b58009e636b611db16211b65a9aadff29c5" (sha-hex (sha256 (slice sha_text 0 1))))
(assert-eq "54 bytes" "160bbf14b458c877b7049e7cb5771dd653930f97d20bdd8ee795c16062906233" (sha-hex (sha256 (slice sha_text 0 54))))
(assert-eq "55 bytes" "e7313d333c272e639f790978283f9eb392e843d0f29b7016828bb1daa4aac70b" (sha-hex (sha256 (slice sha_text 0 55))))
(assert-eq "56 bytes" "4324d65f3c103567f5589c710bc08f8523f929a9272e3af36fc968e52abc6c27" (sha-hex (sha256 (slice sha_text 0 56))))
(assert-eq "57 bytes" "35df609437dcfea3279283ab79fd554e2bf78f8f7ae2de532d8ee300b09e8f73" (sha-hex (sha256 (slice sha_text 0 57))))
(assert-eq "63 bytes" "81c80242132f230c3bd41b3e63bbcff16107339549214a99614ff26664625055" (sha-hex (sha256 (slice sha_text 0 63))))
(assert-eq "64 bytes" "39e3d7b6b5d075d37d053ad89b24b41bef4f3c29760c84447cab3f3be1882241" (sha-hex (sha256 (slice sha_text 0 64))))
(assert-eq "65 bytes" "aacca6ff74fdbb296d165a45cecfa04e5127bc008770fbbdd48006f2d2fae95e" (sha-hex (sha256 (slice sha_text 0 65))))
(assert-eq "119 bytes" "9ce7368e4daf32341631b492e80359dc9f594b48453cd0dd5bf0b19279cc177e" (sha-hex (sha256 (slice sha_text 0 119))))
(assert-eq "120 bytes" "7836b787757e95e58b3ca5aec90b1b004e8deba1e50e9675af9cabf1a13a04b5" (sha-hex (sha256 (slice sha_text 0 120))))
(assert-eq "121 bytes" "1189a98a00c71bc1848ea8bdc9700b442bee0be7c3f45172303f1ab0b6f1617e" (sha-hex (sha256 (slice sha_text 0 121))))
(assert-eq "127 bytes" "a8d23e75d936f303d248888d9b165ee543f4cbafcad3c9dd2a79bd84faa11d07" (sha-hex (sha256 (slice sha_text 0 127))))
(assert-eq "128 bytes" "d2742f1f4ac6bb7ca2b239ee18402ba8b3f9f8e652d2a72973c2b9ba11c08cf6" (sha-hex (sha256 (slice sha_text 0 128))))
(assert-eq "129 bytes" "307f8fc2c1622b92762e818d39a185d4d667ad49a4b07ceae1f4afa008a93ec4" (sha-hex (sha256 (slice sha_text 0 129))))
(assert-eq "200 bytes" "2c7e18c942ef065b526a2d4e5546283749cd3ddfb51d8fc71f42717363685f46" (sha-hex (sha256 (slice sha_text 0 200))))

;a million of the letter a, which is more than the native code is given
; at once
(defq sha_million (str-alloc 1000000))
(each (# (set-long sha_million %0 0x6161616161616161)) (range 0 1000000 8))
(assert-eq "a million a" "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0" (sha-hex (sha256 sha_million)))

;added a part at a time, in parts of any size, it is the same hash
(each (lambda (part)
	(defq ctx (sha256-start) at 0)
	(while (< at 200)
		(sha256-add ctx (slice sha_text at (min 200 (+ at part))))
		(setq at (+ at part)))
	(assert-eq (cat "200 bytes, " (str part) " at a time") "2c7e18c942ef065b526a2d4e5546283749cd3ddfb51d8fc71f42717363685f46" (sha-hex (sha256-end ctx))))
	'(1 3 63 64 65 127 199 200 500))

;what is hashed is not changed, and a hash can be taken twice
(defq sha_before (cat sha_text))
(sha256 sha_text)
(assert-eq "the data is as it was" sha_before sha_text)
(assert-eq "the same again" (sha256 sha_text) (sha256 sha_text))
(assert-eq "32 bytes" 32 (length (sha256 "")))

;what the native code refuses
(assert-error "a state of the wrong size" (sha256-blocks "short" sha_text 0 1))
(assert-error "more blocks than there are" (sha256-blocks (str-alloc 32) sha_text 0 4))
(assert-error "an offset past the end" (sha256-blocks (str-alloc 32) sha_text 192 1))
(assert-error "a count below 0" (sha256-blocks (str-alloc 32) sha_text 0 -1))

(report-header "Crypto: HMAC with SHA-256")

(defun sha-bytes (value count)
	;count bytes of that value
	(apply (const cat) (map (lambda (_) (char value)) (range 0 count))))

;the test cases of RFC 4231
(assert-eq "case 1, a key of 20 bytes" "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"
	(sha-hex (hmac-sha256 (sha-bytes 0x0b 20) "Hi There")))
(assert-eq "case 2, a key shorter than the hash" "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"
	(sha-hex (hmac-sha256 "Jefe" "what do ya want for nothing?")))
(assert-eq "case 3, data of 50 bytes" "773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe"
	(sha-hex (hmac-sha256 (sha-bytes 0xaa 20) (sha-bytes 0xdd 50))))
(assert-eq "case 6, a key longer than a block" "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54"
	(sha-hex (hmac-sha256 (sha-bytes 0xaa 131) "Test Using Larger Than Block-Size Key - Hash Key First")))
(assert-eq "a key of just a block" "806f51f48af1a48ae87cd0df340c616f23dd57b833a66177fd22a791124dd49c"
	(sha-hex (hmac-sha256 (sha-bytes 0x61 64) (slice sha_text 0 100))))
(assert-eq "no key and no data" "b613679a0814d9ec772f95d778c35fc5ff1697c493715653c6c712144292c5ad"
	(sha-hex (hmac-sha256 "" "")))
(assert-true "another key, another hash"
	(not (eql (hmac-sha256 "key one" "data") (hmac-sha256 "key two" "data"))))
