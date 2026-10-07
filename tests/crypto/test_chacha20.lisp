(report-header "Crypto: ChaCha20")
(import "lib/crypto/sha256.inc")
(import "lib/crypto/chacha20.inc")

(defun cc-hex (bytes)
	(to-lower (apply (const cat) (map (# (byte-to-hex-str (code %0))) bytes))))
(defun cc-unhex (text)
	;the bytes of a str of hex
	(apply (const cat) (map (# (char (str-to-num (cat "0x" (slice text %0 (+ %0 2)))))) (range 0 (length text) 2))))

;the example of RFC 8439, 2.4.2
(defq cc_key (apply (const cat) (map (const char) (range 0 32)))
	cc_nonce (cc-unhex "000000000000004a00000000")
	cc_plain "Ladies and Gentlemen of the class of '99: If I could offer you only one tip for the future, sunscreen would be it.")
(assert-eq "RFC 8439, the sunscreen text"
	"6e2e359a2568f98041ba0728dd0d6981e97e7aec1d4360c20a27afccfd9fae0bf91b65c5524733ab8f593dabcd62b3571639d624e65152ab8f530c359f0861d807ca0dbf500d6a6156a38e088a22b65e52bc514d16ccf806818ce91ab77937365af90bbf74a35be6b40b8eedf2785e42874d"
	(cc-hex (chacha20 cc_key cc_nonce 1 cc_plain)))
(assert-eq "and back again" cc_plain (chacha20 cc_key cc_nonce 1 (chacha20 cc_key cc_nonce 1 cc_plain)))

;the key stream itself, the first block of a key and a nonce of all 0, RFC
;8439 2.3.2 has it for another key
(assert-eq "zeros in, the key stream out"
	"224f51f3401bd9e12fde276fb8631ded8c131f823d2c06e27e4fcaec9ef3cf788a3b0aa372600a92b57974cded2b9334794cba40c63e34cdea212c4cf07d41b7"
	(cc-hex (chacha20 cc_key cc_nonce 1 (cc-unhex "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"))))

;every length about the edges of a block, the answers by their hashes
(defq cc_text (apply (const cat) (map (# (char (logand (+ (* %0 11) 5) 0xff))) (range 0 300))))
(assert-eq "0 bytes" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" (cc-hex (sha256 (chacha20 cc_key cc_nonce 7 (slice cc_text 0 0)))))
(assert-eq "1 bytes" "ffe679bb831c95b67dc17819c63c5090d221aac6f4c7bf530f594ab43d21fa1e" (cc-hex (sha256 (chacha20 cc_key cc_nonce 7 (slice cc_text 0 1)))))
(assert-eq "2 bytes" "ee98dc6af27a9f1c8cc4aaa2fd05b5f6e7c98f51390253a5263b0d096c3510fe" (cc-hex (sha256 (chacha20 cc_key cc_nonce 7 (slice cc_text 0 2)))))
(assert-eq "31 bytes" "fbbb050d60bd246fa1d118d497e9cee885f2e2b156c3ea84ae8dff471cd3962c" (cc-hex (sha256 (chacha20 cc_key cc_nonce 7 (slice cc_text 0 31)))))
(assert-eq "63 bytes" "0385b9b5b2ed253809d8cad9fd294b47fcb42fb77e26189bbbf2eb69e47fcc62" (cc-hex (sha256 (chacha20 cc_key cc_nonce 7 (slice cc_text 0 63)))))
(assert-eq "64 bytes" "b51b519934666891b103acfd88b6d933e9d4a1776aca68cdf6193ec422d5b05d" (cc-hex (sha256 (chacha20 cc_key cc_nonce 7 (slice cc_text 0 64)))))
(assert-eq "65 bytes" "c8fa8cac86ab6607ea385ff52c9b5d2aec747dd206b0036f778883cfa7283b55" (cc-hex (sha256 (chacha20 cc_key cc_nonce 7 (slice cc_text 0 65)))))
(assert-eq "127 bytes" "dcb9b472fb9088c02a1d76e40409a944dcbeea7b117174272538e5fb155cb0c1" (cc-hex (sha256 (chacha20 cc_key cc_nonce 7 (slice cc_text 0 127)))))
(assert-eq "128 bytes" "7606956401fc6bec283bbebae9bfd8a9a4ac9f871e05350fc112cb2e2bbc8b51" (cc-hex (sha256 (chacha20 cc_key cc_nonce 7 (slice cc_text 0 128)))))
(assert-eq "129 bytes" "2157a3e256daaeb217e79bdc26e264f5c0d33f1557a0275a0764f3039bcd0a28" (cc-hex (sha256 (chacha20 cc_key cc_nonce 7 (slice cc_text 0 129)))))
(assert-eq "191 bytes" "5b81d2ff8fef0319d91754c0c7df2ccac8b61f17349bbcf7b3f3daea95bea33c" (cc-hex (sha256 (chacha20 cc_key cc_nonce 7 (slice cc_text 0 191)))))
(assert-eq "192 bytes" "32c3d79d48102891e4c3db2d74ddb593b6e19e13258f36b34f07e653a2633742" (cc-hex (sha256 (chacha20 cc_key cc_nonce 7 (slice cc_text 0 192)))))
(assert-eq "193 bytes" "8eb9da65dc63bb8bce051f4beb2a04b43177e1fb9c86c1e657f44b93c7f27b78" (cc-hex (sha256 (chacha20 cc_key cc_nonce 7 (slice cc_text 0 193)))))
(assert-eq "300 bytes" "be416aa4a0b7b5336ef573e8c1ee12aa78b66734e857b455dd9d662f0960ea86" (cc-hex (sha256 (chacha20 cc_key cc_nonce 7 (slice cc_text 0 300)))))

;more than the native code is given at once, and not a whole number of
; blocks, so the counter has to carry on right from one part to the next
(defq cc_big (apply (const cat) (map (# (char (logand (+ (* %0 31) 7) 0xff))) (range 0 200003))))
(defq cc_big_out (chacha20 cc_key cc_nonce 0 cc_big))
(assert-eq "200,003 bytes" "e4cca1ad6e7627fb786aede51634476e9be9f2670f0347c3a0002b6cb36ded9a" (cc-hex (sha256 cc_big_out)))
(assert-eq "200,003 bytes, and back" (sha256 cc_big) (sha256 (chacha20 cc_key cc_nonce 0 cc_big_out)))
(assert-eq "the data is as it was" "33428445ea70f68e46231e7e5fac8fbb40eb1809e4f93b37dd20ba8a14199ba8" (cc-hex (sha256 cc_big)))

;a counter that does not start at 0 is the stream from that block on
(assert-eq "block 3 on is the stream from 192 bytes in"
	(slice (chacha20 cc_key cc_nonce 0 cc_text) 192 300)
	(chacha20 cc_key cc_nonce 3 (slice cc_text 192 300)))

;the native code, a part of a str, to another str, or to itself
(defq cc_out (cat cc_text))
(chacha20-xor cc_key cc_nonce 2 cc_text cc_out 128 100)
(assert-eq "a part, what is before it is not touched" (slice cc_text 0 128) (slice cc_out 0 128))
(assert-eq "a part, what is after it is not touched" (slice cc_text 228 300) (slice cc_out 228 300))
(assert-eq "a part, as the whole has it"
	(slice (chacha20 cc_key cc_nonce 0 cc_text) 128 228) (slice cc_out 128 228))
(defq cc_self (cat cc_text))
(chacha20-xor cc_key cc_nonce 0 cc_self cc_self 0 300)
(assert-eq "onto itself" (chacha20 cc_key cc_nonce 0 cc_text) cc_self)
(assert-eq "nothing to do" cc_self (chacha20-xor cc_key cc_nonce 0 cc_text cc_self 300 0))

;another key, nonce or counter is another stream
(defq cc_one (chacha20 cc_key cc_nonce 0 cc_text))
(assert-true "another key" (not (eql cc_one (chacha20 (cat (slice cc_key 0 31) "!") cc_nonce 0 cc_text))))
(assert-true "another nonce" (not (eql cc_one (chacha20 cc_key (cat (slice cc_nonce 0 11) "!") 0 cc_text))))
(assert-true "another counter" (not (eql cc_one (chacha20 cc_key cc_nonce 1 cc_text))))

;what the native code refuses
(assert-error "a key of the wrong size" (chacha20 "short" cc_nonce 0 cc_text))
(assert-error "a nonce of the wrong size" (chacha20 cc_key "short" 0 cc_text))
(assert-error "past the end of the data" (chacha20-xor cc_key cc_nonce 0 cc_text cc_out 200 101))
(assert-error "past the end of where it goes" (chacha20-xor cc_key cc_nonce 0 cc_text (str-alloc 10) 0 11))
(assert-error "an offset below 0" (chacha20-xor cc_key cc_nonce 0 cc_text cc_out -1 10))
