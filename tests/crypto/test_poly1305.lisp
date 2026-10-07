(report-header "Crypto: Poly1305")
(import "lib/crypto/poly1305.inc")

(defun cr-hex (bytes)
	(to-lower (apply (const cat) (map (# (byte-to-hex-str (code %0))) bytes))))
(defun cr-unhex (text)
	;the bytes of a str of hex
	(apply (const cat) (map (# (char (str-to-num (cat "0x" (slice text %0 (+ %0 2)))))) (range 0 (length text) 2))))

;the example of RFC 8439, 2.5.2
(defq pl_key (cr-unhex "85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b"))
(assert-eq "RFC 8439, Cryptographic Forum Research Group" "a8061dc1305136c6c22b8baf0c0127a9"
	(cr-hex (poly1305 pl_key "Cryptographic Forum Research Group")))

;every length about the edges of a block, with keys that have every bit
;set, which is where the sums are biggest
(defq pl_text (apply (const cat) (map (# (char (logand (+ (* %0 13) 9) 0xff))) (range 0 200)))
	pl_ones (apply (const cat) (map (lambda (_) (char 0xff)) (range 0 32)))
	pl_ffs (apply (const cat) (map (lambda (_) (char 0xff)) (range 0 200))))
(assert-eq "0 bytes" "0103808afb0db2fd4abff6af4149f51b" (cr-hex (poly1305 pl_key (slice pl_text 0 0))))
(assert-eq "1 bytes" "b8120c98f861df89aaa31f839008086b" (cr-hex (poly1305 pl_key (slice pl_text 0 1))))
(assert-eq "15 bytes" "7b8d2890f95c6a79085a74bae1146381" (cr-hex (poly1305 pl_key (slice pl_text 0 15))))
(assert-eq "16 bytes" "c342c2a6f6a6186a053b8d0a5265600a" (cr-hex (poly1305 pl_key (slice pl_text 0 16))))
(assert-eq "17 bytes" "71583852d24bf57e1f434c1fdc4e9382" (cr-hex (poly1305 pl_key (slice pl_text 0 17))))
(assert-eq "31 bytes" "a88c5e0a29ad4915a6d7119f97268da8" (cr-hex (poly1305 pl_key (slice pl_text 0 31))))
(assert-eq "32 bytes" "aa87eb6e26572af59570cfeb0cdda83f" (cr-hex (poly1305 pl_key (slice pl_text 0 32))))
(assert-eq "33 bytes" "ba939dc2e207bcd47c899ee0156d38b1" (cr-hex (poly1305 pl_key (slice pl_text 0 33))))
(assert-eq "47 bytes" "2a82730c1c76a18b5bfc7402e7e098f2" (cr-hex (poly1305 pl_key (slice pl_text 0 47))))
(assert-eq "48 bytes" "e6c2f3be1980b45a3e4dd74b61fdd297" (cr-hex (poly1305 pl_key (slice pl_text 0 48))))
(assert-eq "49 bytes" "a3b857834b71e829bf14f9007b538652" (cr-hex (poly1305 pl_key (slice pl_text 0 49))))
(assert-eq "64 bytes" "fa91779ba1fbb4c9fb6bec139125af5b" (cr-hex (poly1305 pl_key (slice pl_text 0 64))))
(assert-eq "200 bytes" "167f828683dec0b2422e4fe6359e2736" (cr-hex (poly1305 pl_key (slice pl_text 0 200))))
(assert-eq "0 bytes of 0xff, a key of all ones" "ffffffffffffffffffffffffffffffff" (cr-hex (poly1305 pl_ones (slice pl_ffs 0 0))))
(assert-eq "1 bytes of 0xff, a key of all ones" "23feffef23f8ffef23f8ffef23f8ffef" (cr-hex (poly1305 pl_ones (slice pl_ffs 0 1))))
(assert-eq "16 bytes of 0xff, a key of all ones" "fbffff17faffff17faffff17faffff17" (cr-hex (poly1305 pl_ones (slice pl_ffs 0 16))))
(assert-eq "17 bytes of 0xff, a key of all ones" "7cfe7ff768f81f2763f8bf565df85f86" (cr-hex (poly1305 pl_ones (slice pl_ffs 0 17))))
(assert-eq "32 bytes of 0xff, a key of all ones" "5400801f3f00204f3900c07e330060ae" (cr-hex (poly1305 pl_ones (slice pl_ffs 0 32))))
(assert-eq "48 bytes of 0xff, a key of all ones" "5efc6a6b51fcec4c787c5075997c95e4" (cr-hex (poly1305 pl_ones (slice pl_ffs 0 48))))
(assert-eq "200 bytes of 0xff, a key of all ones" "38e84b52575a58bcdb84ae0860f4417d" (cr-hex (poly1305 pl_ones (slice pl_ffs 0 200))))
(assert-eq "h that goes over the prime" "02000000000000000000000000000000"
	(cr-hex (poly1305 (cr-unhex "02000000000000000000000000000000ffffffffffffffffffffffffffffffff") (slice pl_ffs 0 16))))
(assert-eq "a sum that comes to all ones" "ffffffffffffffffffffffffffffffff"
	(cr-hex (poly1305 (cr-unhex "0100000000000000000000000000000000000000000000000000000000000000") (cr-unhex "fbfffffffffffffffffffffffffffffffefefefefefefefefefefefefefefefe01010101010101010101010101010101"))))

;more than the native code is given at once
(defq pl_big (apply (const cat) (map (# (char (logand (+ (* %0 29) 3) 0xff))) (range 0 150001))))
(assert-eq "150,001 bytes" "b8ad0715bad0f596debbbe4a9d0281ee" (cr-hex (poly1305 pl_key pl_big)))

;added a part at a time, in parts of any size, it is the same tag
(each (lambda (part)
	(defq ctx (poly1305-start pl_key) at 0)
	(while (< at 200)
		(poly1305-add ctx (slice pl_text at (min 200 (+ at part))))
		(setq at (+ at part)))
	(assert-eq (cat "200 bytes, " (str part) " at a time") "167f828683dec0b2422e4fe6359e2736" (cr-hex (poly1305-end ctx))))
	'(1 3 15 16 17 33 199 200 500))

(assert-eq "16 bytes" 16 (length (poly1305 pl_key "")))
(assert-true "another key, another tag" (not (eql (poly1305 pl_key pl_text) (poly1305 pl_ones pl_text))))
(assert-true "a byte changed, another tag"
	(not (eql (poly1305 pl_key pl_text) (poly1305 pl_key (cat (slice pl_text 0 199) "!")))))

;what is refused
(assert-error "a key of the wrong size" (poly1305 "short" pl_text))
(assert-error "a state of the wrong size" (poly1305-blocks "short" pl_text 0 1 1))
(assert-error "more blocks than there are" (poly1305-blocks (str-alloc 120) pl_text 0 13 1))
(assert-error "a top that is not 0 or 1" (poly1305-blocks (str-alloc 120) pl_text 0 1 2))
