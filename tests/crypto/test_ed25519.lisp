(report-header "Crypto: Ed25519, a signature")
(import "lib/crypto/ed25519.inc")
(import "lib/crypto/sha256.inc")

(defun ed-hex (bytes)
	(to-lower (apply (const cat) (map (# (byte-to-hex-str (code %0))) bytes))))
(defun ed-bytes (hex)
	(if (eql hex "-") "" (hex-decode (to-upper hex))))

;the numbers of the field
(assert-eq "1 as its bytes" "0100000000000000000000000000000000000000000000000000000000000000" (ed-hex (ed-pack *ed_one*)))
(assert-eq "a number and 1 over it are 1" (ed-pack *ed_one*)
	(ed-pack (ed-mul (ed-copy *ed_zero*) *ed_d* (ed-inv *ed_d*))))
(assert-eq "the root of -1, squared, and 1, is nothing" (ed-pack *ed_zero*)
	(ed-pack (nums-add (ed-mul (ed-copy *ed_zero*) *ed_i* *ed_i*) *ed_one*)))
(assert-eq "bytes to a number and back" (ed-pack *ed_x*) (ed-pack (ed-unpack (ed-pack *ed_x*))))
(assert-eq "the base point as its bytes" "5866666666666666666666666666666666666666666666666666666666666666"
	(ed-hex (ed-encode (ed-point *ed_x* *ed_y*))))

;the native multiply against the one in Lisp, on numbers of every size a part can be
(defq ed_rnd 12345)
(defun ed-rand (lo hi) (setq ed_rnd (logand (+ (* ed_rnd 6364136223846793005) 1442695040888963407) 0x7fffffffffffffff))
	(+ lo (% (>> ed_rnd 20) (- hi lo))))
(defq ed_same :t)
(each (lambda ((lo hi))
	(times 40
		(defq a (apply nums (map (lambda (&) (ed-rand lo hi)) *ed_16*))
			b (apply nums (map (lambda (&) (ed-rand lo hi)) *ed_16*)))
		(unless (eql (str (ed-mul (ed-copy *ed_zero*) a b)) (str (ed-mul-ref (ed-copy *ed_zero*) a b)))
			(setq ed_same :nil))))
	'((0 65536) (-65536 65536) (0 2) (65535 65536) (-200000 200000) (-1 1)))
(assert-eq "the native multiply is the one in Lisp, 240 pairs" :t ed_same)
(defq ed_alias (ed-copy *ed_x*))
(ed-mul ed_alias ed_alias ed_alias)
(assert-eq "a number times itself, into itself" (str (ed-mul-ref (ed-copy *ed_zero*) *ed_x* *ed_x*)) (str ed_alias))
(assert-error "a nums too short" (ed-mul (nums 1 2 3) *ed_x* *ed_y*))
(assert-error "not a nums" (ed-mul (list 1 2) *ed_x* *ed_y*))

;the test cases of RFC 8032, and two more, the answers from a Python of the
;RFC written for the job, which gives the RFC's own
(each (lambda ((name seed message public signature))
	(setq seed (ed-bytes seed) message (ed-bytes message))
	(assert-eq (cat name ", the public key") public (ed-hex (ed25519-public seed)))
	(assert-eq (cat name ", the signature") signature (ed-hex (ed25519-sign seed message)))
	(assert-eq (cat name ", it checks") :t (ed25519-verify (ed-bytes public) message (ed-bytes signature))))
	'(("RFC 8032 test 1, no message" "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60" "-"
		"d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
		"e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b")
	("RFC 8032 test 2, a byte" "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb" "72"
		"3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c"
		"92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00")
	("RFC 8032 test 3, two bytes" "c5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7" "af82"
		"fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025"
		"6291d657deec24024827e69c3abe01a30ce548a284743a445e3680d7db5ac3ac18ff9b538d16f290ae67f760984dc6594a7c15e9716ed28dc027beceea1ec40a")
	("a line of text" "8f44d509e0492acec4fc109a030a5c751f063fb743cc388f0ed722bacc3bc66c" "4368727973614c6973702072656c6561736520372e33"
		"6ee961ff5bf2b5d1e867b93c7fa50a6e634053e9a95b77294580bafa10b3abb2"
		"f135843456b61b22295922222e5412c39b047eb982e4de7bd15e575f08c8565ccc2528391f4dba8af6d12cbbeda113fed3f68473af3d0b22c884a5cf5db0950f")
	("a message of 1024 bytes" "ae448ac86c4e8e4dec645729708ef41873ae79c6dff84eff73360989487f08e5" "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"
		"1db6b9e65febcc73396356d0d823871388452c544814eb74e0e209a957c4c9e6"
		"f83f70c4668a830d187419fd7d3080168ae429017bee494827e42c008f1cbc25826abccb1329751b5f40e61cb149e620a294313218cc7da3f6809a105f087a0c")))

;what does not check. A bit of the message, of either half of the
;signature, or of the key, another message, another key, and what is cut short
(defq ed_seed (ed-bytes "8f44d509e0492acec4fc109a030a5c751f063fb743cc388f0ed722bacc3bc66c") ed_msg "ChrysaLisp release 7.3"
	ed_pub (ed25519-public ed_seed) ed_sig (ed25519-sign ed_seed ed_msg))
(defun ed-flip (bytes at)
	(cat (slice bytes 0 at) (char (logxor (code bytes 1 at) 1)) (slice bytes (inc at) -1)))
(assert-eq "as it was made, it checks" :t (ed25519-verify ed_pub ed_msg ed_sig))
(assert-eq "a bit of the message changed" :nil (ed25519-verify ed_pub (ed-flip ed_msg 3) ed_sig))
(assert-eq "a message a byte longer" :nil (ed25519-verify ed_pub (cat ed_msg " ") ed_sig))
(assert-eq "no message" :nil (ed25519-verify ed_pub "" ed_sig))
(assert-eq "a bit of the first half of the signature" :nil (ed25519-verify ed_pub ed_msg (ed-flip ed_sig 5)))
(assert-eq "a bit of the second half" :nil (ed25519-verify ed_pub ed_msg (ed-flip ed_sig 40)))
(assert-eq "a bit of the key" :nil (ed25519-verify (ed-flip ed_pub 7) ed_msg ed_sig))
(assert-eq "the key of another secret" :nil
	(ed25519-verify (ed25519-public (sha256 "someone else")) ed_msg ed_sig))
(assert-eq "the signature of another message" :nil
	(ed25519-verify ed_pub ed_msg (ed25519-sign ed_seed "ChrysaLisp release 7.4")))
(assert-eq "a signature cut short" :nil (ed25519-verify ed_pub ed_msg (slice ed_sig 0 63)))
(assert-eq "a key cut short" :nil (ed25519-verify (slice ed_pub 0 31) ed_msg ed_sig))
(assert-eq "a signature of nothing but zeros" :nil (ed25519-verify ed_pub ed_msg (str-alloc 64)))
;the second half with the order of the base point added is the same number, written another way, and is refused
(defq ed_carry 0 ed_more (apply (const cat) (map (lambda (j)
	(defq v (+ (code ed_sig 1 (+ 32 j)) (elem-get *ed_l* j) ed_carry))
	(setq ed_carry (>> v 8))
	(char (logand v 255))) *ed_32*)))
(assert-eq "a second half that is not the least way to write it" :nil
	(ed25519-verify ed_pub ed_msg (cat (slice ed_sig 0 32) ed_more)))
(assert-eq "signed again it is the same, there is no chance in it" ed_sig (ed25519-sign ed_seed ed_msg))
(assert-error "a seed that is not 32 bytes" (ed25519-public "short"))
(assert-error "nor to sign with" (ed25519-sign "short" ed_msg))

(undef (env) 'ed_rnd 'ed_same 'ed_alias 'ed_seed 'ed_msg 'ed_pub 'ed_sig 'ed_carry 'ed_more)
