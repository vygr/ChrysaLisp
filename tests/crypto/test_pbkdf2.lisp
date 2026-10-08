(report-header "Crypto: a key from a password, PBKDF2 with HMAC SHA-256")
(import "lib/crypto/pbkdf2.inc")

(defun pk-hex (bytes)
	(to-lower (apply (const cat) (map (# (byte-to-hex-str (code %0))) bytes))))

;the test cases that go round with RFC 6070, for SHA-256, the answers from
;Python's hashlib
(assert-eq "once round" "120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b"
	(pk-hex (pbkdf2-sha256 "password" "salt" 1 32)))
(assert-eq "twice round" "ae4d0c95af6b46d32d0adff928f06dd02a303f8ef3c251dfd6e2d85a95474c43"
	(pk-hex (pbkdf2-sha256 "password" "salt" 2 32)))
(assert-eq "4096 times round" "c5e478d59288c841aa530db6845c4c8d962893a001ce4e11a4963873aa98134a"
	(pk-hex (pbkdf2-sha256 "password" "salt" 4096 32)))
(assert-eq "a long password and salt, a key of 40 bytes, two blocks of it"
	"348c89dbcbd32b2f32d814b8116e84cf2b17347ebc1800181c4e2a1fb8dd53e1c635518c7dac47e9"
	(pk-hex (pbkdf2-sha256 "passwordPASSWORDpassword" "saltSALTsaltSALTsaltSALTsaltSALTsalt" 4096 40)))
(assert-eq "a 0 byte in the password and in the salt" "89b69d0516f829893c696226650a8687"
	(pk-hex (pbkdf2-sha256 (cat "pass" (char 0) "word") (cat "sa" (char 0) "lt") 4096 16)))
(assert-eq "no password and no salt" "f7ce0b653d2d72a4108cf5abe912ffdd777616dbbb27a70e8204f3ae2d0f6fad"
	(pk-hex (pbkdf2-sha256 "" "" 1 32)))
(assert-eq "a key of 65 bytes, a byte into a third block"
	"120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b4dbf3a2f3dad3377264bb7b8e8330d4efc7451418617dabef683735361cdc18c22"
	(pk-hex (pbkdf2-sha256 "password" "salt" 1 65)))
;a password longer than a block is hashed first, as HMAC has it
(defq pk_long (apply (const cat) (map (lambda (_) "0123456789") (range 0 10))))
(assert-eq "a password longer than a block is the key its hash is"
	(pk-hex (pbkdf2-sha256 (sha256 pk_long) "salt" 3 32)) (pk-hex (pbkdf2-sha256 pk_long "salt" 3 32)))
;the size asked for, and the same again
(assert-eq "no bytes asked for" "" (pbkdf2-sha256 "password" "salt" 1 0))
(assert-eq "one byte" 1 (length (pbkdf2-sha256 "password" "salt" 1 1)))
(assert-eq "a short key is the start of a long one"
	(slice (pbkdf2-sha256 "password" "salt" 5 64) 0 20) (pbkdf2-sha256 "password" "salt" 5 20))
(assert-eq "the same four, the same key" (pbkdf2-sha256 "pw" "s" 7 32) (pbkdf2-sha256 "pw" "s" 7 32))
(assert-true "another salt, another key"
	(not (eql (pbkdf2-sha256 "pw" "s" 7 32) (pbkdf2-sha256 "pw" "t" 7 32))))
(assert-true "another count, another key"
	(not (eql (pbkdf2-sha256 "pw" "s" 7 32) (pbkdf2-sha256 "pw" "s" 8 32))))
;the password and the salt are as they were
(defq pk_pw (cat "password") pk_salt (cat "salt"))
(pbkdf2-sha256 pk_pw pk_salt 3 32)
(assert-eq "the password is as it was" "password" pk_pw)
(assert-eq "the salt is as it was" "salt" pk_salt)
(assert-error "a count of 0" (pbkdf2-sha256 "password" "salt" 0 32))
(assert-error "a size below 0" (pbkdf2-sha256 "password" "salt" 1 -1))
