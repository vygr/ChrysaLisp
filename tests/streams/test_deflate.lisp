(report-header "Streams: DEFLATE and zlib inflated, a stream to a stream")

(import "lib/streams/deflate.inc")
(import "lib/crypto/sha256.inc")

(defun td-inflate (data &optional zlib)
	; (td-inflate data [zlib]) -> (ok text)
	(defq out (string-stream (cat "")))
	(list (inflate-decompress (string-stream data) out zlib) (str out)))

(assert-list-eq "fixed codes, zlib round them" '(:t "hello hello hello world, a fixed code")
	(td-inflate (hex-decode "78DACB48CDC9C957C84022CBF38B725274141215D2322B52531492F353520102740D5D") :t))
(assert-list-eq "the same, raw" '(:t "hello hello hello world, a fixed code")
	(td-inflate (hex-decode "CB48CDC9C957C84022CBF38B725274141215D2322B52531492F3535201")))
(assert-list-eq "a stored block" '(:t "stored as it is, no codes at all")
	(td-inflate (hex-decode "7801012000DFFF73746F7265642061732069742069732C206E6F20636F64657320617420616C6CBB330B24") :t))

;20,000 bytes of text, codes of its own, and copies back up to 32KB
(bind '(td_ok td_text) (td-inflate (load "tests/streams/data/deflate_dynamic.z") :t))
(assert-eq "codes of its own: inflated" :t td_ok)
(assert-eq "all of it" 20000 (length td_text))
(assert-eq "and what it was" "4B229338BD5B03EBB0C2A2BE0E06FF7CA2C9CC1B36383C83F88594BEB73F3AA5" (hex-encode (sha256 td_text)))

(assert-eq "not zlib is not taken for it" :nil (first (td-inflate "not zlib at all" :t)))

;the sums, as zlib has them
(import "lib/hash/sums.inc")
(defq td_fox "The quick brown fox jumps over the lazy dog")
(assert-eq "crc32" 0x414fa339 (crc32 td_fox))
(assert-eq "crc32 a part at a time" 0x414fa339 (crc32 (slice td_fox 10 -1) (crc32 (slice td_fox 0 10))))
(assert-eq "adler32" 0x5bdc0fda (adler32 td_fox))
(assert-eq "adler32 a part at a time" 0x5bdc0fda (adler32 (slice td_fox 10 -1) (adler32 (slice td_fox 0 10))))

;deflate, and back
(defun td-round (text)
	(defq z (string-stream (cat "")))
	(deflate-compress (string-stream text) z :t)
	(defq z (str z))
	(list (td-inflate z :t) (length z)))
(bind '(td_back td_size) (td-round td_text))
(assert-list-eq "deflate of the 20,000, and back" (list :t td_text) td_back)
(assert-true "and smaller" (< td_size (length td_text)))
(assert-list-eq "deflate of nothing" '(:t "") (first (td-round "")))
;32KB that do not repeat, and their start again: a copy from 32768 back,
;the most there is, and a block of the second 32KB
(defq td_seed 7 td_noise (apply (const cat) (map (lambda (&)
	(char (logand (>> (setq td_seed (logand (+ (* td_seed 1103515245) 12345) 0x7fffffff)) 16) 0xff))) (range 0 32768))))
(defq td_far (cat td_noise (slice td_noise 0 300)))
(assert-list-eq "a copy from 32768 back" (list :t td_far) (first (td-round td_far)))
