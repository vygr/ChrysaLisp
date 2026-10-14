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
