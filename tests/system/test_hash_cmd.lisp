(report-header "hash: the hash of files from a command line, a line each, the path then the hash")

(import "lib/task/pipe.inc")
(import "lib/crypto/sha256.inc")
(import "lib/hash/sums.inc")

(defun hc-run (cmdline)
	;what a command line says, its lines
	(defq out (list))
	(pipe-run cmdline (# (push out %0)))
	(filter (const nempty?) (split (apply (const cat) (cat (list "") out)) "\n")))

;files of this run's own, by its pid
(defq hc_pid (str (pii-pid)) hc_abc (cat "tests/scratch/test_hash_abc_" hc_pid ".txt")
	hc_big (cat "tests/scratch/test_hash_big_" hc_pid ".bin")
	hc_dir (cat "tests/scratch/test_hash_" hc_pid "/")
	hc_big_data (apply (const cat) (cat (list "") (map! (# (char (logand (+ (* (!) 13) 7) 255))) '() 0 200003)))
	hc_many (map! (lambda () (cat hc_dir "f" (str (!)) ".txt")) '() 0 10))
(save "abc" hc_abc)
(save hc_big_data hc_big)
(each (lambda (f) (save (cat "file " f) f)) hc_many)

;of "abc", the values the standards give
(test-cases
	(hc-run (cat "hash " hc_abc))
	(list (cat hc_abc " ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))
	(hc-run (cat "hash -a sha384 " hc_abc))
	(list (cat hc_abc " cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed8086072ba1e7cc2358baeca134c825a7"))
	(hc-run (cat "hash -a sha512 " hc_abc))
	(list (cat hc_abc " ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"))
	(hc-run (cat "hash -a crc32 " hc_abc))
	(list (cat hc_abc " 352441c2"))
	(hc-run (cat "hash -a adler32 " hc_abc))
	(list (cat hc_abc " 024d0127")))

;a file of more than one piece read, 64 KB, as the library has it whole
(assert-list-eq "a file of more than a piece is the hash of all of it"
	(list (cat hc_big " " (to-lower (hex-encode (sha256 hc_big_data)))))
	(hc-run (cat "hash " hc_big)))
(assert-list-eq "and its crc32 the sum of all of it"
	(list (cat hc_big " " (to-lower (hex-encode (reverse (char (crc32 hc_big_data) 4))))))
	(hc-run (cat "hash -a crc32 " hc_big)))

(assert-list-eq "a file that is not there is said, and has no hash"
	(list (cat hc_dir "none can not be read")) (hc-run (cat "hash " hc_dir "none")))
(assert-list-eq "an algorithm it has not is said, and what it has"
	'("hash: no such algorithm md5, sha256, sha384, sha512, crc32 or adler32") (hc-run (cat "hash -a md5 " hc_abc)))

;paths from stdin, more than a batch so farmed, each line its path then its
;hash, in whatever order they come, so sorted to be compared
(defq hc_got (sort (hc-run (cat "files " hc_dir " | hash -a crc32")) (const cmp)))
(assert-list-eq "paths from stdin, farmed, each its path then its hash"
	(sort (map (lambda (f) (cat f " " (to-lower (hex-encode (reverse (char (crc32 (cat "file " f)) 4)))))) hc_many) (const cmp))
	hc_got)

(each (# (if (pii-fstat %0) (pii-remove %0))) (cat (list hc_abc hc_big) hc_many))
(pii-remove hc_dir)
