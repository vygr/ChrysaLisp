(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "service/lock/app.inc")
(import "lib/crypto/sha256.inc")
(import "lib/crypto/sha512.inc")
(import "lib/hash/sums.inc")

(defq usage `(
(("-h" "--help")
"Usage: hash [options] [path] ...

    options:
        -h --help: this help info.
        -a --algorithm name: sha256, the default, sha384,
            sha512, crc32 or adler32.
        -j --jobs num: max jobs per batch, default 4.

    The hash of each file, a line each, its path then
    the hash in hex, the path first so the lines can be
    sorted and compared. sha256 is what shasum -a 256
    gives, crc32 as zip and gzip, adler32 as zlib. A file
    that can not be read is said, and has no hash.

    If no paths given on command line
    then will take paths from stdin.")
(("-a" "--algorithm") ,(opt-str 'opt_a))
(("-j" "--jobs") ,(opt-num 'opt_j))
))

;how much of a file is hashed at a time
(defq +hash_piece 65536)

;each a start, an add of more data and an end that is the hash in hex,
;lower case as shasum has it; a sum of 32 bits as 8 digits, its first byte
;its highest
(defq +hash_kinds (scatter (Fmap 7)
	"sha256" (list (const sha256-start) (const sha256-add) (lambda (ctx) (to-lower (hex-encode (sha256-end ctx)))))
	"sha384" (list (const sha384-start) (const sha512-add) (lambda (ctx) (to-lower (hex-encode (sha384-end ctx)))))
	"sha512" (list (const sha512-start) (const sha512-add) (lambda (ctx) (to-lower (hex-encode (sha512-end ctx)))))
	"crc32" (list (# 0) (lambda (ctx data) (crc32 data ctx)) (lambda (ctx) (to-lower (hex-encode (reverse (char ctx 4))))))
	"adler32" (list (# 1) (lambda (ctx data) (adler32 data ctx)) (lambda (ctx) (to-lower (hex-encode (reverse (char ctx 4))))))))

;the hash of a file, a piece at a time, or :nil if it can not be read
(defun file-hash (file)
	(bind '(start add end) (. +hash_kinds :find opt_a))
	(with-read-lock file
		(when (defq stream (file-stream file))
			(defq ctx (start))
			(while (defq data (read-blk stream +hash_piece))
				(setq ctx (add ctx data))
				(task-slice))
			(setq stream :nil)
			(end ctx))))

(defun work (file)
	(print file " " (ifn (file-hash file) "can not be read")))

(defun main ()
	;initialize pipe details and command args, abort on error
	(and
		(defq stdio (create-stdio))
		(defq opt_a "sha256" opt_j 4 args (options stdio usage))
		(cond
			((not (. +hash_kinds :find opt_a))
				(print "hash: no such algorithm " opt_a ", sha256, sha384, sha512, crc32 or adler32"))
			(:t ;from args ?
				(if (empty? (defq jobs (rest args)))
					;no, so from stdin
					(lines! (# (push jobs %0) :nil) (io-stream 'stdin)))
				(if (<= (length jobs) opt_j)
					;do the work when batch size ok !
					(each (const work) jobs)
					;do the jobs out there, by calling myself !
					(each (lambda ((job result)) (prin result))
						(pipe-farm (map (# (str (first args)
								" -a " opt_a " -j " opt_j
								" " (slice (str %0) 1 -2)))
							(partition jobs opt_j)))))))))
