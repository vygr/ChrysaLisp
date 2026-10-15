(import "lib/options/options.inc")
(import "lib/streams/huffman.inc")

(defq usage `(
(("-h" "--help")
"Usage: unhuff [options] [file]

    options:
        -h --help: this help info.
        -c --codebook path: codebook filename, default :nil.

    Decompresses a file huff made, with the codebook it
    was made with, if it was.

    If no file is given, it reads from stdin.
    Output is written to stdout.")
(("-c" "--codebook") ,(opt-str 'opt_c))
))

(defun main ()
	(when (and
			(defq stdio (create-stdio))
			(defq opt_c :nil args (options stdio usage)))
		(defq out (io-stream 'stdout) book :nil)
		(cond
			((and opt_c (not (setq book (options-input (list "" opt_c) (const huffman-read-book)))))
				(print "unhuff: no codebook " opt_c))
			((not (options-input args (if book
					(# (huffman-decompress-static %0 out book))
					(# (huffman-decompress %0 out)))))
				(print "unhuff: it stops before its end")))
		(stream-flush out)))
