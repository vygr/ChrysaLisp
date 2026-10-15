(import "lib/options/options.inc")
(import "lib/streams/huffman.inc")

(defq usage `(
(("-h" "--help")
"Usage: huff [options] [file]

    options:
        -h --help: this help info.
        -t --tbits num: bit size for data tokens, default 8.
        -c --codebook path: codebook filename, default :nil.

    Compresses a file with Huffman codes, a block of
    65536 tokens at a time, each block with a code made
    for it. With a codebook, made by hbook, the code of
    the book is used for the whole file, and none is
    written; a token the book has no code for is an error.

    If no file is given, it reads from stdin.
    Output is written to stdout.")
(("-t" "--tbits") ,(opt-num 'opt_t))
(("-c" "--codebook") ,(opt-str 'opt_c))
))

(defun main ()
	(when (and
			(defq stdio (create-stdio))
			(defq opt_c :nil opt_t 8 args (options stdio usage)))
		(defq out (io-stream 'stdout) book :nil)
		(cond
			((and opt_c (not (setq book (options-input (list "" opt_c) (const huffman-read-book)))))
				(print "huff: no codebook " opt_c))
			((not (options-input args (if book
					(# (huffman-compress-static %0 out book))
					(# (huffman-compress %0 out opt_t) :t))))
				(print "huff: a token the codebook has no code for")))
		(stream-flush out)))
