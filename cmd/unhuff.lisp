(import "lib/options/options.inc")
(import "lib/streams/huffman.inc")

(defq usage `(
(("-h" "--help")
"Usage: unrle [options] [file]

	options:
		-h --help: this help info.
		-t --tbits num: bit size for data tokens, default 8.
		-c --codebook path: codebook filename, default :nil.

	Deompresses a file using static or adaptive Huffman coding.
	If a codebook is provided then it will load that model for
	static operation.

	If no file is given, it reads from stdin.
	Output is written to stdout.")
(("-t" "--tbits") ,(opt-num 'opt_t))
(("-c" "--codebook") ,(opt-str 'opt_c))
))

(defun main ()
	; Initialize options and streams
	(when (and
			(defq stdio (create-stdio))
			(defq opt_c :nil opt_t 8 args (options stdio usage)))
		(defq in_stream (if (> (length args) 1)
						   (file-stream (second args))
						   (io-stream 'stdin))
			  out_stream (io-stream 'stdout))
		(if opt_c (setq opt_c (huffman-read-codebook (file-stream opt_c))))
		; Perform decompression
		(when in_stream
			(if opt_c
				(huffman-decompress-static in_stream out_stream opt_c)
				(huffman-decompress in_stream out_stream opt_t)))))
