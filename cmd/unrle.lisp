(import "lib/options/options.inc")
(import "lib/streams/rle.inc")

(defq usage `(
(("-h" "--help")
"Usage: unrle [options] [file]

	options:
		-h --help: this help info.
		-t --tbits num: bit size for data tokens, default 8.
		-r --rbits num: bit size for run length tokens, default 8.

	Decompresses a file using Run-Length encoding.

	If no file is given, it reads from stdin.
	Output is written to stdout.")
(("-t" "--tbits") ,(opt-num 'opt_t))
(("-r" "--rbits") ,(opt-num 'opt_r))
))

(defun main ()
	; Initialize options and streams
	(when (and
			(defq stdio (create-stdio))
			(defq opt_t 8 opt_r 8 args (options stdio usage)))

		(defq in_stream (if (> (length args) 1)
						   (file-stream (second args))
						   (io-stream 'stdin))
			  out_stream (io-stream 'stdout))

		; Perform decompression
		(when in_stream
			(rle-decompress in_stream out_stream opt_t opt_r)
			(stream-flush out_stream))))
