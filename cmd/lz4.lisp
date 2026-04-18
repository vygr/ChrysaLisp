(import "lib/options/options.inc")
(import "lib/streams/lz4.inc")

(defq usage `(
(("-h" "--help")
"Usage: lz4 [options] [file]

	options:
		-h --help: this help info.
		-w --window num: max window size, default 65536.

	Compresses a file using standard LZ4 Framed encoding.

	If no file is given, it reads from stdin.
	Output is written to stdout.")
(("-w" "--window") ,(opt-num 'opt_w))
))

(defun main ()
	; Initialize options and streams
	(when (and
			(defq stdio (create-stdio))
			(defq opt_w 65536 args (options stdio usage)))

		(defq in_stream (if (> (length args) 1)
						   (file-stream (second args))
						   (io-stream 'stdin))
			  out_stream (io-stream 'stdout))

		; Perform compression
		(when in_stream
			(lz4-compress in_stream out_stream opt_w)
			(stream-flush out_stream))))