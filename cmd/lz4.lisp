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
	(when (and
			(defq stdio (create-stdio))
			(defq opt_w 65536 args (options stdio usage)))
		(defq out (io-stream 'stdout))
		(options-input args (# (lz4-compress %0 out opt_w)))
		(stream-flush out)))
