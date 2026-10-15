(import "lib/options/options.inc")
(import "lib/streams/rle.inc")

(defq usage `(
(("-h" "--help")
"Usage: rle [options] [file]

    options:
        -h --help: this help info.
        -t --tbits num: bit size for data tokens, default 8.
        -r --rbits num: bit size for run length tokens, default 8.

    Compresses a file using Run-Length encoding.

    If no file is given, it reads from stdin.
    Output is written to stdout.")
(("-t" "--tbits") ,(opt-num 'opt_t))
(("-r" "--rbits") ,(opt-num 'opt_r))
))

(defun main ()
	(when (and
			(defq stdio (create-stdio))
			(defq opt_t 8 opt_r 8 args (options stdio usage)))
		(defq out (io-stream 'stdout))
		(options-input args (# (rle-compress %0 out opt_t opt_r)))
		(stream-flush out)))
