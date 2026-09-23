(import "lib/options/options.inc")
(import "lib/streams/huffman.inc")
(import "service/lock/app.inc")

(defq usage `(
(("-h" "--help")
"Usage: huff [options] [file]

    options:
        -h --help: this help info.
        -t --tbits num: bit size for data tokens, default 8.
        -c --codebook path: codebook filename, default :nil.

    Compresses a file using static or adaptive Huffman coding.
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
		(when opt_c
			(when (lock-claim-rpc opt_c +lock_mode_read)
				(when (defq cstream (file-stream opt_c))
					(setq opt_c (huffman-read-codebook cstream) cstream :nil))
				(lock-release-rpc opt_c)))
		(defq file_path (if (> (length args) 1) (second args))
			out_stream (io-stream 'stdout))
		(if file_path
			(when (lock-claim-rpc file_path +lock_mode_read)
				(when (defq in_stream (file-stream file_path))
					(if opt_c
						(huffman-compress-static in_stream out_stream opt_c)
						(huffman-compress in_stream out_stream opt_t))
					(stream-flush out_stream)
					(setq in_stream :nil))
				(lock-release-rpc file_path))
			(when (defq in_stream (io-stream 'stdin))
				(if opt_c
					(huffman-compress-static in_stream out_stream opt_c)
					(huffman-compress in_stream out_stream opt_t))
				(stream-flush out_stream)))))

