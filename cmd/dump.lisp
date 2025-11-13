(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: dump [options] [path] ...

	options:
		-h --help: this help info.
		-c --chunk num: chunk size, default 8.

	If no paths given on command line
	then will dump stdin.")
(("-c" "--chunk") ,(opt-num 'opt_c))
))

;dump a stream to stdout
(defun dump-file (stream)
	(when stream
		(defq adr 0)
		(while (defq blk (read-blk stream opt_c))
			(print (int-to-hex-str adr) " "
				(join (partition (hex-encode blk) 2) " " 2)
				(pad "" (* 3 (- opt_c (length blk))) "            ")
				(apply (const cat)
					(map (# (if (find %0 +char_class_printable) %0 ".")) blk)))
			(setq adr (+ adr opt_c)))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_c 8 args (options stdio usage)))
		(if (<= (length args) 1)
			;dump from stdin
			(dump-file (io-stream 'stdin))
			;dump from args as files
			(each (# (dump-file (file-stream %0))) (rest args)))))
