;imports
(import 'class/lisp.inc)
(import 'lib/options/options.inc)

;read up to chunk_size chars from stream
(defun read-chunk (_)
	(defq chunk (list))
	(while (and (/= chunk_size (length chunk))
			(defq c (read-char _))
			(push chunk c)))
	(if (/= 0 (length chunk)) chunk))

;hex strings of number
(defun as-hex-byte (_)
	(cat (num-to-char (logand (>> _ 4) 0xf)) (num-to-char (logand _ 0xf))))
(defun as-hex-short (_)
	(cat (as-hex-byte (>> _ 8)) (as-hex-byte (logand _ 0xff))))
(defun as-hex-int (_)
	(cat (as-hex-short (>> _ 16)) (as-hex-short (logand _ 0xffff))))
(defun as-hex-long (_)
	(cat (as-hex-int (>> _ 32)) (as-hex-int (logand _ 0xffffffff))))

;dump a file to stdout
(defun dump-file (_)
	(when _
		(defq adr 0)
		(while (defq c (read-chunk _))
			(prin (as-hex-int adr) " " (apply cat (map (lambda (_)
				(cat (as-hex-byte _) " ")) c)))
			(times (- chunk_size (length c)) (prin "   "))
			(print (apply cat (map (lambda (_)
				(if (<= 32 _ 126) (char _) ".")) c)))
			(setq adr (+ adr chunk_size)))))

(defq usage `(
(("-h" "--help")
"Usage: dump [options] [path] ...
	options:
		-h --help: this help info.
	If no paths given on command line
	then will dump stdin.")
))

(defun-bind main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(defq chunk_size 8)
		(if (<= (length args) 1)
			;dump from stdin
			(dump-file (io-stream 'stdin))
			;dump from args as files
			(each (# (dump-file (file-stream %0))) (slice 1 -1 args)))))
