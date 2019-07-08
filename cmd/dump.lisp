;imports
(import 'class/lisp.inc)

;read up to chunk_size chars from stream
(defun read-chunk (_)
	(defq chunk (list))
	(while (and (ne chunk_size (length chunk))
			(defq c (read-char _))
			(push chunk c)))
	(if (ne 0 (length chunk)) chunk))

;hex strings of number
(defun as-hex-byte (_)
	(cat (to-base-char (logand (bit-shr _ 4) 0xf)) (to-base-char (logand _ 0xf))))
(defun as-hex-short (_)
	(cat (as-hex-byte (bit-shr _ 8)) (as-hex-byte (logand _ 0xff))))
(defun as-hex-int (_)
	(cat (as-hex-short (bit-shr _ 16)) (as-hex-short (logand _ 0xffff))))
(defun as-hex-long (_)
	(cat (as-hex-int (bit-shr _ 32)) (as-hex-int (logand _ 0xffffffff))))

;dump a file to stdout
(defun dump-file (_)
	(when (setq _ (file-stream _))
		(defq adr 0)
		(while (defq c (read-chunk _))
			(prin (as-hex-int adr) " " (apply cat (map (lambda (_)
				(cat (as-hex-byte _) " ")) c)))
			(times (sub chunk_size (length c)) (prin "   "))
			(print (apply cat (map (lambda (_)
				(if (le 32 _ 126) (char _) ".")) c)))
			(setq adr (add adr chunk_size)))))

;initialize pipe details and command args, abort on error
(when (defq chunk_size 8 slave (create-slave))
	(if (le (length (defq args (slave-get-args slave))) 1)
		;dump from stdin
		(dump-file 'stdin)
		;dump from args as files
		(each dump-file (slice 1 -1 args))))
