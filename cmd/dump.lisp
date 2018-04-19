;import settings
(run 'apps/cmd.inc)

;read up to chunk_size chars from stream
(defun read-chunk (_)
	(defq chunk (list))
	(while (and (ne chunk_size (length chunk))
			(defq c (read-char _))
			(push chunk c)))
	(if (ne 0 (length chunk)) chunk))

;hex strings of number
(defun as-hex-byte (_)
	(cat (to-base-char (bit-and (bit-shr _ 4) 0xf)) (to-base-char (bit-and _ 0xf))))
(defun as-hex-short (_)
	(cat (as-hex-byte (bit-shr _ 8)) (as-hex-byte (bit-and _ 0xff))))
(defun as-hex-int (_)
	(cat (as-hex-short (bit-shr _ 16)) (as-hex-short (bit-and _ 0xffff))))
(defun as-hex-long (_)
	(cat (as-hex-int (bit-shr _ 32)) (as-hex-int (bit-and _ 0xffffffff))))

;dump chunk to stdout
(defun dump-chunk (_)
	(prin (as-hex-int adr) " ")
	(prin (apply cat (map (lambda (_)
		(cat (as-hex-byte _) " ")) _)))
	(times (sub chunk_size (length _))
		(prin "   "))
	(print (apply cat (map (lambda (_)
		(if (le 32 _ 126) (char _) ".")) _)))
	(setq adr (add adr chunk_size)))

;dump stream to stdout
(defun dump-stream (_)
	(defq adr 0 input t)
	(while (and input (defq c (read-chunk _)))
		(dump-chunk c)
		(setq input (eq chunk_size (length c)))))

;dump a file to stdout
(defun dump-file (_)
	(when (setq _ (file-stream _))
		(dump-stream _)))

;initialize pipe details and command args, abort on error
(when (defq chunk_size 8 slave (create-slave))
	(if (le (length (defq args (slot get_args slave))) 1)
		;dump from stdin
		(dump-stream (file-stream 'stdin))
		;dump from args as files
		(each dump-file (slice 1 -1 args))))
