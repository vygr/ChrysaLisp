(import "lib/options/options.inc")
(import "service/lock/app.inc")

(defq usage `(
(("-h" "--help")
"Usage: cp [options] path1 path2

    options:
        -h --help: this help info.

    Copy file path1 to path2.")
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage))
			(= (length args) 3))
		(bind '(src_path dst_path) (rest args))
		(with-read-lock src_path
			(with-write-lock dst_path
				(when (and (defq src (file-stream src_path))
						(defq dst (file-stream dst_path +file_open_write)))
					(while (defq c (read-blk src 1024)) (write-blk dst c))
					(stream-flush dst))
				(setq src :nil dst :nil)))))

