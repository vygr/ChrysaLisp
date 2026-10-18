(import "lib/options/options.inc")
(import "lib/files/zip.inc")
(import "lib/files/files.inc")

(defq usage `(
(("-h" "--help")
"Usage: zip [options] archive path ...

    options:
        -h --help: this help info.

    Make a .zip of files: each path a file, or a folder and all
    in it, its files named in the zip as they are named here.
    Each is packed by DEFLATE, or stored as it is when that is
    no smaller. The archive is made new, what was there before
    is gone. unzip takes them out again.")
))

(defun main ()
	(and
		(defq stdio (create-stdio))
		(defq args (options stdio usage))
		(cond
			((< (length args) 3) (print "zip: an archive and what to put in it"))
			(:t (defq archive (second args) files (list) bytes 0)
				;each file the once, a folder all it holds
				(each! (lambda (path)
						(defq path (if (starts-with "./" path) (slice path 2 -1) path))
						(cond
							((defq info (pii-fstat path))
								(each (lambda (file)
										(defq file (if (starts-with "./" file) (slice file 2 -1) file))
										(unless (or (eql file archive) (some (# (eql (first %0) file)) files))
											(push files (list file (load file)))))
									(if (= (logand (elem-get info 2) 0xf000) 0x4000) (files-all path) (list path))))
							((print "zip: no " path))))
					(list args) 2 -1)
				(each (# (setq bytes (+ bytes (length (second %0))))) files)
				(defq out (file-stream archive +file_open_write) count (zip-write out files))
				(stream-flush out)
				(setq out :nil)
				(if count
					(print "zip: " count " files, " bytes " bytes, to " archive " of " (length (load archive)))
					(print "zip: too many or too big for a zip"))))))
