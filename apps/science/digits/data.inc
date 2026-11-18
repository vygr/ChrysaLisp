;the data set, fetched the first time it is wanted and kept, and loaded into
;the block the children read their pictures from

(import "lib/net/http.inc")
(import "lib/streams/deflate.inc")

(defun mnist-cache (name)
	; (mnist-cache name) -> path
	;where a file is kept, of the tree's downloads/, which any app that
	;wants the data set shares
	(cat "downloads/mnist/" name))

(defun mnist-have? (file)
	; (mnist-have? (name bytes head)) -> :t | :nil
	;kept already, all of it
	(bind '(name bytes &ignore) file)
	(and (defq info (pii-fstat (mnist-cache name))) (= (second info) bytes)))

(defun mnist-missing ()
	; (mnist-missing) -> ((name bytes head) ...)
	;the files not yet kept
	(filter (# (not (mnist-have? %0))) +mnist_files))

(defun mnist-fetch (file)
	; (mnist-fetch (name bytes head)) -> :nil | why
	;the .gz of a file fetched, from each mirror in turn till one has it,
	;inflated and kept. :nil if it is kept, else why it could not be
	(bind '(name bytes &ignore) file)
	(defq why "no mirror")
	(some (lambda (mirror)
			(defq gz (memory-stream) resp (http-get (cat mirror name ".gz") (pmap) gz))
			(cond
				((not resp) (setq why (ifn (tls-why) "no connection")) :nil)
				((/= (pfind resp :status) 200)
					(setq why (cat (str (pfind resp :status)) " " (ifn (pfind resp :reason) ""))) :nil)
				(:t (stream-seek gz 0 0)
					(defq path (mnist-cache name) out (file-stream path +file_open_write)
						ok (inflate-gzip gz out))
					(setq out :nil)
					(cond
						((and ok (mnist-have? file)) (setq why :nil) :t)
						(:t (setq why "not a whole file") :nil)))))
		+mnist_mirrors)
	why)

(defun mnist-load (data)
	; (mnist-load buf) -> buf
	;the kept files into the data block, each at its place, their heads
	;left out, a part at a time
	(each (lambda ((name bytes head) at)
			(defq stream (file-stream (mnist-cache name)) at at)
			(stream-seek stream head 0)
			(while (defq part (read-blk stream 1048576))
				(buf-set data at (length part) part)
				(setq at (+ at (length part)))))
		+mnist_files (list +mnist_train_images +mnist_train_labels
			+mnist_test_images +mnist_test_labels))
	data)
