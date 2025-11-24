;;;;;;;;;;;;;;;
; lib/fs/fs.inc
;;;;;;;;;;;;;;;

(defclass Fs () :nil
	;; The underlying stream for the filesystem
	(def this :stream :nil)

	(defabstractmethod :format ())
	(defabstractmethod :mount ())
	(defabstractmethod :unmount ())
	
	;; Directory operations
	(defabstractmethod :create_dir (path))
	(defabstractmethod :delete_dir (path))
	(defabstractmethod :list_dir (path))

	;; File operations
	(defabstractmethod :create_file (path))
	(defabstractmethod :delete_file (path))
	(defabstractmethod :stat (path))

	;; Handle operations
	(defabstractmethod :open_file (path mode))
	(defabstractmethod :close_file (handle))
	(defabstractmethod :read_file (handle size))
	(defabstractmethod :write_file (handle data))
	(defabstractmethod :seek_file (handle offset whence))
)