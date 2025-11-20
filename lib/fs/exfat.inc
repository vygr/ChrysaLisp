;;;;;;;;;;;;;;;;;;;;
; lib/fs/exfat.inc
;;;;;;;;;;;;;;;;;;;;

(import "./fs.inc")

(defclass ExFat () (Fs)
	(defmethod :format ())
	(defmethod :mount ())
	(defmethod :unmount ())
	
	(defmethod :create_dir (path))
	(defmethod :delete_dir (path))
	(defmethod :list_dir (path))

	(defmethod :create_file (path))
	(defmethod :delete_file (path))
	(defmethod :open_file (path mode))
	(defmethod :close_file (handle))
	(defmethod :read_file (handle length))
	(defmethod :write_file (handle data))
	(defmethod :seek_file (handle offset whence))
	(defmethod :stat (path))
)