;;;;;;;;;;;;;;;;;;;;;;
; lib/fs/ramdisk.inc
;;;;;;;;;;;;;;;;;;;;;;

(import "./exfat.inc")

(defclass RamDisk () :nil
	(def this :stream :nil :disk_size 0)

	(defmethod :create (size_mb)
		(defq size (* size_mb 1048576))
		(defq mstream (memory-stream))
		(lower (:stream mstream) (:disk_size size))
		this)

	(defmethod :get_stream ()
		(raise :stream)
		stream)

	(defmethod :stream_size ()
		(raise :disk_size)
		disk_size)
)

;; Usage Example:
;; (defq disk (RamDisk))
;; (. disk :create 4) ; 4MB disk
;; (. disk :mount)
;; (print (. disk :list_dir "/"))