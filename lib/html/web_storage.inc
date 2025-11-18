;; Web Storage API
;; localStorage and sessionStorage implementation

(defclass web-storage nil
	(defq
		items (env)
		file_path :nil
		storage_type "local"))  ; "local" or "session"

(defmethod :init web-storage (storage-type &optional file-path)
	(setq this-items (env)
		  this-file_path file-path
		  this-storage_type storage-type)
	this)

(defmethod :set-item web-storage (key value)
	; Store an item
	; Storage.setItem(key, value)
	(set-insert this-items (sym key) (str value))
	:nil)

(defmethod :get-item web-storage (key)
	; Retrieve an item
	; Storage.getItem(key) -> value | nil
	(get this-items (sym key)))

(defmethod :remove-item web-storage (key)
	; Remove an item
	; Storage.removeItem(key)
	(set-remove this-items (sym key))
	:nil)

(defmethod :clear web-storage ()
	; Clear all items
	; Storage.clear()
	(setq this-items (env))
	:nil)

(defmethod :key web-storage (index)
	; Get key at index
	; Storage.key(n) -> key | nil
	(defq keys (env-keys this-items))
	(when (and (>= index 0) (< index (length keys)))
		(str (elem index keys))))

(defmethod :length web-storage ()
	; Get number of items
	; Storage.length
	(length (env-keys this-items)))

(defmethod :save web-storage ()
	; Save to file (for persistence)
	(when this-file_path
		(catch
			(progn
				(when (defq stream (file-stream this-file_path +file_open_write))
					(write-data stream (print this-items))
					:t))
			(lambda (e)
				(print "Failed to save storage: " e)
				:nil))))

(defmethod :load web-storage ()
	; Load from file
	(when this-file_path
		(catch
			(progn
				(when (path? this-file_path)
					(when (defq stream (file-stream this-file_path))
						(defq data (read-data stream (file-size this-file_path)))
						(when data
							(setq this-items (read (cat data)))
							:t))))
			(lambda (e)
				(print "Failed to load storage: " e)
				:nil))))

;; Helper Functions

(defun create-local-storage (&optional file-path)
	; Create localStorage instance
	(defq storage (web-storage))
	(. storage :init "local" (if file-path file-path ".chrysalisp_localStorage"))
	; Auto-load if file exists
	(. storage :load)
	storage)

(defun create-session-storage (&optional file-path)
	; Create sessionStorage instance (not persisted by default)
	(defq storage (web-storage))
	(. storage :init "session" file-path)
	storage)

(defun sym (str)
	; Convert string to symbol
	(read (cat ":" str)))

(defun str (value)
	; Convert value to string
	(if (str? value)
		value
		(print value)))
