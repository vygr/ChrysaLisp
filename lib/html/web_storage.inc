;; Web Storage API
;; localStorage and sessionStorage implementation

(import "lib/class/class.inc")

(defclass web-storage () :nil
	; Web Storage (localStorage/sessionStorage)

	; Initialize fields
	(def this
		:items (env)
		:file_path :nil
		:storage_type "local")  ; "local" or "session"

	(defmethod :init (storage-type &optional file-path)
		(def this
			:items (env)
			:file_path file-path
			:storage_type storage-type)
		this)

	(defmethod :set-item (key value)
		; Store an item
		; Storage.setItem(key, value)
		(defq items-map (get :items this))
		(set-insert items-map (sym key) (str value))
		(def this :items items-map)
		:nil)

	(defmethod :get-item (key)
		; Retrieve an item
		; Storage.getItem(key) -> value | nil
		(get (get :items this) (sym key)))

	(defmethod :remove-item (key)
		; Remove an item
		; Storage.removeItem(key)
		(defq items-map (get :items this))
		(set-remove items-map (sym key))
		(def this :items items-map)
		:nil)

	(defmethod :clear ()
		; Clear all items
		; Storage.clear()
		(def this :items (env))
		:nil)

	(defmethod :key (index)
		; Get key at index
		; Storage.key(n) -> key | nil
		(defq keys (env-keys (get :items this)))
		(when (and (>= index 0) (< index (length keys)))
			(str (elem index keys))))

	(defmethod :length ()
		; Get number of items
		; Storage.length
		(length (env-keys (get :items this))))

	(defmethod :save ()
		; Save to file (for persistence)
		(defq fp (get :file_path this))
		(when fp
			(catch
				(progn
					(defq items-map (get :items this))
					(when (defq stream (file-stream fp +file_open_write))
						(write-data stream (print items-map))
						:t))
				(lambda (e)
					(print "Failed to save storage: " e)
					:nil))))

	(defmethod :load ()
		; Load from file
		(defq fp (get :file_path this))
		(when fp
			(catch
				(progn
					(when (path? fp)
						(when (defq stream (file-stream fp))
							(defq data (read-data stream (file-size fp)))
							(when data
								(def this :items (read (cat data)))
								:t))))
				(lambda (e)
					(print "Failed to load storage: " e)
					:nil)))))

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

