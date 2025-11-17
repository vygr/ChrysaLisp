;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Base Filesystem Class (Fs)
; Base class for all filesystem implementations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "class/obj/lisp.inc")

(defclass Fs ()
	; Abstract base class for filesystem implementations
	; Subclasses should override these methods to provide filesystem-specific behavior

	(defmethod :init (stream)
		; Initialize filesystem with a stream (typically memory-stream)
		; inputs
		; stream = stream object for the filesystem image
		; outputs
		; this
		(setd this :stream stream)
		this)

	(defmethod :deinit ()
		; Deinitialize filesystem and cleanup resources
		this)

	(defmethod :format (size)
		; Format the filesystem with the given size
		; inputs
		; size = total size in bytes
		; outputs
		; t if successful, nil otherwise
		(throw "Fs:format not implemented" this))

	(defmethod :mount ()
		; Mount the filesystem (read and validate filesystem structures)
		; outputs
		; t if successful, nil otherwise
		(throw "Fs:mount not implemented" this))

	(defmethod :unmount ()
		; Unmount the filesystem (flush all pending writes)
		; outputs
		; t if successful, nil otherwise
		(throw "Fs:unmount not implemented" this))

	(defmethod :mkdir (path)
		; Create a directory at the given path
		; inputs
		; path = directory path (str)
		; outputs
		; t if successful, nil otherwise
		(throw "Fs:mkdir not implemented" this))

	(defmethod :create (path)
		; Create a file at the given path
		; inputs
		; path = file path (str)
		; outputs
		; t if successful, nil otherwise
		(throw "Fs:create not implemented" this))

	(defmethod :open (path mode)
		; Open a file and return a file handle
		; inputs
		; path = file path (str)
		; mode = open mode (:read, :write, :append)
		; outputs
		; file handle or nil if failed
		(throw "Fs:open not implemented" this))

	(defmethod :close (handle)
		; Close a file handle
		; inputs
		; handle = file handle
		; outputs
		; t if successful, nil otherwise
		(throw "Fs:close not implemented" this))

	(defmethod :read (handle buffer size)
		; Read data from a file
		; inputs
		; handle = file handle
		; buffer = buffer to read into (or nil to allocate)
		; size = number of bytes to read
		; outputs
		; str containing data read, or nil on error
		(throw "Fs:read not implemented" this))

	(defmethod :write (handle data)
		; Write data to a file
		; inputs
		; handle = file handle
		; data = data to write (str)
		; outputs
		; number of bytes written, or -1 on error
		(throw "Fs:write not implemented" this))

	(defmethod :seek (handle offset whence)
		; Seek to a position in a file
		; inputs
		; handle = file handle
		; offset = offset in bytes
		; whence = seek mode (0=start, 1=current, 2=end)
		; outputs
		; new position, or -1 on error
		(throw "Fs:seek not implemented" this))

	(defmethod :tell (handle)
		; Get current position in a file
		; inputs
		; handle = file handle
		; outputs
		; current position in bytes
		(throw "Fs:tell not implemented" this))

	(defmethod :remove (path)
		; Remove a file
		; inputs
		; path = file path (str)
		; outputs
		; t if successful, nil otherwise
		(throw "Fs:remove not implemented" this))

	(defmethod :rmdir (path)
		; Remove a directory
		; inputs
		; path = directory path (str)
		; outputs
		; t if successful, nil otherwise
		(throw "Fs:rmdir not implemented" this))

	(defmethod :exists (path)
		; Check if a file or directory exists
		; inputs
		; path = path (str)
		; outputs
		; t if exists, nil otherwise
		(throw "Fs:exists not implemented" this))

	(defmethod :stat (path)
		; Get file/directory information
		; inputs
		; path = path (str)
		; outputs
		; hash map with file info (:size, :type, :created, :modified), or nil
		(throw "Fs:stat not implemented" this))

	(defmethod :list (path)
		; List directory contents
		; inputs
		; path = directory path (str)
		; outputs
		; list of filenames, or nil on error
		(throw "Fs:list not implemented" this))

	(defmethod :rename (old_path new_path)
		; Rename/move a file or directory
		; inputs
		; old_path = current path (str)
		; new_path = new path (str)
		; outputs
		; t if successful, nil otherwise
		(throw "Fs:rename not implemented" this))

	(defmethod :get-stream ()
		; Get the underlying stream object
		; outputs
		; stream object
		(get this :stream))

	(defmethod :get-size ()
		; Get total filesystem size
		; outputs
		; size in bytes
		(throw "Fs:get-size not implemented" this)))
