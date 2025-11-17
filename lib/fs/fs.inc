;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Base Filesystem Class (Fs)
; Base class for all filesystem implementations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "class/obj/lisp.inc")

(defclass Fs () :nil
	; Abstract base class for filesystem implementations
	; Subclasses should override these methods to provide filesystem-specific behavior

	(defmethod :init (stream_obj)
		; Initialize filesystem with a stream (typically memory-stream)
		; inputs
		; stream_obj = stream object for the filesystem image
		; outputs
		; this
		(def this :stream stream_obj)
		this)

	(defmethod :deinit ()
		; Deinitialize filesystem and cleanup resources
		this)

	(defabstractmethod :format (size))
		; Format the filesystem with the given size
		; inputs
		; size = total size in bytes
		; outputs
		; :t if successful, :nil otherwise

	(defabstractmethod :mount ())
		; Mount the filesystem (read and validate filesystem structures)
		; outputs
		; :t if successful, :nil otherwise

	(defabstractmethod :unmount ())
		; Unmount the filesystem (flush all pending writes)
		; outputs
		; :t if successful, :nil otherwise

	(defabstractmethod :mkdir (path))
		; Create a directory at the given path
		; inputs
		; path = directory path (str)
		; outputs
		; :t if successful, :nil otherwise

	(defabstractmethod :create (path))
		; Create a file at the given path
		; inputs
		; path = file path (str)
		; outputs
		; :t if successful, :nil otherwise

	(defabstractmethod :open (path mode))
		; Open a file and return a file handle
		; inputs
		; path = file path (str)
		; mode = open mode (:read, :write, :append)
		; outputs
		; file handle or :nil if failed

	(defabstractmethod :close (file_handle))
		; Close a file handle
		; inputs
		; file_handle = file handle
		; outputs
		; :t if successful, :nil otherwise

	(defabstractmethod :read (file_handle buffer_unused size))
		; Read data from a file
		; inputs
		; file_handle = file handle
		; buffer_unused = buffer to read into (or :nil to allocate)
		; size = number of bytes to read
		; outputs
		; str containing data read, or :nil on error

	(defabstractmethod :write (file_handle data))
		; Write data to a file
		; inputs
		; file_handle = file handle
		; data = data to write (str)
		; outputs
		; number of bytes written, or -1 on error

	(defabstractmethod :seek (file_handle offset whence))
		; Seek to a position in a file
		; inputs
		; file_handle = file handle
		; offset = offset in bytes
		; whence = seek mode (0=start, 1=current, 2=end)
		; outputs
		; new position, or -1 on error

	(defabstractmethod :tell (file_handle))
		; Get current position in a file
		; inputs
		; file_handle = file handle
		; outputs
		; current position in bytes

	(defabstractmethod :remove (path))
		; Remove a file
		; inputs
		; path = file path (str)
		; outputs
		; :t if successful, :nil otherwise

	(defabstractmethod :rmdir (path))
		; Remove a directory
		; inputs
		; path = directory path (str)
		; outputs
		; :t if successful, :nil otherwise

	(defabstractmethod :exists (path))
		; Check if a file or directory exists
		; inputs
		; path = path (str)
		; outputs
		; :t if exists, :nil otherwise

	(defabstractmethod :stat (path))
		; Get file/directory information
		; inputs
		; path = path (str)
		; outputs
		; hash map with file info (:size, :type, :created, :modified), or :nil

	(defabstractmethod :list (path))
		; List directory contents
		; inputs
		; path = directory path (str)
		; outputs
		; list of filenames, or :nil on error

	(defabstractmethod :rename (old_path new_path))
		; Rename/move a file or directory
		; inputs
		; old_path = current path (str)
		; new_path = new path (str)
		; outputs
		; :t if successful, :nil otherwise

	(defmethod :get_stream ()
		; Get the underlying stream object
		; outputs
		; stream object
		(get this :stream))

	(defabstractmethod :get_size ())
		; Get total filesystem size
		; outputs
		; size in bytes
	)
