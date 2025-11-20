;; File Utilities for HTML Browser
;; Simple file I/O operations for file:// protocol support

;; Read entire file into a string
(defun read-file-to-string (filepath)
	; Opens a file and reads all content into a single string
	; Returns the file content or nil if file cannot be read
	; Uses load() which is the built-in ChrysaLisp function for reading entire files
	(load filepath))

;; Extract filepath from file:// URL
(defun filepath-from-url (url)
	; Parse file:// URL and extract filesystem path
	; Format: file:///path/to/file.html
	; Returns the path portion or nil if not a valid file:// URL
	(defq prefix "file://")
	(defq prefix-len (length prefix))
	(if (and (>= (length url) prefix-len)
			(eql (slice url 0 prefix-len) prefix))
		(slice url prefix-len)
		:nil))

;; Load HTML from file:// URL
(defun load-html-from-url (url)
	; Load HTML content from a file:// URL
	; Returns the HTML content or nil if file not found
	(defq filepath (filepath-from-url url))
	(if filepath
		(read-file-to-string filepath)
		:nil))

;; Check if file exists (by attempting to read it)
(defun file-exists? (filepath)
	; Check if a file exists by using age() which returns 0 if file doesn't exist
	; Returns true if file exists and is readable, nil otherwise
	(not (eql (age filepath) 0)))

;; Get file size
(defun file-size (filepath)
	; Get the size of a file in bytes
	; Returns the file size or 0 if file not found
	(defq content (load filepath))
	(if content
		(length content)
		0))

;; Extract filename from path
(defun get-filename (filepath)
	; Extract just the filename from a full path
	; Example: /home/user/test.html -> test.html
	(defq parts (split filepath "/"))
	(if (> (length parts) 0)
		(last parts)
		filepath))

;; Extract filename from file:// URL
(defun get-filename-from-url (url)
	; Extract filename from a file:// URL
	(defq filepath (filepath-from-url url))
	(if filepath
		(get-filename filepath)
		:nil))

;; Validate HTML filename extension
(defun is-html-file? (filename)
	; Check if filename has HTML-related extension
	(defq lower-name (to-lower filename))
	(or
		(string-ends-with? lower-name ".html")
		(string-ends-with? lower-name ".htm")
		(string-ends-with? lower-name ".xhtml")))

;; Helper: check if string ends with suffix
(defun string-ends-with? (str suffix)
	; Check if string ends with the given suffix
	(defq str-len (length str))
	(defq suffix-len (length suffix))
	(if (>= str-len suffix-len)
		(eql (slice str (- str-len suffix-len)) suffix)
		:nil))

;; Format file:// URL from filepath
(defun make-file-url (filepath)
	; Create a valid file:// URL from a filepath
	(cat "file://" filepath))
