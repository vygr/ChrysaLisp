;; Cookie Storage Library
;; File-based cookie persistence for ChrysaLisp browser

(import "lib/class/class.inc")

(defclass cookie-store (&optional _method) :nil
	; Cookie storage with file persistence

	; Initialize fields
	(def this
		:cookies (env)
		:file_path :nil)

	(defmethod :init (&optional file-path)
		(def this
			:cookies (env)
			:file_path file-path)
		this)

	(defmethod :set-cookie (name value &rest options)
		; Set a cookie
		; Options: :path "/path" :expires timestamp :max-age seconds
		(defq cookie (env))
		(set cookie :name name)
		(set cookie :value value)
		(set cookie :created (time))

		; Parse options
		(defq i 0)
		(while (< i (length options))
			(defq key (elem i options))
			(setq i (+ i 1))
			(when (< i (length options))
				(defq val (elem i options))
				(cond
					((eql key :path)
						(set cookie :path val))
					((eql key :expires)
						(set cookie :expires val))
					((eql key :max-age)
						(set cookie :max-age val)))
				(setq i (+ i 1))))

		; Store cookie
		(defq cookies-map (get :cookies this))
		(set cookies-map (string-to-sym name) cookie)
		(def this :cookies cookies-map)
		:nil)

	(defmethod :get-cookie (name)
		; Get cookie value (returns :nil if not found or expired)
		(defq cookie (get (get :cookies this) (string-to-sym name)))
		(when cookie
			; Check if expired
			(defq expires (get cookie :expires))
			(when (and expires (< expires (time)))
				; Expired - remove it
				(. this :delete-cookie name)
				(setq cookie :nil)))

		(if cookie
			(get cookie :value)
			:nil))

	(defmethod :get-cookie-full (name)
		; Get full cookie object with all metadata
		(defq cookie (get (get :cookies this) (string-to-sym name)))
		(when cookie
			; Check if expired
			(defq expires (get cookie :expires))
			(when (and expires (< expires (time)))
				; Expired - remove it
				(. this :delete-cookie name)
				(setq cookie :nil)))
		cookie)

	(defmethod :delete-cookie (name)
		; Delete a cookie
		(defq cookies-map (get :cookies this))
		(set-remove cookies-map (string-to-sym name))
		(def this :cookies cookies-map)
		:nil)

	(defmethod :get-all-cookies ()
		; Get all cookie names and values as a list
		(defq result (list))
		(defq cookies-map (get :cookies this))
		(defq keys (env-keys cookies-map))

		(each! (lambda (key)
			(defq cookie (get cookies-map key))
			(when cookie
				; Check not expired
				(defq expires (get cookie :expires))
				(when (or (eql expires :nil) (>= expires (time)))
					(defq entry (env))
					(set entry :name (get cookie :name))
					(set entry :value (get cookie :value))
					(push result entry))))
			keys)

		result)

	(defmethod :clear-all ()
		; Clear all cookies
		(def this :cookies (env))
		:nil)

	(defmethod :save ()
		; Save cookies to file
		(defq fp (get :file_path this))
		(when fp
			(catch
				(progn
					; Convert cookies to saveable format
					(defq data (env))
					(defq cookies-map (get :cookies this))
					(defq keys (env-keys cookies-map))

					(each! (lambda (key)
						(set data key (get cookies-map key)))
						keys)

					; Save to file using print (simple serialization)
					(when (defq stream (file-stream fp +file_open_write))
						(write-data stream (print data))
						:t))
				(lambda (e)
					(print "Failed to save cookies: " e)
					:nil))))

	(defmethod :load ()
		; Load cookies from file
		(defq fp (get :file_path this))
		(when fp
			(catch
				(progn
					(when (path? fp)
						(when (defq stream (file-stream fp))
							(defq data (read-data stream (file-size fp)))
							(when data
								(def this :cookies (read (cat data)))
								:t))))
				(lambda (e)
					(print "Failed to load cookies: " e)
					:nil)))))

;; Helper Functions

(defun create-cookie-store (&optional file-path)
	; Create a new cookie store
	(defq store (cookie-store))
	(. store :init file-path)
	store)

(defun string-to-sym (str)
	; Convert string to symbol
	(read (cat ":" str)))

(defun set-remove (env key)
	; Remove a key from an environment
	(set env key :nil))

(defun env-keys (env-obj)
	; Get all keys from an environment
	(defq result (list))
	(each! (lambda ((k v))
		(push result k))
		(tolist env-obj))
	result)

(defun time ()
	; Get current time in milliseconds
	; Placeholder - would use actual system time
	1000000)

