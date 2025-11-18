;; Browser Driver Library (WebDriver-compatible)
;; Selenium-style browser automation for testing

(import "lib/html/parser.inc")
(import "lib/html/dom.inc")
(import "lib/html/script.inc")

;; Browser Driver Class

(defclass browser-driver nil
	(defq
		document nil
		executor nil
		current_url ""
		window_width 1024
		window_height 768
		base_path ""))

(defmethod :init browser-driver ()
	(setq this-document nil
		  this-executor nil
		  this-current_url ""
		  this-window_width 1024
		  this-window_height 768
		  this-base_path "")
	this)

;; Navigation

(defmethod :navigate browser-driver (url)
	; Navigate to a URL
	; For now, only file:// URLs are supported
	(setq this-current_url url)

	; Extract file path from file:// URL
	(when (starts-with url "file://")
		(defq file-path (slice 7 (length url) url))
		(setq this-base_path file-path)

		; Load file content (placeholder - would need file reading)
		; For testing, we'll just store the URL
		)

	this)

(defmethod :load-html browser-driver (html-string)
	; Load HTML content directly (for testing)
	(setq this-document (parse-html html-string))

	; Execute scripts
	(setq this-executor (execute-document-scripts this-document))

	this)

(defmethod :refresh browser-driver ()
	; Refresh the page (re-execute scripts)
	(when this-document
		(setq this-executor (execute-document-scripts this-document)))
	this)

(defmethod :current-url browser-driver ()
	; Get current URL
	this-current_url)

(defmethod :title browser-driver ()
	; Get page title
	(when this-document
		(defq titles (. this-document :get-elements-by-tag-name "title"))
		(when (> (length titles) 0)
			(defq title-elem (first titles))
			(. title-elem :get-text-content)))
	"")

;; Element Finding

(defmethod :find-element-by-id browser-driver (id)
	; Find element by ID
	; Returns: WebElement wrapper
	(when this-document
		(defq elem (. this-document :get-element-by-id id))
		(when elem
			(create-web-element elem this)))
	:nil)

(defmethod :find-elements-by-tag-name browser-driver (tag-name)
	; Find all elements with tag name
	; Returns: list of WebElement wrappers
	(when this-document
		(defq elements (. this-document :get-elements-by-tag-name tag-name))
		(map (lambda (elem) (create-web-element elem this)) elements))
	(list))

(defmethod :find-element-by-selector browser-driver (selector)
	; Find element by CSS selector (simplified)
	; Currently supports: .classname, #id, tagname
	(when this-document
		(cond
			; ID selector: #id
			((starts-with selector "#")
				(defq id (slice 1 (length selector) selector))
				(. this :find-element-by-id id))

			; Class selector: .classname
			((starts-with selector ".")
				(defq classname (slice 1 (length selector) selector))
				(defq all-elems (. this-document :get-elements-by-tag-name "*"))
				(defq found :nil)
				(each! 0 -1 (lambda (elem)
					(when (and (eql found :nil) (. elem :has-class classname))
						(setq found (create-web-element elem this))))
					all-elems)
				found)

			; Tag name selector
			(:t
				(defq elements (. this :find-elements-by-tag-name selector))
				(if (> (length elements) 0)
					(first elements)
					:nil))))
	:nil)

(defmethod :wait-for-element-by-id browser-driver (id timeout-ms)
	; Wait for element to exist (simplified - immediate check)
	; In real implementation, would poll with timeout
	(. this :find-element-by-id id))

;; Script Execution

(defmethod :execute-script browser-driver (script-code)
	; Execute LispScript and return result
	(when this-executor
		(defq ctx (. this-executor :get-context))
		(catch
			(eval (read (cat script-code)))
			(lambda (e)
				(print "Script execution error: " e)
				:nil))))

(defmethod :script-context browser-driver ()
	; Get the script execution context
	(when this-executor
		(. this-executor :get-context))
	:nil)

;; Window Management

(defmethod :window-size browser-driver ()
	; Get window size
	(defq size (env))
	(set-insert size :width this-window_width)
	(set-insert size :height this-window_height)
	size)

(defmethod :set-window-size browser-driver (width height)
	; Set window size
	(setq this-window_width width
		  this-window_height height)
	this)

;; Dialog Handling

(defmethod :get-alerts browser-driver ()
	; Get all alert messages
	(when this-executor
		(defq ctx (. this-executor :get-context))
		(defq win (. ctx 'window))
		(. win :get-alerts))
	(list))

(defmethod :set-confirm-result browser-driver (result)
	; Set mock confirm() result
	(when this-executor
		(defq ctx (. this-executor :get-context))
		(defq win (. ctx 'window))
		(. win :set-confirm-result result))
	this)

(defmethod :set-prompt-result browser-driver (result)
	; Set mock prompt() result
	(when this-executor
		(defq ctx (. this-executor :get-context))
		(defq win (. ctx 'window))
		(. win :set-prompt-result result))
	this)

;; Cookies (placeholder)

(defmethod :get-cookies browser-driver ()
	; Get all cookies (placeholder)
	(list))

;; Screenshot (placeholder)

(defmethod :screenshot browser-driver ()
	; Take screenshot (placeholder)
	(env :type "screenshot" :data "placeholder"))

;; WebElement Wrapper Class

(defclass web-element nil
	(defq
		element nil
		driver nil))

(defmethod :init web-element (elem drv)
	(setq this-element elem
		  this-driver drv)
	this)

(defmethod :click web-element ()
	; Click the element
	(when this-element
		; Dispatch click event
		(. this-element :dispatch-event (env :type "click")))
	this)

(defmethod :send-keys web-element (keys)
	; Send keys to element (type into it)
	(when this-element
		; For input/textarea, set value attribute
		(defq tag (. this-element 'tag_name))
		(when (or (eql tag "input") (eql tag "textarea"))
			; Append to existing value
			(defq current (. this-element :get-attribute "value"))
			(when (eql current :nil) (setq current ""))
			(. this-element :set-attribute "value" (cat current keys))

			; Trigger change event
			(. this-element :dispatch-event (env :type "change"))))
	this)

(defmethod :clear web-element ()
	; Clear element value
	(when this-element
		(. this-element :set-attribute "value" "")
		(. this-element :dispatch-event (env :type "change")))
	this)

(defmethod :text web-element ()
	; Get element text content
	(when this-element
		(. this-element :get-text-content))
	"")

(defmethod :get-attribute web-element (name)
	; Get attribute value
	(when this-element
		(. this-element :get-attribute name))
	:nil)

(defmethod :is-displayed web-element ()
	; Check if element is displayed
	(when this-element
		; Check for display:none style
		(defq style (. this-element :get-attribute "style"))
		(if (and style (find-in-string style "display:none"))
			:nil
			:t))
	:nil)

(defmethod :is-enabled web-element ()
	; Check if element is enabled
	(when this-element
		; Check for disabled attribute
		(if (. this-element :has-attribute "disabled")
			:nil
			:t))
	:t)

(defmethod :find-element-by-id web-element (id)
	; Find child element by ID
	(when this-element
		; Search within this element's subtree
		(defq found (find-element-in-tree this-element id))
		(when found
			(create-web-element found this-driver)))
	:nil)

;; Helper Functions

(defun create-browser-driver ()
	; Create a new browser driver instance
	(defq driver (browser-driver))
	(. driver :init)
	driver)

(defun create-web-element (elem driver)
	; Create a WebElement wrapper
	(defq web-elem (web-element))
	(. web-elem :init elem driver)
	web-elem)

(defun starts-with (str prefix)
	; Check if string starts with prefix
	(when (and str (>= (length str) (length prefix)))
		(eql (slice 0 (length prefix) str) prefix))
	:nil)

(defun find-in-string (str pattern)
	; Simple string find (returns position or :nil)
	(defq i 0)
	(defq found :nil)
	(while (and (eql found :nil) (<= (+ i (length pattern)) (length str)))
		(when (eql (slice i (+ i (length pattern)) str) pattern)
			(setq found i))
		(setq i (+ i 1)))
	found)

(defun find-element-in-tree (root id)
	; Recursively find element by ID in tree
	(if (and (. root :has-attribute "id")
			 (eql (. root :get-attribute "id") id))
		; Found it
		root

		; Search children
		(progn
			(defq found :nil)
			(defq children (. root 'child_nodes))
			(when children
				(each! 0 -1 (lambda (child)
					(when (and (eql found :nil) (= (. child 'node_type) NODE_ELEMENT))
						(defq result (find-element-in-tree child id))
						(when result
							(setq found result))))
					children))
			found)))
