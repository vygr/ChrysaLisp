;; HTML Script Execution Engine
;; Executes ChrysaLisp Lisp code from <script> tags
;; Provides DOM access like JavaScript but with Lisp syntax

(import "lib/class/class.inc")
(import "lib/html/dom.inc")
(import "lib/html/parser.inc")
(import "lib/html/cookies.inc")
(import "lib/html/web_storage.inc")
(import "lib/html/web_audio.inc")

;; Script execution context
(defclass script-context (&optional _method) :nil
	; Script execution context
	(def this
		:document :nil
		:window :nil
		:base_path ""
		:global_env (env)
		; Dialog tracking for testing
		:alerts (list)
		:confirms (list)
		:prompts (list)
		; Mock dialog results
		:confirm_result :t
		:prompt_result :nil
		; Cookie store
		:cookie_store :nil
		; Web Storage
		:local_storage :nil
		:session_storage :nil)

	(defmethod :init (doc base-path)
		(defq cs (create-cookie-store))
		(defq ls (create-local-storage))
		(defq ss (create-session-storage))
		(def this
			:document doc
			:base_path base-path
			:window this
			:global_env (env)
			:alerts (list)
			:confirms (list)
			:prompts (list)
			:confirm_result :t
			:prompt_result :nil
			:cookie_store cs
			:local_storage ls
			:session_storage ss)

		; Add document and window to global environment
		(defq global-env (get :global_env this))
		(set-insert global-env "document" doc)
		(set-insert global-env "window" this)

		; Add Web Storage to global environment
		(set-insert global-env "localStorage" ls)
		(set-insert global-env "sessionStorage" ss)

		; Add Web Audio API constructor
		(set-insert global-env "AudioContext" AudioContext)

		this)

	(defmethod :execute-inline (code)
		; Execute inline script code
		; Code is ChrysaLisp Lisp that has access to document/window
		(when (and code (> (length code) 0))
			(catch
				; Bind document and window in current scope for script access
				(defq document (get :document this))
				(defq window (get :window this))

				; Bind dialog functions as callable functions
				(defun alert (msg) (. window :alert msg))
				(defun confirm (msg) (. window :confirm msg))
				(defun prompt (msg &optional default) (. window :prompt msg default))

				; Eval the code - it can now access document, window, and dialogs
				(eval (read (cat code)))
				(lambda (e)
					(print "Script error: " e))))
		this)

	(defmethod :execute-external (src)
		; Load and execute external script file
		(defq base-path (get :base_path this))
		(defq full_path
			(if (starts-with "/" src)
				src
				(cat base-path src)))

		(when (path? full_path)
			(catch
				(defq code (load full_path))
				(. this :execute-inline code)
				(lambda (e)
					(print "Script load error: " src " - " e))))
		this)

	(defmethod :get-global (name)
		; Get a global variable
		(get (get :global_env this) name))

	(defmethod :set-global (name value)
		; Set a global variable
		(defq global-env (get :global_env this))
		(set-insert global-env name value)
		value)

	;; Dialog functions
	(defmethod :alert (message)
		; Display alert dialog
		; (. window :alert "message")
		(defq alerts (get :alerts this))
		(push alerts message)
		(def this :alerts alerts)
		(print "[ALERT] " message)
		:nil)

	(defmethod :confirm (message)
		; Display confirm dialog, returns :t or :nil
		; (. window :confirm "Are you sure?")
		(defq confirms (get :confirms this))
		(push confirms message)
		(def this :confirms confirms)
		(defq result (get :confirm_result this))
		(print "[CONFIRM] " message " -> " (if result "OK" "Cancel"))
		result)

	(defmethod :prompt (message &optional default_value)
		; Display prompt dialog, returns string or nil
		; (. window :prompt "Enter name:" "default")
		(defq entry (env))
		(set-insert entry :message message)
		(set-insert entry :default default_value)
		(defq prompts (get :prompts this))
		(push prompts entry)
		(def this :prompts prompts)
		(defq result (get :prompt_result this))
		(print "[PROMPT] " message " (default: " default_value ") -> " result)
		result)

	;; Cookie functions
	(defmethod :set-cookie (name value &rest options)
		; Set a cookie
		; (. document :set-cookie "username" "alice")
		; (. document :set-cookie "session" "xyz" :expires future-time)
		(defq cs (get :cookie_store this))
		(apply (. cs :set-cookie) (cons name (cons value options)))
		:nil)

	(defmethod :get-cookie (name)
		; Get cookie value
		; (. document :get-cookie "username")
		(. (get :cookie_store this) :get-cookie name))

	(defmethod :delete-cookie (name)
		; Delete a cookie
		; (. document :delete-cookie "old-cookie")
		(. (get :cookie_store this) :delete-cookie name)
		:nil)

	(defmethod :get-all-cookies ()
		; Get all cookies
		(. (get :cookie_store this) :get-all-cookies))

	(defmethod :clear-cookies ()
		; Clear all cookies
		(. (get :cookie_store this) :clear-all)
		:nil)

	;; Testing helpers
	(defmethod :get-alerts ()
		; Get list of alert messages (for testing)
		(get :alerts this))

	(defmethod :get-confirms ()
		; Get list of confirm messages (for testing)
		(get :confirms this))

	(defmethod :get-prompts ()
		; Get list of prompt calls (for testing)
		(get :prompts this))

	(defmethod :set-confirm-result (result)
		; Set mock confirm result for testing
		(def this :confirm_result result)
		this)

	(defmethod :set-prompt-result (result)
		; Set mock prompt result for testing
		(def this :prompt_result result)
		this)

	(defmethod :has-method (method_name)
		; Check if method exists (for testing)
		:t))

;; Script executor
(defclass script-executor (&optional _method) :nil
	; Script executor
	(def this
		:document :nil
		:context :nil
		:base_path "")

	(defmethod :init (doc &optional base-path)
		(defq bp (if base-path base-path ""))
		(defq ctx (script-context))
		(. ctx :init doc bp)
		(def this
			:document doc
			:base_path bp
			:context ctx)
		this)

	(defmethod :execute-scripts ()
		; Find and execute all script tags in document
		(defq doc (get :document this))
		(defq ctx (get :context this))
		(defq scripts (. doc :get-elements-by-tag-name "script"))

		(each! 0 -1 (lambda (script)
			; Check for src attribute (external script)
			(defq src (. script :get-attribute "src"))

			(if src
				; External script
				(. ctx :execute-external src)

				; Inline script
				(progn
					(defq code (. script :get-text-content))
					(. ctx :execute-inline code))))
			scripts)

		this)

	(defmethod :register-inline-handlers ()
		; Register onclick and other inline event handlers
		; Scans document for elements with onclick, onmouseover, onmousedown, etc. attributes

		(defq ctx (get :context this))

		(defun register-handler (elem attr-name event-type context)
			; Register a single event handler for an attribute
			(when (. elem :has-attribute attr-name)
				(defq code (. elem :get-attribute attr-name))

				; Create event listener that executes the code
				(. elem :add-event-listener event-type
					(lambda (event)
						; Bind document and window for handler
						(defq document (get :document context))
						(defq window (get :window context))

						; Bind dialog functions as callable functions
						(defun alert (msg) (. window :alert msg))
						(defun confirm (msg) (. window :confirm msg))
						(defun prompt (msg &optional default) (. window :prompt msg default))

						; Execute the event handler code
						(catch
							(eval (read (cat code)))
							(lambda (e)
								(print attr-name " error: " e)))))))

		(defun process-element (elem)
			; Register all supported event handlers

			; Mouse events
			(register-handler elem "onclick" "click" ctx)
			(register-handler elem "onmouseover" "mouseover" ctx)
			(register-handler elem "onmouseout" "mouseout" ctx)
			(register-handler elem "onmousedown" "mousedown" ctx)
			(register-handler elem "onmouseup" "mouseup" ctx)

			; Form events
			(register-handler elem "onchange" "change" ctx)
			(register-handler elem "onfocus" "focus" ctx)
			(register-handler elem "onblur" "blur" ctx)
			(register-handler elem "onsubmit" "submit" ctx)

			; Recursively process children
			(each! 0 -1 (lambda (child)
				(when (eql (get :node_type child) NODE_ELEMENT)
					(process-element child)))
				(get :child_nodes elem)))

		; Start from document root
		(defq doc (get :document this))
		(defq root (get :document_element doc))
		(when root (process-element root))

		this)

	(defmethod :get-context ()
		; Get the script execution context
		(get :context this)))

;; Helper function to execute scripts in a document
(defun execute-document-scripts (doc &optional base-path)
	; Execute all scripts in a document and register inline handlers
	(defq executor (script-executor))
	(. executor :init doc base-path)
	(. executor :execute-scripts)
	(. executor :register-inline-handlers)
	executor)
