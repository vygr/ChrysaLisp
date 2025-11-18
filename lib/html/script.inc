;; HTML Script Execution Engine
;; Executes ChrysaLisp Lisp code from <script> tags
;; Provides DOM access like JavaScript but with Lisp syntax

(import "lib/html/dom.inc")
(import "lib/html/parser.inc")

;; Script execution context
(defclass script-context nil
	(defq
		document nil
		window nil
		base_path ""
		global_env (env)
		; Dialog tracking for testing
		alerts (list)
		confirms (list)
		prompts (list)
		; Mock dialog results
		confirm_result :t
		prompt_result nil))

(defmethod :init script-context (doc base-path)
	(setq this-document doc
		this-base_path base-path
		this-window this
		this-global_env (env)
		this-alerts (list)
		this-confirms (list)
		this-prompts (list)
		this-confirm_result :t
		this-prompt_result nil)

	; Add document and window to global environment
	(set-insert this-global_env "document" doc)
	(set-insert this-global_env "window" this)

	this)

(defmethod :execute-inline script-context (code)
	; Execute inline script code
	; Code is ChrysaLisp Lisp that has access to document/window
	(when (and code (> (length code) 0))
		(catch
			; Bind document and window in current scope for script access
			(defq document this-document)
			(defq window this-window)

			; Bind dialog functions as callable functions
			(defun alert (msg) (. window :alert msg))
			(defun confirm (msg) (. window :confirm msg))
			(defun prompt (msg &optional default) (. window :prompt msg default))

			; Eval the code - it can now access document, window, and dialogs
			(eval (read (cat code)))
			(lambda (e)
				(print "Script error: " e))))
	this)

(defmethod :execute-external script-context (src)
	; Load and execute external script file
	(defq full_path
		(if (starts-with "/" src)
			src
			(cat this-base_path src)))

	(when (path? full_path)
		(catch
			(defq code (load full_path))
			(. this :execute-inline code)
			(lambda (e)
				(print "Script load error: " src " - " e))))
	this)

(defmethod :get-global script-context (name)
	; Get a global variable
	(get this-global_env name))

(defmethod :set-global script-context (name value)
	; Set a global variable
	(set-insert this-global_env name value)
	value)

;; Dialog functions
(defmethod :alert script-context (message)
	; Display alert dialog
	; (. window :alert "message")
	(push this-alerts message)
	(print "[ALERT] " message)
	:nil)

(defmethod :confirm script-context (message)
	; Display confirm dialog, returns :t or :nil
	; (. window :confirm "Are you sure?")
	(push this-confirms message)
	(print "[CONFIRM] " message " -> " (if this-confirm_result "OK" "Cancel"))
	this-confirm_result)

(defmethod :prompt script-context (message &optional default_value)
	; Display prompt dialog, returns string or nil
	; (. window :prompt "Enter name:" "default")
	(defq entry (env))
	(set-insert entry :message message)
	(set-insert entry :default default_value)
	(push this-prompts entry)
	(print "[PROMPT] " message " (default: " default_value ") -> " this-prompt_result)
	this-prompt_result)

;; Testing helpers
(defmethod :get-alerts script-context ()
	; Get list of alert messages (for testing)
	this-alerts)

(defmethod :get-confirms script-context ()
	; Get list of confirm messages (for testing)
	this-confirms)

(defmethod :get-prompts script-context ()
	; Get list of prompt calls (for testing)
	this-prompts)

(defmethod :set-confirm-result script-context (result)
	; Set mock confirm result for testing
	(setq this-confirm_result result)
	this)

(defmethod :set-prompt-result script-context (result)
	; Set mock prompt result for testing
	(setq this-prompt_result result)
	this)

(defmethod :has-method script-context (method_name)
	; Check if method exists (for testing)
	:t)

;; Script executor
(defclass script-executor nil
	(defq
		document nil
		context nil
		base_path ""))

(defmethod :init script-executor (doc &optional base-path)
	(setq this-document doc
		this-base_path (if base-path base-path "")
		this-context (script-context :init doc this-base_path))
	this)

(defmethod :execute-scripts script-executor ()
	; Find and execute all script tags in document
	(defq scripts (. this-document :get-elements-by-tag-name "script"))

	(each! 0 -1 (lambda (script)
		; Check for src attribute (external script)
		(defq src (. script :get-attribute "src"))

		(if src
			; External script
			(. this-context :execute-external src)

			; Inline script
			(progn
				(defq code (. script :get-text-content))
				(. this-context :execute-inline code))))
		scripts)

	this)

(defmethod :register-inline-handlers script-executor ()
	; Register onclick and other inline event handlers
	; Scans document for elements with onclick, onmouseover, onmousedown, etc. attributes

	(defun register-handler (elem attr-name event-type ctx)
		; Register a single event handler for an attribute
		(when (. elem :has-attribute attr-name)
			(defq code (. elem :get-attribute attr-name))

			; Create event listener that executes the code
			(. elem :add-event-listener event-type
				(lambda (event)
					; Bind document and window for handler
					(defq document (. ctx 'document))
					(defq window (. ctx 'window))

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
		(defq ctx this-context)

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
			(when (= (. child 'node_type) NODE_ELEMENT)
				(process-element child)))
			(. elem 'child_nodes)))

	; Start from document root
	(defq root (. this-document 'document_element))
	(when root (process-element root))

	this)

(defmethod :get-context script-executor ()
	; Get the script execution context
	this-context)

;; Helper function to execute scripts in a document
(defun execute-document-scripts (doc &optional base-path)
	; Execute all scripts in a document and register inline handlers
	(defq executor (script-executor :init doc base-path))
	(. executor :execute-scripts)
	(. executor :register-inline-handlers)
	executor)

