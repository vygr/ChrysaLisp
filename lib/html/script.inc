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
		global_env (env)))

(defmethod :init script-context (doc base-path)
	(setq this-document doc
		this-base_path base-path
		this-window this
		this-global_env (env))

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

			; Eval the code - it can now access document and window
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

(defmethod :get-context script-executor ()
	; Get the script execution context
	this-context)

;; Helper function to execute scripts in a document
(defun execute-document-scripts (doc &optional base-path)
	; Execute all scripts in a document
	(defq executor (script-executor :init doc base-path))
	(. executor :execute-scripts)
	executor)

