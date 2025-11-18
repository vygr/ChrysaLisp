;REXX Action Handlers

(defun dispatch-action (&rest action)
	; (dispatch-action action ...)
	;safely dispatch action with error handling
	(catch (eval action)
		(progn
			(prin "Action error: ")
			(prin _)
			(print)
			:t)))

(defun action-execute ()
	; (action-execute)
	;execute REXX code from input area
	(defq source (get-text (. *window* :input_text))
		interp (. *window* :interpreter))

	;clear previous output
	(set-text (. *window* :output_text) "Executing...")

	;execute REXX code
	(defq rc (. interp :run source))

	;display results
	(defq output (. interp :get_output)
		result (. interp :getvar "RESULT"))

	(if (= rc 0)
		(set-text (. *window* :output_text)
			(if (nempty? output) output "OK"))
		(set-text (. *window* :output_text)
			(cat "Error (RC=" (str rc) ")\n" output)))

	;update status line with current ADDRESS
	(set-text (. *window* :status_label)
		(cat "ADDRESS: " (. (. interp :port_manager) :current))))

(defun action-clear ()
	; (action-clear)
	;clear input and output areas
	(set-text (. *window* :input_text) "")
	(set-text (. *window* :output_text) "")
	(set-text (. *window* :status_label) "ADDRESS: COMMAND"))

(defun action-show-ports ()
	; (action-show-ports)
	;show available REXX ports
	(defq ports (rexx-discover-ports)
		output (list "Available REXX Ports:" ""))

	(if (empty? ports)
		(push output "No ports currently registered.")
		(each! 0 -1 (lambda (port_info)
			(bind '(name mbox info) port_info)
			(push output (cat "  " name " - " info)))
			ports))

	(set-text (. *window* :output_text) (join output (ascii-char 10))))

(defun action-show-help ()
	; (action-show-help)
	;show REXX help
	(defq help_text
"REXX for ChrysaLisp - Help

BASIC COMMANDS:
  SAY text              - Output text
  ADDRESS port          - Change current port
  variable = value      - Assign variable
  EXIT [code]           - Exit with return code
  PARSE ARG var ...     - Parse arguments

MODERN FEATURES (RexxJS-inspired):
  String Interpolation:
    SAY \"User {name} is {age} years old\"
    Uses {variable} syntax in quoted strings

  C-Style Comments:
    // Modern comment style
    SAY \"Hello\"  // Inline comment
    -- Traditional style also works

ESCAPE SEQUENCES (in quoted strings):
  \\n                    - Newline
  \\t                    - Tab
  \\r                    - Carriage return
  \\\\                   - Backslash
  \\\"                   - Quote
  Example: SAY \"Line 1\\nLine 2\"

BUILT-IN FUNCTIONS:
  Core: UPPER, LOWER, SUBSTR, LENGTH, WORD, WORDS
        POS, REVERSE, LEFT, RIGHT

  Additional (RexxJS-inspired):
    TRIM_START(str)           - Remove leading whitespace
    TRIM_END(str)             - Remove trailing whitespace
    PROPER(str)               - Title case (capitalize words)
    PAD_START(str,len[,pad])  - Pad beginning to length
    PAD_END(str,len[,pad])    - Pad end to length
    COPIES(str,count)         - Repeat string count times

  Validation (returns 1 or 0):
    IS_ALPHA(str)             - Only letters?
    IS_NUMERIC(str)           - Only digits?
    IS_ALPHANUMERIC(str)      - Only letters/digits?
    IS_EMPTY(str)             - Empty or whitespace?
    STARTS_WITH(str,prefix)   - Starts with prefix?
    ENDS_WITH(str,suffix)     - Ends with suffix?
    INCLUDES(str,substring)   - Contains substring?

ADDRESS/PORTS:
  ADDRESS SYSTEM        - Send commands to host system
  ADDRESS COMMAND       - Send to internal command processor
  ADDRESS port          - Send to custom port

EXAMPLES:

  // Simple SAY with interpolation
  name = \"ChrysaLisp\"
  version = \"1.0\"
  SAY \"Running {name} version {version}\"

  // SYSTEM port with interpolation
  user = \"admin\"
  action = \"backup\"
  ADDRESS SYSTEM
  \"logger '{user} performed {action}'\"

  // Built-in functions
  text = \"hello world\"
  SAY UPPER(text)  // Output: HELLO WORLD
  SAY LENGTH(text) // Output: 11

  // Escape sequences
  SAY \"Name:\\tJohn\\nAge:\\t25\"
")
	(set-text (. *window* :output_text) help_text))

(defun handle-key-event (msg)
	; (handle-key-event msg)
	;handle keyboard events
	(defq key (getf msg +ev_msg_key_key)
		mod (getf msg +ev_msg_key_mod))

	;Ctrl+E or Cmd+E = Execute
	(when (and (bits? mod +ev_key_mod_control +ev_key_mod_alt +ev_key_mod_meta)
			(= key (ascii-code "e")))
		(action-execute))

	;Ctrl+L or Cmd+L = Clear
	(when (and (bits? mod +ev_key_mod_control +ev_key_mod_alt +ev_key_mod_meta)
			(= key (ascii-code "l")))
		(action-clear)))
