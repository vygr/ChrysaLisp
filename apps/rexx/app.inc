;REXX IPC Message Protocol
;
;This defines the message structures for REXX inter-process communication,
;implementing ARexx-style ADDRESS/PORTS capability.

;Helper functions for list access
(defun first (lst)
	; (first list) -> elem
	(elem-get lst 0))

(defun second (lst)
	; (second list) -> elem
	(elem-get lst 1))

(defun third (lst)
	; (third list) -> elem
	(elem-get lst 2))

(defun last (lst)
	; (last list) -> elem
	(elem-get lst -1))

;REXX command types
(enums +rexx_cmd_type 0
	(enum execute query getvar setvar quit))

;REXX result codes (standard REXX RC values)
(enums +rexx_rc 0
	(enum ok error syntax halt failure))

;Base REXX RPC structure
(structure +rexx_rpc 0
	(netid reply_id)     ; where to send result
	(uint type)          ; command type
	(uint flags))        ; execution flags

;REXX execute command - execute REXX code on addressed port
(structure +rexx_execute +rexx_rpc_size
	(offset source))     ; REXX source code follows

;REXX query command - query port status/capabilities
(structure +rexx_query +rexx_rpc_size
	(offset query))      ; query string follows

;REXX getvar command - get variable value from port
(structure +rexx_getvar +rexx_rpc_size
	(offset varname))    ; variable name follows

;REXX setvar command - set variable value in port
(structure +rexx_setvar +rexx_rpc_size
	(uint varname_len)   ; length of variable name
	(offset data))       ; varname + value data follows

;REXX quit command - tell port to quit
(structure +rexx_quit +rexx_rpc_size)

;REXX result structure
(structure +rexx_result 0
	(uint rc)            ; return code (0=success, >0=error)
	(uint result_len)    ; length of result string
	(uint error_len)     ; length of error string
	(offset data))       ; result + error strings follow

;Helper functions for REXX RPC

(defun rexx-alloc-result (rc result error)
	; (rexx-alloc-result rc result error) -> msg
	;allocate a REXX result message
	(defq result_str (if (str? result) result (str result))
		error_str (if (str? error) error (str error)))
	(setf-> (cat (str-alloc +rexx_result_size) result_str error_str)
		(+rexx_result_rc rc)
		(+rexx_result_result_len (length result_str))
		(+rexx_result_error_len (length error_str))))

(defun rexx-parse-result (msg)
	; (rexx-parse-result msg) -> (rc result error)
	;parse a REXX result message
	(defq rc (getf msg +rexx_result_rc)
		result_len (getf msg +rexx_result_result_len)
		error_len (getf msg +rexx_result_error_len)
		data_offset +rexx_result_size)
	(list rc
		(slice msg data_offset (+ data_offset result_len))
		(slice msg (+ data_offset result_len) (+ data_offset result_len error_len))))

;REXX port RPC functions

(defun rexx-send-command (port_name source)
	; (rexx-send-command port_name source) -> (rc result error)
	;send REXX command to named port and wait for result
	(defq services (mail-enquire (cat "Rexx." port_name ",")))
	(if (empty? services)
		(list +rexx_rc_error "" (cat "REXX port not found: " port_name))
		(progn
			(defq service (hex-decode (second (split (pop services) ",")))
				mbox (mail-mbox))
			(mail-send service (setf-> (cat (str-alloc +rexx_execute_size) source)
				(+rexx_rpc_reply_id mbox)
				(+rexx_rpc_type +rexx_cmd_type_execute)
				(+rexx_rpc_flags 0)))
			(defq result_msg (mail-read mbox))
			(rexx-parse-result result_msg))))

(defun rexx-query-port (port_name query)
	; (rexx-query-port port_name query) -> (rc result error)
	;query REXX port
	(defq services (mail-enquire (cat "Rexx." port_name ",")))
	(if (empty? services)
		(list +rexx_rc_error "" (cat "REXX port not found: " port_name))
		(progn
			(defq service (hex-decode (second (split (pop services) ",")))
				mbox (mail-mbox))
			(mail-send service (setf-> (cat (str-alloc +rexx_query_size) query)
				(+rexx_rpc_reply_id mbox)
				(+rexx_rpc_type +rexx_cmd_type_query)
				(+rexx_rpc_flags 0)))
			(defq result_msg (mail-read mbox))
			(rexx-parse-result result_msg))))

(defun rexx-list-ports (&optional prefix)
	; (rexx-list-ports [prefix]) -> (list of port_info)
	;list all REXX ports matching optional prefix
	(defq search_str (if prefix (cat "Rexx." prefix) "Rexx."))
	(map (lambda (s)
		(defq parts (split s ","))
		(list (slice (first parts) 5 -1)  ; remove "Rexx." prefix
			(hex-decode (second parts))    ; mailbox id
			(if (> (length parts) 2) (third parts) "")))  ; info string
		(mail-enquire search_str)))
