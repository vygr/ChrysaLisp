;REXX ADDRESS/PORTS System
;
;This implements ARexx-style ADDRESS command and port management,
;allowing REXX scripts to communicate with services via IPC.

(import "./app.inc")

;Port class - represents an addressable REXX port
(defclass Port () (service_name mailbox_id info current)
	; (Port service_name mailbox_id info) -> port

	(defmethod :init (name mbox inf)
		(setq service_name name mailbox_id mbox info inf current :nil)
		this)

	(defmethod :name ()
		service_name)

	(defmethod :mbox ()
		mailbox_id)

	(defmethod :info ()
		info)

	(defmethod :send (command)
		; (. port :send command) -> (rc result error)
		;send command to this port
		(defq reply_mbox (mail-mbox))
		(mail-send mailbox_id (setf-> (cat (str-alloc +rexx_execute_size) command)
			(+rexx_rpc_reply_id reply_mbox)
			(+rexx_rpc_type +rexx_cmd_type_execute)
			(+rexx_rpc_flags 0)))
		(defq result_msg (mail-read reply_mbox))
		(rexx-parse-result result_msg)))

;PortManager class - manages ADDRESS context for REXX interpreter
(defclass PortManager () (ports current_port default_port)
	; (PortManager) -> port_manager

	(defmethod :init ()
		(setq ports (Fmap) current_port "COMMAND" default_port "COMMAND")
		this)

	(defmethod :register (port_name mailbox_id &optional info)
		; (. pm :register port_name mailbox_id [info]) -> port
		;register a new port
		(defq port (Port port_name mailbox_id (if info info "")))
		(. ports :insert port_name port)
		port)

	(defmethod :unregister (port_name)
		; (. pm :unregister port_name)
		;unregister a port
		(. ports :erase port_name))

	(defmethod :find (port_name)
		; (. pm :find port_name) -> port | :nil
		;find a registered port
		(. ports :find port_name))

	(defmethod :address (port_name)
		; (. pm :address port_name) -> old_port_name
		;change current address (like ARexx ADDRESS command)
		(defq old current_port)
		(cond
			((= port_name "")
				;ADDRESS with no arg - return current
				current_port)
			((= (ascii-upper port_name) "VALUE")
				;ADDRESS VALUE expr - evaluate expression as port name
				;(simplified - just use as-is for now)
				(setq current_port port_name)
				old)
			(:t
				;ADDRESS portname - change current port
				(setq current_port (ascii-upper port_name))
				old)))

	(defmethod :current ()
		; (. pm :current) -> port_name
		;get current port name
		current_port)

	(defmethod :send (command &optional port_name)
		; (. pm :send command [port_name]) -> (rc result error)
		;send command to current or specified port
		(defq target (if port_name port_name current_port))
		(cond
			((defq port (. this :find target))
				;registered port - send directly
				(. port :send command))
			((defq services (mail-enquire (cat "Rexx." target ",")))
				;not registered but exists - send via RPC
				(when (nempty? services)
					(defq service (hex-decode (second (split (pop services) ",")))
						reply_mbox (mail-mbox))
					(mail-send service (setf-> (cat (str-alloc +rexx_execute_size) command)
						(+rexx_rpc_reply_id reply_mbox)
						(+rexx_rpc_type +rexx_cmd_type_execute)
						(+rexx_rpc_flags 0)))
					(defq result_msg (mail-read reply_mbox))
					(rexx-parse-result result_msg)))
			(:t
				;port not found
				(list +rexx_rc_error "" (cat "Port not found: " target)))))

	(defmethod :list (&optional prefix)
		; (. pm :list [prefix]) -> (list of port_names)
		;list available ports
		(defq local_ports (map (lambda (kv) (first kv)) (. ports :to_list))
			remote_ports (map (lambda (s)
				(slice (first (split s ",")) 5 -1))  ; remove "Rexx." prefix
				(mail-enquire (if prefix (cat "Rexx." prefix) "Rexx."))))
		(sort (uniq (cat local_ports remote_ports)))))

;Port discovery and management functions

(defun rexx-discover-ports ()
	; (rexx-discover-ports) -> (list of (name mailbox info))
	;discover all available REXX ports on the network
	(map (lambda (s)
		(defq parts (split s ","))
		(list (slice (first parts) 5 -1)  ; remove "Rexx." prefix
			(hex-decode (second parts))    ; mailbox id
			(if (> (length parts) 2) (third parts) "")))  ; info string
		(mail-enquire "Rexx.")))

(defun rexx-declare-port (port_name &optional info)
	; (rexx-declare-port port_name [info]) -> service_id
	;declare a REXX port (register with mail system)
	;port_name should NOT include "Rexx." prefix - it will be added
	(mail-declare (task-mbox)
		(cat "Rexx." (ascii-upper port_name))
		(if info info (cat "REXX Port: " port_name))))

(defun rexx-forget-port (service_id)
	; (rexx-forget-port service_id)
	;unregister a REXX port
	(mail-forget service_id))

;Built-in port implementations

(defclass CommandPort () (vars output)
	; (CommandPort) -> port
	;COMMAND port - internal command execution

	(defmethod :init ()
		(setq vars (Fmap) output (list))
		this)

	(defmethod :execute (command)
		; (. port :execute command) -> (rc result error)
		;execute internal REXX command
		;for now, just echo commands
		(push output command)
		(list +rexx_rc_ok command ""))

	(defmethod :get_output ()
		;get accumulated output
		(join output (ascii-char 10)))

	(defmethod :clear_output ()
		;clear accumulated output
		(setq output (list))))

(defclass SystemPort () (command_pipe)
	; (SystemPort) -> port
	;SYSTEM port - execute host system commands

	(defmethod :init ()
		(setq command_pipe :nil)
		this)

	(defmethod :execute (command)
		; (. port :execute command) -> (rc result error)
		;execute system command
		;In ChrysaLisp, we can use pipe-run to execute commands
		(catch
			(progn
				(defq result (pipe-run command))
				(list +rexx_rc_ok result ""))
			(list +rexx_rc_error "" (str _)))))

;Example of creating a custom port service

(defclass EchoPort () (prefix)
	; (EchoPort [prefix]) -> port
	;Simple echo port for testing

	(defmethod :init (&optional pfx)
		(setq prefix (if pfx pfx "ECHO: "))
		this)

	(defmethod :execute (command)
		; (. port :execute command) -> (rc result error)
		;echo command with prefix
		(list +rexx_rc_ok (cat prefix command) "")))
