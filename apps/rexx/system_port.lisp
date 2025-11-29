;REXX SYSTEM Port Service
;
;Implements the SYSTEM port for executing host commands.
;This is similar to ARexx's SYSTEM ADDRESS target.

(import "./app.inc")

(defun execute-command (command)
	; (execute-command command) -> (rc output error)
	;execute a system command using pipe
	(catch
		(progn
			;use pipe-run to execute command
			(defq output (pipe-run command))
			(list +rexx_rc_ok output ""))
		;error handling
		(list +rexx_rc_error "" (str _))))

(defun main ()
	;declare SYSTEM port service
	(defq system_service (mail-declare (task-mbox) "Rexx.SYSTEM" "REXX SYSTEM Port 1.0"))

	;main service loop
	(while :t
		(defq msg (mail-read (task-mbox))
			reply_id (getf msg +rexx_rpc_reply_id)
			msg_type (getf msg +rexx_rpc_type))

		(case msg_type
			(+rexx_cmd_type_execute
				;execute command
				(defq command (slice msg +rexx_execute_size -1))
				(bind '(rc result error) (execute-command command))
				(mail-send reply_id (rexx-alloc-result rc result error)))

			(+rexx_cmd_type_query
				;query port info
				(mail-send reply_id
					(rexx-alloc-result +rexx_rc_ok "SYSTEM Port - Execute host commands" "")))

			(+rexx_cmd_type_quit
				;quit command
				(mail-send reply_id (rexx-alloc-result +rexx_rc_ok "SYSTEM Port shutting down" ""))
				(mail-forget system_service)
				(exit))))

	;cleanup (unreachable unless quit)
	(mail-forget system_service))
