;REXX Echo Service - Demonstrates PORT capability
;
;This service registers as "Rexx.ECHO" and echoes back any message received
;with a prefix. This demonstrates the receiver side of ADDRESS/PORTS IPC.

(import "./app.inc")

(defun main ()
	;declare ECHO port service
	(defq echo_service (mail-declare (task-mbox) "Rexx.ECHO" "REXX Echo Service 1.0")
		message_count 0)

	(prin "ECHO Service started, registered as Rexx.ECHO") (print)
	(prin "Waiting for messages...") (print)

	;main service loop
	(while :t
		(defq msg (mail-read (task-mbox))
			reply_id (getf msg +rexx_rpc_reply_id)
			msg_type (getf msg +rexx_rpc_type))

		(case msg_type
			(+rexx_cmd_type_execute
				;execute command (echo it back)
				(setq message_count (inc message_count))
				(defq command (slice msg +rexx_execute_size -1))
				(defq response (cat "ECHO [" (str message_count) "]: " command))
				(prin "Received: ") (prin command) (print)
				(prin "Sending:  ") (prin response) (print)
				(mail-send reply_id (rexx-alloc-result +rexx_rc_ok response "")))

			(+rexx_cmd_type_query
				;query port info
				(mail-send reply_id
					(rexx-alloc-result +rexx_rc_ok
						(cat "ECHO Service - Messages processed: " (str message_count))
						"")))

			(+rexx_cmd_type_quit
				;quit command
				(prin "ECHO Service received QUIT command") (print)
				(mail-send reply_id (rexx-alloc-result +rexx_rc_ok "ECHO Service shutting down" ""))
				(mail-forget echo_service)
				(exit))))

	;cleanup (unreachable unless quit)
	(mail-forget echo_service))
