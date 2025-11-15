;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TCP Client Example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/net/consts.inc")
(import "lib/net/utils.inc")
(import "lib/net/ip.inc")
(import "lib/net/tcp.inc")
(import "lib/net/tcp_state.inc")
(import "lib/net/socket.inc")

(defun tcp-client/main (args)
	; Simple TCP client
	; Usage: (tcp-client/main '("192.168.1.100" "7777" "Hello, World!"))

	(when (< (length args) 3)
		(prin "Usage: tcp-client <ip-address> <port> <message>")
		(prinl)
		(exit 1))

	(defq target-ip-str (elem_get args 0)
	      target-ip (net/string-to-ip target-ip-str)
	      target-port (num (elem-get args 1))
	      message (elem-get args 2))

	(unless target-ip
		(prin "Invalid IP address: " target-ip-str)
		(prinl)
		(exit 1))

	; Initialize TCP
	(tcp/init)

	; Create socket
	(defq sock (socket/create sock_stream))

	(prin "Connecting to " target-ip-str ":" target-port "...")
	(prinl)

	; Connect
	(if (socket/connect sock target-ip target-port)
		(progn
			; Wait for connection to be established (simplified)
			(defq timeout 5000000  ; 5 seconds
			      start-time (time))

			(while (and (not (socket/is-connected sock))
			           (< (- (time) start-time) timeout))
				(task-sleep 100000))  ; Sleep 100ms

			(if (socket/is-connected sock)
				(progn
					(prin "Connected!")
					(prinl)

					; Convert message to byte array
					(defq data (array))
					(each (# (push data (char-code %0)))
					      (explode message))

					; Send message
					(prin "Sending: " message)
					(prinl)
					(socket/send sock data)

					; Wait for response
					(task-sleep 1000000)  ; Wait 1 second

					; Receive response
					(defq response (socket/recv sock))
					(if (> (length response) 0)
						(progn
							(prin "Received: ")
							(each (# (prin (code-char %0))) response)
							(prinl))
						(progn
							(prin "No response received")
							(prinl)))

					; Close connection
					(socket/close sock)
					(prin "Connection closed")
					(prinl))
				(progn
					(prin "Connection timeout")
					(prinl)
					(socket/close sock)
					(exit 1))))
		(progn
			(prin "Failed to connect")
			(prinl)
			(exit 1))))

; Export main function
tcp-client/main
