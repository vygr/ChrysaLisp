;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; UDP (User Datagram Protocol)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/net/consts.inc")
(import "lib/net/packet.inc")
(import "lib/net/utils.lisp")
(import "lib/net/ip.lisp")

;;;;;;;;;;;;;;;;;;
; UDP State
;;;;;;;;;;;;;;;;;;

(defq *udp-sockets* (env))  ; UDP socket registry: port -> handler-fn
(defq *udp-next-ephemeral-port* 49152)  ; Next ephemeral port

(defun udp/init ()
	; Initialize UDP layer
	(setq *udp-sockets* (env))
	(setq *udp-next-ephemeral-port* 49152)
	; Register UDP as IP protocol handler
	(ip/register-handler ip_proto_udp udp/process))

;;;;;;;;;;;;;;;;;;
; UDP Packet Creation
;;;;;;;;;;;;;;;;;;

(defun udp/create-packet (src-port dst-port data)
	; Create UDP packet
	; Inputs:
	;   src-port, dst-port - port numbers (16-bit)
	;   data - payload data (byte array)
	; Output: UDP packet (byte array)
	(defq pkt (array)
	      total-len (+ udp_hdr_len (length data)))

	; Source port
	(net/write-u16 pkt 0 src-port)

	; Destination port
	(net/write-u16 pkt 2 dst-port)

	; Length
	(net/write-u16 pkt 4 total-len)

	; Checksum (0 = no checksum for now, optional in IPv4)
	(net/write-u16 pkt 6 0)

	; Add data
	(each (# (push pkt %0)) data)

	pkt)

(defun udp/calculate-checksum (src-ip dst-ip udp-packet)
	; Calculate UDP checksum with pseudo-header
	; Inputs:
	;   src-ip, dst-ip - IP addresses (4-byte arrays)
	;   udp-packet - UDP packet (byte array)
	; Output: checksum value

	; Create pseudo-header
	(defq pseudo (array)
	      udp-len (length udp-packet))

	; Source IP
	(each (# (push pseudo %0)) src-ip)

	; Destination IP
	(each (# (push pseudo %0)) dst-ip)

	; Zero
	(push pseudo 0)

	; Protocol (UDP = 17)
	(push pseudo ip_proto_udp)

	; UDP length
	(net/write-u16 pseudo (length pseudo) udp-len)

	; Add UDP packet
	(each (# (push pseudo %0)) udp-packet)

	; Calculate checksum
	(defq cksum (net/checksum pseudo 0 (length pseudo)))

	; Checksum of 0 is represented as 0xFFFF in UDP
	(if (= cksum 0) 0xFFFF cksum))

;;;;;;;;;;;;;;;;;;
; UDP Packet Parsing
;;;;;;;;;;;;;;;;;;

(defun udp/parse (data)
	; Parse UDP packet
	; Input: data - byte array
	; Output: environment with parsed fields or nil if invalid
	(if (< (length data) udp_hdr_len)
		nil
		(env
			:src-port (net/read-u16 data 0)
			:dst-port (net/read-u16 data 2)
			:length (net/read-u16 data 4)
			:checksum (net/read-u16 data 6)
			:data (slice data udp_hdr_len (length data)))))

;;;;;;;;;;;;;;;;;;
; UDP Socket Management
;;;;;;;;;;;;;;;;;;

(defun udp/bind (port handler-fn)
	; Bind UDP port to handler function
	; Inputs:
	;   port - port number (1-65535)
	;   handler-fn - function to call with (src-ip src-port data)
	; Output: t if success, nil if port already in use
	(if (elem-get *udp-sockets* port)
		nil
		(progn
			(elem-set *udp-sockets* port handler-fn)
			t)))

(defun udp/unbind (port)
	; Unbind UDP port
	; Input: port - port number
	(elem-set *udp-sockets* port nil))

(defun udp/allocate-port ()
	; Allocate ephemeral port
	; Output: port number or nil if none available
	(defq start-port *udp-next-ephemeral-port*
	      port *udp-next-ephemeral-port*)

	; Find next available port
	(while (and (elem-get *udp-sockets* port)
	            (not (= (+ port 1) start-port)))
		(setq port (+ port 1))
		(when (>= port 65536)
			(setq port 49152)))

	; Check if we found a free port
	(if (elem-get *udp-sockets* port)
		nil
		(progn
			(setq *udp-next-ephemeral-port* (+ port 1))
			(when (>= *udp-next-ephemeral-port* 65536)
				(setq *udp-next-ephemeral-port* 49152))
			port)))

;;;;;;;;;;;;;;;;;;
; UDP Sending
;;;;;;;;;;;;;;;;;;

(defun udp/send (dst-ip src-port dst-port data)
	; Send UDP packet
	; Inputs:
	;   dst-ip - destination IP (4-byte array)
	;   src-port - source port
	;   dst-port - destination port
	;   data - payload data
	; Output: IP packet or nil
	(defq udp-pkt (udp/create-packet src-port dst-port data))

	; Optionally calculate checksum
	; (defq cksum (udp/calculate-checksum (ip/get-addr) dst-ip udp-pkt))
	; (net/write-u16 udp-pkt 6 cksum)

	; Send via IP layer
	(ip/send-packet dst-ip ip_proto_udp udp-pkt))

;;;;;;;;;;;;;;;;;;
; UDP Reception
;;;;;;;;;;;;;;;;;;

(defun udp/process (src-ip dst-ip data)
	; Process incoming UDP packet
	; Inputs: src-ip, dst-ip - IP addresses (4-byte arrays), data - UDP packet
	; Output: t if processed, nil if error
	(defq udp-pkt (udp/parse data))

	(if udp-pkt
		(progn
			; Find handler for destination port
			(defq handler (elem-get *udp-sockets* (elem-get udp-pkt :dst-port)))

			(if handler
				(progn
					; Call handler
					(handler src-ip
					         (elem-get udp-pkt :src-port)
					         (elem-get udp-pkt :data))
					t)
				; No handler for this port - could send ICMP port unreachable
				nil))
		; Invalid packet
		nil))

;;;;;;;;;;;;;;;;;;
; UDP Socket API
;;;;;;;;;;;;;;;;;;

(defun udp/create-socket ()
	; Create UDP socket
	; Output: socket environment
	(env
		:type sock_dgram
		:port nil
		:handler nil))

(defun udp/socket-bind (socket port handler-fn)
	; Bind UDP socket to port
	; Inputs: socket - socket environment, port - port number, handler-fn
	; Output: t if success, nil if failed
	(if (udp/bind port handler-fn)
		(progn
			(elem-set socket :port port)
			(elem-set socket :handler handler-fn)
			t)
		nil))

(defun udp/socket-send (socket dst-ip dst-port data)
	; Send data via UDP socket
	; Inputs: socket, dst-ip, dst-port, data
	; Output: packet or nil
	(defq src-port (elem-get socket :port))
	(if src-port
		(udp/send dst-ip src-port dst-port data)
		nil))

(defun udp/socket-close (socket)
	; Close UDP socket
	; Input: socket - socket environment
	(defq port (elem-get socket :port))
	(when port
		(udp/unbind port)
		(elem-set socket :port nil)
		(elem-set socket :handler nil)))
