;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; UDP (User Datagram Protocol)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./consts.inc")
(import "./packet.inc")
(import "./utils.inc")
(import "./ip.inc")

;;;;;;;;;;;;;;;;;;
; UDP State
;;;;;;;;;;;;;;;;;;

(defq *udp_sockets* (env 32))  ; UDP socket registry: port -> handler-fn
(defq *udp_next_ephemeral_port* 49152)  ; Next ephemeral port

(defun udp/init ()
	; Initialize UDP layer
	(setq *udp_sockets* (env 32))
	(setq *udp_next_ephemeral_port* 49152)
	; Register UDP as IP protocol handler
	(ip/register-handler ip_proto_udp udp/process))

;;;;;;;;;;;;;;;;;;
; UDP Packet Creation
;;;;;;;;;;;;;;;;;;

(defun udp/create-packet (src_port dst_port data)
	; Create UDP packet
	; Inputs:
	;   src_port, dst_port - port numbers (16-bit)
	;   data - payload data (byte array)
	; Output: UDP packet (byte array)
	(defq pkt (array)
	      total_len (+ udp_hdr_len (length data)))

	; Source port
	(net/write-u16 pkt 0 src_port)

	; Destination port
	(net/write-u16 pkt 2 dst_port)

	; Length
	(net/write-u16 pkt 4 total_len)

	; Checksum (0 = no checksum for now, optional in IPv4)
	(net/write-u16 pkt 6 0)

	; Add data
	(each (# (push pkt %0)) data)

	pkt)

(defun udp/calculate-checksum (src_ip dst_ip udp_packet)
	; Calculate UDP checksum with pseudo-header
	; Inputs:
	;   src_ip, dst_ip - IP addresses (4-byte arrays)
	;   udp_packet - UDP packet (byte array)
	; Output: checksum value

	; Create pseudo-header
	(defq pseudo (array)
	      udp_len (length udp_packet))

	; Source IP
	(each (# (push pseudo %0)) src_ip)

	; Destination IP
	(each (# (push pseudo %0)) dst_ip)

	; Zero
	(push pseudo 0)

	; Protocol (UDP = 17)
	(push pseudo ip_proto_udp)

	; UDP length
	(net/write-u16 pseudo (length pseudo) udp_len)

	; Add UDP packet
	(each (# (push pseudo %0)) udp_packet)

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

(defun udp/bind (port handler_fn)
	; Bind UDP port to handler function
	; Inputs:
	;   port - port number (1-65535)
	;   handler_fn - function to call with (src_ip src_port data)
	; Output: t if success, nil if port already in use
	(if (elem-get *udp_sockets* port)
		nil
		(progn
			(elem-set *udp_sockets* port handler_fn)
			t)))

(defun udp/unbind (port)
	; Unbind UDP port
	; Input: port - port number
	(elem-set *udp_sockets* port nil))

(defun udp/allocate-port ()
	; Allocate ephemeral port
	; Output: port number or nil if none available
	(defq start_port *udp_next_ephemeral_port*
	      port *udp_next_ephemeral_port*)

	; Find next available port
	(while (and (elem-get *udp_sockets* port)
	            (not (= (+ port 1) start_port)))
		(setq port (+ port 1))
		(when (>= port 65536)
			(setq port 49152)))

	; Check if we found a free port
	(if (elem-get *udp_sockets* port)
		nil
		(progn
			(setq *udp_next_ephemeral_port* (+ port 1))
			(when (>= *udp_next_ephemeral_port* 65536)
				(setq *udp_next_ephemeral_port* 49152))
			port)))

;;;;;;;;;;;;;;;;;;
; UDP Sending
;;;;;;;;;;;;;;;;;;

(defun udp/send (dst_ip src_port dst_port data)
	; Send UDP packet
	; Inputs:
	;   dst_ip - destination IP (4-byte array)
	;   src_port - source port
	;   dst_port - destination port
	;   data - payload data
	; Output: IP packet or nil
	(defq udp_pkt (udp/create-packet src_port dst_port data))

	; Optionally calculate checksum
	; (defq cksum (udp/calculate-checksum (ip/get-addr) dst_ip udp_pkt))
	; (net/write-u16 udp_pkt 6 cksum)

	; Send via IP layer
	(ip/send-packet dst_ip ip_proto_udp udp_pkt))

;;;;;;;;;;;;;;;;;;
; UDP Reception
;;;;;;;;;;;;;;;;;;

(defun udp/process (src_ip dst_ip data)
	; Process incoming UDP packet
	; Inputs: src_ip, dst_ip - IP addresses (4-byte arrays), data - UDP packet
	; Output: t if processed, nil if error
	(defq udp_pkt (udp/parse data))

	(if udp_pkt
		(progn
			; Find handler for destination port
			(defq handler (elem-get *udp_sockets* (elem-get udp_pkt :dst-port)))

			(if handler
				(progn
					; Call handler
					(handler src_ip
					         (elem-get udp_pkt :src-port)
					         (elem-get udp_pkt :data))
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

(defun udp/socket-bind (socket port handler_fn)
	; Bind UDP socket to port
	; Inputs: socket - socket environment, port - port number, handler_fn
	; Output: t if success, nil if failed
	(if (udp/bind port handler_fn)
		(progn
			(elem-set socket :port port)
			(elem-set socket :handler handler_fn)
			t)
		nil))

(defun udp/socket-send (socket dst_ip dst_port data)
	; Send data via UDP socket
	; Inputs: socket, dst_ip, dst_port, data
	; Output: packet or nil
	(defq src_port (elem-get socket :port))
	(if src_port
		(udp/send dst_ip src_port dst_port data)
		nil))

(defun udp/socket-close (socket)
	; Close UDP socket
	; Input: socket - socket environment
	(defq port (elem-get socket :port))
	(when port
		(udp/unbind port)
		(elem-set socket :port nil)
		(elem-set socket :handler nil)))
