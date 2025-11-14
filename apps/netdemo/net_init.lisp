;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Network Stack Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/net/consts.inc")
(import "lib/net/utils.lisp")
(import "lib/net/ethernet.lisp")
(import "lib/net/arp.lisp")
(import "lib/net/ip.lisp")
(import "lib/net/icmp.lisp")
(import "lib/net/udp.lisp")
(import "lib/net/tcp.lisp")

(defun net/initialize (mac ip netmask gateway send_fn)
	; Initialize the complete TCP/IP stack
	; Inputs:
	;   mac - our MAC address (6-byte array or string "XX:XX:XX:XX:XX:XX")
	;   ip - our IP address (4-byte array or string "A.B.C.D")
	;   netmask - network mask (4-byte array or string)
	;   gateway - default gateway (4-byte array or string)
	;   send-fn - function to send raw Ethernet frames
	; Output: t if successful

	; Convert string inputs to byte arrays if needed
	(when (str? mac)
		(setq mac (net/string-to-mac mac)))
	(when (str? ip)
		(setq ip (net/string-to-ip ip)))
	(when (str? netmask)
		(setq netmask (net/string-to-ip netmask)))
	(when (str? gateway)
		(setq gateway (net/string-to-ip gateway)))

	; Validate inputs
	(unless (and mac ip netmask gateway)
		(prin "Invalid network configuration")
		(prinl)
		(exit 1))

	(prin "Initializing TCP/IP stack...")
	(prinl)

	; Initialize layers from bottom up
	(prin "  - Ethernet layer")
	(prinl)
	(eth/init mac send-fn)

	(prin "  - ARP")
	(prinl)
	(arp/init)

	(prin "  - IP layer")
	(prinl)
	(ip/init ip netmask gateway)

	(prin "  - ICMP")
	(prinl)
	(icmp/init)

	(prin "  - UDP")
	(prinl)
	(udp/init)

	(prin "  - TCP")
	(prinl)
	(tcp/init)

	; Register protocol handlers
	(prin "  - Registering protocol handlers")
	(prinl)
	(eth/init-handlers)

	(prin "TCP/IP stack initialized successfully!")
	(prinl)
	(prin "  MAC Address: " (net/mac-to-string mac))
	(prinl)
	(prin "  IP Address:  " (net/ip-to-string ip))
	(prinl)
	(prin "  Netmask:     " (net/ip-to-string netmask))
	(prinl)
	(prin "  Gateway:     " (net/ip-to-string gateway))
	(prinl)

	t)

(defun net/demo-init ()
	; Demo initialization with example configuration
	; This would typically interface with the host OS network

	(prin "Network Stack Demo Initialization")
	(prinl)
	(prin)
	(prinl)

	; Example configuration
	(defq demo_mac "00:11:22:33:44:55"
	      demo-ip "192.168.1.100"
	      demo-netmask "255.255.255.0"
	      demo-gateway "192.168.1.1")

	; Dummy send function for demo
	(defq demo-send_fn
		(lambda (frame)
			(prin "[SEND] Ethernet frame, size: " (length frame) " bytes")
			(prinl)))

	; Initialize stack
	(net/initialize demo-mac demo-ip demo-netmask demo-gateway demo-send-fn))

; Export functions
(env
	:initialize net/initialize
	:demo-init net/demo-init)
