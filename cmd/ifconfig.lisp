;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ifconfig - Network Interface Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/options/options.inc")
(import "lib/net/utils.lisp")
(import "lib/net/ethernet.lisp")
(import "lib/net/ip.lisp")

; Global network interface state
(defq *net_interfaces* (env))

(defun show-interface (if_name if_data)
	; Display interface information
	(print if_name ": ")

	; Flags
	(defq flags (get if_data :flags))
	(print "flags=" (get flags :value) "<")
	(when (get flags :up) (print "UP"))
	(when (get flags :broadcast) (print ",BROADCAST"))
	(when (get flags :multicast) (print ",MULTICAST"))
	(print ">")
	(prinl)

	; MTU
	(print "  mtu " (get if_data :mtu))
	(prinl)

	; Hardware address
	(when (get if_data :mac)
		(print "  ether " (net/mac-to-string (get if_data :mac)))
		(prinl))

	; IPv4 address
	(when (get if_data :ipv4)
		(defq ipv4 (get if_data :ipv4))
		(print "  inet " (net/ip-to-string (get ipv4 :addr)))
		(print "  netmask " (net/ip-to-string (get ipv4 :netmask)))
		(when (get ipv4 :broadcast)
			(print "  broadcast " (net/ip-to-string (get ipv4 :broadcast))))
		(prinl))

	; Statistics
	(when (get if_data :stats)
		(defq stats (get if_data :stats))
		(print "  RX packets " (get stats :rx-packets)
		       "  bytes " (get stats :rx-bytes))
		(prinl)
		(print "  TX packets " (get stats :tx-packets)
		       "  bytes " (get stats :tx-bytes))
		(prinl))

	(prinl))

(defun show-all-interfaces ()
	; Show all interfaces
	(if (= (length *net-interfaces*) 0)
		(progn
			(print "No network interfaces configured")
			(prinl))
		(each (# (show-interface (elem 0 %0) (elem 1 %0)))
		      (pairs *net-interfaces*))))

(defun add-interface (if_name mac ip netmask gateway mtu)
	; Add or update network interface
	(defq if_data (env
		:flags (env :value 0x1043 :up t :broadcast t :multicast t)
		:mtu mtu
		:mac mac
		:ipv4 (env
			:addr ip
			:netmask netmask
			:broadcast (array 255 255 255 255)
			:gateway gateway)
		:stats (env
			:rx-packets 0
			:rx-bytes 0
			:tx-packets 0
			:tx-bytes 0)))

	(def *net-interfaces* (keyword if_name) if_data)

	(print "Interface " if_name " configured")
	(prinl))

(defun set-interface-up (if_name up)
	; Set interface up or down
	(defq if_data (get *net_interfaces* (keyword if_name)))
	(if if_data
		(progn
			(def (get if_data :flags) :up up)
			(print "Interface " if_name (if up " up" " down"))
			(prinl))
		(progn
			(print "Interface " if_name " not found")
			(prinl)
			(exit 1))))

(defun main ()
	; Parse command-line arguments
	(defq usage `(
		(("-h" "--help") "show this help message")
		(("-a" "--all") "show all interfaces")
		(("-s" "--stats") "show interface statistics")
		(("interface") "interface name (optional)"))
		args (options-parse usage))

	; Check for help
	(when (get-option args "-h")
		(print "Usage: ifconfig [options] [interface]")
		(prinl)
		(print (usage-string usage))
		(prinl)
		(print)
		(prinl)
		(print "Examples:")
		(prinl)
		(print "  ifconfig              - show all interfaces")
		(prinl)
		(print "  ifconfig eth0         - show specific interface")
		(prinl)
		(print "  ifconfig eth0 up      - bring interface up")
		(prinl)
		(print "  ifconfig eth0 down    - bring interface down")
		(prinl)
		(exit 0))

	; Initialize with demo interface if none exist
	(when (= (length *net-interfaces*) 0)
		(add-interface "eth0"
		               (array 0x00 0x11 0x22 0x33 0x44 0x55)
		               (array 192 168 1 100)
		               (array 255 255 255 0)
		               (array 192 168 1 1)
		               1500)
		(add-interface "lo"
		               (array 0x00 0x00 0x00 0x00 0x00 0x00)
		               (array 127 0 0 1)
		               (array 255 0 0 0)
		               (array 127 0 0 1)
		               65536))

	; Get interface name if provided
	(defq if_args (get_option args "*"))

	(cond
		; No interface specified - show all
		((= (length if_args) 0)
			(show-all-interfaces))

		; Interface with command (up/down)
		((= (length if_args) 2)
			(defq if_name (elem_get if_args 0)
			      cmd (elem-get if_args 1))
			(cond
				((= cmd "up")
					(set-interface-up if_name t))
				((= cmd "down")
					(set-interface-up if_name nil))
				(t
					(print "Unknown command: " cmd)
					(prinl)
					(exit 1))))

		; Show specific interface
		((= (length if_args) 1)
			(defq if_name (elem_get if_args 0)
			      if_data (get *net-interfaces* (keyword if_name)))
			(if if_data
				(show-interface if_name if_data)
				(progn
					(print if_name ": interface not found")
					(prinl)
					(exit 1))))

		; Too many arguments
		(t
			(print "Invalid arguments")
			(prinl)
			(exit 1))))

; Helper to get option value
(defun get-option (args key)
	(elem-get args (keyword key)))

; Run main
(main)
