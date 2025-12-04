; Network ABI definitions

; Network driver function indices (matching net_pcap.cpp host_net_funcs array)
(defcvar 'net_init 0)
(defcvar 'net_deinit 1)
(defcvar 'net_send 2)
(defcvar 'net_recv 3)
(defcvar 'net_poll 4)
(defcvar 'net_get_info 5)
(defcvar 'net_get_stats 6)

; Network configuration structure
(def-struct net_info 0
	(ubyte mac 6)
	(ubyte ip 4)
	(ubyte netmask 4)
	(ubyte gateway 4)
	(uint mtu))

; Network statistics structure
(def-struct net_stats 0
	(ulong rx_packets rx_bytes tx_packets tx_bytes))
