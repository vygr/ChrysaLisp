// Host Network Driver - libpcap implementation
// Portable Ethernet frame send/receive for Windows/Mac/Linux
// Uses libpcap (Unix/Linux), WinPcap/Npcap (Windows)

#if defined(_HOST_NET)
#if _HOST_NET == 0

#include <stdint.h>
#include <string.h>
#include <stdio.h>

// Platform-specific includes
#ifdef _WIN32
	#include <winsock2.h>
	#include <windows.h>
	#include <pcap.h>
	#pragma comment(lib, "wpcap.lib")
	#pragma comment(lib, "ws2_32.lib")
#else
	#include <pcap.h>
	#include <unistd.h>
	#include <sys/types.h>
	#include <sys/socket.h>
	#include <netinet/in.h>
	#include <arpa/inet.h>
	#ifdef __linux__
		#include <linux/if_ether.h>
	#else
		#include <net/ethernet.h>
	#endif
#endif

// Network interface state
static pcap_t *net_handle = nullptr;
static char net_interface[256] = "";
static uint8_t net_mac[6] = {0x00, 0x11, 0x22, 0x33, 0x44, 0x55};
static uint8_t net_ip[4] = {192, 168, 1, 100};
static uint8_t net_netmask[4] = {255, 255, 255, 0};
static uint8_t net_gateway[4] = {192, 168, 1, 1};
static uint32_t net_mtu = 1500;
static int net_initialized = 0;

// Statistics
static uint64_t net_rx_packets = 0;
static uint64_t net_rx_bytes = 0;
static uint64_t net_tx_packets = 0;
static uint64_t net_tx_bytes = 0;

// Error buffer
static char net_errbuf[PCAP_ERRBUF_SIZE];

//////////////////////////////////////////////
// Network Interface Functions
//////////////////////////////////////////////

// Initialize network interface
// Parameters:
//   iface - interface name (e.g., "eth0", "en0", "\Device\NPF_{GUID}" on Windows)
//           If NULL or empty, uses first available interface
//   mac - MAC address (6 bytes), if NULL uses interface's actual MAC
//   ip - IP address (4 bytes)
//   netmask - network mask (4 bytes)
//   gateway - gateway address (4 bytes)
//   mtu - Maximum Transmission Unit
// Returns: 0 on success, -1 on error
int host_net_init(const char *iface, uint8_t *mac, uint8_t *ip,
                  uint8_t *netmask, uint8_t *gateway, uint32_t mtu)
{
	if (net_initialized) {
		fprintf(stderr, "Network already initialized\n");
		return 0;
	}

	// Copy configuration
	if (mac) memcpy(net_mac, mac, 6);
	if (ip) memcpy(net_ip, ip, 4);
	if (netmask) memcpy(net_netmask, netmask, 4);
	if (gateway) memcpy(net_gateway, gateway, 4);
	net_mtu = mtu ? mtu : 1500;

	// Find interface if not specified
	if (!iface || iface[0] == '\0') {
		pcap_if_t *alldevs = nullptr;
		if (pcap_findalldevs(&alldevs, net_errbuf) == -1) {
			fprintf(stderr, "Error finding devices: %s\n", net_errbuf);
			return -1;
		}

		if (!alldevs) {
			fprintf(stderr, "No network interfaces found\n");
			pcap_freealldevs(alldevs);
			return -1;
		}

		// Use first non-loopback interface
		pcap_if_t *dev = alldevs;
		while (dev) {
			if (!(dev->flags & PCAP_IF_LOOPBACK)) {
				strncpy(net_interface, dev->name, sizeof(net_interface) - 1);
				net_interface[sizeof(net_interface) - 1] = '\0';
				break;
			}
			dev = dev->next;
		}

		if (net_interface[0] == '\0' && alldevs) {
			// Fall back to first interface (even if loopback)
			strncpy(net_interface, alldevs->name, sizeof(net_interface) - 1);
			net_interface[sizeof(net_interface) - 1] = '\0';
		}

		pcap_freealldevs(alldevs);

		if (net_interface[0] == '\0') {
			fprintf(stderr, "No suitable network interface found\n");
			return -1;
		}

		fprintf(stderr, "Using interface: %s\n", net_interface);
	} else {
		strncpy(net_interface, iface, sizeof(net_interface) - 1);
		net_interface[sizeof(net_interface) - 1] = '\0';
	}

	// Open interface for capture
	// snaplen: 65535 (max), promisc: 1 (yes), timeout: 1ms
	net_handle = pcap_open_live(net_interface, 65535, 1, 1, net_errbuf);
	if (!net_handle) {
		fprintf(stderr, "Error opening interface %s: %s\n", net_interface, net_errbuf);
		return -1;
	}

	// Set non-blocking mode
	if (pcap_setnonblock(net_handle, 1, net_errbuf) == -1) {
		fprintf(stderr, "Error setting non-blocking mode: %s\n", net_errbuf);
		pcap_close(net_handle);
		net_handle = nullptr;
		return -1;
	}

	// Check datalink type (should be Ethernet)
	int datalink = pcap_datalink(net_handle);
	if (datalink != DLT_EN10MB) {
		fprintf(stderr, "Warning: Interface %s is not Ethernet (datalink=%d)\n",
		        net_interface, datalink);
		// Continue anyway - might still work
	}

	net_initialized = 1;
	fprintf(stderr, "Network initialized on %s (MAC: %02x:%02x:%02x:%02x:%02x:%02x)\n",
	        net_interface,
	        net_mac[0], net_mac[1], net_mac[2],
	        net_mac[3], net_mac[4], net_mac[5]);

	return 0;
}

// Deinitialize network interface
// Returns: 0 on success
int host_net_deinit()
{
	if (!net_initialized) {
		return 0;
	}

	if (net_handle) {
		pcap_close(net_handle);
		net_handle = nullptr;
	}

	net_initialized = 0;
	fprintf(stderr, "Network deinitialized\n");
	return 0;
}

// Send Ethernet frame
// Parameters:
//   data - frame data (including Ethernet header)
//   length - frame length in bytes
// Returns: number of bytes sent, or -1 on error
int host_net_send(uint8_t *data, uint32_t length)
{
	if (!net_initialized || !net_handle) {
		fprintf(stderr, "Network not initialized\n");
		return -1;
	}

	if (length < 14 || length > 65535) {
		fprintf(stderr, "Invalid frame length: %u\n", length);
		return -1;
	}

	// Send the packet
	if (pcap_sendpacket(net_handle, data, length) != 0) {
		fprintf(stderr, "Error sending packet: %s\n", pcap_geterr(net_handle));
		return -1;
	}

	net_tx_packets++;
	net_tx_bytes += length;

	return length;
}

// Receive Ethernet frame (non-blocking)
// Parameters:
//   buffer - buffer to store received frame
//   buffer_size - size of buffer
// Returns: number of bytes received, 0 if no data, -1 on error
int host_net_recv(uint8_t *buffer, uint32_t buffer_size)
{
	if (!net_initialized || !net_handle) {
		return -1;
	}

	struct pcap_pkthdr *header;
	const uint8_t *packet;

	// Non-blocking read
	int result = pcap_next_ex(net_handle, &header, &packet);

	if (result == 1) {
		// Packet received
		uint32_t len = header->caplen;

		if (len > buffer_size) {
			fprintf(stderr, "Received packet too large: %u > %u\n", len, buffer_size);
			return -1;
		}

		memcpy(buffer, packet, len);

		net_rx_packets++;
		net_rx_bytes += len;

		return len;
	} else if (result == 0) {
		// Timeout (no packet available)
		return 0;
	} else if (result == -1) {
		// Error
		fprintf(stderr, "Error receiving packet: %s\n", pcap_geterr(net_handle));
		return -1;
	} else if (result == -2) {
		// EOF (shouldn't happen in live capture)
		return -1;
	}

	return 0;
}

// Poll for received frames
// Returns: 1 if data available, 0 if not, -1 on error
int host_net_poll()
{
	if (!net_initialized || !net_handle) {
		return -1;
	}

#ifdef _WIN32
	// Windows doesn't support pcap_get_selectable_fd
	// Always return 1 to allow recv to check
	return 1;
#else
	// Get file descriptor for select()
	int fd = pcap_get_selectable_fd(net_handle);
	if (fd == -1) {
		// Not supported on this platform
		return 1;  // Just let recv check
	}

	fd_set readfds;
	struct timeval tv;

	FD_ZERO(&readfds);
	FD_SET(fd, &readfds);

	tv.tv_sec = 0;
	tv.tv_usec = 0;

	int ret = select(fd + 1, &readfds, nullptr, nullptr, &tv);
	if (ret < 0) {
		return -1;
	}

	return (ret > 0 && FD_ISSET(fd, &readfds)) ? 1 : 0;
#endif
}

// Get interface information
// Parameters:
//   info - pointer to buffer (22 bytes minimum)
// Layout: 6 bytes MAC, 4 bytes IP, 4 bytes netmask, 4 bytes gateway, 4 bytes MTU
// Returns: 0 on success, -1 on error
int host_net_get_info(void *info)
{
	if (!info) {
		return -1;
	}

	uint8_t *ptr = (uint8_t *)info;

	// MAC address (6 bytes)
	memcpy(ptr, net_mac, 6);
	ptr += 6;

	// IP address (4 bytes)
	memcpy(ptr, net_ip, 4);
	ptr += 4;

	// Netmask (4 bytes)
	memcpy(ptr, net_netmask, 4);
	ptr += 4;

	// Gateway (4 bytes)
	memcpy(ptr, net_gateway, 4);
	ptr += 4;

	// MTU (4 bytes)
	memcpy(ptr, &net_mtu, 4);

	return 0;
}

// Get interface statistics
// Parameters:
//   stats - pointer to buffer (32 bytes minimum)
// Layout: 8 bytes rx_packets, 8 bytes rx_bytes, 8 bytes tx_packets, 8 bytes tx_bytes
// Returns: 0 on success, -1 on error
int host_net_get_stats(void *stats)
{
	if (!stats) {
		return -1;
	}

	uint64_t *ptr = (uint64_t *)stats;

	ptr[0] = net_rx_packets;
	ptr[1] = net_rx_bytes;
	ptr[2] = net_tx_packets;
	ptr[3] = net_tx_bytes;

	return 0;
}

// Host network function table (must match ChrysaLisp expectations)
void (*host_net_funcs[]) = {
	(void*)host_net_init,
	(void*)host_net_deinit,
	(void*)host_net_send,
	(void*)host_net_recv,
	(void*)host_net_poll,
	(void*)host_net_get_info,
	(void*)host_net_get_stats,
};

#endif  // _HOST_NET == 0
#endif  // defined(_HOST_NET)
