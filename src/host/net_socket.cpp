// Host Network Driver - Standard Socket implementation
// Portable TCP/IP socket interface for Windows/Mac/Linux
// This provides IP-level access (not raw Ethernet) for testing
// For production, use net_pcap.cpp with libpcap for raw Ethernet access

#if defined(_HOST_NET)
#if _HOST_NET == 1

#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef _WIN32
	#include <winsock2.h>
	#include <ws2tcpip.h>
	#pragma comment(lib, "ws2_32.lib")
	typedef int socklen_t;
	#define close closesocket
#else
	#include <unistd.h>
	#include <sys/socket.h>
	#include <sys/types.h>
	#include <sys/ioctl.h>
	#include <netinet/in.h>
	#include <netinet/tcp.h>
	#include <arpa/inet.h>
	#include <net/if.h>
	#include <fcntl.h>
	#include <errno.h>
	#ifdef __linux__
		#include <linux/if_packet.h>
	#endif
#endif

// Network state
static int net_initialized = 0;
static int raw_socket = -1;
static char net_interface[256] = "lo";
static uint8_t net_mac[6] = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
static uint8_t net_ip[4] = {127, 0, 0, 1};
static uint8_t net_netmask[4] = {255, 0, 0, 0};
static uint8_t net_gateway[4] = {127, 0, 0, 1};
static uint32_t net_mtu = 65536;

// Statistics
static uint64_t net_rx_packets = 0;
static uint64_t net_rx_bytes = 0;
static uint64_t net_tx_packets = 0;
static uint64_t net_tx_bytes = 0;

//////////////////////////////////////////////
// Network Interface Functions
//////////////////////////////////////////////

// Initialize network interface
// For socket-based implementation, this is simplified
// Parameters same as net_pcap.cpp
// Returns: 0 on success, -1 on error
int host_net_init(const char *iface, uint8_t *mac, uint8_t *ip,
                  uint8_t *netmask, uint8_t *gateway, uint32_t mtu)
{
	if (net_initialized) {
		return 0;
	}

#ifdef _WIN32
	// Initialize Winsock
	WSADATA wsaData;
	if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) {
		fprintf(stderr, "WSAStartup failed\n");
		return -1;
	}
#endif

	// Copy configuration
	if (iface) {
		strncpy(net_interface, iface, sizeof(net_interface) - 1);
		net_interface[sizeof(net_interface) - 1] = '\0';
	}
	if (mac) memcpy(net_mac, mac, 6);
	if (ip) memcpy(net_ip, ip, 4);
	if (netmask) memcpy(net_netmask, netmask, 4);
	if (gateway) memcpy(net_gateway, gateway, 4);
	net_mtu = mtu ? mtu : 65536;

	// Create raw IP socket (for testing - limited functionality)
	// Note: This provides IP-level access, not Ethernet-level
	// For full TCP/IP stack, use net_pcap.cpp with libpcap
	raw_socket = socket(AF_INET, SOCK_RAW, IPPROTO_RAW);
	if (raw_socket < 0) {
		fprintf(stderr, "Failed to create raw socket (need root/admin): %s\n",
		        strerror(errno));
		// Fall back to regular UDP socket for testing
		raw_socket = socket(AF_INET, SOCK_DGRAM, 0);
		if (raw_socket < 0) {
			fprintf(stderr, "Failed to create socket: %s\n", strerror(errno));
#ifdef _WIN32
			WSACleanup();
#endif
			return -1;
		}
	}

	// Set non-blocking
#ifdef _WIN32
	u_long mode = 1;
	ioctlsocket(raw_socket, FIONBIO, &mode);
#else
	int flags = fcntl(raw_socket, F_GETFL, 0);
	fcntl(raw_socket, F_SETFL, flags | O_NONBLOCK);
#endif

	// Enable IP header include (if raw socket)
#ifdef IP_HDRINCL
	int one = 1;
	if (setsockopt(raw_socket, IPPROTO_IP, IP_HDRINCL, (char*)&one, sizeof(one)) < 0) {
		// Not critical
	}
#endif

	net_initialized = 1;
	fprintf(stderr, "Network initialized (socket mode) on %s\n", net_interface);
	fprintf(stderr, "Note: Socket mode provides limited functionality.\n");
	fprintf(stderr, "      For full Ethernet access, use net_pcap.cpp with libpcap.\n");

	return 0;
}

// Deinitialize network interface
// Returns: 0 on success
int host_net_deinit()
{
	if (!net_initialized) {
		return 0;
	}

	if (raw_socket >= 0) {
		close(raw_socket);
		raw_socket = -1;
	}

#ifdef _WIN32
	WSACleanup();
#endif

	net_initialized = 0;
	fprintf(stderr, "Network deinitialized\n");
	return 0;
}

// Send packet (limited - sends IP-level, not Ethernet)
// Parameters:
//   data - packet data (expects Ethernet frame, extracts IP portion)
//   length - packet length
// Returns: number of bytes sent, or -1 on error
int host_net_send(uint8_t *data, uint32_t length)
{
	if (!net_initialized || raw_socket < 0) {
		return -1;
	}

	// Socket mode limitation: Skip Ethernet header (14 bytes)
	// In real implementation with libpcap, we'd send the full frame
	if (length < 14) {
		fprintf(stderr, "Packet too small: %u\n", length);
		return -1;
	}

	// Extract destination IP from IP header (offset 14+16 = 30)
	if (length < 34) {
		// Not enough for IP header
		return -1;
	}

	struct sockaddr_in dest_addr;
	memset(&dest_addr, 0, sizeof(dest_addr));
	dest_addr.sin_family = AF_INET;
	memcpy(&dest_addr.sin_addr.s_addr, data + 30, 4);  // dst IP from IP header

	// Send IP packet (skip Ethernet header)
	ssize_t sent = sendto(raw_socket, (char*)(data + 14), length - 14, 0,
	                      (struct sockaddr*)&dest_addr, sizeof(dest_addr));

	if (sent < 0) {
#ifdef _WIN32
		if (WSAGetLastError() != WSAEWOULDBLOCK)
#else
		if (errno != EAGAIN && errno != EWOULDBLOCK)
#endif
		{
			fprintf(stderr, "Send error: %s\n", strerror(errno));
		}
		return -1;
	}

	net_tx_packets++;
	net_tx_bytes += sent;

	return sent + 14;  // Add back Ethernet header size for consistency
}

// Receive packet (limited - receives IP-level, not Ethernet)
// Parameters:
//   buffer - buffer to store packet (will prepend dummy Ethernet header)
//   buffer_size - size of buffer
// Returns: number of bytes received, 0 if no data, -1 on error
int host_net_recv(uint8_t *buffer, uint32_t buffer_size)
{
	if (!net_initialized || raw_socket < 0) {
		return -1;
	}

	if (buffer_size < 14) {
		return -1;  // Need space for dummy Ethernet header
	}

	struct sockaddr_in src_addr;
	socklen_t addr_len = sizeof(src_addr);

	// Receive into buffer after Ethernet header space
	ssize_t received = recvfrom(raw_socket, (char*)(buffer + 14),
	                            buffer_size - 14, 0,
	                            (struct sockaddr*)&src_addr, &addr_len);

	if (received < 0) {
#ifdef _WIN32
		if (WSAGetLastError() == WSAEWOULDBLOCK)
#else
		if (errno == EAGAIN || errno == EWOULDBLOCK)
#endif
		{
			return 0;  // No data
		}
		return -1;
	}

	if (received == 0) {
		return 0;
	}

	// Prepend dummy Ethernet header (for compatibility)
	// Dst MAC (our MAC)
	memcpy(buffer, net_mac, 6);
	// Src MAC (dummy)
	memset(buffer + 6, 0, 6);
	// EtherType (0x0800 = IPv4)
	buffer[12] = 0x08;
	buffer[13] = 0x00;

	net_rx_packets++;
	net_rx_bytes += received;

	return received + 14;
}

// Poll for received packets
// Returns: 1 if data available, 0 if not, -1 on error
int host_net_poll()
{
	if (!net_initialized || raw_socket < 0) {
		return -1;
	}

	fd_set readfds;
	struct timeval tv;

	FD_ZERO(&readfds);
	FD_SET(raw_socket, &readfds);

	tv.tv_sec = 0;
	tv.tv_usec = 0;

	int ret = select(raw_socket + 1, &readfds, nullptr, nullptr, &tv);
	if (ret < 0) {
		return -1;
	}

	return (ret > 0 && FD_ISSET(raw_socket, &readfds)) ? 1 : 0;
}

// Get interface information
// Same as net_pcap.cpp
int host_net_get_info(void *info)
{
	if (!info) {
		return -1;
	}

	uint8_t *ptr = (uint8_t *)info;

	memcpy(ptr, net_mac, 6);      ptr += 6;
	memcpy(ptr, net_ip, 4);       ptr += 4;
	memcpy(ptr, net_netmask, 4);  ptr += 4;
	memcpy(ptr, net_gateway, 4);  ptr += 4;
	memcpy(ptr, &net_mtu, 4);

	return 0;
}

// Get interface statistics
// Same as net_pcap.cpp
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

// Host network function table
void (*host_net_funcs[]) = {
	(void*)host_net_init,
	(void*)host_net_deinit,
	(void*)host_net_send,
	(void*)host_net_recv,
	(void*)host_net_poll,
	(void*)host_net_get_info,
	(void*)host_net_get_stats,
};

#endif  // _HOST_NET == 1
#endif  // defined(_HOST_NET)
