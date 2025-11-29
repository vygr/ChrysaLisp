// Host Network Driver API - Raw Socket Implementation
// Provides Ethernet frame send/receive via raw sockets or TUN/TAP

#if defined(_HOST_NET)
#if _HOST_NET == 0

#include <stdint.h>
#include <string.h>
#include <stdio.h>

#ifdef __linux__
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <netinet/if_ether.h>
#include <linux/if_packet.h>
#include <net/if.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#elif defined(__APPLE__) || defined(__FreeBSD__)
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <net/if_dl.h>
#include <net/bpf.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <ifaddrs.h>
#include <unistd.h>
#include <fcntl.h>
#elif defined(_WIN32)
// Windows would use WinPcap or Npcap
#include <winsock2.h>
#include <ws2tcpip.h>
#include <iphlpapi.h>
#pragma comment(lib, "iphlpapi.lib")
#pragma comment(lib, "ws2_32.lib")
#endif

// Network interface state
static int net_socket = -1;
static char net_interface[32] = "eth0";
static uint8_t net_mac[6] = {0};
static uint8_t net_ip[4] = {0};
static uint8_t net_netmask[4] = {255, 255, 255, 0};
static uint8_t net_gateway[4] = {0};
static uint32_t net_mtu = 1500;
static int net_initialized = 0;

// Statistics
static uint64_t net_rx_packets = 0;
static uint64_t net_rx_bytes = 0;
static uint64_t net_tx_packets = 0;
static uint64_t net_tx_bytes = 0;

// Receive buffer
#define RX_BUFFER_SIZE 65536
static uint8_t rx_buffer[RX_BUFFER_SIZE];
static int rx_length = 0;

// Initialize network interface
// Parameters: interface name, MAC address (6 bytes), IP address (4 bytes),
//             netmask (4 bytes), gateway (4 bytes), MTU
// Returns: 0 on success, -1 on error
int host_net_init(const char *iface, uint8_t *mac, uint8_t *ip,
                  uint8_t *netmask, uint8_t *gateway, uint32_t mtu)
{
    if (net_initialized) {
        return 0;  // Already initialized
    }

    // Copy interface name
    strncpy(net_interface, iface, sizeof(net_interface) - 1);
    net_interface[sizeof(net_interface) - 1] = '\0';

    // Copy configuration
    if (mac) memcpy(net_mac, mac, 6);
    if (ip) memcpy(net_ip, ip, 4);
    if (netmask) memcpy(net_netmask, netmask, 4);
    if (gateway) memcpy(net_gateway, gateway, 4);
    net_mtu = mtu;

#ifdef __linux__
    // Create raw socket
    net_socket = socket(AF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
    if (net_socket < 0) {
        perror("socket");
        return -1;
    }

    // Get interface index
    struct ifreq ifr;
    memset(&ifr, 0, sizeof(ifr));
    strncpy(ifr.ifr_name, net_interface, IFNAMSIZ - 1);

    if (ioctl(net_socket, SIOCGIFINDEX, &ifr) < 0) {
        perror("ioctl SIOCGIFINDEX");
        close(net_socket);
        net_socket = -1;
        return -1;
    }

    int if_index = ifr.ifr_ifindex;

    // Get MAC address if not provided
    if (!mac || (mac[0] == 0 && mac[1] == 0 && mac[2] == 0)) {
        if (ioctl(net_socket, SIOCGIFHWADDR, &ifr) >= 0) {
            memcpy(net_mac, ifr.ifr_hwaddr.sa_data, 6);
        }
    }

    // Get IP address if not provided
    if (!ip || (ip[0] == 0 && ip[1] == 0)) {
        if (ioctl(net_socket, SIOCGIFADDR, &ifr) >= 0) {
            struct sockaddr_in *addr = (struct sockaddr_in *)&ifr.ifr_addr;
            memcpy(net_ip, &addr->sin_addr.s_addr, 4);
        }
    }

    // Bind to interface
    struct sockaddr_ll sll;
    memset(&sll, 0, sizeof(sll));
    sll.sll_family = AF_PACKET;
    sll.sll_protocol = htons(ETH_P_ALL);
    sll.sll_ifindex = if_index;

    if (bind(net_socket, (struct sockaddr *)&sll, sizeof(sll)) < 0) {
        perror("bind");
        close(net_socket);
        net_socket = -1;
        return -1;
    }

    // Set non-blocking
    int flags = fcntl(net_socket, F_GETFL, 0);
    fcntl(net_socket, F_SETFL, flags | O_NONBLOCK);

#elif defined(__APPLE__) || defined(__FreeBSD__)
    // On macOS/BSD, use BPF (Berkeley Packet Filter)
    // This is a simplified stub - full implementation would open /dev/bpf*
    fprintf(stderr, "BPF network driver not yet implemented for macOS/BSD\n");
    return -1;

#elif defined(_WIN32)
    // On Windows, use WinPcap/Npcap or raw sockets
    // This is a stub
    fprintf(stderr, "Network driver not yet implemented for Windows\n");
    return -1;

#else
    fprintf(stderr, "Network driver not supported on this platform\n");
    return -1;
#endif

    net_initialized = 1;
    return 0;
}

// Deinitialize network interface
// Returns: 0 on success, -1 on error
int host_net_deinit()
{
    if (!net_initialized) {
        return 0;
    }

    if (net_socket >= 0) {
        close(net_socket);
        net_socket = -1;
    }

    net_initialized = 0;
    return 0;
}

// Send Ethernet frame
// Parameters: frame data, frame length
// Returns: number of bytes sent, or -1 on error
int host_net_send(uint8_t *data, uint32_t length)
{
    if (!net_initialized || net_socket < 0) {
        return -1;
    }

    if (length > net_mtu + 14) {  // 14 = Ethernet header
        fprintf(stderr, "Packet too large: %u > %u\n", length, net_mtu + 14);
        return -1;
    }

#ifdef __linux__
    ssize_t sent = send(net_socket, data, length, 0);
    if (sent < 0) {
        if (errno != EAGAIN && errno != EWOULDBLOCK) {
            perror("send");
        }
        return -1;
    }

    net_tx_packets++;
    net_tx_bytes += sent;
    return sent;

#else
    return -1;
#endif
}

// Receive Ethernet frame (non-blocking)
// Parameters: buffer to store frame, buffer size
// Returns: number of bytes received, 0 if no data, -1 on error
int host_net_recv(uint8_t *buffer, uint32_t buffer_size)
{
    if (!net_initialized || net_socket < 0) {
        return -1;
    }

#ifdef __linux__
    ssize_t received = recv(net_socket, buffer, buffer_size, 0);
    if (received < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            return 0;  // No data available
        }
        perror("recv");
        return -1;
    }

    if (received > 0) {
        net_rx_packets++;
        net_rx_bytes += received;
    }

    return received;

#else
    return 0;
#endif
}

// Poll for received frames (checks if data available)
// Returns: 1 if data available, 0 if not, -1 on error
int host_net_poll()
{
    if (!net_initialized || net_socket < 0) {
        return -1;
    }

#ifdef __linux__
    fd_set readfds;
    struct timeval tv;

    FD_ZERO(&readfds);
    FD_SET(net_socket, &readfds);

    // Immediate return (timeout = 0)
    tv.tv_sec = 0;
    tv.tv_usec = 0;

    int ret = select(net_socket + 1, &readfds, NULL, NULL, &tv);
    if (ret < 0) {
        if (errno != EINTR) {
            perror("select");
        }
        return -1;
    }

    return (ret > 0 && FD_ISSET(net_socket, &readfds)) ? 1 : 0;

#else
    return 0;
#endif
}

// Get interface information
// Parameters: pointer to structure to fill (MAC, IP, netmask, gateway, MTU)
// Structure layout: 6 bytes MAC, 4 bytes IP, 4 bytes netmask, 4 bytes gateway, 4 bytes MTU
// Returns: 0 on success, -1 on error
int host_net_get_info(void *info)
{
    if (!info) {
        return -1;
    }

    uint8_t *ptr = (uint8_t *)info;

    // Copy MAC (6 bytes)
    memcpy(ptr, net_mac, 6);
    ptr += 6;

    // Copy IP (4 bytes)
    memcpy(ptr, net_ip, 4);
    ptr += 4;

    // Copy netmask (4 bytes)
    memcpy(ptr, net_netmask, 4);
    ptr += 4;

    // Copy gateway (4 bytes)
    memcpy(ptr, net_gateway, 4);
    ptr += 4;

    // Copy MTU (4 bytes)
    uint32_t mtu = net_mtu;
    memcpy(ptr, &mtu, 4);

    return 0;
}

// Get interface statistics
// Parameters: pointer to structure to fill (rx_packets, rx_bytes, tx_packets, tx_bytes)
// Structure layout: 8 bytes rx_packets, 8 bytes rx_bytes, 8 bytes tx_packets, 8 bytes tx_bytes
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

#endif  // _HOST_NET == 0
#endif  // defined(_HOST_NET)
