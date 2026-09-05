#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>

#ifdef _WIN64
	#include <winsock2.h>
	#include <ws2tcpip.h>
	typedef SOCKET socket_t;
	#define SOCK_INVALID INVALID_SOCKET
	#define SOCK_ERR SOCKET_ERROR
#else
	#include <sys/types.h>
	#include <sys/socket.h>
	#include <netinet/in.h>
	#include <netinet/tcp.h>
	#include <arpa/inet.h>
	#include <netdb.h>
	#include <unistd.h>
	#include <fcntl.h>
	#include <poll.h>
	#include <signal.h>
	#include <errno.h>
	typedef int socket_t;
	#define SOCK_INVALID (-1)
	#define SOCK_ERR (-1)
#endif

#define MAX_NET_HANDLES 256

static struct addrinfo hints;
static char port_str[16];
static struct sockaddr_in net_addr;
static struct sockaddr_in client_addr;
#ifdef _WIN64
static WSAPOLLFD pfd;
#else
static struct pollfd pfd;
#endif

enum SockState {
	SOCK_STATE_FREE = 0,
	SOCK_STATE_CONNECTING,
	SOCK_STATE_CONNECTED,
	SOCK_STATE_LISTENING
};

struct NetSlot {
	socket_t fd;
	int state;
};

static NetSlot slots[MAX_NET_HANDLES];

static void set_nonblocking(socket_t fd) {
#ifdef _WIN64
	u_long mode = 1;
	ioctlsocket(fd, FIONBIO, &mode);
#else
	int flags = fcntl(fd, F_GETFL, 0);
	fcntl(fd, F_SETFL, flags | O_NONBLOCK);
#endif
}

static void close_fd(socket_t fd) {
#ifdef _WIN64
	closesocket(fd);
#else
	close(fd);
#endif
}

static uint32_t alloc_slot(socket_t fd, int state) {
	for (uint32_t i = 1; i < MAX_NET_HANDLES; ++i) {
		if (slots[i].state == SOCK_STATE_FREE) {
			slots[i].fd = fd;
			slots[i].state = state;
			return i;
		}
	}
	close_fd(fd);
	return 0;
}

int64_t host_net_init() {
#ifdef _WIN64
	WSADATA wsaData;
	if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) return -1;
#else
	signal(SIGPIPE, SIG_IGN);
#endif
	for (int i = 0; i < MAX_NET_HANDLES; ++i) {
		slots[i].fd = SOCK_INVALID;
		slots[i].state = SOCK_STATE_FREE;
	}
	return 0;
}

int64_t host_net_deinit() {
	for (int i = 1; i < MAX_NET_HANDLES; ++i) {
		if (slots[i].state != SOCK_STATE_FREE) {
			close_fd(slots[i].fd);
			slots[i].state = SOCK_STATE_FREE;
			slots[i].fd = SOCK_INVALID;
		}
	}
#ifdef _WIN64
	WSACleanup();
#endif
	return 0;
}

uint32_t host_net_connect(const char *host, uint32_t port) {
	snprintf(port_str, sizeof(port_str), "%u", port);
	struct addrinfo *res = nullptr;

	memset(&hints, 0, sizeof(hints));
	hints.ai_family = AF_INET;
	hints.ai_socktype = SOCK_STREAM;

	if (getaddrinfo(host, port_str, &hints, &res) != 0 || !res) return 0;

	socket_t fd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
	if (fd == SOCK_INVALID) {
		freeaddrinfo(res);
		return 0;
	}

	set_nonblocking(fd);

	int ret = connect(fd, res->ai_addr, (int)res->ai_addrlen);
	freeaddrinfo(res);

#ifdef _WIN64
	if (ret == SOCK_ERR && WSAGetLastError() != WSAEWOULDBLOCK) {
		close_fd(fd);
		return 0;
	}
#else
	if (ret == SOCK_ERR && errno != EINPROGRESS) {
		close_fd(fd);
		return 0;
	}
#endif

	int state = (ret == 0) ? SOCK_STATE_CONNECTED : SOCK_STATE_CONNECTING;
	uint32_t h = alloc_slot(fd, state);
	return h;
}

uint32_t host_net_listen(uint32_t port) {
	socket_t fd = socket(AF_INET, SOCK_STREAM, 0);
	if (fd == SOCK_INVALID) return 0;

	int opt = 1;
#ifdef _WIN64
	setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (const char *)&opt, sizeof(opt));
#else
	setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
#endif

	set_nonblocking(fd);

	struct sockaddr_in addr;
	memset(&addr, 0, sizeof(addr));
	addr.sin_family = AF_INET;
	addr.sin_addr.s_addr = INADDR_ANY;
	addr.sin_port = htons((uint16_t)port);

	if (bind(fd, (struct sockaddr *)&addr, sizeof(addr)) == SOCK_ERR) {
		close_fd(fd);
		return 0;
	}

	if (listen(fd, 32) == SOCK_ERR) {
		close_fd(fd);
		return 0;
	}

	return alloc_slot(fd, SOCK_STATE_LISTENING);
}

uint32_t host_net_accept(uint32_t handle) {
	if (handle == 0 || handle >= MAX_NET_HANDLES || slots[handle].state != SOCK_STATE_LISTENING) {
		return 0;
	}

	struct sockaddr_in client_addr;
#ifdef _WIN64
	int addr_len = sizeof(client_addr);
#else
	socklen_t addr_len = sizeof(client_addr);
#endif
	socket_t client_fd = accept(slots[handle].fd, (struct sockaddr *)&client_addr, &addr_len);
	if (client_fd == SOCK_INVALID) return 0;

	set_nonblocking(client_fd);
	return alloc_slot(client_fd, SOCK_STATE_CONNECTED);
}

int64_t host_net_send(uint32_t handle, const void *buf, size_t len) {
	if (handle == 0 || handle >= MAX_NET_HANDLES || slots[handle].state != SOCK_STATE_CONNECTED) {
		return -1;
	}

	int ret = send(slots[handle].fd, (const char *)buf, (int)len, 0);
	if (ret >= 0) return ret;

#ifdef _WIN64
	if (WSAGetLastError() == WSAEWOULDBLOCK) return 0;
#else
	if (errno == EAGAIN || errno == EWOULDBLOCK) return 0;
#endif
	return -1;
}

int64_t host_net_recv(uint32_t handle, void *buf, size_t max_len) {
	if (handle == 0 || handle >= MAX_NET_HANDLES || slots[handle].state != SOCK_STATE_CONNECTED) {
		return -1;
	}

	int ret = recv(slots[handle].fd, (char *)buf, (int)max_len, 0);
	if (ret > 0) return ret;
	if (ret == 0) return -1; // Graceful EOF / shutdown

#ifdef _WIN64
	if (WSAGetLastError() == WSAEWOULDBLOCK) return 0;
#else
	if (errno == EAGAIN || errno == EWOULDBLOCK) return 0;
#endif
	return -1;
}

int64_t host_net_close(uint32_t handle) {
	if (handle == 0 || handle >= MAX_NET_HANDLES || slots[handle].state == SOCK_STATE_FREE) {
		return -1;
	}
	close_fd(slots[handle].fd);
	slots[handle].fd = SOCK_INVALID;
	slots[handle].state = SOCK_STATE_FREE;
	return 0;
}

int64_t host_net_poll(uint32_t handle) {
	if (handle == 0 || handle >= MAX_NET_HANDLES || slots[handle].state == SOCK_STATE_FREE) {
		return 4; // Error / disconnected
	}

	short events = POLLIN | POLLOUT;
#ifdef _WIN64
	pfd.fd = slots[handle].fd;
	pfd.events = (SHORT)events;
	pfd.revents = 0;
	int ret = WSAPoll(&pfd, 1, 0);
#else
	pfd.fd = slots[handle].fd;
	pfd.events = events;
	pfd.revents = 0;
	int ret = poll(&pfd, 1, 0);
#endif
	if (ret <= 0) return 0;

	// Check for socket-level errors via getsockopt
	int so_err = 0;
#ifdef _WIN64
	int err_len = sizeof(so_err);
	getsockopt(slots[handle].fd, SOL_SOCKET, SO_ERROR, (char *)&so_err, &err_len);
#else
	socklen_t err_len = sizeof(so_err);
	getsockopt(slots[handle].fd, SOL_SOCKET, SO_ERROR, &so_err, &err_len);
#endif

	if (so_err != 0) {
		return 4;
	}

	int64_t flags = 0;
	if (pfd.revents & (POLLERR | POLLHUP | POLLNVAL)) flags |= 4;
	if (pfd.revents & POLLIN) flags |= 1;
	if (pfd.revents & POLLOUT) {
		if (slots[handle].state == SOCK_STATE_CONNECTING) {
			slots[handle].state = SOCK_STATE_CONNECTED;
		}
		flags |= 2;
	}
	return flags;
}

void (*host_net_funcs[]) = {
	(void*)host_net_init,
	(void*)host_net_deinit,
	(void*)host_net_connect,
	(void*)host_net_listen,
	(void*)host_net_accept,
	(void*)host_net_send,
	(void*)host_net_recv,
	(void*)host_net_close,
	(void*)host_net_poll
};