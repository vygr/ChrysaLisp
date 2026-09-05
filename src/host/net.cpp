#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>

#ifdef _WIN64
	#include <winsock2.h>
	#include <ws2tcpip.h>
	#include <windows.h>
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
	#include <pthread.h>
	typedef int socket_t;
	#define SOCK_INVALID (-1)
	#define SOCK_ERR (-1)
#endif

#define MAX_NET_HANDLES 256

enum SockState {
	SOCK_STATE_FREE = 0,
	SOCK_STATE_RESOLVING,
	SOCK_STATE_CONNECTING,
	SOCK_STATE_CONNECTED,
	SOCK_STATE_LISTENING,
	SOCK_STATE_FAILED
};

struct NetSlot {
	socket_t fd;
	volatile int state;
	char host[256];
	uint32_t port;
	volatile int cancelled;
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
			slots[i].cancelled = 0;
			return i;
		}
	}
	if (fd != SOCK_INVALID) close_fd(fd);
	return 0;
}

static void resolve_and_connect_worker(uint32_t handle) {
	NetSlot *slot = &slots[handle];

	char p_str[16];
	snprintf(p_str, sizeof(p_str), "%u", slot->port);
	struct addrinfo h_hints, *res = nullptr;

	memset(&h_hints, 0, sizeof(h_hints));
	h_hints.ai_family = AF_INET;
	h_hints.ai_socktype = SOCK_STREAM;

	int gai_err = getaddrinfo(slot->host, p_str, &h_hints, &res);

	if (slot->cancelled) {
		if (res) freeaddrinfo(res);
		slot->state = SOCK_STATE_FREE;
		return;
	}

	if (gai_err != 0 || !res) {
		slot->state = SOCK_STATE_FAILED;
		return;
	}

	socket_t fd = SOCK_INVALID;
	int ret = SOCK_ERR;

	for (struct addrinfo *p = res; p != nullptr; p = p->ai_next) {
		fd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
		if (fd == SOCK_INVALID) continue;

		set_nonblocking(fd);
		ret = connect(fd, p->ai_addr, (int)p->ai_addrlen);

#ifdef _WIN64
		if (ret == 0 || WSAGetLastError() == WSAEWOULDBLOCK) {
			break;
		}
#else
		if (ret == 0 || errno == EINPROGRESS) {
			break;
		}
#endif
		close_fd(fd);
		fd = SOCK_INVALID;
	}
	freeaddrinfo(res);

	if (slot->cancelled) {
		if (fd != SOCK_INVALID) close_fd(fd);
		slot->state = SOCK_STATE_FREE;
		return;
	}

	if (fd == SOCK_INVALID) {
		slot->state = SOCK_STATE_FAILED;
		return;
	}

	slot->fd = fd;
	__sync_synchronize();
	slot->state = (ret == 0) ? SOCK_STATE_CONNECTED : SOCK_STATE_CONNECTING;
}

#ifdef _WIN64
static DWORD WINAPI resolve_worker_win(LPVOID arg) {
	resolve_and_connect_worker((uint32_t)(uintptr_t)arg);
	return 0;
}

static void start_resolve_thread(uint32_t handle) {
	HANDLE th = CreateThread(nullptr, 0, resolve_worker_win, (LPVOID)(uintptr_t)handle, 0, nullptr);
	if (th) {
		CloseHandle(th);
	} else {
		slots[handle].state = SOCK_STATE_FAILED;
	}
}
#else
static void* resolve_worker_posix(void* arg) {
	resolve_and_connect_worker((uint32_t)(uintptr_t)arg);
	return nullptr;
}

static void start_resolve_thread(uint32_t handle) {
	pthread_t th;
	pthread_attr_t attr;
	pthread_attr_init(&attr);
	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
	if (pthread_create(&th, &attr, resolve_worker_posix, (void*)(uintptr_t)handle) != 0) {
		slots[handle].state = SOCK_STATE_FAILED;
	}
	pthread_attr_destroy(&attr);
}
#endif

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
		slots[i].cancelled = 0;
	}
	return 0;
}

int64_t host_net_deinit() {
	for (int i = 1; i < MAX_NET_HANDLES; ++i) {
		if (slots[i].state != SOCK_STATE_FREE) {
			if (slots[i].state == SOCK_STATE_RESOLVING) {
				slots[i].cancelled = 1;
			} else {
				close_fd(slots[i].fd);
				slots[i].state = SOCK_STATE_FREE;
				slots[i].fd = SOCK_INVALID;
			}
		}
	}
#ifdef _WIN64
	WSACleanup();
#endif
	return 0;
}

uint32_t host_net_connect(const char *host, uint32_t port) {
	uint32_t h = alloc_slot(SOCK_INVALID, SOCK_STATE_RESOLVING);
	if (h == 0) return 0;

	slots[h].port = port;
	slots[h].cancelled = 0;
	strncpy(slots[h].host, host ? host : "", sizeof(slots[h].host) - 1);
	slots[h].host[sizeof(slots[h].host) - 1] = '\0';

	start_resolve_thread(h);
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
	if (slots[handle].state == SOCK_STATE_RESOLVING) {
		slots[handle].cancelled = 1;
		return 0;
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

	if (slots[handle].state == SOCK_STATE_RESOLVING) {
		return 0; // Still resolving in background
	}

	if (slots[handle].state == SOCK_STATE_FAILED) {
		return 4; // Resolution failed or connect error
	}

#ifdef _WIN64
	WSAPOLLFD local_pfd;
	local_pfd.fd = slots[handle].fd;
	local_pfd.events = POLLIN | POLLOUT;
	local_pfd.revents = 0;
	int ret = WSAPoll(&local_pfd, 1, 0);
#else
	struct pollfd local_pfd;
	local_pfd.fd = slots[handle].fd;
	local_pfd.events = POLLIN | POLLOUT;
	local_pfd.revents = 0;
	int ret = poll(&local_pfd, 1, 0);
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
	if (local_pfd.revents & (POLLERR | POLLHUP | POLLNVAL)) flags |= 4;
	if (local_pfd.revents & POLLIN) flags |= 1;
	if (local_pfd.revents & POLLOUT) {
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