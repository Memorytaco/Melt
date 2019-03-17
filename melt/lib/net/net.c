
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <errno.h>
#include <unistd.h>

#if __cplusplus
#  define EXPORT extern "C"
#else
#  define EXPORT extern
#endif

struct fd_result {
	int fd;
	int errval;
	char *(*strerror)(int);
};

EXPORT struct fd_result *c_dial(int proto, const char *host, const char *serv, struct fd_result *fdr);
EXPORT struct fd_result *c_listen(int proto, const char *host, const char *serv, struct fd_result *fdr);
EXPORT struct fd_result *c_accept(int fd, struct fd_result *fdr);

const int ipproto_tcp = IPPROTO_TCP;
const int ipproto_udp = IPPROTO_UDP;

struct fd_result *c_dial(int proto, const char *host, const char *serv, struct fd_result *fdr) {
	struct addrinfo hints = {
		.ai_family = AF_UNSPEC,
		.ai_flags = AI_PASSIVE,
		.ai_protocol = proto
	};
	struct addrinfo *addr = 0;

	if (proto == IPPROTO_TCP) hints.ai_socktype = SOCK_STREAM;
	else if (proto == IPPROTO_UDP) hints.ai_socktype = SOCK_DGRAM;
	else {
		fdr->fd = -1;
		fdr->errval = ENOTSUP;
		fdr->strerror = strerror;
		goto out;
	}

	fdr->errval = getaddrinfo(host, serv, &hints, &addr);
	if (fdr->errval) {
		fdr->fd = -1;
		fdr->strerror = gai_strerror;
		goto out;
	}

	fdr->fd = socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol);
	if (fdr->fd < 0) {
		fdr->errval = errno;
		fdr->strerror = strerror;
		goto out;
	}
	
	if (connect(fdr->fd, addr->ai_addr, addr->ai_addrlen) < 0) {
		fdr->errval = errno;
		fdr->strerror = strerror;
		close(fdr->fd);
		fdr->fd = -1;
	}
out:
	if (addr) freeaddrinfo(addr);
	return fdr;
}

struct fd_result *c_listen(int proto, const char *host, const char *serv, struct fd_result *fdr) {
	struct addrinfo hints = {
		.ai_family = AF_UNSPEC,
		.ai_flags = AI_PASSIVE,
		.ai_protocol = proto
	};
	struct addrinfo *addr = 0;

	if (proto == IPPROTO_TCP) hints.ai_socktype = SOCK_STREAM;
	else if (proto == IPPROTO_UDP) hints.ai_socktype = SOCK_DGRAM;
	else {
		fdr->fd = -1;
		fdr->errval = ENOTSUP;
		fdr->strerror = strerror;
		goto out;
	}

	fdr->errval = getaddrinfo(host, serv, &hints, &addr);
	if (fdr->errval) {
		fdr->fd = -1;
		fdr->strerror = gai_strerror;
		goto out;
	}

	fdr->fd = socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol);
	if (fdr->fd < 0) {
		fdr->errval = errno;
		fdr->strerror = strerror;
		goto out;
	}

	fdr->errval = bind(fdr->fd, addr->ai_addr, addr->ai_addrlen);
	if (fdr->errval < 0) {
		fdr->errval = errno;
		fdr->strerror = strerror;
		close(fdr->fd);
	}

	fdr->errval = listen(fdr->fd, 1);
	if (fdr->errval < 0) {
		fdr->errval = errno;
		fdr->strerror = strerror;
		close(fdr->fd);
	}

out:
	if (addr) freeaddrinfo(addr);
	return fdr;
}

struct fd_result *c_accept(int fd, struct fd_result *fdr) {
	fdr->fd = accept(fd, 0, 0);
	if (fdr->fd < 0) {
		fdr->errval = errno;
		fdr->strerror = strerror;
	}
	return fdr;
}

