#include <errno.h>
#include <netinet/ip.h>
#include <stdarg.h>
#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "list.h"
#include "log.h"

static int open_listener_socket_on_port(in_port_t);
static void handle_requests(int listen_socket);
//static void handle_request(int client_socket);

int main(int argc, char** argv)
{
    int listener = open_listener_socket_on_port(8080);
    if (listener == -1) exit(1);
    handle_requests(listener);
    close(listener);
    return 0;
}

typedef struct slice {
    char *buf;
    size_t begin;
    size_t end;
} slice;

typedef struct buf_descriptor
{
    char *buf;
    int len;
    int cur;
} buf_descriptor;


// TODO:DOUG No No No! No copying. Use vectored I/O to write so that you don't have to copy
// into a single contiguous buffer.

#define MIN(a, b) ( (a < b) ? (a) : (b) )

// Returns bytes written.
static int buf_write(buf_descriptor *bd, char* data, int data_len) {
    int left = bd->len - bd->cur;
    int to_copy = MIN(left, data_len);
    memcpy(bd->buf+bd->cur, data, to_copy);
    bd->cur += to_copy;
    return to_copy;
}

static void handle_requests(int listen_socket)
{
    char request_buf[4096];
    char response_buf[4096];

    buf_descriptor bd;
    bd.buf = response_buf;
    bd.len = sizeof(response_buf);
    bd.cur = 0;

    buf_write(&bd, "HTTP/1.1 200 OK\r\n\r\n", 19);
    while(buf_write(&bd, "abcdefghijklmnopqrstuvwxyz", 26) > 0);

    while(1) {
        int client = accept4(listen_socket, NULL, NULL, SOCK_CLOEXEC);
        if (client == -1) {
            if (errno == EINTR) continue;
            log_errno("accept4 failed with fatal error");
            return;
        }

        // TODO: Non blocking, timeout, singal interruptions, etc.
        int rc = recv(client, request_buf, sizeof(request_buf), 0);
        if (rc == -1) {
            log_errno("recv failed");
            goto loop_end;
        }

        rc = send(client, response_buf, sizeof(response_buf), 0);
        if (rc != sizeof(response_buf)) {
            log_format("Didn't send entire response. rc = %d", rc);
        }

loop_end:
        close(client);
    }
}


#if 0
static void handle_request(int client_socket,
        char* request_buf, size_t request_buf_size,
        char* response_buf, size_t* response_buf_size)
{

}
#endif

static int open_listener_socket_on_port(in_port_t port)
{ 
    int fd = socket(AF_INET, SOCK_STREAM | SOCK_CLOEXEC, 0);
    if (fd == -1) {
        log_errno("socket failed");
        return -1;
    }

    struct sockaddr_in saddr;
    memset(&saddr, 0, sizeof saddr);
    saddr.sin_family = AF_INET;
    saddr.sin_port = htons(port);
    saddr.sin_addr.s_addr = INADDR_ANY;

    int on = 1;
    int rc = setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));
    if (rc == -1) {
        log_errno("setsockopt SO_REUSEADDR failed");
        close(fd);
        return -1;
    }

    rc = bind(fd, (struct sockaddr*)&saddr, sizeof(saddr));
    if (rc == -1) {
        log_errno("bind failed");
        close(fd);
        return -1;
    }

    int const backlog = 5;
    rc = listen(fd, backlog);
    if (rc == -1) {
        log_errno("listen failed");
        close(fd);
        return -1;
    }

    return fd;
}

