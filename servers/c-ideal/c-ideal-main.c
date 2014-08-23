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

enum WorkerMode {
    WorkerMode_SingleThread,
    WorkerMode_MultiThreaded
};

//enum WorkerMode g_worker_mode = WorkerMode_SingleThread;
enum WorkerMode g_worker_mode = WorkerMode_MultiThreaded;
unsigned int g_worker_count = 100;
unsigned int g_accept_backlog = 400;

static int open_listener_socket_on_port(in_port_t);
static void handle_requests(int listen_socket);
static void handle_requests_multi(int listen_socket, int workers);
//static void handle_request(int client_socket);

int main(int argc, char** argv)
{
    int listener = open_listener_socket_on_port(8080);
    if (listener == -1) exit(1);

    switch(g_worker_mode) {
    case WorkerMode_SingleThread:
        handle_requests(listener);
        break;
    case WorkerMode_MultiThreaded:
        handle_requests_multi(listener, g_worker_count);
        break;
    }
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

    int last_read = -1;
    int last_write = -1;

    while(1) {
        int client = accept4(listen_socket, NULL, NULL, SOCK_CLOEXEC);
        if (client == -1) {
            if (errno == EINTR) continue;
            log_errno("accept4 failed with fatal error");
            return;
        }


        int to_read = 81; // TODO: Fix just for testing.
        while(to_read > 0) {
            // TODO: Non blocking, timeout, singal interruptions, etc.
            int rc = recv(client, request_buf, sizeof(request_buf), 0);
            if (rc == -1) {
                if (errno == EINTR) continue;

                log_errno("recv failed");
                goto loop_end;
            }

            if (rc == 0) {
                // peer shutdown.
                goto loop_end;
            }

            if (last_read == -1) {
                last_read = rc;
            } else {
                if (last_read != rc) {
                    log_format("Last time I read %d bytes, but this time I read %d", last_read, rc);
                }
            }

            to_read -= rc;
        }

        // TODO: Make sure you send everything.
        int rc = send(client, response_buf, sizeof(response_buf), 0);
        if (rc != sizeof(response_buf)) {
            log_format("Didn't send entire response. rc = %d", rc);
        }

        if (last_write == -1) {
            last_write = rc;
        } else if (last_write != rc) {
            log_format("Last write %d not equal to this write %d", last_write, rc);
        }

loop_end:
        close(client);
    }
}

static void* handle_request_worker_thread(void* arg)
{
    int listen_socket = *(int*)arg;
    handle_requests(listen_socket);
    return NULL;
}

static void handle_requests_multi(int listen_socket, int worker_count)
{
    pthread_t *worker_threads = calloc(worker_count, sizeof(worker_threads));
    int rc;
    
    for(int i = 0; i < worker_count; ++i) {
        rc = pthread_create(worker_threads+i, NULL, handle_request_worker_thread, &listen_socket);
        if (rc != 0) {
            log_with_errno(rc, "pthread_create failed on %d", i);
            exit(1);
        }
    }

    for(int i = 0; i < worker_count; ++i) {
        rc = pthread_join(worker_threads[i], NULL);
    }

    free(worker_threads);
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

    rc = listen(fd, g_accept_backlog);
    if (rc == -1) {
        log_errno("listen failed");
        close(fd);
        return -1;
    }

    return fd;
}

