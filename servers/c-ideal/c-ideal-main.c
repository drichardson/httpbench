#include <errno.h>
#include <netinet/ip.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <pthread.h>
#include "likely.h"
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

static bool sendloop(int fd, void const* data, int length, int flags)
{
    char const* bytes = (char const*)data;
    int sent = 0;
    while(sent < length) {
        ssize_t rc = send(fd, bytes+sent, length-sent, flags);
        if (unlikely(rc == -1)) {
            if (errno == EINTR) continue;
            log_errno("error sending");
            return false;
        }
        sent += rc;
    }
    return true;
}

static void handle_requests(int listen_socket)
{
    char const* response_header =
      "HTTP/1.1 200 OK\r\n"
      "Content-Length: 1024\r\n"
      "\r\n";

    int const response_content_length = 1024;
    int const response_buf_len = strlen(response_header) + response_content_length;
    char* response_buf = malloc(response_buf_len);

    memcpy(response_buf, response_header, strlen(response_header));
    for(int i = strlen(response_header); i < response_buf_len; ++i) {
      response_buf[i] = 'a' + (i % 26);
    }

    while(1) {
        int client = accept4(listen_socket, NULL, NULL, SOCK_CLOEXEC);
        if (client == -1) {
            if (errno == EINTR) continue;
            log_errno("accept4 failed with fatal error");
            return;
        }

	// Don't bother reading the request. Just send the response.

        bool ok = sendloop(client, response_buf, response_buf_len, 0);
        if (!ok) {
            log_format("Didn't send entire response.");
        }

        close(client);
    }

    free(response_buf);
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

