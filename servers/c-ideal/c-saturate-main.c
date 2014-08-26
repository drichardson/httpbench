#include "likely.h"
#include "list.h"
#include "log.h"
#include <arpa/inet.h>
#include <stdbool.h>
#include <errno.h>
#include <netinet/ip.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

enum WorkerMode {
    WorkerMode_SingleThread,
    WorkerMode_MultiThreaded
};

enum WorkerMode g_worker_mode = WorkerMode_SingleThread;
//enum WorkerMode g_worker_mode = WorkerMode_MultiThreaded;
unsigned int g_worker_count = 100;
unsigned int g_accept_backlog = 400;

static int open_listener_socket_on_port(in_port_t);
static void handle_requests(int listen_socket);
static void handle_requests_multi(int listen_socket, int workers);
//static void handle_request(int client_socket);

static void run_listener(in_port_t);
static void run_connector(struct in_addr, in_port_t, uint64_t content_length);

static void log_usage() {
    log_string(
            "Usage:\n"
            "  c-saturate listen <port>\n"
            "  c-saturate connect <host> <port> <content-length>");
}

int main(int argc, char** argv)
{
    if (argc < 2) {
        log_string("Missing listen or connect argument");
        log_usage();
        exit(1);
    }

    char const* mode = argv[1];

    if (strcmp(mode, "listen") == 0) {
        if (argc < 3) {
            log_string("Missing port argument");
            log_usage();
            exit(1);
        }
        char const* port_str = argv[2];
        int port_num = atoi(port_str);
        if (port_num <= 0 || port_num > 0xffff) {
            log_format("Invalid port %s", port_str);
            exit(1);
        }
        run_listener(port_num);
    } else if (strcmp(mode, "connect") == 0) {
        if (argc < 5) {
            log_string("Missing host or port or content-length argument");
            log_usage();
            exit(1);
        }
        char const* host = argv[2];
        char const* port_str = argv[3];
        char const* content_length_str = argv[4];
        int port_num = atoi(port_str);
        if (port_num <= 0 || port_num > 0xffff) {
            log_format("Invalid port %s", port_str);
            exit(1);
        }
        struct in_addr addr;
        int rc = inet_aton(host, &addr);
        if (!rc) {
            log_format("Invalid host address. Should be IPv4 dotted quad (e.g. 192.168.1.23)");
            exit(1);
        }
        int content_length = atoi(content_length_str);
        if (content_length < 0) {
            log_format("Invalid content length argument %s", content_length_str);
            exit(1);
        }
        run_connector(addr, port_num, content_length);
    }

    return 0;
}

static void run_listener(in_port_t port) {
    int listener = open_listener_socket_on_port(port);
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


static bool recvloop(int fd, void* data, int length, int flags)
{
    char* bytes = (char*)data;
    int received = 0;
    while(received < length) {
        ssize_t rc = recv(fd, bytes+received, length-received, flags);
        if(unlikely(rc == -1)) {
            if (errno == EINTR) continue;
            log_errno("error receiving");
            return false;
        } else if (unlikely(rc == 0)) {
            log_string("recvloop detected that peer performed orderly shutdown");
            return false;
        }
        received += rc;
    }
    return true;
}

static void run_connector(struct in_addr addr, in_port_t port, uint64_t content_length) {
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    char* content = NULL;

    if (fd == -1) {
        log_errno("socket failed");
        return;
    }

    struct sockaddr_in saddr;
    memset(&saddr, 0, sizeof saddr);
    saddr.sin_family = AF_INET;
    saddr.sin_port = htons(port);
    saddr.sin_addr = addr;

    int rc = connect(fd, &saddr, sizeof saddr);
    if (rc == -1) {
        log_errno("connect failed");
        close(fd);
        return;
    }

    content = (char*)malloc(content_length);
    if (content == NULL) {
        log_errno("couldn't malloc buffer for content");
        goto end;
    }

    // send content length (note: note using network byte order... assuming machines are the same).
    bool ok = sendloop(fd, &content_length, sizeof content_length, 0);
    if (!ok) goto end;

    ok = sendloop(fd, content, content_length, 0);
    if (!ok) goto end;

    uint8_t status = 0;
    rc = recvloop(fd, &status, sizeof status, 0);
    if (rc == -1) {
        log_errno("Error receiving status from listener");
        goto end;
    }
    log_format("Got status %d", status);

end:
    close(fd);
    free(content);
}

static void handle_requests(int listen_socket)
{
    while(1) {
        int client = accept4(listen_socket, NULL, NULL, SOCK_CLOEXEC);
        char* content = NULL;
        if (client == -1) {
            if (errno == EINTR) continue;
            log_errno("accept4 failed with fatal error");
            return;
        }


        uint64_t content_length = 0;
        bool ok = recvloop(client, &content_length, sizeof content_length, 0);
        if (!ok) {
            log_string("didn't receive content length");
            goto loop_end;
        }

        // create one big ass buffer to read the entire content
        content = (char*)malloc(content_length);
        if (content == NULL) {
            log_errno("failed to malloc content buffer");
            goto loop_end;
        }
        log_format("going to receive %ull bytes", content_length);

        ok = recvloop(client, content, content_length, 0);
        if (!ok) {
            log_string("receive failed on content loop");
            goto loop_end;
        }

        uint64_t status = content_length;
        ok = sendloop(client, &status, sizeof(status), 0);
        if (!ok) {
            log_string("Didn't send status.");
        }

loop_end:
        close(client);
        free(content);
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

