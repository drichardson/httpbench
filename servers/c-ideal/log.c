#include "log.h"
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

void log_string_fast(char const* str, size_t len) {
    fwrite(str, 1, len, stderr);
    fputc('\n', stderr);
}

void log_string(char const* str) {
    fputs(str, stderr);
}

static void log_format_v(char const* fmt, va_list va) {
    char buf[4096];
    int count = vsnprintf(buf, sizeof(buf), fmt, va);
    log_string_fast(buf, count);
}

void log_format(char const* fmt, ...)
{
    va_list va;
    va_start(va, fmt);
    log_format_v(fmt, va);
    va_end(va);
}


static void log_with_errnov(int e, char const* fmt, va_list va) {
    char buf[4096];
    vsnprintf(buf, sizeof(buf), fmt, va);
    log_format("%s Error: (%d) %s.", buf, e, strerror(e));
}

void log_with_errno(int e, char const* fmt, ...)
{
    va_list va;
    va_start(va, fmt);
    char buf[4096];
    vsnprintf(buf, sizeof(buf), fmt, va);
    log_format("%s Error: (%d) %s.", buf, e, strerror(e));
    va_end(va);
}

void log_errno(char const* fmt, ...)
{
    va_list va;
    va_start(va, fmt);
    log_with_errnov(errno, fmt, va);
    va_end(va);
}

