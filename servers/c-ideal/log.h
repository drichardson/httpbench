#pragma once

void log_string(char const* msg);
void log_format(char const* fmt, ...);
void log_errno(char const* fmt, ...);
void log_with_errno(int e, char const* fmt, ...);

