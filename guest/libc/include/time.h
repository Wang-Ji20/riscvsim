#ifndef _TIME_H
#define _TIME_H
typedef long time_t;
struct timespec { time_t tv_sec; long tv_nsec; };
#define CLOCK_REALTIME 0
#define CLOCK_MONOTONIC 1
int clock_gettime(int clk, struct timespec *ts);
time_t time(time_t *t);
#endif
