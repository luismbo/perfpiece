/* -*- indent-tabs-mode: nil -*- */

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <papi.h>
#include <monitor.h>

#ifdef PROCESS_ONLY
#error "Please compile libmonitor with thread support."
#endif

struct measurement {
    unsigned tid;
    long long real_nsec;
    long long user_nsec;
    struct measurement *next;
};

static pthread_mutex_t measurements_lock = PTHREAD_MUTEX_INITIALIZER;
struct measurement *perfpiece_measurements = NULL;

int perfpiece_active_p = 0;
int libppmonitor_loaded_p = 0;

void free_perfpiece_measurements()
{
    struct measurement *m, *next;

    for (m = perfpiece_measurements; m != NULL; m = next) {
        next = m->next;
        free(m);
    }

    perfpiece_measurements = NULL;
}

void monitor_init_library(void)
{
    libppmonitor_loaded_p = 1;
    monitor_opt_error = 0;
}

void monitor_init_process(char *process, int *argc, char **argv, unsigned tid)
{
    PAPI_library_init(PAPI_VER_CURRENT);
    PAPI_thread_init(pthread_self);
}

void *monitor_init_thread(unsigned tid)
{
    struct measurement *m;

    if (!perfpiece_active_p)
        return NULL;

    PAPI_register_thread();

    m = malloc(sizeof(struct measurement));

    m->tid = tid;
    m->user_nsec = PAPI_get_virt_nsec();
    m->real_nsec = PAPI_get_real_nsec();

    /* Add to the 'perfpiece_measurements' linked list. */
    pthread_mutex_lock(&measurements_lock);
    m->next = perfpiece_measurements;
    perfpiece_measurements = m;
    pthread_mutex_unlock(&measurements_lock);

    return m;
}

void monitor_fini_thread(void *ptr)
{
    struct measurement *m;

    if (!perfpiece_active_p)
        return;

    m = (struct measurement *) ptr;

    m->real_nsec = PAPI_get_real_nsec() - m->real_nsec;
    m->user_nsec = PAPI_get_virt_nsec() - m->user_nsec;

    PAPI_unregister_thread();
}
