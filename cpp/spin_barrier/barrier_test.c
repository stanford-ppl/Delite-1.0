#include <stdio.h>
#include <stdlib.h>
#include "spin_barrier.h"
#include <pthread.h>

//#define USE_PTHREADS

typedef struct WorkerParam_t {
  pthread_t tid;
  int id;
  int reps;
#ifdef USE_PTHREADS
  pthread_barrier_t* barrier;
#else
  spin_barrier_t* barrier;
#endif
} WorkerParam;

void* worker(void* opaque) {
  WorkerParam* param = (WorkerParam*) opaque;
  int i;
  
  for (i = 0; i < param->reps; ++i) {
#ifdef USE_PTHREADS
    pthread_barrier_wait(param->barrier);
#else
    spin_barrier_wait(param->barrier, param->id);
#endif
  }
  
  return NULL;
}

int main(int argc, char ** argv) {
  int num_threads = 32;
  WorkerParam params[1024];
  int i;
#ifdef USE_PTHREADS
  pthread_barrier_t barrier;
#else
  spin_barrier_t barrier;
#endif
  int reps = 10000;

  if (argc > 1) {
    num_threads = atoi(argv[1]);
  }
  if (argc > 2) {
    reps = atoi(argv[2]);
  }
  printf("testing %s using %d threads, %d barriers each\n",
#ifdef USE_PTHREADS
         "pthread_barrier_t",
#else
         "spin_barrier_t",
#endif
         num_threads, reps);
  if (num_threads < 1 || num_threads > sizeof(params)/sizeof(params[0])) {
    fprintf(stderr, "num_threads is not a valid value\n");
    return 1;
  }

#ifdef USE_PTHREADS
  pthread_barrier_init(&barrier, NULL, num_threads);
#else
  spin_barrier_init(&barrier, num_threads);
#endif

  for (i = 0; i < num_threads; ++i) {
    params[i].id = i;
    params[i].barrier = &barrier;
    params[i].reps = reps;
    if (i != 0) {
      pthread_create(&params[i].tid, NULL, &worker, params + i);
    }
  }
  worker(params + 0);
  for (i = 1; i < num_threads; ++i) {
    void* dummy;
    pthread_join(params[i].tid, &dummy);
  }
  return 0;
}
