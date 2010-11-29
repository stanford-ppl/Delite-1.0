#include "spin_barrier.h"
#include <malloc.h>

#define SPINS_BEFORE_YIELD 1000000
#define SPINS_BEFORE_SLEEP 10000

void spin_barrier_init(spin_barrier_t* barrier, unsigned int count) {
  // add one to prevent false sharing of the last used element
  barrier->count = count;
  barrier->slots = calloc(sizeof(barrier->slots[0]), count + 1);
}

void spin_barrier_destroy(spin_barrier_t* barrier) {
  free(barrier->slots);
  barrier->slots = NULL;
}

void backoff(int backoff_count) {
  if (backoff_count <= SPINS_BEFORE_SLEEP) {
    sched_yield();
  }
  else {
    usleep(backoff_count - SPINS_BEFORE_SLEEP);
  }
}

inline void spin_once(int* spin_count) {
  if (++*spin_count >= SPINS_BEFORE_YIELD) {
    backoff(*spin_count - SPINS_BEFORE_YIELD);
  }
}

#define BRANCHING_FACTOR (1 << SB_LOG_BRANCHING_FACTOR)

void spin_barrier_wait(spin_barrier_t* barrier, unsigned int id) {
  int const count = barrier->count;
  int const goal = 1 - barrier->slots[id].value;
  int mask = BRANCHING_FACTOR - 1;

  // wait for the sibling for which we are responsible, may be none
  while ((id & mask) == 0 && mask < count * BRANCHING_FACTOR) {
    int const spacing = (mask + 1) >> SB_LOG_BRANCHING_FACTOR;
    int i;
    for (i = 1; i < BRANCHING_FACTOR && id + i*spacing < count; ++i) {
      // wait for id + i*spacing
      int spin_count = 0;
      while (barrier->slots[id + i*spacing].value != goal) {
        spin_once(&spin_count);
      }
    }
    mask = (mask + 1) * BRANCHING_FACTOR - 1;
  }

  // arrive
  barrier->slots[id].value = goal;

  // wait for the master to arrive
  int spin_count = 0;
  while (barrier->slots[0].value != goal) {
    spin_once(&spin_count);
  }
}
