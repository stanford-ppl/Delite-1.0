#ifndef __SPIN_BARRIER_H__
#define __SPIN_BARRIER_H__


// The log2 of the branching factor of the tree merge
#define SB_LOG_BRANCHING_FACTOR 2

// The byte spacing of elements of the barrier.  Generally this should
// be the cache-line size of the data L1, unless it can be arranged that
// consecutive IDs share an L1.  Some common sizes:
//   Niagara, T2, T2+      16 byte L1
//   Opteron               64 byte L1
#define SB_SPACING 64

typedef struct __spin_barrier_t {
  unsigned int count;

  struct {
    char padding[SB_SPACING - sizeof(int)];
    volatile int value;
  } * slots;
} spin_barrier_t;

void spin_barrier_init(spin_barrier_t* barrier, unsigned int count);

void spin_barrier_destroy(spin_barrier_t* barrier);

// id must be < count, different from all other waiters
void spin_barrier_wait(spin_barrier_t* barrier, unsigned int id);

#endif // __SPIN_BARRIER_H__
