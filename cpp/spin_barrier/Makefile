TEST_REPS = 10000

CC = gcc
CCFLAGS += -O3 -mcpu=niagara2 -pthread
LDFLAGS += -lrt

.PHONY: all test32 test64 test128 test256

all: bt_spin bt_pthread

bt_spin: spin_barrier.c barrier_test.c
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $@ $^

bt_pthread: spin_barrier.c barrier_test.c
	$(CC) -DUSE_PTHREADS $(CCFLAGS) $(LDFLAGS) -o $@ $^

test32: bt_spin bt_pthread
	time ./bt_spin 32 $(TEST_REPS)
	@echo ----
	time ./bt_pthread 32 $(TEST_REPS)
	@echo ----

test64: test32
	time ./bt_spin 64 $(TEST_REPS)
	@echo ----
	time ./bt_pthread 64 $(TEST_REPS)
	@echo ----

test128: test64
	time ./bt_spin 128 $(TEST_REPS)
	@echo ----
	time ./bt_pthread 128 $(TEST_REPS)
	@echo ----

test256: test128
	time ./bt_spin 256 $(TEST_REPS)
	@echo ----
	time ./bt_pthread 256 $(TEST_REPS)
	@echo ----
