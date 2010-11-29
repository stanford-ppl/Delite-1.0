CC=gcc -O3
ICC=icc -O3

test_goto: test.c
	$(CC) -I/usr/include -I/usr/local/intel/mkl/include -L/usr/local/lib/GotoBLAS2 -L/usr/lib -lgsl -lgoto2 -lgfortran -lm test.c rbm_blas.c -o test.out

test_mkl: test.c
	$(ICC) -I/usr/include -I/usr/local/intel/mkl/include -L/usr/local/intel/mkl/lib/em64t -L/usr/local/intel/lib/intel64 -lgsl -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread -lgfortran test.c rbm_blas.c -lm -o test.out

#	$(CC) -I/usr/include -I/home/hyouklee/intel/mkl/10.2.5.035/include -L/home/hyouklee/intel/mkl/10.2.5.035/lib/em64t -L/usr/lib -lgsl -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread -lm -lgfortran -lm test.c rbm_blas.c -o test.out

clean: 
	rm -rf *.out
