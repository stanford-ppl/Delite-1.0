for i in 1 2 4 8 16
do
	export OMP_NUM_THREADS=$i
	./test.out
done
