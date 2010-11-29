#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>
#include "rbm.h"
#include "mkl.h"
#include "mkl_vml_functions.h"

/* Structure for performance logging */
struct bucket {
	char *str;
	int cnt;
	long long time;
} logs[100];
int bucket_size = 0;

struct timeval prev, now;

void performance_check(char *str) {
	int i = 0;
	gettimeofday(&now, NULL);
	for(i=0; i<bucket_size; i++) {
		if(strcmp(logs[i].str,str)==0) {
			logs[i].cnt += 1;
			logs[i].time += ((now.tv_sec*1000000+now.tv_usec)-(prev.tv_sec*1000000+prev.tv_usec));
			break;
		}
	}

	if(i==bucket_size) {
		bucket_size += 1;
		logs[i].str = strdup(str);
		logs[i].cnt = 1;
		logs[i].time = ((now.tv_sec*1000000+now.tv_usec)-(prev.tv_sec*1000000+prev.tv_usec));
	}

	gettimeofday(&prev, NULL);
}

void printVector(Vector v) {
	int i = 0;
	printf("Vector \n");
	printf("length: %d\n", v.length);
	for(i=0; i<v.length; i++) {
		printf("%f \t", v.data[i]);
	}
	printf("\n");
}

void printMatrix(Matrix m) {
	int i = 0;
	int j = 0;
	printf("Matrix \n");
	printf("NumRows: %d, NumCols: %d\n",m.numRows, m.numCols);
	for(i=0; i<m.numRows; i++) {
		for(j=0; j<m.numCols; j++) {
			printf("%f \t", m.data[i*m.numCols+j]);
		}
		printf("\n");
	}
}

void print_logger(void) {
	int i = 0;
	printf("PERFORMANCE ANALYSIS\n");
	for(i=0; i<bucket_size; i++) {
		printf("%s\t %d\t %lld\n",logs[i].str,logs[i].cnt,logs[i].time/1000);
	}
}

#define SIZE 5000

int main (void)
{
	static struct timeval start, end;
	Matrix mat1;
	Matrix mat2;
	Matrix mat3;

	int i = 0;
	int j = 0;
	
	mat1.data = (float *)malloc(sizeof(float)*SIZE*SIZE);
	mat1.numRows = SIZE;
	mat1.numCols = SIZE;
	for(i=0; i<SIZE; i++) {
		for(j=0; j<SIZE; j++) {
			mat1.data[i*SIZE+j] = i+j;
		}
	}

	mat2.data = (float *)malloc(sizeof(float)*SIZE*SIZE);
	mat2.numRows = SIZE;
	mat2.numCols = SIZE;
	for(i=0; i<SIZE; i++) {
		for(j=0; j<SIZE; j++) {
			mat2.data[i*SIZE+j] = i+j;
		}
	}

	mat3.data = (float *)malloc(sizeof(float)*SIZE*SIZE);

	printf("MATRIX Multiplication Test: (%d*%d)\n", SIZE, SIZE);

	//printMatrix(mat1);
	//printMatrix(mat2);
	gettimeofday(&start, NULL);

	for(i=0; i<5; i++) {
		matMult(&mat3, mat1, mat2);
		//cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, mat1.numRows, mat1.numCols, mat2.numCols, 1.0, mat1.data, mat1.numRows, mat2.data, mat2.numRows, 0.0, mat3.data, mat1.numRows);
	}

	gettimeofday(&end, NULL);
	printf("Total exe time is %ld [ms]\n", ((end.tv_sec*1000000+end.tv_usec)-(start.tv_sec*1000000+start.tv_usec))/1000);
	
	//printMatrix(mat3);

	return 0;
}
