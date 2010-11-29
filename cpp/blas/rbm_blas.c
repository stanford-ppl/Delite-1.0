#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "rbm.h"
#include <gsl/gsl_blas.h>

void gpuInit(void) {
}

void gpuAlloc(int n, int elemSize, void **devicePtr) {
}

void gpuCopy(float *dptr, float *sptr, int byte) {

}

float fastexp9(float x) {
	return (362880+x*(362880+x*(181440+x*(60480+x*(15120+x*(3024+x*(504+x*(72+x*(9+x)))))))))*2.75573192e-6;
}

float fastexp6(float x) {
    return 720+x*(720+x*(360+x*(120+x*(30+x*(6+x)))))*0.0013888888f;
}

Matrix reciprocal(Matrix *out, Matrix m) {
	int i = 0;
	(*out).numRows = m.numRows;
	(*out).numCols = m.numCols;

	for(i=0; i<m.numRows*m.numCols; i++)
		(*out).data[i] = 1 / (m.data[i]);
	
	LOGGER("RECIP")

	return *out;
}

Matrix matPlusS(Matrix *out, Matrix m, float scalar) {
	int i = 0;
	(*out).numRows = m.numRows;
	(*out).numCols = m.numCols;

	for(i=0; i<m.numRows*m.numCols; i++)
		(*out).data[i] = m.data[i] + scalar;

	LOGGER("MATPLUS_S")
	
	return *out;
}

Matrix matExp(Matrix *out, Matrix m) {
	int i = 0;
	(*out).numRows = m.numRows;
	(*out).numCols = m.numCols;

	for(i=0; i<m.numRows*m.numCols; i++) {
		//(*out).data[i] = fastexp9(m.data[i]);
		//(*out).data[i] = fastexp6(m.data[i]);
		(*out).data[i] = expf(m.data[i]);
	}

	LOGGER("MATEXP")
	return *out;
}

Matrix matMinus(Matrix *out, Matrix m1, Matrix m2) { 
	int i = 0;
	(*out).numRows = m1.numRows;
	(*out).numCols = m1.numCols;

	gsl_vector_float_view A = gsl_vector_float_view_array(m1.data, m1.numRows*m1.numCols);
	gsl_vector_float_view B = gsl_vector_float_view_array(m2.data, m2.numRows*m2.numCols);
	gsl_vector_float_view C = gsl_vector_float_view_array(out->data, out->numRows*out->numCols);
	gsl_blas_scopy (&A.vector, &C.vector);
	gsl_blas_saxpy (-1.0, &B.vector, &C.vector);
	
	LOGGER("MATMINUS")
	
	return *out;

}

Matrix matMultS(Matrix *out, Matrix m, float scalar) {
	int i = 0;
	(*out).numRows = m.numRows;
	(*out).numCols = m.numCols;

	gsl_vector_float_view A = gsl_vector_float_view_array(m.data, m.numRows*m.numCols);
	gsl_vector_float_view C = gsl_vector_float_view_array(out->data, out->numRows*out->numCols);
	gsl_blas_scopy (&A.vector, &C.vector);
	gsl_blas_sscal (scalar, &C.vector);

	LOGGER("MATMULT_S")
	return *out;
}

Matrix matMult(Matrix *out, Matrix m1, Matrix m2) {
	int i = 0;
	int j = 0;
	int k = 0;
	float sum;
	(*out).numRows = m1.numRows;
	(*out).numCols = m2.numCols;

	gsl_matrix_float_view A = gsl_matrix_float_view_array(m1.data, m1.numRows, m1.numCols);
	gsl_matrix_float_view B = gsl_matrix_float_view_array(m2.data, m2.numRows, m2.numCols);
	gsl_matrix_float_view C = gsl_matrix_float_view_array(out->data, out->numRows, out->numCols);
	m1.data[1] = 21.42;
	gsl_blas_sgemm (CblasNoTrans, CblasNoTrans, 1.0, &A.matrix, &B.matrix, 0.0, &C.matrix);

	LOGGER("MATMULT")
	return *out;
}

Matrix matRep(Matrix *out, Vector v, int rep) {
	int repIdx = 0;
	int i = 0;
	out->numRows = rep;
	out->numCols = v.length;
	
	for(repIdx=0; repIdx<rep; repIdx++) {
		for(i=0; i<v.length; i++) {
			out->data[repIdx*v.length+i] = v.data[i];
		}
	}

	LOGGER("MATREP")
	return *out;
}

Matrix matTrans(Matrix *out, Matrix m) {
	int i = 0;
	int j = 0;
	out->numRows = m.numCols;
	out->numCols = m.numRows;

	for(i=0; i<m.numRows; i++) {
		for(j=0; j<m.numCols; j++) {
			out->data[j*m.numRows+i] = m.data[i*m.numCols+j];
		}
	}
	
	LOGGER("MATTRANS")
	return *out;
}


Vector matSumCol(Vector *out, Matrix m) {
	int i = 0;
	int j = 0;
	memset(out->data, 0, sizeof(float)*m.numCols);
	out->length = m.numCols;

	for(i=0; i<m.numRows; i++) {
		for(j=0; j<m.numCols; j++) {
			out->data[j] += m.data[i*m.numCols+j];
		}
	}
	
	LOGGER("MATSUMCOL")
	return *out;
}

Matrix matCompare(Matrix *out, Matrix m1, Matrix m2) {
	int i = 0;
	out->numRows = m1.numRows;
	out->numCols = m1.numCols;
	
	for(i=0; i<m1.numRows*m1.numCols; i++) {
		if(m1.data[i] > m2.data[i])
			out->data[i] = 1.0;
		else
			out->data[i] = 0.0;
	}

	LOGGER("MATCOMPARE")
	return *out;
}

Matrix matRandGauss(Matrix *out, int numRows, int numCols) {
	int i = 0;
	out->numRows = numRows;
	out->numCols = numCols;
	
	for(i=0; i<numRows*numCols; i++)
		out->data[i] = 4*(0.5-(1.0*rand())/(1.0*RAND_MAX));

	LOGGER("MATRAND")
	return *out;
}

Matrix matRand(Matrix *out, int numRows, int numCols) {
	int i = 0;
	out->numRows = numRows;
	out->numCols = numCols;
	
	for(i=0; i<numRows*numCols; i++)
		out->data[i] = (1.0*rand())/(1.0*RAND_MAX);

	LOGGER("MATRAND")
	return *out;
}

float matSum(Matrix m) {
	int i = 0;
	float sum = 0.0;

	for(i=0; i<m.numRows*m.numCols; i++)
		sum += m.data[i];
	
	return sum;
}

Matrix matDot(Matrix *out, Matrix m1, Matrix m2) {
	int i = 0;
	(*out).numRows = m1.numRows;
	(*out).numCols = m1.numCols;

	for(i=0; i<m1.numRows*m1.numCols; i++)
		(*out).data[i] = m1.data[i] * m2.data[i];
	
	LOGGER("MATDOT")
	return *out;
}

Matrix matPlus(Matrix *out, Matrix m1, Matrix m2) { 
	int i = 0;
	(*out).numRows = m1.numRows;
	(*out).numCols = m1.numCols;

	gsl_vector_float_view A = gsl_vector_float_view_array(m1.data, m1.numRows*m1.numCols);
	gsl_vector_float_view B = gsl_vector_float_view_array(m2.data, m2.numRows*m2.numCols);
	gsl_vector_float_view C = gsl_vector_float_view_array(out->data, out->numRows*out->numCols);
	gsl_blas_scopy (&A.vector, &C.vector);
	gsl_blas_saxpy (1.0, &B.vector, &C.vector);

	LOGGER("MATPLUS")
	return *out;
}

Vector vectPlus(Vector *out, Vector v1, Vector v2) {
	int i = 0;
	(*out).length = v1.length;

	gsl_vector_float_view A = gsl_vector_float_view_array(v1.data, v1.length);
	gsl_vector_float_view B = gsl_vector_float_view_array(v2.data, v2.length);
	gsl_vector_float_view C = gsl_vector_float_view_array(out->data, out->length);
	gsl_blas_scopy (&A.vector, &C.vector);
	gsl_blas_saxpy (1.0, &B.vector, &C.vector);

	LOGGER("VECPLUS")
	return *out;
}

Vector vectMultS(Vector *out, Vector v, float scalar) {
	int i = 0;
	(*out).length = v.length;

	gsl_vector_float_view A = gsl_vector_float_view_array(v.data, v.length);
	gsl_vector_float_view C = gsl_vector_float_view_array(out->data, out->length);
	gsl_blas_scopy (&A.vector, &C.vector);
	gsl_blas_sscal (scalar, &C.vector);
	
	/*
	for(i=0; i<v.length; i++)
		(*out).data[i] = v.data[i] * scalar;
	*/

	LOGGER("VECMULT_S")
	return *out;
}

Vector vectMinus(Vector *out, Vector v1, Vector v2) {
	int i = 0;
	(*out).length = v1.length;

	gsl_vector_float_view A = gsl_vector_float_view_array(v1.data, v1.length);
	gsl_vector_float_view B = gsl_vector_float_view_array(v2.data, v2.length);
	gsl_vector_float_view C = gsl_vector_float_view_array(out->data, out->length);
	gsl_blas_scopy (&A.vector, &C.vector);
	gsl_blas_saxpy (-1.0, &B.vector, &C.vector);

	LOGGER("VECMINUS")
	return *out;
}
