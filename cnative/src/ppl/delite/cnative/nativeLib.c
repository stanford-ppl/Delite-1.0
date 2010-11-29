#include <stdlib.h>
#include <stdio.h>
#include "ppl_delite_cnative_DeliteNative.h"
#include <gsl/gsl_blas.h>
//#include "mkl.h"

void pprint(jdouble* mat1, jint mat1_r, jint mat1_c);

JNIEXPORT void JNICALL Java_ppl_delite_cnative_DeliteNative_hello(JNIEnv *env, jobject obj)
{
	printf("Hello World\n");
	return;
}

JNIEXPORT void JNICALL Java_ppl_delite_cnative_DeliteNative_exportEnvVar(JNIEnv *env, jobject obj, jstring var, jstring val)
{
	int ret;
	jboolean copy;
	const char *cVar = (*env)->GetStringUTFChars(env, var, &copy);
	const char *cVal = (*env)->GetStringUTFChars(env, val, &copy);
	//printf("setting variable %s to value %s\n", cVar, cVal);
  ret = setenv(cVar, cVal, 1);
	//printf("ret is %d\n", ret);
	(*env)->ReleaseStringUTFChars(env, var, cVar);
	(*env)->ReleaseStringUTFChars(env, val, cVal);
}

JNIEXPORT void JNICALL Java_ppl_delite_cnative_DeliteNative_matrixMultDouble(JNIEnv *env, jobject obj, jdoubleArray mat1, jdoubleArray mat2, jdoubleArray mat3, jint mat1_r, jint mat1_c, jint mat2_c)
{
	//char* threads = getenv("GOTO_NUM_THREADS");
  //printf("BLAS * with %s threads\n", threads);

	jboolean copy;
	jdouble *mat1_ptr = (*env)->GetPrimitiveArrayCritical(env, (jarray)mat1, &copy);
	jdouble *mat2_ptr = (*env)->GetPrimitiveArrayCritical(env, (jarray)mat2, &copy);
	jdouble *mat3_ptr = (*env)->GetPrimitiveArrayCritical(env, (jarray)mat3, &copy);
	
  //printf("START JNI EXECUTION\n");
  //pprint(mat1_ptr, mat1_r, mat1_c);
  //pprint(mat2_ptr, mat1_c, mat2_c);

	gsl_matrix_view A = gsl_matrix_view_array(mat1_ptr, mat1_r, mat1_c);
	gsl_matrix_view B = gsl_matrix_view_array(mat2_ptr, mat1_c, mat2_c);
	gsl_matrix_view C = gsl_matrix_view_array(mat3_ptr, mat1_r, mat2_c);

	gsl_blas_dgemm (CblasNoTrans, CblasNoTrans, 1.0, &A.matrix, &B.matrix, 0.0, &C.matrix);
	//cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, mat1_r, mat2_c, mat1_c, 1.0, mat1_ptr, mat1_r, mat2_ptr, mat1_c, 0.0, mat3_ptr, mat1_r);

	(*env)->ReleasePrimitiveArrayCritical(env, mat1, mat1_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, mat2, mat2_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, mat3, mat3_ptr, 0);
  
  //pprint(mat3_ptr, mat1_r, mat2_c);
  //printf("END JNI EXECUTION\n");
}

JNIEXPORT void JNICALL Java_ppl_delite_cnative_DeliteNative_matrixMultFloat(JNIEnv *env, jobject obj, jfloatArray mat1, jfloatArray mat2, jfloatArray mat3, jint mat1_r, jint mat1_c, jint mat2_c)
{
	//char* threads = getenv("OMP_NUM_THREADS");
  	//printf("BLAS * with %s threads\n", threads);
	
	jboolean copy;
	
	/*
	jfloat *mat1_ptr = (*env)->GetFloatArrayElements(env, mat1, &copy);
	jfloat *mat2_ptr = (*env)->GetFloatArrayElements(env, mat2, &copy);
	jfloat *mat3_ptr = (*env)->GetFloatArrayElements(env, mat3, &copy);
	*/

	jfloat *mat1_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)mat1, &copy));
	jfloat *mat2_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)mat2, &copy));
	jfloat *mat3_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)mat3, &copy));
	//printf("is copy: %d\n", copy);

	if((mat1_ptr==NULL) || (mat2_ptr==NULL) || (mat3_ptr==NULL))
		printf("ERROR on PrimitiveCritical\n");

	gsl_matrix_float_view A = gsl_matrix_float_view_array(mat1_ptr, mat1_r, mat1_c);
	gsl_matrix_float_view B = gsl_matrix_float_view_array(mat2_ptr, mat1_c, mat2_c);
	gsl_matrix_float_view C = gsl_matrix_float_view_array(mat3_ptr, mat1_r, mat2_c);

	//cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, mat1_r, mat2_c, mat1_c, 1.0, mat1_ptr, mat1_r, mat2_ptr, mat1_c, 0.0, mat3_ptr, mat1_r);
	gsl_blas_sgemm (CblasNoTrans, CblasNoTrans, 1.0, &A.matrix, &B.matrix, 0.0, &C.matrix);

	/*
	(*env)->ReleaseFloatArrayElements(env, mat1, mat1_ptr, 0);
	(*env)->ReleaseFloatArrayElements(env, mat2, mat2_ptr, 0);
	(*env)->ReleaseFloatArrayElements(env, mat3, mat3_ptr, 0);
	*/
	(*env)->ReleasePrimitiveArrayCritical(env, mat1, mat1_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, mat2, mat2_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, mat3, mat3_ptr, 0);
}

JNIEXPORT void JNICALL Java_ppl_delite_cnative_DeliteNative_vectMultDouble(JNIEnv *env, jobject obj, jdoubleArray vec1, jdoubleArray vec2, jdoubleArray vec3, jint length)
{
	jboolean copy;
	
	jdouble *vec1_ptr = (jdouble*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec1, &copy));
	jdouble *vec2_ptr = (jdouble*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec2, &copy));
	jdouble *vec3_ptr = (jdouble*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec3, &copy));

	if((vec1_ptr==NULL) || (vec2_ptr==NULL) || (vec3_ptr==NULL))
		printf("ERROR(vectMultDouble): Array pointers cannot be NULL.\n");

	// Call VML function
	vdMul(length, vec1_ptr, vec2_ptr, vec3_ptr);

	(*env)->ReleasePrimitiveArrayCritical(env, vec1, vec1_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, vec2, vec2_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, vec3, vec3_ptr, 0);
}

JNIEXPORT void JNICALL Java_ppl_delite_cnative_DeliteNative_vectMultFloat(JNIEnv *env, jobject obj, jfloatArray vec1, jfloatArray vec2, jfloatArray vec3, jint length)
{
	jboolean copy;
	
	jfloat *vec1_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec1, &copy));
	jfloat *vec2_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec2, &copy));
	jfloat *vec3_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec3, &copy));

	if((vec1_ptr==NULL) || (vec2_ptr==NULL) || (vec3_ptr==NULL))
		printf("ERROR(vectMultFloat): Array pointers cannot be NULL.\n");

	// Call VML function
	vsMul(length, vec1_ptr, vec2_ptr, vec3_ptr);

	(*env)->ReleasePrimitiveArrayCritical(env, vec1, vec1_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, vec2, vec2_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, vec3, vec3_ptr, 0);
}

JNIEXPORT void JNICALL Java_ppl_delite_cnative_DeliteNative_matVMultFloat(JNIEnv *env, jobject obj, jfloatArray mat1, jfloatArray vec2, jfloatArray vec3, jint mat_row, jint mat_col, jint vec_offset, jint vec_stride)
{
	jboolean copy;
	
	jfloat *mat1_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)mat1, &copy));
	jfloat *vec2_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec2, &copy));
	jfloat *vec3_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec3, &copy));

	vec2_ptr += vec_offset;

	if((mat1_ptr==NULL) || (vec2_ptr==NULL) || (vec3_ptr==NULL))
		printf("ERROR(matVMultFloat): Array pointers cannot be NULL.\n");

	// Call VML function
	cblas_sgemv(CblasRowMajor, CblasNoTrans, mat_row, mat_col, 1.0, mat1_ptr, mat_row, vec2_ptr, vec_stride, 0.0, vec3_ptr, 1); 

	(*env)->ReleasePrimitiveArrayCritical(env, mat1, mat1_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, vec2, vec2_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, vec3, vec3_ptr, 0);
}

JNIEXPORT void JNICALL Java_ppl_delite_cnative_DeliteNative_matVMultDouble(JNIEnv *env, jobject obj, jdoubleArray mat1, jdoubleArray vec2, jdoubleArray vec3, jint mat_row, jint mat_col, jint vec_offset, jint vec_stride)
{
	jboolean copy;
	
	jdouble *mat1_ptr = (jdouble*)((*env)->GetPrimitiveArrayCritical(env, (jarray)mat1, &copy));
	jdouble *vec2_ptr = (jdouble*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec2, &copy));
	jdouble *vec3_ptr = (jdouble*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec3, &copy));

	vec2_ptr += vec_offset;

	if((mat1_ptr==NULL) || (vec2_ptr==NULL) || (vec3_ptr==NULL))
		printf("ERROR(matVMultDouble): Array pointers cannot be NULL.\n");

	// Call VML function
	cblas_dgemv(CblasRowMajor, CblasNoTrans, mat_row, mat_col, 1.0, mat1_ptr, mat_col, vec2_ptr, vec_stride, 0.0, vec3_ptr, 1); 

	(*env)->ReleasePrimitiveArrayCritical(env, mat1, mat1_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, vec2, vec2_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, vec3, vec3_ptr, 0);
}

JNIEXPORT void JNICALL Java_ppl_delite_cnative_DeliteNative_vecExpFloat(JNIEnv *env, jobject obj, jfloatArray vec1, jfloatArray vec2, jint length)
{
	jboolean copy;
	
	jfloat *vec1_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec1, &copy));
	jfloat *vec2_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec2, &copy));

	if((vec1_ptr==NULL) || (vec2_ptr==NULL))
		printf("ERROR(vecExpDouble): Array pointers cannot be NULL.\n");

	// Call VML function
	vsExp(length, vec1_ptr, vec2_ptr);

	(*env)->ReleasePrimitiveArrayCritical(env, vec1, vec1_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, vec2, vec2_ptr, 0);
}

JNIEXPORT void JNICALL Java_ppl_delite_cnative_DeliteNative_vecExpDouble(JNIEnv *env, jobject obj, jdoubleArray vec1, jdoubleArray vec2, jint length)
{
	jboolean copy;
	
	jdouble *vec1_ptr = (jdouble*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec1, &copy));
	jdouble *vec2_ptr = (jdouble*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec2, &copy));

	if((vec1_ptr==NULL) || (vec2_ptr==NULL))
		printf("ERROR(vecExpDouble): Array pointers cannot be NULL.\n");

	// Call VML function
	vdExp(length, vec1_ptr, vec2_ptr);

	(*env)->ReleasePrimitiveArrayCritical(env, vec1, vec1_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, vec2, vec2_ptr, 0);
}

JNIEXPORT void JNICALL Java_ppl_delite_cnative_DeliteNative_vecSigmoidFloat(JNIEnv *env, jobject obj, jfloatArray vec1, jfloatArray vec2, jint start, jint end)
{
	int i = 0;
	jboolean copy;
	
	jfloat *vec1_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec1, &copy));
	jfloat *vec2_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec2, &copy));
	
	if((vec1_ptr==NULL) || (vec2_ptr==NULL))
		printf("ERROR(vecExpDouble): Array pointers cannot be NULL.\n");
	
	for(i=start; i<end; i++) {
		vec2_ptr[i] = 1.0 / (1.0+expf(-1.0*vec1_ptr[i]));
	}

	(*env)->ReleasePrimitiveArrayCritical(env, vec1, vec1_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, vec2, vec2_ptr, 0);
}

JNIEXPORT void JNICALL Java_ppl_delite_cnative_DeliteNative_vecSigmoidDouble(JNIEnv *env, jobject obj, jdoubleArray vec1, jdoubleArray vec2, jint start, jint end)
{
	int i = 0;
	jboolean copy;
	
	jdouble *vec1_ptr = (jdouble*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec1, &copy));
	jdouble *vec2_ptr = (jdouble*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec2, &copy));

	if((vec1_ptr==NULL) || (vec2_ptr==NULL))
		printf("ERROR(vecExpDouble): Array pointers cannot be NULL.\n");

	for(i=start; i<end; i++) {
		vec2_ptr[i] = 1.0 / (1.0+exp(-1.0*vec1_ptr[i]));
	}

	(*env)->ReleasePrimitiveArrayCritical(env, vec1, vec1_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, vec2, vec2_ptr, 0);
}

JNIEXPORT void JNICALL Java_ppl_delite_cnative_DeliteNative_vecRandFloat(JNIEnv *env, jobject obj, jfloatArray vec, jint start, jint end)
{
	int i = 0;
	jboolean copy;
	jfloat *vec_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec, &copy));
	
	if(vec_ptr==NULL)
		printf("ERROR(vecRandFloat): Array pointers cannot be NULL.\n");
	
	for(i=start; i<end; i++) {
		vec_ptr[i] = (1.0*rand())/(1.0*RAND_MAX);
	}

	(*env)->ReleasePrimitiveArrayCritical(env, vec, vec_ptr, 0);
}

void pprint(jdouble* mat1, jint mat1_r, jint mat1_c) {
  int i = 0;
  int j = 0;
  for (i=0; i < mat1_r; i++){
    printf("[ ");
    for (j=0; j < mat1_c; j++){
      printf("%f\t", mat1[i*mat1_c + j]);
    }
    printf("]");
    printf("\n");
  }
  printf("\n\n");
}
