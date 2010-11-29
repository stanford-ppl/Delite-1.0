#include <stdlib.h>
#include <stdio.h>
#include "ppl_delite_cnative_DeliteNative.h"
#include "sunperf.h"

void pprint(jdouble* mat1, jint mat1_r, jint mat1_c);

JNIEXPORT void JNICALL Java_ppl_delite_cnative_DeliteNative_matrixMultDouble(JNIEnv *env, jobject obj, jdoubleArray mat1, jdoubleArray mat2, jdoubleArray mat3, jint mat1_r, jint mat1_c, jint mat2_c)
{
	printf("Not supported now\n");

	/*
	jboolean copy;
	jdouble *mat1_ptr = (*env)->GetPrimitiveArrayCritical(env, (jarray)mat1, &copy);
	jdouble *mat2_ptr = (*env)->GetPrimitiveArrayCritical(env, (jarray)mat2, &copy);
	jdouble *mat3_ptr = (*env)->GetPrimitiveArrayCritical(env, (jarray)mat3, &copy);
	
	if((mat1_ptr==NULL) || (mat2_ptr==NULL) || (mat3_ptr==NULL))
		printf("ERROR on PrimitiveCritical\n");
	
	dgemm_64('n', 'n', mat2_c, mat1_r, mat1_c, 1.0, mat2_ptr, mat2_c, mat1_ptr, mat1_c, 0.0, mat3_ptr, mat2_c);

	(*env)->ReleasePrimitiveArrayCritical(env, mat1, mat1_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, mat2, mat2_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, mat3, mat3_ptr, 0);
	*/
}

JNIEXPORT void JNICALL Java_ppl_delite_cnative_DeliteNative_matrixMultFloat(JNIEnv *env, jobject obj, jfloatArray mat1, jfloatArray mat2, jfloatArray mat3, jint mat1_r, jint mat1_c, jint mat2_c)
{
	printf("Not supported now\n");
	
	/*
	int i  = 0;
	jboolean copy;
	jfloat *mat1_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)mat1, &copy));
	jfloat *mat2_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)mat2, &copy));
	jfloat *mat3_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)mat3, &copy));

	if((mat1_ptr==NULL) || (mat2_ptr==NULL) || (mat3_ptr==NULL))
		printf("ERROR on PrimitiveCritical\n");
	
	printf("Before\n");
	sgemm_64('n', 'n', mat2_c, mat1_r, mat1_c, 1.0, mat2_ptr, mat2_c, mat1_ptr, mat1_c, 0.0, mat3_ptr, mat2_c);
	printf("After!\n");
	
	(*env)->ReleasePrimitiveArrayCritical(env, mat1, mat1_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, mat2, mat2_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, mat3, mat3_ptr, 0);
	*/
}

JNIEXPORT void JNICALL Java_ppl_delite_cnative_DeliteNative_vectMultDouble(JNIEnv *env, jobject obj, jdoubleArray vec1, jdoubleArray vec2, jdoubleArray vec3, jint length)
{
	printf("Not supported now\n");
}

JNIEXPORT void JNICALL Java_ppl_delite_cnative_DeliteNative_vectMultFloat(JNIEnv *env, jobject obj, jfloatArray vec1, jfloatArray vec2, jfloatArray vec3, jint length)
{
	printf("Not supported now\n");
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
	
	sgemv_64('t', mat_col, mat_row, 1.0, mat1_ptr, mat_col, vec2_ptr, vec_stride, 0.0, vec3_ptr, 1);

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

	dgemv_64('t', mat_col, mat_row, 1.0, mat1_ptr, mat_col, vec2_ptr, vec_stride, 0.0, vec3_ptr, 1);

	(*env)->ReleasePrimitiveArrayCritical(env, mat1, mat1_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, vec2, vec2_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, vec3, vec3_ptr, 0);
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
