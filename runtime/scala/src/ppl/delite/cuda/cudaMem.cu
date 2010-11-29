#include <stdio.h>
#include "cuda.h"
#include "ppl_delite_cuda_DeliteCudaDriver.h"

JNIEXPORT jlong JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaMemAlloc(JNIEnv *env, jobject obj, jint size)
{

	CUdeviceptr dptr;

	if(cuMemAlloc(&dptr, size) != CUDA_SUCCESS)
	{
		printf("Error while allocating device memory of size %d\n", size);
		return -1;
	}
	
	return (jlong)dptr;
}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaMemFree(JNIEnv *env, jobject obj, jlong devPtr)
{
	if(cuMemFree((CUdeviceptr)devPtr) != CUDA_SUCCESS)
	{
		printf("ERROR: cuMemFree\n");
		return;
	}
}

JNIEXPORT jlong JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaMemAllocHost(JNIEnv *env, jobject obj, jint size)
{
	void *host_mem;
	//cuMemAllocHost(&host_mem, size);
	if(cuMemHostAlloc(&host_mem, size, CU_MEMHOSTALLOC_DEVICEMAP) != CUDA_SUCCESS)
	{
		printf("ERROR: cuMemHostAlloc\n");
		return -1;
	}

	return (jlong)host_mem;
}

JNIEXPORT jlong JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaMemHostGetDevicePointer(JNIEnv *env, jobject obj, jlong hostptr)
{
	CUdeviceptr dptr;

	if(cuMemHostGetDevicePointer(&dptr, (void *)hostptr, 0) != CUDA_SUCCESS)
	{
		printf("ERROR: cuMemHostGetDevicePointer\n");
		return -1;
	}

	return (jlong)dptr;
}

/*
JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaMemCpyHtoD(JNIEnv *env, jobject obj, jlong devPtr, jdoubleArray host_arr, jint offset, jint count)
{
	CUdeviceptr dptr = (CUdeviceptr)devPtr;

	jdouble *h_A = (jdouble *)malloc(count*sizeof(jdouble));

	env->GetDoubleArrayRegion(host_arr, offset, count, h_A);
	
	if(cuMemcpyHtoD(dptr, h_A, count*sizeof(jdouble)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuMemcpyHtoD\n");
		return;
	}

	free(h_A);
}


JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaMemCpyDtoH(JNIEnv *env, jobject obj, jdoubleArray host_arr, jlong dev_ptr, jint offset, jint count)
{
	
	jdouble *h_A = (jdouble *)malloc(count*sizeof(jdouble));

	if(cuMemcpyDtoH(h_A, (CUdeviceptr)dev_ptr, count*sizeof(jdouble)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuMemcpyDtoH\n");
		return;
	}

	env->SetDoubleArrayRegion(host_arr, offset, count, h_A);

	free(h_A);
}


JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaMemCpyDtoHNative(JNIEnv *env, jobject obj, jlong host_ptr, jlong dev_ptr, jint offset, jint count)
{
	if(cuMemcpyDtoH((void*)host_ptr, (CUdeviceptr)dev_ptr, count*sizeof(jdouble)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuMemcpyDtoHNative\n");
		return;
	}
}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaMemCpyHtoDNative(JNIEnv *env, jobject obj, jlong dev_ptr, jlong host_ptr, jint offset, jint count)
{
	if(cuMemcpyHtoD((CUdeviceptr)dev_ptr, (void*)host_ptr, count*sizeof(jdouble)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuMemcpyDtoHNative\n");
		return;
	}
}
*/

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaMemCpyHtoDAsync(JNIEnv *env, jobject obj, jlong devPtr, jdoubleArray host_arr, jint offset, jint count, jlong h_A, jlong stream)
{
	CUdeviceptr dptr = (CUdeviceptr)devPtr;

	env->GetDoubleArrayRegion(host_arr, offset, count, (jdouble*)h_A);

	if(cuMemcpyHtoDAsync(dptr, (jdouble*)h_A, count*sizeof(jdouble), (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuMemcpyHtoDAsync\n");
		return;
	}
	//cuStreamSynchronize((CUstream)stream);
}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaMemCpyDtoHAsync(JNIEnv *env, jobject obj, jdoubleArray host_arr, jlong dev_ptr, jint offset, jint count, jlong h_A, jlong stream)
{
	if(cuMemcpyDtoHAsync((jdouble*)h_A, (CUdeviceptr)dev_ptr, count*sizeof(jdouble), (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuMemcpyDtoHAsync\n");
		return;
	}

	cuStreamSynchronize((CUstream)stream);

	env->SetDoubleArrayRegion(host_arr, offset, count, (jdouble*)h_A);
}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaMemCpyHtoDAsyncFloat(JNIEnv *env, jobject obj, jlong devPtr, jfloatArray host_arr, jint offset, jint count, jlong h_A, jlong stream)
{
	CUdeviceptr dptr = (CUdeviceptr)devPtr;

	env->GetFloatArrayRegion(host_arr, offset, count, (jfloat*)h_A);
	
	if(cuMemcpyHtoDAsync(dptr, (jfloat*)h_A, count*sizeof(jfloat), (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuMemcpyHtoDAsync Float\n");
		return;
	}
	//cuStreamSynchronize((CUstream)stream);
}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaMemCpyDtoHAsyncFloat(JNIEnv *env, jobject obj, jfloatArray host_arr, jlong dev_ptr, jint offset, jint count, jlong h_A, jlong stream)
{
	if(cuMemcpyDtoHAsync((jfloat*)h_A, (CUdeviceptr)dev_ptr, count*sizeof(jfloat), (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuMemcpyDtoHAsync Float\n");
		return;
	}

	cuStreamSynchronize((CUstream)stream);

	env->SetFloatArrayRegion(host_arr, offset, count, (jfloat*)h_A);
}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaMemCpyHtoDAsyncInt(JNIEnv *env, jobject obj, jlong devPtr, jintArray host_arr, jint offset, jint count, jlong h_A, jlong stream)
{
	CUdeviceptr dptr = (CUdeviceptr)devPtr;

	env->GetIntArrayRegion(host_arr, offset, count, (jint*)h_A);

	if(cuMemcpyHtoDAsync(dptr, (jint*)h_A, count*sizeof(jint), (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuMemcpyHtoDAsync Int\n");
		return;
	}
	//cuStreamSynchronize((CUstream)stream);
}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaMemCpyDtoHAsyncInt(JNIEnv *env, jobject obj, jintArray host_arr, jlong dev_ptr, jint offset, jint count, jlong h_A, jlong stream)
{
	if(cuMemcpyDtoHAsync((jint*)h_A, (CUdeviceptr)dev_ptr, count*sizeof(jint), (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuMemcpyDtoHAsync Int\n");
		return;
	}

	cuStreamSynchronize((CUstream)stream);

	env->SetIntArrayRegion(host_arr, offset, count, (jint*)h_A);
}


JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaMemCpyDtoD(JNIEnv *env, jobject obj, jlong dst_ptr, jlong src_ptr, jint size)
{
	if(cuMemcpyDtoD((CUdeviceptr)dst_ptr, (CUdeviceptr)src_ptr, size) != CUDA_SUCCESS)
	{
		printf("ERROR: cuMemcpyDtoD\n");
		return;
	}
}
