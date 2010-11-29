#include <stdio.h>
#include "cuda.h"
#include "ppl_delite_cuda_DeliteCudaDriver.h"

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaSched(JNIEnv *env, jobject obj, jlong func_id, jlong cnt_ptr, jint count, jlong stream)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)cnt_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaSched)\n");
		return;
	}
	offset += sizeof(ptr);

	offset = (offset + __alignof(count) - 1) & ~(__alignof(count) - 1);
	if(cuParamSeti(func, offset, count) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaSched)\n");
		return;
	}
	offset += sizeof(count);

	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaSched)\n");
		return;
	}
	
	if(cuFuncSetBlockShape(func, 1, 1, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaSched)\n");
		return;
	}

	if(cuLaunchGridAsync(func, 1, 1, (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuLaunchGrid (cudaSched)\n");
		return;
	}
	
}

JNIEXPORT jint JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_readDevTimeStamp(JNIEnv *env, jobject obj, jlong TS_ptr)
{
	int *ptr = (int *)TS_ptr;
	return (*ptr);
}

