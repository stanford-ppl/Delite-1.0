#include <stdio.h>
#include "cuda.h"
#include "ppl_delite_cuda_DeliteCudaDriver.h"

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_dummy(JNIEnv *env, jobject obj)
{
	return;
}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaInit(JNIEnv *env, jclass cls)
{
    if(cuInit(0) != CUDA_SUCCESS)
    {
         printf("ERROR: cuInit\n");
    }
}


JNIEXPORT jlong JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaDevSet(JNIEnv *env, jobject obj, jint devIdx)
{
	CUdevice dev;
	CUcontext pctx;
	CUmodule module;

    /*
    int deviceNum;
    if(cuDeviceGetCount(&deviceNum) != CUDA_SUCCESS) {
        printf("ERROR: cuDeviceGetCount\n");
            return -1;
    }
    printf("Initializing %d device out of %d devices\n", devIdx, deviceNum);
    */
    
    if(cuDeviceGet(&dev, devIdx) != CUDA_SUCCESS)
    {
        printf("ERROR: cuDeviceGet\n");
        return -1;
    }

    if(cuCtxCreate(&pctx, CU_CTX_MAP_HOST, dev) != CUDA_SUCCESS)
    {
        printf("ERROR: cuCtxCreate\n");
        return -1;
    }

    if(cuModuleLoad(&module, "/tmp/cuda/cudaKernels.ptx") != CUDA_SUCCESS)
    {
        printf("ERROR: cuModuleLoad\n");
        return -1;
    }
	return (jlong)(module);
}


JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaDestroyContext(JNIEnv *env, jobject obj, jint devIdx)
{
	CUcontext pctx;
    if(cuCtxPopCurrent(&pctx) != CUDA_SUCCESS) {
        printf("ERROR: cuCtxPopCurrent\n");
        return;
    }

    if(cuCtxDestroy(pctx) != CUDA_SUCCESS) {
        printf("ERROR: cuCtxDestroy\n");
        return;
    }
}

JNIEXPORT jlong JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaCreateStream(JNIEnv *env, jobject obj)
{
	CUstream stream;

	if(cuStreamCreate(&stream, 0) != CUDA_SUCCESS)
	{
		printf("ERROR: cuModuleGetFunction\n");
		return -1;
	}

	return (jlong)stream;
}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaDestroyStream(JNIEnv *env, jobject obj, jlong stream)
{
	if(cuStreamDestroy((CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuStreamDestroy\n");
		return;
	}
}

JNIEXPORT jlong JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaGetFunction(JNIEnv *env, jobject obj, jlong module, jstring kernel)
{
	CUfunction func;
	
	// Get Kernel name character pointer
	const char *KernelName = env->GetStringUTFChars(kernel, 0);

	if(cuModuleGetFunction(&func, (CUmodule)module, (const char *)KernelName) != CUDA_SUCCESS)
	{
		printf("ERROR: cuModuleGetFunction: %s\n", KernelName);
		return -1;
	}

	// Let Java VM know that the char pointer is not needed by the native code
	env->ReleaseStringUTFChars(kernel, KernelName);

	return (jlong)func;

}

JNIEXPORT jint JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaGetDevNum(JNIEnv *env, jobject obj)
{
	jint dev_num = 0;

	cuDeviceGetCount(&dev_num);
	
	return dev_num;
}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaStreamSync(JNIEnv *env, jobject obj, jlong stream)
{
	if(cuStreamSynchronize((CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cudaStreamSync\n");
		return;
	}
	if(cuStreamQuery((CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cudaStreamQeury\n");
		return;
	}
}

