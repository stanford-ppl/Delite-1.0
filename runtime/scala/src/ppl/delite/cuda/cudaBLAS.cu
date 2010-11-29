#include <stdio.h>
#include "cuda.h"
#include "ppl_delite_cuda_DeliteCudaDriver.h"

/* Global variables for primitive type classes and get methods.
   Initialized in initIDs() function
*/
jclass CLS_Integer;
jclass CLS_Long;
jclass CLS_Float;
jclass CLS_Double;
jmethodID MID_intValue;
jmethodID MID_longValue;
jmethodID MID_floatValue;
jmethodID MID_doubleValue;
jmethodID MID_getArg;
jmethodID MID_getArgSize;

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_initIDs(JNIEnv *env, jobject obj)
{
	// Get local reference to primitive type classes
	jclass local_CLS_Integer = env->FindClass("java/lang/Integer");
	jclass local_CLS_Long = env->FindClass("java/lang/Long");
	jclass local_CLS_Float = env->FindClass("java/lang/Float");
	jclass local_CLS_Double = env->FindClass("java/lang/Double");

	// Save local reference to global references for later uses 
	CLS_Integer = (jclass)(env->NewGlobalRef(local_CLS_Integer));
	CLS_Long = (jclass)(env->NewGlobalRef(local_CLS_Long));
	CLS_Float = (jclass)(env->NewGlobalRef(local_CLS_Float));
	CLS_Double = (jclass)(env->NewGlobalRef(local_CLS_Double));

	// Get method IDs for get methods of primitive type classes
	MID_intValue = env->GetMethodID(CLS_Integer, "intValue", "()I");
	MID_longValue = env->GetMethodID(CLS_Long, "longValue", "()J");
	MID_floatValue = env->GetMethodID(CLS_Float, "floatValue", "()F");
	MID_doubleValue = env->GetMethodID(CLS_Double, "doubleValue", "()D");
	
	// Get class/method ID for argument list queue
	jclass local_CLS_Queue = env->FindClass("java/util/concurrent/ArrayBlockingQueue");
	MID_getArg = env->GetMethodID(local_CLS_Queue, "poll", "()Ljava/lang/Object;");
	MID_getArgSize = env->GetMethodID(local_CLS_Queue, "size", "()I");
}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaAsyncLaunch1D(JNIEnv *env, jobject obj, jlong func_id, jobject argsList, jint length1D, jlong stream)
{
	int i = 0;
	int offset = 0;
	CUfunction func = (CUfunction)func_id;
	jobject arg;
	int argsSize = env->CallIntMethod(argsList, MID_getArgSize);

	while(i < argsSize) {
		arg = env->CallObjectMethod(argsList, MID_getArg);
		if(env->IsInstanceOf(arg, CLS_Integer)) {
			int value = env->CallIntMethod(arg, MID_intValue);
			offset = (offset + __alignof(value) - 1) & ~(__alignof(value) - 1);
			if(cuParamSeti(func, offset,  value) != CUDA_SUCCESS) {
				printf("ERROR: cuParamSeti (Async1D)\n");
				return;
			}
			offset += sizeof(value);
			//printf("Integer : %d\n", value);
		}
		else if(env->IsInstanceOf(arg, CLS_Long)) {
			long value = env->CallLongMethod(arg, MID_longValue);
			offset = (offset + __alignof(value) - 1) & ~(__alignof(value) - 1);
			if(cuParamSetv(func, offset, &value, sizeof(value)) != CUDA_SUCCESS) {
				printf("ERROR: cuParamSetv (Async1D)\n");
				return;
			}
			offset += sizeof(value);
			//printf("Long : %ld\n", value);
		}
		else if(env->IsInstanceOf(arg, CLS_Float)) {
		    float value = env->CallFloatMethod(arg, MID_floatValue);
			offset = (offset + __alignof(value) - 1) & ~(__alignof(value) - 1);
			if(cuParamSetv(func, offset, &value, sizeof(value)) != CUDA_SUCCESS) {
				printf("ERROR: cuParamSetv (Async1D)\n");
				return;
			}
			offset += sizeof(value);
			//printf("Float : %f\n", env->CallFloatMethod(arg, MID_floatValue));
		}
		else if(env->IsInstanceOf(arg, CLS_Double)) {
		    double value = env->CallDoubleMethod(arg, MID_doubleValue);
			offset = (offset + __alignof(value) - 1) & ~(__alignof(value) - 1);
			if(cuParamSetv(func, offset, &value, sizeof(value)) != CUDA_SUCCESS) {
				printf("ERROR: cuParamSetv (Async1D)\n");
				return;
			}
			offset += sizeof(value);
			//printf("Double : %f\n", env->CallDoubleMethod(arg, MID_doubleValue));
		}
		else {
			printf("Not Supported Type to pass to GPU.\n");
		}
		i += 1;
	}

	if(cuParamSetSize(func, offset) != CUDA_SUCCESS) {
		printf("ERROR: cuParamSetSize (Async1D)\n");
		return;
	}
	
	if(cuFuncSetBlockShape(func, 512, 1, 1) != CUDA_SUCCESS) {
		printf("ERROR: cuFuncSetBlockShape (Async1D)\n");
		return;
	}
	if(cuLaunchGridAsync(func, 1+(length1D-1)/512, 1, (CUstream)stream) != CUDA_SUCCESS) {
		printf("ERROR: cuLaunchGrid (Async1D)\n");
		return;
	}
}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaAsyncLaunch2D(JNIEnv *env, jobject obj, jlong func_id, jobject argsList, jint length1D, jint length2D, jlong stream)
{
	int i = 0;
	int offset = 0;
	CUfunction func = (CUfunction)func_id;
	jobject arg;
    int argsSize = env->CallIntMethod(argsList, MID_getArgSize);
    
	while(i < argsSize) {
		arg = env->CallObjectMethod(argsList, MID_getArg);
		if(env->IsInstanceOf(arg, CLS_Integer)) {
			int value = env->CallIntMethod(arg, MID_intValue);
			offset = (offset + __alignof(value) - 1) & ~(__alignof(value) - 1);
			if(cuParamSeti(func, offset,  value) != CUDA_SUCCESS) {
				printf("ERROR: cuParamSeti (Async2D)\n");
				return;
			}
			offset += sizeof(value);
			//printf("Integer : %d\n", value);
		}
		else if(env->IsInstanceOf(arg, CLS_Long)) {
			long value = env->CallLongMethod(arg, MID_longValue);
			offset = (offset + __alignof(value) - 1) & ~(__alignof(value) - 1);
			if(cuParamSetv(func, offset, &value, sizeof(value)) != CUDA_SUCCESS) {
				printf("ERROR: cuParamSetv (Async2D)\n");
				return;
			}
			offset += sizeof(value);
			//printf("Long : %ld\n", value);
		}
		else if(env->IsInstanceOf(arg, CLS_Float)) {
		    float value = env->CallFloatMethod(arg, MID_floatValue);
			offset = (offset + __alignof(value) - 1) & ~(__alignof(value) - 1);
			if(cuParamSetv(func, offset, &value, sizeof(value)) != CUDA_SUCCESS) {
				printf("ERROR: cuParamSetv (Async2D)\n");
				return;
			}
			offset += sizeof(value);
			//printf("Float : %f\n", env->CallFloatMethod(arg, MID_floatValue));
		}
		else if(env->IsInstanceOf(arg, CLS_Double)) {
		    double value = env->CallDoubleMethod(arg, MID_doubleValue);
			offset = (offset + __alignof(value) - 1) & ~(__alignof(value) - 1);
			if(cuParamSetv(func, offset, &value, sizeof(value)) != CUDA_SUCCESS) {
				printf("ERROR: cuParamSetv (Async2D)\n");
				return;
			}
			offset += sizeof(value);
			//printf("Double : %f\n", env->CallDoubleMethod(arg, MID_doubleValue));
		}
		else {
			printf("Not Supported Type to pass to GPU.\n");
		}
		i += 1;
	}

	if(cuParamSetSize(func, offset) != CUDA_SUCCESS) {
		printf("ERROR: cuParamSetSize (Async2D)\n");
		return;
	}
	
	if(cuFuncSetBlockShape(func, 32, 16, 1) != CUDA_SUCCESS) {
		printf("ERROR: cuFuncSetBlockShape (Async2D)\n");
		return;
	}
	if(cuLaunchGridAsync(func, 1+(length1D-1)/32, 1+(length2D-1)/16, (CUstream)stream) != CUDA_SUCCESS) {
		printf("ERROR: cuLaunchGrid (Async2D)\n");
		return;
	}
}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaAsyncLaunch1DSpec(JNIEnv *env, jobject obj, jlong func_id, jobject argsList, jint length1D, jint blockDim1D, jlong stream)
{
	int i = 0;
	int offset = 0;
	CUfunction func = (CUfunction)func_id;
	jobject arg;
	int argsSize = env->CallIntMethod(argsList, MID_getArgSize);

	while(i < argsSize) {
		arg = env->CallObjectMethod(argsList, MID_getArg);
		if(env->IsInstanceOf(arg, CLS_Integer)) {
			int value = env->CallIntMethod(arg, MID_intValue);
			offset = (offset + __alignof(value) - 1) & ~(__alignof(value) - 1);
			if(cuParamSeti(func, offset,  value) != CUDA_SUCCESS) {
				printf("ERROR: cuParamSeti (Async1D)\n");
				return;
			}
			offset += sizeof(value);
			//printf("Integer : %d\n", value);
		}
		else if(env->IsInstanceOf(arg, CLS_Long)) {
			long value = env->CallLongMethod(arg, MID_longValue);
			offset = (offset + __alignof(value) - 1) & ~(__alignof(value) - 1);
			if(cuParamSetv(func, offset, &value, sizeof(value)) != CUDA_SUCCESS) {
				printf("ERROR: cuParamSetv (Async1D)\n");
				return;
			}
			offset += sizeof(value);
			//printf("Long : %ld\n", value);
		}
		else if(env->IsInstanceOf(arg, CLS_Float)) {
		    float value = env->CallFloatMethod(arg, MID_floatValue);
			offset = (offset + __alignof(value) - 1) & ~(__alignof(value) - 1);
			if(cuParamSetv(func, offset, &value, sizeof(value)) != CUDA_SUCCESS) {
				printf("ERROR: cuParamSetv (Async1D)\n");
				return;
			}
			offset += sizeof(value);
			//printf("Float : %f\n", env->CallFloatMethod(arg, MID_floatValue));
		}
		else if(env->IsInstanceOf(arg, CLS_Double)) {
		    double value = env->CallDoubleMethod(arg, MID_doubleValue);
			offset = (offset + __alignof(value) - 1) & ~(__alignof(value) - 1);
			if(cuParamSetv(func, offset, &value, sizeof(value)) != CUDA_SUCCESS) {
				printf("ERROR: cuParamSetv (Async1D)\n");
				return;
			}
			offset += sizeof(value);
			//printf("Double : %f\n", env->CallDoubleMethod(arg, MID_doubleValue));
		}
		else {
			printf("Not Supported Type to pass to GPU.\n");
		}
		i += 1;
	}

	if(cuParamSetSize(func, offset) != CUDA_SUCCESS) {
		printf("ERROR: cuParamSetSize (Async1D)\n");
		return;
	}

	if(cuFuncSetBlockShape(func, blockDim1D, 1, 1) != CUDA_SUCCESS) {
		printf("ERROR: cuFuncSetBlockShape (Async1D)\n");
		return;
	}
	if(cuLaunchGridAsync(func, 1+(length1D-1)/blockDim1D, 1, (CUstream)stream) != CUDA_SUCCESS) {
		printf("ERROR: cuLaunchGrid (Async1D)\n");
		return;
	}
}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaAsyncLaunch2DSpec(JNIEnv *env, jobject obj, jlong func_id, jobject argsList, jint length1D, jint length2D, jint blockDim1D, jint blockDim2D, jlong stream)
{
	int i = 0;
	int offset = 0;
	CUfunction func = (CUfunction)func_id;
	jobject arg;
    int argsSize = env->CallIntMethod(argsList, MID_getArgSize);

	while(i < argsSize) {
		arg = env->CallObjectMethod(argsList, MID_getArg);
		if(env->IsInstanceOf(arg, CLS_Integer)) {
			int value = env->CallIntMethod(arg, MID_intValue);
			offset = (offset + __alignof(value) - 1) & ~(__alignof(value) - 1);
			if(cuParamSeti(func, offset,  value) != CUDA_SUCCESS) {
				printf("ERROR: cuParamSeti (Async2D)\n");
				return;
			}
			offset += sizeof(value);
			//printf("Integer : %d\n", value);
		}
		else if(env->IsInstanceOf(arg, CLS_Long)) {
			long value = env->CallLongMethod(arg, MID_longValue);
			offset = (offset + __alignof(value) - 1) & ~(__alignof(value) - 1);
			if(cuParamSetv(func, offset, &value, sizeof(value)) != CUDA_SUCCESS) {
				printf("ERROR: cuParamSetv (Async2D)\n");
				return;
			}
			offset += sizeof(value);
			//printf("Long : %ld\n", value);
		}
		else if(env->IsInstanceOf(arg, CLS_Float)) {
		    float value = env->CallFloatMethod(arg, MID_floatValue);
			offset = (offset + __alignof(value) - 1) & ~(__alignof(value) - 1);
			if(cuParamSetv(func, offset, &value, sizeof(value)) != CUDA_SUCCESS) {
				printf("ERROR: cuParamSetv (Async2D)\n");
				return;
			}
			offset += sizeof(value);
			//printf("Float : %f\n", env->CallFloatMethod(arg, MID_floatValue));
		}
		else if(env->IsInstanceOf(arg, CLS_Double)) {
		    double value = env->CallDoubleMethod(arg, MID_doubleValue);
			offset = (offset + __alignof(value) - 1) & ~(__alignof(value) - 1);
			if(cuParamSetv(func, offset, &value, sizeof(value)) != CUDA_SUCCESS) {
				printf("ERROR: cuParamSetv (Async2D)\n");
				return;
			}
			offset += sizeof(value);
			//printf("Double : %f\n", env->CallDoubleMethod(arg, MID_doubleValue));
		}
		else {
			printf("Not Supported Type to pass to GPU.\n");
		}
		i += 1;
	}

	if(cuParamSetSize(func, offset) != CUDA_SUCCESS) {
		printf("ERROR: cuParamSetSize (Async2D)\n");
		return;
	}

	if(cuFuncSetBlockShape(func, blockDim1D, blockDim2D, 1) != CUDA_SUCCESS) {
		printf("ERROR: cuFuncSetBlockShape (Async2D)\n");
		return;
	}
	if(cuLaunchGridAsync(func, 1+(length1D-1)/blockDim1D, 1+(length2D-1)/blockDim2D, (CUstream)stream) != CUDA_SUCCESS) {
		printf("ERROR: cuLaunchGrid (Async2D)\n");
		return;
	}
}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaAsync3D3I(JNIEnv *env, jobject obj, jlong func_id, jlong a_ptr, jlong b_ptr, jlong c_ptr, jint widthA, jint widthB, jint heightA, jlong stream)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)a_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D3I)\n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)b_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D3I)\n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)c_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D3I)\n");
		return;
	}
	offset += sizeof(ptr);

	offset = (offset + __alignof(widthA) - 1) & ~(__alignof(widthA) - 1);
	if(cuParamSeti(func, offset, widthA) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D3I)\n");
		return;
	}
	offset += sizeof(widthA);

	offset = (offset + __alignof(widthB) - 1) & ~(__alignof(widthB) - 1);
	if(cuParamSeti(func, offset,  widthB) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D3I)\n");
		return;
	}
	offset += sizeof(widthB);

	offset = (offset + __alignof(heightA) - 1) & ~(__alignof(heightA) - 1);
	if(cuParamSeti(func, offset, heightA) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D3I)\n");
		return;
	}
	offset += sizeof(heightA);


	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaAsync3D3I)\n");
		return;
	}

    //TODO : Need to receive dimension info as inputs
    if(widthB == 0) {
        if(cuFuncSetBlockShape(func, 16, 16, 1) != CUDA_SUCCESS)
        {
            printf("ERROR: cuFuncSetBlockShape\n");
            return;
        }
        if(cuLaunchGridAsync(func, 1+(widthA-1)/16, 1+(heightA-1)/16, (CUstream)stream) != CUDA_SUCCESS)
        {
            printf("ERROR: cuLaunchGrid\n");
            return;
        }
    }
    else {
        if(cuFuncSetBlockShape(func, 16, 4, 1) != CUDA_SUCCESS)
        //if(cuFuncSetBlockShape(func, 16, 16, 1) != CUDA_SUCCESS)
        {
            printf("ERROR: cuFuncSetBlockShape\n");
            return;
        }
        //if(cuLaunchGridAsync(func, 1+(widthB-1)/16, 1+(heightA-1)/16, (CUstream)stream) != CUDA_SUCCESS)
        if(cuLaunchGridAsync(func, (widthB+63)/64, (heightA+15)/16, (CUstream)stream) != CUDA_SUCCESS)
        {
            printf("ERROR: cuLaunchGrid\n");
            return;
        }
    }
}


/*
JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaBLAS1D(JNIEnv *env, jobject obj, jlong func_id, jlong a_ptr, jlong b_ptr, jlong c_ptr, jint count)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)a_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaBLAS1D) \n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)b_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1); 
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaBLAS1D) \n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)c_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaBLAS1D) \n");
		return;
	}
	offset += sizeof(ptr);

	offset = (offset + __alignof(count) - 1) & ~(__alignof(count) - 1);
	if(cuParamSeti(func, offset, count) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaBLAS1D) \n");
		return;
	}
	offset += sizeof(count);

	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaBLAS1D) \n");
		return;
	}
	
	//TODO : Need to receive dimension info as inputs
	if(cuFuncSetBlockShape(func, 512, 1, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaBLAS1D)\n");
		return;
	}

	if(cuLaunchGrid(func, 1 + (count-1)/512, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuLaunchGrid (cudaBLAS1D)\n");
		return;
	}

	if(cuCtxSynchronize() != CUDA_SUCCESS)
    {
        printf("ERROR: cudaContextSync (cudaBLAS1D)\n");
        return;
    }
}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaBLAS2D(JNIEnv *env, jobject obj, jlong func_id, jlong a_ptr, jlong b_ptr, jlong c_ptr, jint count)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)a_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaBLAS2D) \n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)b_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1); 
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaBLAS2D) \n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)c_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaBLAS2D) \n");
		return;
	}
	offset += sizeof(ptr);

	offset = (offset + __alignof(count) - 1) & ~(__alignof(count) - 1);
	if(cuParamSeti(func, offset, count) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaBLAS2D) \n");
		return;
	}
	offset += sizeof(count);

	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaBLAS2D) \n");
		return;
	}
	
	//TODO : Need to receive dimension info as inputs
	if(cuFuncSetBlockShape(func, 16, 1, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaBLAS2D)\n");
		return;
	}

	if(cuLaunchGrid(func, 1 + (count-1)/16, 1 + (count-1)/16) != CUDA_SUCCESS)
	{
		printf("ERROR: cuLaunchGrid (cudaBLAS2D)\n");
		return;
	}

	if(cuCtxSynchronize() != CUDA_SUCCESS)
    {
        printf("ERROR: cudaContextSync (cudaBLAS2D)\n");
        return;
    }
}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaBLAS1DAsync(JNIEnv *env, jobject obj, jlong func_id, jlong a_ptr, jlong b_ptr, jlong c_ptr, jint count, jlong stream)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)a_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);

	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaBLAS1DAsync)\n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)b_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaBLAS1DAsync) \n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)c_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaBLAS1DAsync) \n");
		return;
	}
	offset += sizeof(ptr);

	offset = (offset + __alignof(count) - 1) & ~(__alignof(count) - 1);
	if(cuParamSeti(func, offset, count) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaBLAS1DAsync) \n");
		return;
	}
	offset += sizeof(count);

	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaBLAS1DAsync)\n");
		return;
	}

	//TODO : Need to receive dimension info as inputs
	if(cuFuncSetBlockShape(func, 512, 1, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaBLAS1DAsync)\n");
		return;
	}

	if(cuLaunchGridAsync(func, 1+(count-1)/512, 1, (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuLaunchGrid (cudaBLAS1DAsync)\n");
		return;
	}
	
}


JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaBLASRandAsync(JNIEnv *env, jobject obj, jlong func_id, jlong a_ptr, jint count, jlong stream)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)a_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);

	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaBLAS1DAsync)\n");
		return;
	}
	offset += sizeof(ptr);

	offset = (offset + __alignof(count) - 1) & ~(__alignof(count) - 1);
	if(cuParamSeti(func, offset, count) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaBLAS1DAsync) \n");
		return;
	}
	offset += sizeof(count);

	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaBLAS1DAsync)\n");
		return;
	}

	//TODO : Need to receive dimension info as inputs
	if(cuFuncSetBlockShape(func, 512, 1, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaBLAS1DAsync)\n");
		return;
	}

	if(cuLaunchGridAsync(func, 1+(count-1)/512, 1, (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuLaunchGrid (cudaBLAS1DAsync)\n");
		return;
	}

}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaBLAS2DAsync(JNIEnv *env, jobject obj, jlong func_id, jlong a_ptr, jlong b_ptr, jlong c_ptr, jint count, jlong stream)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)a_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaBLAS2DAsync) \n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)b_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1); 
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaBLAS2DAsync) \n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)c_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaBLAS2DAsync) \n");
		return;
	}
	offset += sizeof(ptr);

	offset = (offset + __alignof(count) - 1) & ~(__alignof(count) - 1);
	if(cuParamSeti(func, offset, count) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaBLAS2DAsync) \n");
		return;
	}
	offset += sizeof(count);

	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaBLAS2DAsync) \n");
		return;
	}


	//TODO : Need to receive dimension info as inputs
	if(cuFuncSetBlockShape(func, 16, 1, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaBLAS2DAsync)\n");
		return;
	}

	if(cuLaunchGridAsync(func, 1+(count-1)/16, 1+(count-1)/16, (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuLaunchGrid (cudaBLAS2DAsync)\n");
		return;
	}
	
}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaMapAsync(JNIEnv *env, jobject obj, jlong func_id, jlong a_ptr, jlong b_ptr, jlong c_ptr, jint width, jint height, jint compareWith, jdouble weightedspamcount, jlong stream)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)a_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaMapAsync)\n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)b_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1); 
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaMapAsync)\n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)c_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaMapAsync)\n");
		return;
	}
	offset += sizeof(ptr);

	offset = (offset + __alignof(width) - 1) & ~(__alignof(width) - 1);
	if(cuParamSeti(func, offset, width) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaMapAsync)\n");
		return;
	}
	offset += sizeof(width);
	
	offset = (offset + __alignof(height) - 1) & ~(__alignof(height) - 1);
	if(cuParamSeti(func, offset, height) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaMapAsync)\n");
		return;
	}
	offset += sizeof(height);
	
	offset = (offset + __alignof(compareWith) - 1) & ~(__alignof(compareWith) - 1);
	if(cuParamSeti(func, offset, compareWith) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaMapAsync)\n");
		return;
	}
	offset += sizeof(compareWith);

	offset = (offset + __alignof(weightedspamcount) - 1) & ~(__alignof(weightedspamcount) - 1);
	if(cuParamSetv(func, offset, &weightedspamcount, sizeof(weightedspamcount)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaMapAsync)\n");
		return;
	}
	offset += sizeof(weightedspamcount);


	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaMapAsync)\n");
		return;
	}


	//TODO : Need to receive dimension info as inputs
	if(cuFuncSetBlockShape(func, 16, 16, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaMapAsync)\n");
		return;
	}

	if(cuLaunchGridAsync(func, 1+(width-1)/16, 1, (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuLaunchGrid (cudaMapAsync)\n");
		return;
	}
}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaMapToVecAsync(JNIEnv *env, jobject obj, jlong func_id, jlong a_ptr, jlong b_ptr, jint width, jint height, jlong stream)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)a_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaMapToVecAsync)\n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)b_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1); 
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaMapToVecAsync)\n");
		return;
	}
	offset += sizeof(ptr);

	offset = (offset + __alignof(width) - 1) & ~(__alignof(width) - 1);
	if(cuParamSeti(func, offset, width) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaMapToVecAsync)\n");
		return;
	}
	offset += sizeof(width);
	
	offset = (offset + __alignof(height) - 1) & ~(__alignof(height) - 1);
	if(cuParamSeti(func, offset, height) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaMapToVecAsync)\n");
		return;
	}
	offset += sizeof(height);
	
	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaMapToVecAsync)\n");
		return;
	}


	//TODO : Need to receive dimension info as inputs
	if(cuFuncSetBlockShape(func, 256, 1, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaMapToVecAsync)\n");
		return;
	}

	if(cuLaunchGridAsync(func, 1+(width-1)/256, 1, (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuLaunchGrid (cudaMapToVecAsync)\n");
		return;
	}
	
}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaMapLRAsync(JNIEnv *env, jobject obj, jlong func_id, jlong a_ptr, jlong b_ptr, jdouble x_cur, jint tau, jint count, jlong stream)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)a_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaMapLRAsync)\n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)b_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1); 
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaMapLRAsync)\n");
		return;
	}
	offset += sizeof(ptr);

	offset = (offset + __alignof(x_cur) - 1) & ~(__alignof(x_cur) - 1);
	if(cuParamSetv(func, offset, &x_cur, sizeof(x_cur)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaMapLRAsync)\n");
		return;
	}
	offset += sizeof(x_cur);
	
	offset = (offset + __alignof(tau) - 1) & ~(__alignof(tau) - 1);
	if(cuParamSeti(func, offset, tau) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaMapLRAsync)\n");
		return;
	}
	offset += sizeof(tau);
	
	offset = (offset + __alignof(count) - 1) & ~(__alignof(count) - 1);
	if(cuParamSeti(func, offset, count) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaMapLRAsync)\n");
		return;
	}
	offset += sizeof(count);
	
	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaMapLRAsync)\n");
		return;
	}


	//TODO : Need to receive dimension info as inputs
	if(cuFuncSetBlockShape(func, 32, 1, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaMapLRAsync)\n");
		return;
	}

	if(cuLaunchGridAsync(func, 1+(count-1)/32, 1, (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuLaunchGrid (cudaMapLRAsync)\n");
		return;
	}
}


JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaAsync3D2I(JNIEnv *env, jobject obj, jlong func_id, jlong a_ptr, jlong b_ptr, jlong c_ptr, jint width, jint height, jint numTB, jlong stream)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)a_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D2I)\n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)b_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1); 
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D2I)\n");
		return;
	}
	offset += sizeof(ptr);
	
	ptr = (void*)(size_t)c_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1); 
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D2I)\n");
		return;
	}
	offset += sizeof(ptr);

	offset = (offset + __alignof(width) - 1) & ~(__alignof(width) - 1);
	if(cuParamSeti(func, offset, width) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D2I)\n");
		return;
	}
	offset += sizeof(width);
	
	offset = (offset + __alignof(height) - 1) & ~(__alignof(height) - 1);
	if(cuParamSeti(func, offset, height) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D2I)\n");
		return;
	}
	offset += sizeof(height);
	
	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaAsync3D2I)\n");
		return;
	}


	//TODO : Need to receive dimension info as inputs
	if(cuFuncSetBlockShape(func, 256, 1, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaAsync3D2I)\n");
		return;
	}

	if(cuLaunchGridAsync(func, 1+(numTB-1)/256, 1, (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuLaunchGrid (cudaAsync3D2I)\n");
		return;
	}
}


JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaAsync2D1D1I(JNIEnv *env, jobject obj, jlong func_id, jlong a_ptr, jlong b_ptr, jdouble dvalue, jint length, jlong stream)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)a_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync2D1D1I) \n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)b_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1); 
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync2D1D1I) \n");
		return;
	}
	offset += sizeof(ptr);
	
	offset = (offset + __alignof(dvalue) - 1) & ~(__alignof(dvalue) - 1);
	if(cuParamSetv(func, offset, &dvalue, sizeof(dvalue)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync2D1D1I)\n");
		return;
	}
	offset += sizeof(dvalue);
	
	offset = (offset + __alignof(length) - 1) & ~(__alignof(length) - 1);
	if(cuParamSeti(func, offset, length) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync2D1D1I)\n");
		return;
	}
	offset += sizeof(length);
	
	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaAsync2D1D1I)\n");
		return;
	}


	//TODO : Need to receive dimension info as inputs
	if(cuFuncSetBlockShape(func, 256, 1, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaAsync2D1D1I)\n");
		return;
	}

	if(cuLaunchGridAsync(func, 1+(length-1)/256, 1, (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuLaunchGrid (cudaAsync2D1D1I)\n");
		return;
	}
	
}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaAsync2D2I2Dim(JNIEnv *env, jobject obj, jlong func_id, jlong a_ptr, jlong b_ptr, jint width, jint height, jlong stream)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)a_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync2D2I2Dim)\n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)b_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1); 
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync2D2I2Dim)\n");
		return;
	}
	offset += sizeof(ptr);
	
	offset = (offset + __alignof(width) - 1) & ~(__alignof(width) - 1);
	if(cuParamSeti(func, offset, width) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync2D2I2Dim)\n");
		return;
	}
	offset += sizeof(width);
	
	offset = (offset + __alignof(height) - 1) & ~(__alignof(height) - 1);
	if(cuParamSeti(func, offset, height) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync2D2I2Dim)\n");
		return;
	}
	offset += sizeof(height);
	
	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaAsync2D2I2Dim)\n");
		return;
	}


	//TODO : Need to receive dimension info as inputs
	if(cuFuncSetBlockShape(func, 16, 16, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaAsync2D2I2Dim)\n");
		return;
	}

	if(cuLaunchGridAsync(func, 1+(width-1)/16, 1+(height-1)/16, (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuLaunchGrid (cudaAsync2D2I2Dim)\n");
		return;
	}
	
}

*/
/*******************************
** RBM calls
*********************************/
/*
JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaAsyncRBM_11I(JNIEnv *env, jobject obj, jlong func_id, jlong a_ptr, jlong c_ptr, jint length, jlong stream)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)a_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_11I)\n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)c_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_11I)\n");
		return;
	}
	offset += sizeof(ptr);

	offset = (offset + __alignof(length) - 1) & ~(__alignof(length) - 1);
	if(cuParamSeti(func, offset, length) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_11I)\n");
		return;
	}
	offset += sizeof(length);

	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaAsyncRBM_11I)\n");
		return;
	}
	
	if(cuFuncSetBlockShape(func, 512, 1, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaAsyncRBM_11I)\n");
		return;
	}

	if(cuLaunchGridAsync(func, 1+(length-1)/512, 1, (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuLaunchGrid (cudaAsyncRBM_11I)\n");
		return;
	}
}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaAsyncRBM_11I1S(JNIEnv *env, jobject obj, jlong func_id, jlong a_ptr, jlong c_ptr, jdouble svalue, jint length, jlong stream)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)a_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_11I1S)\n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)c_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_11I1S)\n");
		return;
	}
	offset += sizeof(ptr);

    offset = (offset + __alignof(svalue) - 1) & ~(__alignof(svalue) - 1);
	if(cuParamSetv(func, offset, &svalue, sizeof(svalue)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_11I1S)\n");
		return;
	}
	offset += sizeof(svalue);

	offset = (offset + __alignof(length) - 1) & ~(__alignof(length) - 1);
	if(cuParamSeti(func, offset, length) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_11I1S)\n");
		return;
	}
	offset += sizeof(length);

	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaAsyncRBM_11I1S)\n");
		return;
	}

	if(cuFuncSetBlockShape(func, 512, 1, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaAsyncRBM_11I1S)\n");
		return;
	}

	if(cuLaunchGridAsync(func, 1+(length-1)/512, 1, (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuLaunchGrid (cudaAsyncRBM_11I1S)\n");
		return;
	}
}
                  
JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaAsyncRBM_11I1S_1Float(JNIEnv *env, jobject obj, jlong func_id, jlong a_ptr, jlong c_ptr, jfloat svalue, jint length, jlong stream)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)a_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_11I1S)\n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)c_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_11I1S)\n");
		return;
	}
	offset += sizeof(ptr);

    offset = (offset + __alignof(svalue) - 1) & ~(__alignof(svalue) - 1);
	if(cuParamSetv(func, offset, &svalue, sizeof(svalue)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_11I1S)\n");
		return;
	}
	offset += sizeof(svalue);

	offset = (offset + __alignof(length) - 1) & ~(__alignof(length) - 1);
	if(cuParamSeti(func, offset, length) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_11I1S)\n");
		return;
	}
	offset += sizeof(length);

	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaAsyncRBM_11I1S)\n");
		return;
	}

	if(cuFuncSetBlockShape(func, 512, 1, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaAsyncRBM_11I1S)\n");
		return;
	}

	if(cuLaunchGridAsync(func, 1+(length-1)/512, 1, (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuLaunchGrid (cudaAsyncRBM_11I1S)\n");
		return;
	}
}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaAsyncRBM_12I(JNIEnv *env, jobject obj, jlong func_id, jlong a_ptr, jlong b_ptr, jlong c_ptr, jint length, jlong stream)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)a_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_12I)\n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)b_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_12I)\n");
		return;
	}
	offset += sizeof(ptr);

    ptr = (void*)(size_t)c_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_12I)\n");
		return;
	}
	offset += sizeof(ptr);

	offset = (offset + __alignof(length) - 1) & ~(__alignof(length) - 1);
	if(cuParamSeti(func, offset, length) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_12I)\n");
		return;
	}
	offset += sizeof(length);

	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaAsyncRBM_12I)\n");
		return;
	}

	if(cuFuncSetBlockShape(func, 512, 1, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaAsyncRBM_12I)\n");
		return;
	}

	if(cuLaunchGridAsync(func, 1+(length-1)/512, 1, (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuLaunchGrid (cudaAsyncRBM_12I)\n");
		return;
	}
}


JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaAsyncRBM_1Repmat(JNIEnv *env, jobject obj, jlong func_id, jlong a_ptr, jlong c_ptr, jint length, int repRow, int repCol, jlong stream)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)a_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_1Repmat)\n");
		return;
	}
	offset += sizeof(ptr);

    ptr = (void*)(size_t)c_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_1Repmat)\n");
		return;
	}
	offset += sizeof(ptr);

	offset = (offset + __alignof(length) - 1) & ~(__alignof(length) - 1);
	if(cuParamSeti(func, offset, length) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_1Repmat)\n");
		return;
	}
	offset += sizeof(length);

    offset = (offset + __alignof(repRow) - 1) & ~(__alignof(repRow) - 1);
	if(cuParamSeti(func, offset, repRow) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_1Repmat)\n");
		return;
	}
	offset += sizeof(repRow);

	offset = (offset + __alignof(repCol) - 1) & ~(__alignof(repCol) - 1);
	if(cuParamSeti(func, offset, repCol) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_1Repmat)\n");
		return;
	}
	offset += sizeof(repCol);

	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaAsyncRBM_1Repmat)\n");
		return;
	}

	if(cuFuncSetBlockShape(func, 512, 1, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaAsyncRBM_1Repmat)\n");
		return;
	}

	if(cuLaunchGridAsync(func, 1+(length*repCol-1)/512, repRow, (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuLaunchGrid (cudaAsyncRBM_1Repmat)\n");
		return;
	}
}


JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaAsyncRBM_11I2D(JNIEnv *env, jobject obj, jlong func_id, jlong a_ptr, jlong c_ptr, jint numRows, jint numCols, jlong stream)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)a_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_11I2D)\n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)c_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_11I2D)\n");
		return;
	}
	offset += sizeof(ptr);

	offset = (offset + __alignof(numRows) - 1) & ~(__alignof(numRows) - 1);
	if(cuParamSeti(func, offset, numRows) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_11I2D)\n");
		return;
	}
	offset += sizeof(numRows);

	offset = (offset + __alignof(numCols) - 1) & ~(__alignof(numCols) - 1);
	if(cuParamSeti(func, offset, numCols) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_11I2D)\n");
		return;
	}
	offset += sizeof(numCols);

	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaAsyncRBM_11I2D)\n");
		return;
	}

	if(cuFuncSetBlockShape(func, 512, 1, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaAsyncRBM_11I2D)\n");
		return;
	}

	if(cuLaunchGridAsync(func, 1+(numCols-1)/512, 1, (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuLaunchGrid (cudaAsyncRBM_11I2D)\n");
		return;
	}
}


JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaAsyncRBM_11I3D(JNIEnv *env, jobject obj, jlong func_id, jlong a_ptr, jlong b_ptr, jlong c_ptr, jint numRows, jint numCols, jlong stream)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)a_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_11I3D)\n");
		return;
	}
	offset += sizeof(ptr);

    ptr = (void*)(size_t)b_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_11I3D)\n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)c_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_11I3D)\n");
		return;
	}
	offset += sizeof(ptr);

	offset = (offset + __alignof(numRows) - 1) & ~(__alignof(numRows) - 1);
	if(cuParamSeti(func, offset, numRows) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_11I3D)\n");
		return;
	}
	offset += sizeof(numRows);

	offset = (offset + __alignof(numCols) - 1) & ~(__alignof(numCols) - 1);
	if(cuParamSeti(func, offset, numCols) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncRBM_11I3D)\n");
		return;
	}
	offset += sizeof(numCols);

	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaAsyncRBM_11I3D)\n");
		return;
	}

	if(cuFuncSetBlockShape(func, 512, 1, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaAsyncRBM_11I3D)\n");
		return;
	}

	if(cuLaunchGridAsync(func, 1+(numCols-1)/512, 1, (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuLaunchGrid (cudaAsyncRBM_11I3D)\n");
		return;
	}
}
*/

/*
// K-means
JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaAsyncKM1(JNIEnv *env, jobject obj, jlong func_id, jlong x_ptr, jlong mu_ptr, jlong out_ptr, jint x_numRows, jint x_numCols, jint mu_numRows, jint mu_numCols, jlong stream)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)x_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncKM)\n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)mu_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncKM)\n");
		return;
	}
	offset += sizeof(ptr);

    ptr = (void*)(size_t)out_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncKM)\n");
		return;
	}
	offset += sizeof(ptr);

	offset = (offset + __alignof(x_numRows) - 1) & ~(__alignof(x_numRows) - 1);
	if(cuParamSeti(func, offset, x_numRows) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncKM)\n");
		return;
	}
	offset += sizeof(x_numRows);

	offset = (offset + __alignof(x_numCols) - 1) & ~(__alignof(x_numCols) - 1);
	if(cuParamSeti(func, offset, x_numCols) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncKM)\n");
		return;
	}
	offset += sizeof(x_numCols);

	offset = (offset + __alignof(mu_numRows) - 1) & ~(__alignof(mu_numRows) - 1);
	if(cuParamSeti(func, offset, mu_numRows) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncKM)\n");
		return;
	}
	offset += sizeof(mu_numRows);

	offset = (offset + __alignof(mu_numCols) - 1) & ~(__alignof(mu_numCols) - 1);
	if(cuParamSeti(func, offset, mu_numCols) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncKM)\n");
		return;
	}
	offset += sizeof(mu_numCols);

	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaAsyncKM)\n");
		return;
	}


	//if(cuFuncSetBlockShape(func, x_numCols, 1, 1) != CUDA_SUCCESS)
	//{
	//	printf("ERROR: cuFuncSetBlockShape (cudaAsyncKM)\n");
	//	return;
	//}
	//if(cuLaunchGridAsync(func, 1, x_numRows, (CUstream)stream) != CUDA_SUCCESS)
	//{
	//	printf("ERROR: cuLaunchGrid (cudaAsyncKM)\n");
	//	return;
	//}
	int dimy = 512/x_numCols;

	if(cuFuncSetBlockShape(func, x_numCols, dimy, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaAsyncKM)\n");
		return;
	}
	if(cuLaunchGridAsync(func, 1, 1+(x_numRows-1)/dimy, (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuLaunchGrid (cudaAsyncKM)\n");
		return;
	}
}

*/
/*
JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaAsyncKM2(JNIEnv *env, jobject obj, jlong func_id, jlong x_ptr, jlong mu_ptr, jlong c_ptr, jint x_numRows, jint x_numCols, jint mu_numRows, jlong stream)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)x_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncKM2)\n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)mu_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncKM2)\n");
		return;
	}
	offset += sizeof(ptr);

    ptr = (void*)(size_t)c_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncKM2)\n");
		return;
	}
	offset += sizeof(ptr);

	offset = (offset + __alignof(x_numRows) - 1) & ~(__alignof(x_numRows) - 1);
	if(cuParamSeti(func, offset, x_numRows) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncKM2)\n");
		return;
	}
	offset += sizeof(x_numRows);

	offset = (offset + __alignof(x_numCols) - 1) & ~(__alignof(x_numCols) - 1);
	if(cuParamSeti(func, offset, x_numCols) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncKM2)\n");
		return;
	}
	offset += sizeof(x_numCols);

	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaAsyncKM2)\n");
		return;
	}

	if(cuFuncSetBlockShape(func, x_numCols, 1, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaAsyncKM2)\n");
		return;
	}

    //printf("Num rows %d",x_numRows);

	if(cuLaunchGridAsync(func, 1, mu_numRows, (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuLaunchGrid (cudaAsyncKM2)\n");
		return;
	}
}


JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaAsyncKM3(JNIEnv *env, jobject obj, jlong func_id, jlong a_ptr, jlong b_ptr, jlong c_ptr, jint numRows, jint numCols, jlong stream)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)a_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncKM3)\n");
		return;
	}
	offset += sizeof(ptr);

    ptr = (void*)(size_t)b_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncKM3)\n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)c_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncKM3)\n");
		return;
	}
	offset += sizeof(ptr);

	offset = (offset + __alignof(numCols) - 1) & ~(__alignof(numCols) - 1);
	if(cuParamSeti(func, offset, numCols) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsyncKM3)\n");
		return;
	}
	offset += sizeof(numCols);

	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaAsyncKM3)\n");
		return;
	}

	if(cuFuncSetBlockShape(func, 512, 1, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaAsyncKM3)\n");
		return;
	}

	if(cuLaunchGridAsync(func, 1+(numCols-1)/512, numRows, (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuLaunchGrid (cudaAsyncKM3)\n");
		return;
	}
}


JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaAsyncMdotV(JNIEnv *env, jobject obj, jlong func_id, jlong a_ptr, jlong b_ptr, jlong c_ptr, jint height, jint width, jlong stream)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)a_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D2I)\n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)b_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D2I)\n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)c_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D2I)\n");
		return;
	}
	offset += sizeof(ptr);

	offset = (offset + __alignof(width) - 1) & ~(__alignof(width) - 1);
	if(cuParamSeti(func, offset, width) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D2I)\n");
		return;
	}
	offset += sizeof(width);

	offset = (offset + __alignof(height) - 1) & ~(__alignof(height) - 1);
	if(cuParamSeti(func, offset, height) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D2I)\n");
		return;
	}
	offset += sizeof(height);

	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaAsync3D2I)\n");
		return;
	}

	//TODO : Need to receive dimension info as inputs
	if(cuFuncSetBlockShape(func, 4, 128, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaAsync3D2I)\n");
		return;
	}

	if(cuLaunchGridAsync(func, 1, 1+(height-1)/128, (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuLaunchGrid (cudaAsync3D2I)\n");
		return;
	}
}


JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaAsyncMatDotV(JNIEnv *env, jobject obj, jlong func_id, jlong a_ptr, jlong b_ptr, jlong c_ptr, jint width, jint height, jlong stream)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)a_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D2I)\n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)b_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1); 
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D2I)\n");
		return;
	}
	offset += sizeof(ptr);
	
	ptr = (void*)(size_t)c_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1); 
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D2I)\n");
		return;
	}
	offset += sizeof(ptr);

	offset = (offset + __alignof(width) - 1) & ~(__alignof(width) - 1);
	if(cuParamSeti(func, offset, width) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D2I)\n");
		return;
	}
	offset += sizeof(width);
	
	offset = (offset + __alignof(height) - 1) & ~(__alignof(height) - 1);
	if(cuParamSeti(func, offset, height) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D2I)\n");
		return;
	}
	offset += sizeof(height);
	
	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaAsync3D2I)\n");
		return;
	}


	//TODO : Need to receive dimension info as inputs
	if(cuFuncSetBlockShape(func, 256, 1, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaAsync3D2I)\n");
		return;
	}

	if(cuLaunchGridAsync(func, 1+(width-1)/256, height, (CUstream)stream) != CUDA_SUCCESS)
	{
		printf("ERROR: cuLaunchGrid (cudaAsync3D2I)\n");
		return;
	}
}

JNIEXPORT void JNICALL Java_ppl_delite_cuda_DeliteCudaDriver_cudaAsync3D3IReg(JNIEnv *env, jobject obj, jlong func_id, jlong a_ptr, jlong b_ptr, jlong c_ptr, jint widthA, jint widthB, jint heightA, jlong stream)
{
	int offset = 0;
	void* ptr;
	CUfunction func = (CUfunction)func_id;

	ptr = (void*)(size_t)a_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1);
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D3I)\n");
		return;
	}
	offset += sizeof(ptr);

	ptr = (void*)(size_t)b_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1); 
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D3I)\n");
		return;
	}
	offset += sizeof(ptr);
	
	ptr = (void*)(size_t)c_ptr;
	offset = (offset + __alignof(ptr) - 1) & ~(__alignof(ptr) - 1); 
	if(cuParamSetv(func, offset, &ptr, sizeof(ptr)) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D3I)\n");
		return;
	}
	offset += sizeof(ptr);

	offset = (offset + __alignof(widthA) - 1) & ~(__alignof(widthA) - 1);
	if(cuParamSeti(func, offset, widthA) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D3I)\n");
		return;
	}
	offset += sizeof(widthA);
	
	offset = (offset + __alignof(widthB) - 1) & ~(__alignof(widthB) - 1);
	if(cuParamSeti(func, offset,  widthB) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D3I)\n");
		return;
	}
	offset += sizeof(widthB);
	
	offset = (offset + __alignof(heightA) - 1) & ~(__alignof(heightA) - 1);
	if(cuParamSeti(func, offset, heightA) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetv (cudaAsync3D3I)\n");
		return;
	}
	offset += sizeof(heightA);

	
	if(cuParamSetSize(func, offset) != CUDA_SUCCESS)
	{
		printf("ERROR: cuParamSetSize (cudaAsync3D3I)\n");
		return;
	}


	//TODO : Need to receive dimension info as inputs
	if(cuFuncSetBlockShape(func, 16, 16, 1) != CUDA_SUCCESS)
	{
		printf("ERROR: cuFuncSetBlockShape (cudaAsync3D3I)\n");
		return;
	}

	if(widthB == 0) {
	    if(cuLaunchGridAsync(func, 1+(widthA-1)/16, 1+(heightA-1)/16, (CUstream)stream) != CUDA_SUCCESS)
	    {
		    printf("ERROR: cuLaunchGrid (cudaAsync3D3I)\n");
		    return;
	    }
	}
	else {
        if(cuLaunchGridAsync(func, 1+(widthB-1)/16, 1+(heightA-1)/16, (CUstream)stream) != CUDA_SUCCESS)
	    {
		    printf("ERROR: cuLaunchGrid (cudaAsync3D3I)\n");
		    return;
	    }
	}
}
*/	
