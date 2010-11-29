package ppl.delite.cuda;

public class DeliteCudaDriver {

        /*
        private static boolean init = false;
        static {
            if(!init) {
                System.load("/tmp/cuda/libDeliteCudaDriver.so");
                DeliteCudaDriver.cudaInit();
                init = true;
            }
        }
        */

        // Global device init (cudaInit.cu)
        public native static void cudaInit();
    
		// Cuda Initialization calls (cudaInit.cu)
		public native long cudaDevSet(int devIdx);
        public native void cudaDestroyContext(int devIdx);
		public native int cudaGetDevNum();
		public native long cudaCreateStream();
		public native void cudaDestroyStream(long stream);
		public native void cudaStreamSync(long stream);
		public native long cudaGetFunction(long module, String str);

		// Memory allocation related calls (cudaMem.cu)
		public native long cudaMemAlloc(int size);
		public native void cudaMemFree(long devPtr);
		public native long cudaMemAllocHost(int size);
		public native long cudaMemHostGetDevicePointer(long hostptr);

		// Asynchronous Memory copy operations (cudaMem.cu)
		public native void cudaMemCpyHtoDAsync(long devPtr, double[] hostArr, int offset, int count, long hostmem, long stream);
		public native void cudaMemCpyDtoHAsync(double[] hostArr, long devPtr, int offset, int count, long hostmem, long stream);
		public native void cudaMemCpyHtoDAsyncFloat(long devPtr, float[] hostArr, int offset, int count, long hostmem, long stream);
		public native void cudaMemCpyDtoHAsyncFloat(float[] hostArr, long devPtr, int offset, int count, long hostmem, long stream);
        public native void cudaMemCpyHtoDAsyncInt(long devPtr, int[] hostArr, int offset, int count, long hostmem, long stream);
        public native void cudaMemCpyDtoHAsyncInt(int[] hostArr, long devPtr, int offset, int count, long hostmem, long stream);
		public native void cudaMemCpyDtoD(long dstPtr, long srcPtr, int size);

        // GPU kernel launching functions (cudaBLAS.cu)
        public native void initIDs();
        public native void cudaAsyncLaunch1D(long func_idx, java.lang.Object argsList, int length1D, long stream);
        public native void cudaAsyncLaunch2D(long func_idx, java.lang.Object argsList, int length1D, int length2D, long stream);
        public native void cudaAsyncLaunch1DSpec(long func_idx, java.lang.Object argsList, int length1D, int blockDim1D, long stream);
        public native void cudaAsyncLaunch2DSpec(long func_idx, java.lang.Object argsList, int length1D, int length2D, int blockDim1D, int blockDim2D, long stream);
		public native void cudaAsync3D3I(long func_idx, long a_ptr, long b_ptr, long c_ptr, int widthA, int widthB, int heightA, long stream);
        public native void cudaAsync3D3IReg(long func_idx, long a_ptr, long b_ptr, long c_ptr, int widthA, int widthB, int heightA, long stream);

		// Kernel synchronization operations (cudaSched.cu)
		public native void cudaSched(long func_idx, long cnt_ptr, int count, long stream);
		public native int readDevTimeStamp(long TS_ptr);

    	//long PageLockedMem_Out;
        //long[] stream;

        // Deprecated Kernel Launchers
        //RBM kernel operations
        //public native void cudaAsyncRBM_1I(long func_idx, long a_ptr, long c_ptr, int length, long stream);
        //public native void cudaAsyncRBM_1I1S(long func_idx, long a_ptr, long c_ptr, double svalue, int length, long stream);
        //public native void cudaAsyncRBM_1I1S_Float(long func_idx, long a_ptr, long c_ptr, float svalue, int length, long stream);
        //public native void cudaAsyncRBM_2I(long func_idx, long a_ptr, long b_ptr, long c_ptr, int length, long stream);
        //public native void cudaAsyncRBM_Repmat(long func_idx, long a_ptr, long c_ptr, int length, int repRow, int repCol, long stream);
        //public native void cudaAsyncRBM_1I2D(long func_idx, long a_ptr, long c_ptr, int numRows, int numCols, long stream);
        //public native void cudaAsyncRBM_1I3D(long func_idx, long a_ptr, long b_ptr, long c_ptr, int numRows, int numCols, long stream);
        
        //K-means kernel
        //public native void cudaAsyncKM1(long func_idx, long x_ptr, long mu_ptr, long out_ptr, int x_numRows, int x_numCols, int mu_numRows, int mu_numCols, long stream);
        //public native void cudaAsyncKM2(long func_idx, long x_ptr, long mu_ptr, long c_ptr, int x_numRows, int x_numCols, int mu_numRows, long stream);
        //public native void cudaAsyncKM3(long func_idx, long x_ptr, long mu_ptr, long c_ptr, int mu_numRows, int mu_numCols, long stream);

        //public native void cudaAsyncMdotV(long func_idx, long x_ptr, long mu_ptr, long c_ptr, int mu_numRows, int mu_numCols, long stream);
		//public native void cudaAsyncMatDotV(long func_idx, long a_ptr, long b_ptr, long c_ptr, int width, int height, long stream);
		//public native void cudaAsync2D1D1I(long func_idx, long a_ptr, long b_ptr, double dvalue, int length, long stream);
		//public native void cudaAsync2D2I2Dim(long func_idx, long a_ptr, long b_ptr, int width, int height, long stream);

		//public native void cudaBLAS1D(long func_idx, long a_ptr, long b_ptr, long c_ptr, int count);
		//public native void cudaBLAS2D(long func_idx, long a_ptr, long b_ptr, long c_ptr, int count);
		//public native void cudaBLAS1DAsync(long func_idx, long a_ptr, long b_ptr, long c_ptr, int count, long stream);
        //public native void cudaBLASRandAsync(long func_idx, long a_ptr, int count, long stream);
		//public native void cudaBLAS2DAsync(long func_idx, long a_ptr, long b_ptr, long c_ptr, int count, long stream);

		//public native void cudaMapAsync(long func_idx, long a_ptr, long b_ptr, long c_ptr, int width, int height, int compareWith, double weightedspamcount, long stream);
		//public native void cudaMapToVecAsync(long func_idx, long a_ptr, long b_ptr, int width, int height, long stream);
		//public native void cudaMapLRAsync(long func_idx, long a_ptr, long b_ptr, double x_cur, int tau, int count, long stream);
        //public native void cudaAsync3D2I(long func_idx, long a_ptr, long b_ptr, long c_ptr, int width, int height, int numTB, long stream);

        // Deprecated Memory copy operations
		//public native void cudaMemCpyHtoD(long devPtr, double[] hostArr, int offset, int count);
		//public native void cudaMemCpyDtoH(double[] hostArr, long devPtr, int offset, int count);
		//public native void cudaMemCpyHtoDNative(long devPtr, long hostArr, int offset, int count);
		//public native void cudaMemCpyDtoHNative(long hostArr, long devPtr, int offset, int count);
}
