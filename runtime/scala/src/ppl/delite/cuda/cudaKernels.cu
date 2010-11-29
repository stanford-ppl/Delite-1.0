#include "matMultDouble.cu"
#include "matMultFloat.cu"
#include "matrixMul_kernel.cu"

__global__ void vectPlusDouble(double *A, double *B, double *C, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;
	
	while (i < size)
	{
		C[i] = A[i] + B[i];
		i += blockDim.x * gridDim.x;
	}

	__syncthreads();
}

__global__ void vectPlusFloat(float *A, float *B, float *C, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;
	
	while (i < size)
	{
		C[i] = A[i] + B[i];
		i += blockDim.x * gridDim.x;
	}

	__syncthreads();
}

__global__ void vectMinusDouble(double *A, double *B, double *C, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	while (i < size)
	{
		C[i] = A[i] - B[i];
		i += blockDim.x * gridDim.x;
	}

	__syncthreads();
}

__global__ void vectMinusFloat(float *A, float *B, float *C, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	while (i < size)
	{
		C[i] = A[i] - B[i];
		i += blockDim.x * gridDim.x;
	}

	__syncthreads();
}

__global__ void vectMultDouble(double *A, double *B, double *C, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	while (i < size)
	{
		C[i] = A[i] * B[i];
		i += blockDim.x * gridDim.x;
	}

	__syncthreads();
}

__global__ void vectMultFloat(float *A, float *B, float *C, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	while (i < size)
	{
		C[i] = A[i] * B[i];
		i += blockDim.x * gridDim.x;
	}

	__syncthreads();
}

__global__ void vectMultInt(int *A, int *B, int *C, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	while (i < size)
	{
		C[i] = A[i] * B[i];
		i += blockDim.x * gridDim.x;
	}

	__syncthreads();
}

__global__ void vectDivDouble(double *A, double *B, double *C, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	while (i < size)
	{
		C[i] = A[i] / B[i];
		i += blockDim.x * gridDim.x;
	}

	__syncthreads();
}

__global__ void vectDivFloat(float *A, float *B, float *C, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	while (i < size)
	{
		C[i] = A[i] / B[i];
		i += blockDim.x * gridDim.x;
	}

	__syncthreads();
}

__global__ void vectMoveDouble(double *A, double *C, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;
	
	while (i < size)
	{
		C[i] = A[i];
		i += blockDim.x * gridDim.x;
	}

	__syncthreads();
}

__global__ void vectMoveFloat(float *A, float *C, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;
	
	while (i < size)
	{
		C[i] = A[i];
		i += blockDim.x * gridDim.x;
	}

	__syncthreads();
}

__global__ void vectMoveInt(int *A, int *B, int *C, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	while (i < size)
	{
		C[i] = A[i];
		i += blockDim.x * gridDim.x;
	}

	__syncthreads();
}

__global__ void vectGTDouble(double *A, double *B, double *C, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	if (i < size) {
	    if(A[i] > B[i])
	        C[i] = 1;
	    else
	        C[i] = 0;
	}

	__syncthreads();
}

__global__ void vectGTFloat(float *A, float *B, float *C, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	if (i < size) {
	    if(A[i] > B[i])
	        C[i] = 1;
	    else
	        C[i] = 0;
	}

	__syncthreads();
}

__global__ void vectEQDouble(double *A, double *B, double *C, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	if (i < size) {
	    if(A[i] == B[i])
	        C[i] = 1;
	    else
	        C[i] = 0;
	}

	__syncthreads();
}

__global__ void vectEQFloat(float *A, float *B, float *C, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	if (i < size) {
	    if(A[i] == B[i])
	        C[i] = 1;
	    else
	        C[i] = 0;
	}

	__syncthreads();
}

__global__ void vectRecipDouble(double *A, double *C, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	if (i < size)
	    C[i] = 1 / A[i];

	__syncthreads();
}

__global__ void vectRecipFloat(float *A, float *C, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	if (i < size)
	    C[i] = 1 / A[i];

	__syncthreads();
}

__global__ void vectExpDouble(double *A, double *C, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	if (i < size)
	    C[i] = exp(A[i]);

	__syncthreads();
}

__global__ void vectExpFloat(float *A, float *C, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	if (i < size)
	    C[i] = exp(A[i]);

	__syncthreads();
}


__global__ void matOuterDouble(double *A, double *B, double *C, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;
	int j = 0;

	__shared__ double S_A[16];
	__shared__ double S_B[16];

	S_A[threadIdx.x] = A[i];
	S_B[threadIdx.x] = B[blockIdx.y*16+threadIdx.x];

	__syncthreads();

	for(j=0; j<16; j++) {
		C[i+(j+blockIdx.y*16)*size] = S_A[threadIdx.x] * S_B[j];
	}

	__syncthreads();
}

__global__ void matOuterFloat(float *A, float *B, float *C, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;
	int j = 0;

	__shared__ float S_A[16];
	__shared__ float S_B[16];

	S_A[threadIdx.x] = A[i];
	S_B[threadIdx.x] = B[blockIdx.y*16+threadIdx.x];
	
	__syncthreads();

	for(j=0; j<16; j++) {
		C[i+(j+blockIdx.y*16)*size] = S_A[threadIdx.x] * S_B[j];
	}

	__syncthreads();
}


/* 
 * preKernel 
 * Wait until(busy waiting) the counter(epoch) becomes desired value
 */
__global__ void preKernel(int *cntPtr, int cnt)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;
	
	if(i == 0) {
		while(*cntPtr < cnt) {*(cntPtr+1) = 0;}
		//while(*cntPtr < cnt) {}
	}
	__syncthreads();
}

/* 
 * postKernel 
 * Set the counter to desired counter value
 */
__global__ void postKernel(int *cntPtr, int cnt)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;
	
	if(i == 0) {
		*cntPtr = cnt;
	}
	__syncthreads();
}

__global__ void mapNB(double *features, double *classifications, double *phi, int width, int height, int compareWith, double weightedspamcount) 
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;
	int j = 0;

	double spamwordcount = 0.0;
	for(j=0; j<height; j++) {
		if(classifications[j] == compareWith)
			spamwordcount = spamwordcount + features[j*width+i];
	}

	if(i < width)
		phi[i] = (spamwordcount + 1) / (weightedspamcount + width);

	__syncthreads();
	
	/*
	int j = blockDim.x * blockIdx.x + threadIdx.x;
	int i = 0;

	__shared__ double spamwordcount[512];
	double value;

	spamwordcount[threadIdx.y][threadIdx.x] = 0;

	while(i*16 + threadIdx.y < height) {
		value = features[(i*16+threadIdx.y)*width+j];
		if(classifications[i*16+threadIdx.y] == compareWith)
			spamwordcount[threadIdx.y][threadIdx.x] += value;
		i += 1;
	}
	__syncthreads();

	if(threadIdx.y == 0) {
		for(i=1; i<16; i++)
			spamwordcount[0][threadIdx.x] += spamwordcount[i][threadIdx.x];
		if(j < width)
			phi[j] = (spamwordcount[0][threadIdx.x] + 1) / (weightedspamcount + width);
	}
	__syncthreads();
	*/

}

__global__ void mapToVecNB(double *matrix, double *out, int width, int height) 
{
/*
	int i = blockDim.x * blockIdx.x + threadIdx.x;
	int j = 0;

	__shared__ double localsum[128];

	localsum[threadIdx.x] = 0;

    if(i < height) {
	    while(j < width) {
		    localsum[threadIdx.x] += matrix[i*width+j];
		    j += 1;
		}
		out[i] = localsum[threadIdx.x];
	}
	__syncthreads();
*/
	int i = blockDim.x * blockIdx.x + threadIdx.x;
	int j = 0;

	__shared__ double localsum[256];

	localsum[threadIdx.x] = 0;

    if(i < width) {
	    while(j < height) {
		    localsum[threadIdx.x] += matrix[j*width+i];
		    j += 1;
		}
		out[i] = localsum[threadIdx.x];
	}
	__syncthreads();

}

__global__ void mapLR(double *input, double *output, double x_cur, int tau, int count) 
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;
	double base = x_cur - input[i];
	
	if(i < count) {
		output[i] = exp((-0.1*base*base)/(2.0*tau*tau));
	}
	__syncthreads();
}

/*
__global__ void mapLRBig(double *xref, int xref_height, double *x, double *O, double *output, double x_cur, int tau, int count) 
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;
	
	double x_cur = xref[i*xref_height];
	if(i < count) {
		output[i] = exp((-0.1*base*base)/(2.0*tau*tau));
	}
	__syncthreads();
}
*/

__global__ void dotVMDouble(double *inputM, double *inputV, double *outputM, int width, int height) 
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;
	int j = blockDim.y * blockIdx.y + threadIdx.y;
	//int h = 0;
	//__shared__ double v;

	//while(h < height) {
		//if(threadIdx.x == 0)
		//	v = inputV[j];
		//__syncthreads();
		if(i < width)
			outputM[j*width+i] = inputM[j*width+i] * inputV[j];
	//	h += 1;
	//}
	__syncthreads();
}

__global__ void matTransDouble(double *input, double *output, int width, int height)
{
    int j = blockDim.x * blockIdx.x + threadIdx.x;
    int i = blockDim.y * blockIdx.y + threadIdx.y;

    __shared__ double S_input[16][16];

    // Read inputs to shared mem
    if((j<width) && (i<height))
        S_input[threadIdx.y][threadIdx.x] = input[i*width+j];
    __syncthreads();

    // Write result to device mem
    int jj = blockDim.y * blockIdx.y + threadIdx.x;
    int ii = blockDim.x * blockIdx.x + threadIdx.y;
    if((jj<height) && (ii<width))
        output[ii*height+jj] = S_input[threadIdx.x][threadIdx.y];

    __syncthreads();

}

__global__ void matTransFloat(float *input, float *output, int width, int height)
{
    int j = blockDim.x * blockIdx.x + threadIdx.x;
    int i = blockDim.y * blockIdx.y + threadIdx.y;

    __shared__ float S_input[16][16];

    // Read inputs to shared mem
    if((j<width) && (i<height))
        S_input[threadIdx.y][threadIdx.x] = input[i*width+j];
    __syncthreads();

    // Write result to device mem
    int jj = blockDim.y * blockIdx.y + threadIdx.x;
    int ii = blockDim.x * blockIdx.x + threadIdx.y;
    if((jj<height) && (ii<width))
        output[ii*height+jj] = S_input[threadIdx.x][threadIdx.y];

    __syncthreads();

}

__global__ void matInvDouble(double *input, double *output, int dum1, int dum2) 
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	__shared__ double arr[4][4];

	arr[0][i] = input[i];
	arr[1][i] = input[i];
	arr[2][i] = input[i];
	arr[3][i] = input[i];

	double admbc = arr[i][0]*arr[i][3] - arr[i][1]*arr[i][2];

    if(i < 4)
	    output[i] = arr[i][3-i]/admbc;

	if((i==1)||(i==2))
		output[i] *= -1;
	
	__syncthreads();
}

#define DIM_X 16
#define DIM_Y 32

__global__ void MprodVDouble(double *inputM, double *inputV, double *outputV, int width, int height) 
{
	/*
	int i = blockDim.x * blockIdx.x + threadIdx.x;
	int j = blockDim.y * blockIdx.y + threadIdx.y;
    int iter = (511+width) / 512;
    int k = 0;
    int step = 0;
    __shared__ double tempM[512];
    __shared__ double tempV[512];
    __shared__ double localSum[512];
    double finalSum = 0.0;
    
    //get inputs
    while(k < iter) {
        tempM[threadIdx.x] = 0.0;
        tempV[threadIdx.x] = 0.0;

        if(i < width) {
            tempM[threadIdx.x] = inputM[j*width+i];
            tempV[threadIdx.x] = inputV[i];
        }
        localSum[threadIdx.x] = tempM[threadIdx.x] * tempV[threadIdx.x];
        step = 1;
        __syncthreads();

        // Do reduction
        while(step < 512) {
            if(threadIdx.x % (2*step) == 0)
                localSum[threadIdx.x] += localSum[threadIdx.x+step];
            __syncthreads();
            step *= 2;
        }
        if(threadIdx.x == 0)
            finalSum += localSum[0];
        k += 1;
        i += 512;
    }
    
    if(threadIdx.x == 0)
        outputV[j] = finalSum;
    */

	//int i = blockDim.x * blockIdx.x + threadIdx.x;
	int j = blockDim.y * blockIdx.y + threadIdx.y;
	double localsum = 0;
    int ii = 0;
    int iii = 0;
    
	__shared__ double temp[DIM_Y][DIM_X];

	temp[threadIdx.y][threadIdx.x] = 0.0;

	while(ii+DIM_X < width) {
	    temp[threadIdx.y][threadIdx.x] += inputV[ii+threadIdx.x] * inputM[j*width+ii+threadIdx.x];
		/*
	    __syncthreads();
	    if(threadIdx.x == 0) {
	        iii = 0;
	        while(iii < DIM_X) {
	            localsum += temp[threadIdx.y][iii];
	            iii += 1;
	        }
	    }
		*/
	    ii += DIM_X;
	}
	__syncthreads();
	if(threadIdx.x == 0) {
	    iii = 0;
	    while(iii < DIM_X) {
	        localsum += temp[threadIdx.y][iii];
	        iii += 1;
	    }
	}

    if(threadIdx.x == 0) {
        while(ii < width) {
            localsum += (inputV[ii]*inputM[j*width+ii]);
            ii += 1;
        }
        if(j < height)
            outputV[j] = localsum;
    }
    __syncthreads();
    

        
/*
    while(h < width) {
        localsum += (inputV[h]*inputM[i*width+h]);
        h += 1;
    }
    if(i < height)
        outputV[i] = localsum;
*/

	__syncthreads();
}

__global__ void MprodVInt(int *inputM, int *inputV, int *outputV, int width, int height) 
{
	//int i = blockDim.x * blockIdx.x + threadIdx.x;
	int j = blockDim.y * blockIdx.y + threadIdx.y;

	double localsum = 0;
    int ii = 0;
    int iii = 0;

	__shared__ int temp[DIM_Y][DIM_X];

	while(ii+DIM_X < width) {
	    temp[threadIdx.y][threadIdx.x] = inputV[ii+threadIdx.x] * inputM[j*width+ii+threadIdx.x];
	    __syncthreads();

	    if(threadIdx.x == 0) {
	        iii = 0;
	        while(iii < DIM_X) {
	            localsum += temp[threadIdx.y][iii];
	            iii += 1;
	        }
	    }
	    ii += DIM_X;
	}

    if(threadIdx.x == 0) {
        while(ii < width) {
            localsum += (inputV[ii]*inputM[j*width+ii]);
            ii += 1;
        }
        if(j < height)
            outputV[j] = localsum;
    }
    __syncthreads();

	/*
	//int i = blockDim.x * blockIdx.x + threadIdx.x;
	int indx = 0;
	int h = 0;
	int stride = 2;
	int iter = 0;
	int mValue;
	int vValue;
	int result = 0;
	//__shared__ double arrV[256];
	__shared__ int resV[256];

	//arrV[threadIdx.x] = 0;
	resV[threadIdx.x] = 0;

	while(h < height) {
	    result = 0;
		iter = 0;
		//indx = blockDim.x*blockIdx.x+threadIdx.x;

		while(iter < 1+(width-1)/256) {
			//arrV[i] = inputV[indx];
			indx = iter * 256 + threadIdx.x;
			resV[threadIdx.x] = 0;
			mValue = 0;
			vValue = 0;
			if(indx < width) {
				vValue = inputV[indx];
				mValue = inputM[indx+h*width];
			}
			resV[threadIdx.x] = mValue * vValue;

			__syncthreads();
			stride = 1;
			while(stride <= 128) {
				if(threadIdx.x % (stride*2) == 0)
					resV[threadIdx.x] += resV[threadIdx.x+stride];
				__syncthreads();
				stride *= 2;
			}
			//__syncthreads();
			if(threadIdx.x==0)
				result += resV[threadIdx.x];
			iter += 1;
			//__syncthreads();
		}
		if(threadIdx.x==0)
			outputV[h] = result;
		//__syncthreads();
		h += 1;
	}
	*/
	__syncthreads();
}

/* Kernels for RBM */
__global__ void vectPlusDouble_S(double *A, double *C, double B, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	if (i < size)
		C[i] = A[i] + B;

	__syncthreads();
}

__global__ void vectPlusFloat_S(float *A, float *C, float B, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	if (i < size)
		C[i] = A[i] + B;

	__syncthreads();
}

__global__ void vectMinusDouble_S(double *A, double *C, double B, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	if (i < size)
		C[i] = A[i] - B;

	__syncthreads();
}

__global__ void vectMinusFloat_S(float *A, float *C, float B, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	if (i < size)
		C[i] = A[i] - B;

	__syncthreads();
}

__global__ void vectMultDouble_S(double *A, double *C, double B, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	if (i < size)
		C[i] = A[i] * B;

	__syncthreads();
}

__global__ void vectMultFloat_S(float *A, float *C, float B, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	if (i < size)
		C[i] = A[i] * B;

	__syncthreads();
}

__global__ void vectDivDouble_S(double *A, double *C, double B, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	if (i < size)
		C[i] = A[i] / B;

	__syncthreads();
}

__global__ void vectDivFloat_S(float *A, float *C, float B, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;

	if (i < size)
		C[i] = A[i] / B;

	__syncthreads();
}

__global__ void vectRepDouble(double *A, double *C, int size, int repRow, int repCol)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;
    int j = blockDim.y * blockIdx.y + threadIdx.y; //(threadIdx.y==0 always

	if (i < size*repCol)
		C[j*size*repCol+i] = A[i%size];

	__syncthreads();
}

__global__ void vectRepFloat(float *A, float *C, int size, int repRow, int repCol)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;
    int j = blockDim.y * blockIdx.y + threadIdx.y; //(threadIdx.y==0 always

	if (i < size*repCol)
		C[j*size*repCol+i] = A[i%size];

	__syncthreads();
}

__global__ void sumColsDouble(double *A, double *C, int numRows, int numCols)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;
    int j = 0;

    __shared__ double localSum[512];
    localSum[threadIdx.x] = 0;
    
    while(j < numRows) {
        if(i < numCols)
            localSum[threadIdx.x] += A[j*numCols+i];
        j += 1;
    }
    
    if(i < numCols)
        C[i] = localSum[threadIdx.x];
                  
	__syncthreads();
}

__global__ void sumColsFloat(float *A, float *C, int numRows, int numCols)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;
    int j = 0;

    __shared__ float localSum[512];
    localSum[threadIdx.x] = 0;

    while(j < numRows) {
        if(i < numCols)
            localSum[threadIdx.x] += A[j*numCols+i];
        j += 1;
    }

    if(i < numCols)
        C[i] = localSum[threadIdx.x];

	__syncthreads();
}

__global__ void sumColsPredDouble(double *A, double *pred, double *C, int numRows, int numCols)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;
    int j = 0;

    __shared__ double localSum[512];
    localSum[threadIdx.x] = 0;

    while(j < numRows) {
        if(i < numCols) {
            if(pred[j] == 1.0)
                localSum[threadIdx.x] += A[j*numCols+i];
        }
        j += 1;
    }

    if(i < numCols)
        C[i] = localSum[threadIdx.x];

	__syncthreads();
}

__global__ void sumColsPredFloat(float *A, float *pred, float *C, int numRows, int numCols)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;
    int j = 0;

    __shared__ float localSum[512];
    localSum[threadIdx.x] = 0;

    while(j < numRows) {
        if(i < numCols) {
            if(pred[j] == 1.0)
                localSum[threadIdx.x] += A[j*numCols+i];
        }
        j += 1;
    }

    if(i < numCols)
        C[i] = localSum[threadIdx.x];

	__syncthreads();
}

//Input: 2 double matrices (x, mu)
//Output: 1 double vector (out)
__global__ void mapKM1(double *x, double *mu, int *out, int x_numRows, int x_numCols, int mu_numRows, int mu_numCols)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;
	int j,k;
	double min_d = 100000;
	int min_j = -1;
	double dist = 0.0;
	double diff;
	
	if(i < x_numRows) {
		for(j=0; j<mu_numRows; j++) {
			dist = 0.0;
			for(k=0; k<x_numCols; k++) {
				diff = x[i*x_numCols+k] - mu[j*mu_numCols+k];
				dist += diff * diff;
			}
			if(dist < min_d) {
				min_d = dist;
				min_j = j;
			}
		}
		out[i] = min_j;
	}

/*
	
    // Sequential Code (only threadIdx==0 working)
    double diff = 0.0;
    double sum = 0.0;
    int l = 0;
    if(i==0) {
        for(l=0; l<mu_numRows; l++) {
            sum = 0.0;
            for(k=0; k<x_numCols; k++) {
                diff = x[j*x_numCols+k] - mu[l*mu_numCols+k];
                sum += (diff * diff);
            }
            if(sum < min) {
                min = sum;
                min_j = l;
            }
        }
        out[j] = min_j;
    }
  */

	/*
	int i = blockDim.x * blockIdx.x + threadIdx.x;
    int j = blockDim.y * blockIdx.y + threadIdx.y;
    int k = 0;
    double min = 100000.0;     // Need to be changed to the highest value it can have
    int min_k = 0;

    int reduc_phase = 1;
    int pow2 = 1;
    __shared__ double diff[512];
    __shared__ double x_S[512];

    // Read x values
    if(i < x_numCols)
        x_S[threadIdx.x+threadIdx.y*x_numCols] = x[j*x_numCols+i];

    // calculate max number of power of 2 less than x_numCols (==mu_numCols)
    while(2*pow2 <= x_numCols)
        pow2 *= 2;
            
    for(k=0; k<mu_numRows; k++) {

        // this if-statement is not needed if the number of threads per block is exactly same as x_numCols
        if(i < x_numCols) {
            // element-wise power of 2
            diff[threadIdx.x+threadIdx.y*x_numCols] = x_S[threadIdx.x+threadIdx.y*x_numCols] - mu[k*mu_numCols+i];
            diff[threadIdx.x+threadIdx.y*x_numCols] *= diff[threadIdx.x+threadIdx.y*x_numCols];
            __syncthreads();

            // fold the array to have length of power of 2
            if(i >= pow2)
                diff[threadIdx.y*x_numCols+i-pow2] += diff[threadIdx.y*x_numCols+i];
            __syncthreads();
            
            // reduction phase (may cause problem for accessing outside the boundary of shared variable)
            reduc_phase = 1;
            while(reduc_phase != pow2) {
                if(i%(2*reduc_phase) == 0)
                    diff[threadIdx.y*x_numCols+i] += diff[threadIdx.y*x_numCols+i+reduc_phase];
                reduc_phase *= 2;
                __syncthreads();
            }

            // update minimum value & index
            if(diff[threadIdx.y*x_numCols+i] < min) {
                min = diff[threadIdx.y*x_numCols+i];
                min_k = k;
            }
        }
    }

    if(i==0) {
        out[j] = min_k;
    }
	*/

	__syncthreads();
}

__global__ void mapKM2(double *x, double *mu, int *c, int x_numRows, int x_numCols, int m_numRows)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;
	int j = blockDim.y * blockIdx.y + threadIdx.y;
	int k;
	int cval;
    //int points = 0;
    
	__shared__ double weightedpoints[512];
	__shared__ int points[512];

	if(i < m_numRows) {
    	//initialize weightedpoints to 0
		weightedpoints[threadIdx.y*blockDim.x+threadIdx.x] = 0.0;
		points[threadIdx.y*blockDim.x+threadIdx.x] = 0;

		for(k=threadIdx.y*blockDim.x+threadIdx.x; k<x_numRows*x_numCols; k+=(blockDim.x*blockDim.y)) {
			cval = c[k/x_numCols];
			if(cval == i) {
				weightedpoints[threadIdx.y*blockDim.x+threadIdx.x] += x[k];
				points[threadIdx.y*blockDim.x+threadIdx.x] += 1;
			}
		}
		__syncthreads();

		//Do reduction
		if(threadIdx.y < x_numCols) {
			for(k=threadIdx.y; k<blockDim.x*blockDim.y-x_numCols; k+=x_numCols) {
				weightedpoints[threadIdx.y] += weightedpoints[k+x_numCols];
				points[threadIdx.y] += points[k+x_numCols];
			}

			//Answers are in weightedpoitns[k] (k<x_numCols)
			if(points == 0) 
				mu[i*x_numCols+threadIdx.y] = 0.0;
			else
				mu[i*x_numCols+threadIdx.y] = weightedpoints[threadIdx.y] / points[threadIdx.y];
		}
	}

		/*
	int i = blockDim.x * blockIdx.x + threadIdx.x;
	int j,k;
	int cval;
    int points = 0;
    double weightedpoints[512];

	if(i < 16) {
    	//initialize weightedpoints to 0
		for(k=0; k<x_numCols; k++)
			weightedpoints[k] = 0.0;

		for(j=0; j<x_numRows; j++) {
			cval = c[j];
			if(cval == i) {
				for(k=0; k<x_numCols; k++)
					weightedpoints[k] += x[j*x_numCols+k];
				points += 1;
			}
		}

		if(points == 0) {
			for(k=0; k<x_numCols; k++) 
				mu[i*x_numCols+k] = 0.0;
		}
		else {
			for(k=0; k<x_numCols; k++) 
				mu[i*x_numCols+k] = weightedpoints[k] / points; 
		}
	}
	*/
	__syncthreads();
}

/*
__global__ void mapKM2(double *x, double *mu, int *c, int x_numRows, int x_numCols)
{
	//int i = blockDim.x * blockIdx.x + threadIdx.x;
    int j = blockDim.y * blockIdx.y + threadIdx.y;
    int points = 0;
    int cval = 0;
    int k = 0;
    
    __shared__ double weightedpoints[512];

    //initialize weightedpoints to 0

    if(threadIdx.x < x_numCols) {
        weightedpoints[threadIdx.x] = 0.0;
        for(k=0; k<x_numRows; k++) {
            cval = c[k];
            if(cval == j) {
                weightedpoints[threadIdx.x] = weightedpoints[threadIdx.x] + x[k*x_numCols+threadIdx.x];
                points += 1;
            }
        }
        if(points == 0)
            weightedpoints[threadIdx.x] = 0.0;
        else
            mu[j*x_numCols+threadIdx.x] = weightedpoints[threadIdx.x] / points;
        //mu[j*x_numCols+threadIdx.x] = 0;
    }
	__syncthreads();
}
*/

__global__ void reduction(double *x, double *dummy, double *out, int size)
{
	//int i = blockDim.x * blockIdx.x + threadIdx.x;
	int j = 0;
    double sum = 0.0;
    
    //__shared__ double weightedpoints[512];
    if(threadIdx.x == 0) {
        for(j=0; j<size; j++)
            sum += x[j];
        out[0] = sum;
    }
    //out[0] = 2.0;

	__syncthreads();
}

__global__ void VminusMDouble(double *v, double *m, double *out, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;
	int j = blockDim.y * blockIdx.y + threadIdx.y;

    if(i < size) {
        out[j*size+i] = v[i] - m[j*size+i];
    }
	__syncthreads();
}


__global__ void matDiagFloat(float *v, float *out, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;
	int j = blockDim.y * blockIdx.y + threadIdx.y;

    if(i < size) {
        if(i == j)
            out[j*size+i] = v[i];
        else
            out[j*size+i] = 0;
    }
	__syncthreads();
}

__global__ void matDiagDouble(double *v, double *out, int size)
{
	int i = blockDim.x * blockIdx.x + threadIdx.x;
	int j = blockDim.y * blockIdx.y + threadIdx.y;

    if(i < size) {
        if(i == j)
            out[j*size+i] = v[i];
        else
            out[j*size+i] = 0;
    }
	__syncthreads();
}
