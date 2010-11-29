__device__ void rank1_updateD( double a, const double *b, double *c )
{
	c[0] += a*b[0];
	c[1] += a*b[1];
	c[2] += a*b[2];
	c[3] += a*b[3];
	c[4] += a*b[4];
	c[5] += a*b[5];
	c[6] += a*b[6];
	c[7] += a*b[7];
	c[8] += a*b[8];
	c[9] += a*b[9];
	c[10] += a*b[10];
	c[11] += a*b[11];
	c[12] += a*b[12];
	c[13] += a*b[13];
	c[14] += a*b[14];
	c[15] += a*b[15];
}

__device__ void rankk_updateD( int k, const double *A, int lda, const double *b, int ldb, double *c )
{
    if( k <= 0 ) return;

    int i = 0;
    rank1_updateD( A[0], &b[i*ldb], c ); if( ++i >= k ) return; A += lda;
    rank1_updateD( A[0], &b[i*ldb], c ); if( ++i >= k ) return; A += lda;
    rank1_updateD( A[0], &b[i*ldb], c ); if( ++i >= k ) return; A += lda;
    rank1_updateD( A[0], &b[i*ldb], c ); if( ++i >= k ) return; A += lda;
    
    rank1_updateD( A[0], &b[i*ldb], c ); if( ++i >= k ) return; A += lda;
    rank1_updateD( A[0], &b[i*ldb], c ); if( ++i >= k ) return; A += lda;
    rank1_updateD( A[0], &b[i*ldb], c ); if( ++i >= k ) return; A += lda;
    rank1_updateD( A[0], &b[i*ldb], c ); if( ++i >= k ) return; A += lda;
    
    rank1_updateD( A[0], &b[i*ldb], c ); if( ++i >= k ) return; A += lda;
    rank1_updateD( A[0], &b[i*ldb], c ); if( ++i >= k ) return; A += lda;
    rank1_updateD( A[0], &b[i*ldb], c ); if( ++i >= k ) return; A += lda;
    rank1_updateD( A[0], &b[i*ldb], c ); if( ++i >= k ) return; A += lda;
    
    rank1_updateD( A[0], &b[i*ldb], c ); if( ++i >= k ) return; A += lda;
    rank1_updateD( A[0], &b[i*ldb], c ); if( ++i >= k ) return; A += lda;
    rank1_updateD( A[0], &b[i*ldb], c );
}

__device__ void store_blockD( int num, double alpha, double *c, double beta, double *C, int ldc )
{
    if( num <= 0 ) return;

    if( beta == 0 )
    {
        //
        //  for the case when C is initialized with inf or NaN
        //
        int i = 0; 
        C[0] = alpha*c[i++]; if( i >= num ) return; C += ldc;  
        C[0] = alpha*c[i++]; if( i >= num ) return; C += ldc;  
        C[0] = alpha*c[i++]; if( i >= num ) return; C += ldc;  
        C[0] = alpha*c[i++]; if( i >= num ) return; C += ldc;  
        
        C[0] = alpha*c[i++]; if( i >= num ) return; C += ldc;  
        C[0] = alpha*c[i++]; if( i >= num ) return; C += ldc;  
        C[0] = alpha*c[i++]; if( i >= num ) return; C += ldc;  
        C[0] = alpha*c[i++]; if( i >= num ) return; C += ldc;  

        C[0] = alpha*c[i++]; if( i >= num ) return; C += ldc;  
        C[0] = alpha*c[i++]; if( i >= num ) return; C += ldc;  
        C[0] = alpha*c[i++]; if( i >= num ) return; C += ldc;  
        C[0] = alpha*c[i++]; if( i >= num ) return; C += ldc;  

        C[0] = alpha*c[i++]; if( i >= num ) return; C += ldc;  
        C[0] = alpha*c[i++]; if( i >= num ) return; C += ldc;  
        C[0] = alpha*c[i++]; if( i >= num ) return; C += ldc;  
        C[0] = alpha*c[i++];
    }
    else
    {
        int i = 0; 
        C[0] = alpha*c[i++] + beta*C[0]; if( i >= num ) return; C += ldc;  
        C[0] = alpha*c[i++] + beta*C[0]; if( i >= num ) return; C += ldc;  
        C[0] = alpha*c[i++] + beta*C[0]; if( i >= num ) return; C += ldc;  
        C[0] = alpha*c[i++] + beta*C[0]; if( i >= num ) return; C += ldc;  
        
        C[0] = alpha*c[i++] + beta*C[0]; if( i >= num ) return; C += ldc;  
        C[0] = alpha*c[i++] + beta*C[0]; if( i >= num ) return; C += ldc;  
        C[0] = alpha*c[i++] + beta*C[0]; if( i >= num ) return; C += ldc;  
        C[0] = alpha*c[i++] + beta*C[0]; if( i >= num ) return; C += ldc;  

        C[0] = alpha*c[i++] + beta*C[0]; if( i >= num ) return; C += ldc;  
        C[0] = alpha*c[i++] + beta*C[0]; if( i >= num ) return; C += ldc;  
        C[0] = alpha*c[i++] + beta*C[0]; if( i >= num ) return; C += ldc;  
        C[0] = alpha*c[i++] + beta*C[0]; if( i >= num ) return; C += ldc;  

        C[0] = alpha*c[i++] + beta*C[0]; if( i >= num ) return; C += ldc;  
        C[0] = alpha*c[i++] + beta*C[0]; if( i >= num ) return; C += ldc;  
        C[0] = alpha*c[i++] + beta*C[0]; if( i >= num ) return; C += ldc;  
        C[0] = alpha*c[i++] + beta*C[0];
    }
}

//
//  C = alpha*A*B + beta*C
//
/*
	lmem = 0
	smem = 1168
	reg  = 30
	active threads = 512 
 */
 
__global__ void   dgemmNN_device(double *A_ptr, double *B_ptr, double *C_ptr, int widthA, int widthB, int heightA) 
//__global__ void   dgemmNN_device( int m, int n, const double *A, int lda, 
//const double *B, int ldb, double* C, int ldc, int k, double alpha, double beta )
{
	int m = widthB;
	int n = heightA;
	double *A = B_ptr;
	int lda = widthB;
	double *B = A_ptr;
	int ldb = widthA;
	double *C = C_ptr;
	int ldc = widthB;
	int k = widthA;
	double alpha = 1.0;
	double beta = 0.0;

	const int inx = threadIdx.x;
	const int iny = threadIdx.y;
	const int ibx = blockIdx.x * 64;
	const int iby = blockIdx.y * 16;
	const int row = ibx + inx + iny*16;
	
	A += row;
	B += inx + ( iby + iny ) * ldb;
	C += row  + iby * ldc;
	
	double c[16] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
    
	__shared__ double b[16][17];
	for( ; k > 0; k -= 16 )
	{
#pragma unroll
		for( int i = 0; i < 16; i += 4 )
			b[inx][iny+i]  = B[i*ldb];
		__syncthreads();

        if( k < 16 )  break;

#pragma unroll
	    for( int i = 0; i < 16; i++, A += lda )
		    rank1_updateD( A[0], &b[i][0], c ); 
	    __syncthreads();
		
		B += 16;
	};

    rankk_updateD( k, A, lda, &b[0][0], 17, c );

    if( row >= m )  return;
    
    store_blockD( n - iby, alpha, c, beta, C, ldc);
}	


