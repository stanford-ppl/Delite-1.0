#ifndef __RBM__
#define __RBM__

#define MAX_ALLOC 10000

//#define LOGGER(X) performance_check(X);
#define LOGGER(X) 

typedef struct {
	float *data;
	float *gdata;
	int length;
} Vector;

typedef struct {
	float *data;
	float *gdata;
	int numRows;
	int numCols;
} Matrix;

typedef struct {
	double *data;
	double *gdata;
	int length;
} VectorD;

typedef struct {
	double *data;
	double *gdata;
	int numRows;
	int numCols;
} MatrixD;

/* Function Prototypes */
extern Matrix reciprocal(Matrix *out, Matrix m);
extern Matrix matPlusS(Matrix *out, Matrix m, float scalar);
extern Matrix matExp(Matrix *out, Matrix m);
extern Matrix matMinus(Matrix *out, Matrix m1, Matrix m2);
extern Matrix matMultS(Matrix *out, Matrix m, float scalar);
extern Matrix matMult(Matrix *out, Matrix m1, Matrix m2);
extern Matrix matRep(Matrix *out, Vector v, int rep);

extern Matrix matTrans(Matrix *out, Matrix m);
extern Vector matSumCol(Vector *out, Matrix m);
extern Matrix matCompare(Matrix *out, Matrix m1, Matrix m2);
extern Matrix matRandGauss(Matrix *out, int numRows, int numCols);
extern Matrix matRand(Matrix *out, int numRows, int numCols);

extern float matSum(Matrix m);
extern Matrix matDot(Matrix *out, Matrix m1, Matrix m2);

extern Matrix matPlus(Matrix *out, Matrix m1, Matrix m2);
extern Vector vectPlus(Vector *out, Vector v1, Vector v2);
extern Vector vectMultS(Vector *out, Vector v, float scalar);
extern Vector vectMinus(Vector *out, Vector v1, Vector v2);

extern void performance_check(char *str);

void gpuInit(void);
void gpuAlloc(int n, int elemSize, void **devicePtr);
void gpuCopy(float *dptr, float *sptr, int byte);
void gpuSync(void);

extern VectorD matProdV(VectorD *out, MatrixD m1, VectorD v1);

#endif
