package ppl.delite.cnative;

public class DeliteNative {

    static {
        System.load(System.getProperty("delite.basedir")+"/cnative/lib/libDeliteDriver.so");
        //exportEnvVar("OMP_NUM_THREADS", Integer.toString(Config.CPUThreadNum));
    }

    // DOES NOT AFFECT BLAS NUM THREADS
    //public static native void exportEnvVar(String var, String val);
    //public static native void setNumThreadsBLAS(String numThreads);

    public static native void matrixMultDouble(double[] mat1, double[] mat2, double[] mat3, int mat1_r, int mat1_c, int mat2_c);
    public static native void matrixMultFloat(float[] mat1, float[] mat2, float[] mat3, int mat1_r, int mat1_c, int mat2_c);
    
	public static native void vectMultFloat(float[] vec1, float[] vec2, float[] vec3, int length);
	public static native void vectMultDouble(double[] vec1, double[] vec2, double[] vec3, int length);
	
	public static native void matVMultFloat(float[] mat1, float[] vec2, float[] vec3, int mat_row, int mat_col, int vec_offset, int vec_stride);
	public static native void matVMultDouble(double[] mat1, double[] vec2, double[] vec3, int mat_row, int mat_col, int vec_offset, int vec_stride);
	
	public static native void vecExpFloat(float[] vec1, float[] vec2, int length);
	public static native void vecExpDouble(double[] vec1, double[] vec2, int length);
	
	public static native void vecSigmoidFloat(float[] vec1, float[] vec2, int start, int end);
	public static native void vecSigmoidDouble(double[] vec1, double[] vec2, int start, int end);
	
	public static native void vecRandFloat(float[] vec, int start, int end);
}
