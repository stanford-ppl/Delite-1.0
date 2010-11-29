package ppl.delite.core;

public class Config {    

  // Allows the number of threads created by the CPU scheduler in the thread pool to be overridden
  public static int CPUThreadNum = Integer.parseInt(System.getProperty("thread-num", Integer.toString(Runtime.getRuntime().availableProcessors()) ));

  // toggles using native libraries or Delite for ops that support native exeuction
  public static boolean useNativeLibs = Boolean.parseBoolean(System.getProperty("useNativeLibs", "true"));

  // Allows the number of threads created by the GPU scheduler in the thread pool to be overridden
  public static boolean GPUThreadNumOverride = Boolean.parseBoolean(System.getProperty("gpu-thread-num-override", "false"));
  public static int GPUThreadNum = Integer.parseInt(System.getProperty("gpu-thread-num", "1"));

  // Controls the sleep time increment when forcing
  public static int forceSleepTimeIncrement = Integer.parseInt(System.getProperty("force-sleep-inc", "1"));

  // Controls the size of the queue between main thread and executor thread
  public static int executorWindowSize = Integer.parseInt(System.getProperty("executor-window-size", "8"));
  public static int executorInQueueSize = Integer.parseInt(System.getProperty("executor-iq-size", "256"));
  public static int GPUThreadInQueueSize = Integer.parseInt(System.getProperty("gpu-thread-iq-size", "256"));
  public static int CPUThreadInQueueSize = Integer.parseInt(System.getProperty("cpu-thread-iq-size", "256"));
  public static int spinBeforeSleep = Integer.parseInt(System.getProperty("spin-before-sleep", "4096"));
  public static boolean letWorkersSleep = Boolean.parseBoolean(System.getProperty("letWorkersSleep", "true"));

  // Controls the Default size of the queue between active blocks
  public static int activeBlockDefaultQueueSize = Integer.parseInt(System.getProperty("active-block-default-queue-size", "256"));

  public static boolean debugEnabled = Boolean.parseBoolean(System.getProperty("enableDebugOptions", "false"));
  public static boolean bypassExecutor = Boolean.parseBoolean(System.getProperty("bypassExecutor", "false"));
  public static String executorType = System.getProperty("executorType", "default");
    
  // Control the GPU memory block size(in KB)
  public static int GPUMemSize1 = Integer.parseInt(System.getProperty("gpu-mem-size1", "524288"));
  public static int GPUMemSize2 = Integer.parseInt(System.getProperty("gpu-mem-size2", "1048576"));
  public static int GPUMemBlockSize1 = Integer.parseInt(System.getProperty("gpu-memblock-size1", "8"));
  public static int GPUMemBlockSize2 = Integer.parseInt(System.getProperty("gpu-memblock-size2", "8192"));

  // Filename for the list of GPU enabled OPs
  public static String gpuOPsFileName = System.getProperty("gpu-ops-filename", null);

  static {
    if (bypassExecutor && debugEnabled) CPUThreadNum = 1;
  }

 
}
