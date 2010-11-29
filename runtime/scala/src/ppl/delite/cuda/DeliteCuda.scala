package ppl.delite.cuda

import ppl.delite.core.Config

object DeliteCuda {

	//var PageLockedMem_Out:Long = 0
	//var stream:Array[Long] = new Array[Long](30)

  /* GPU kernel launcher IDs */
  val AsyncLaunch1D:Int = 1
  val AsyncLaunch2D:Int = 2
  val Async3D3I:Int = 3

  /* GPU Kernel IDs */
	val preKernel:Int = 0 
	val postKernel:Int = 1
	
	val DVectPlusDouble:Int = 2  	// Double Vector Addition
	val DVectMinusDouble:Int = 3	// Double Vector Minus
	val DVectMultDouble:Int = 4
	val DVectDivDouble:Int = 5		// Double Vector Division
	val DVectMoveDouble:Int = 6 	// Double Vector Device-Device Copy
	val DVectOuterDouble:Int = 7	// Double Vector Outer
	
	val DVectPlusFloat:Int = 8  	// Float Vector Addition
	val DVectMinusFloat:Int = 9		// Float Vector Minus
	val DVectMultFloat:Int = 10
	val DVectDivFloat:Int = 11		// Float Vector Division
	val DVectMoveFloat:Int = 12 	// Float Vector Device-Device Copy
	val DVectOuterFloat:Int = 13	// Float Vector Outer

	val DVectPlusInt:Int = -1  		// Int Vector Addition
  val DVectMultInt:Int = 49
	val DVectMinusInt:Int = -1		// Int Vector Minus
	val DVectDivInt:Int = -1		// Int Vector Division
	val DVectMoveInt:Int = 48 		// Int Vector Device-Device Copy
	val DVectOuterInt:Int = -1		// Int Vector Outer

	val DVectPlusLong:Int = -1  	// Long Vector Addition
	val DVectMinusLong:Int = -1		// Long Vector Minus
	val DVectDivLong:Int = -1		// Long Vector Division
	val DVectMoveLong:Int = -1 		// Long Vector Device-Device Copy
	val DVectOuterLong:Int = -1 	// Long Vector Outer

	val DVectPlusByte:Int = -1  	// Byte Vector Addition
	val DVectMinusByte:Int = -1		// Byte Vector Minus
	val DVectDivByte:Int = -1		// Byte Vector Division
	val DVectMoveByte:Int = -1 		// Byte Vector Device-Device Copy
	val DVectOuterByte:Int = -1 	// Byte Vector Outer

	val DVectPlusBoolean:Int = -1  	// Boolean Vector Addition
	val DVectMinusBoolean:Int = -1	// Boolean Vector Minus
	val DVectDivBoolean:Int = -1	// Boolean Vector Division
	val DVectMoveBoolean:Int = -1 	// Boolean Vector Device-Device Copy
	val DVectOuterBoolean:Int = -1 	// Boolean Vector Outer


	val MAP_NB:Int = 14	// Map kernel used in Naive Bayes
	val MAPTOVEC_NB:Int = 15	// MaptoVec kernel used in Naive Bayes
	val MAP_LR:Int = 16	// Map kernel used in Linear Regression

	// Kernels for LinReg Application
	val MatDotV:Int = 17
	val MatMulDouble:Int = 18
	val MatTransDouble:Int = 19
	val MatInv:Int = 20
	val MatProdV:Int = 21

  // Kernels for RBM
  val VectGTDouble:Int = 22  //_Z12vectGTDoublePdS_S_i
  val VectGTFloat:Int = 23   // _Z11vectGTFloatPfS_S_i
  val VectRecipDouble:Int = 24   // _Z15vectRecipDoublePdS_i
  val VectRecipFloat:Int = 25   //_Z14vectRecipFloatPfS_i
  val VectExpDouble:Int = 26    // _Z13vectExpDoublePdS_S_i
  val VectExpFloat:Int = 27     // _Z12vectExpFloatPfS_S_i
  val VectPlusDouble_S:Int = 28   // _Z16vectPlusDouble_SPdS_S_i
  val VectPlusFloat_S:Int = 29    //_Z15vectPlusFloat_SPfS_S_i
  val VectMinusDouble_S:Int = 30   // _Z17vectMinusDouble_SPdS_S_i
  val VectMinusFloat_S:Int = 31    //_Z16vectMinusFloat_SPfS_S_i
  val VectMultDouble_S:Int = 32   // _Z16vectMultDouble_SPdS_S_i
  val VectMultFloat_S:Int = 33    //_Z15vectMultFloat_SPfS_S_i
  val VectDivDouble_S:Int = 34   // _Z15vectDivDouble_SPdS_S_i
  val VectDivFloat_S:Int = 35    //_Z14vectDivFloat_SPfS_S_i
  val VectRepDouble:Int = 36      //_Z13vectRepDoublePdS_iii
  val VectRepFloat:Int = 37      //_Z12vectRepFloatPfS_iii
  val sumColsDouble:Int = 38
  val sumColsFloat:Int = 39

  //K-means function
  val mapKM1:Int = 40
  val mapKM2:Int = 41

  //Reduction
  val sumDouble:Int = 42

  //Further
  val VectEQDouble:Int = 43
  val VectEQFloat:Int = 44
  val sumColsPredDouble:Int = 45
  val sumColsPredFloat:Int = 46
  val VminusMDouble: Int = 47
  val MatProdVInt:Int = 50

  val MatTransFloat:Int = 51
  val MatMulFloat:Int = 52

  //val MatRandFloat:Int = 53
  val MatMulDoubleReg:Int = 53
  val MatMulFloatReg:Int = -1

  val matDiagDouble:Int = 54
  val matDiagFloat:Int = 55

	// Array keeping track of all the kernel name/handlers in the system
	// Kernel handlers are returned from the cuda call to cudaGetFunction
	// These two array indexes are the gpukernelID of each OP
	// TODO: Where should the list of the kernel names come from?
	val gKernelName = Array[String](
			"_Z9preKernelPii", 
			"_Z10postKernelPii", 
			"_Z14vectPlusDoublePdS_S_i", 
			"_Z15vectMinusDoublePdS_S_i", 
			"_Z14vectMultDoublePdS_S_i",
			"_Z13vectDivDoublePdS_S_i", 
      "_Z14vectMoveDoublePdS_i",
			"_Z14matOuterDoublePdS_S_i", 
			"_Z13vectPlusFloatPfS_S_i", 
			"_Z14vectMinusFloatPfS_S_i", 
			"_Z13vectMultFloatPfS_S_i",
			"_Z12vectDivFloatPfS_S_i", 
      "_Z13vectMoveFloatPfS_i",
			"_Z13matOuterFloatPfS_S_i", 
			"_Z5mapNBPdS_S_iiid", 
			"_Z10mapToVecNBPdS_ii", 
			"_Z5mapLRPdS_dii",
			"_Z11dotVMDoublePdS_S_ii",
			"_Z14dgemmNN_devicePdS_S_iii", //"_Z9matrixMulPdS_S_iii",
      "_Z14matTransDoublePdS_ii",
      "_Z12matInvDoublePdS_ii",
			"_Z12MprodVDoublePdS_S_ii",
      //RBM kernels
      "_Z12vectGTDoublePdS_S_i",
      "_Z11vectGTFloatPfS_S_i",
      "_Z15vectRecipDoublePdS_i",
      "_Z14vectRecipFloatPfS_i",
      "_Z13vectExpDoublePdS_i",
      "_Z12vectExpFloatPfS_i",
      "_Z16vectPlusDouble_SPdS_di",
      "_Z15vectPlusFloat_SPfS_fi",
      "_Z17vectMinusDouble_SPdS_di",
      "_Z16vectMinusFloat_SPfS_fi",
      "_Z16vectMultDouble_SPdS_di",
      "_Z15vectMultFloat_SPfS_fi",
      "_Z15vectDivDouble_SPdS_di",
      "_Z14vectDivFloat_SPfS_fi",
      "_Z13vectRepDoublePdS_iii",
      "_Z12vectRepFloatPfS_iii",
      "_Z13sumColsDoublePdS_ii",
      "_Z12sumColsFloatPfS_ii",
      "_Z6mapKM1PdS_Piiiii",
      "_Z6mapKM2PdS_Piiii", //"_Z6mapKM2PdS_Piii",
      "_Z9reductionPdS_S_i",
      "_Z12vectEQDoublePdS_S_i",
      "_Z11vectEQFloatPfS_S_i",
      "_Z17sumColsPredDoublePdS_S_ii",
      "_Z16sumColsPredFloatPfS_S_ii",
      "_Z13VminusMDoublePdS_S_i",
      "_Z11vectMoveIntPiS_S_i", // 48
      "_Z11vectMultIntPiS_S_i",
      "_Z9MprodVIntPiS_S_ii",
      "_Z13matTransFloatPfS_ii",
      "_Z14sgemmNN_devicePfS_S_iii", //"_Z14sgemmNN_deviceiiPKfiS0_iPfiiff"
			"_Z9matrixMulPdS_S_iii",
      "_Z13matDiagDoublePdS_i",
      "_Z12matDiagFloatPfS_i"
			)
	//val gKernelHandler = new Array[Long](gKernelName.length)
	
	def initialize(devIdx: Int) : (DeliteCudaDriver, Array[Long]) = {
	    
		// Load CUDA Library
		//TODO: This argument should come from Config.java
		//System.load("/home/hyouklee/goodDelite/delite/runtime/scala/src/ppl/delite/cuda/libDeliteCudaDriver.so")
		//System.load("/tmp/cuda/libDeliteCudaDriver.so")
		
		// Initialize GPU Device
		val driver = new DeliteCudaDriver
		
		//TODO: Need to pass device number as the parameter
		val module = driver.cudaDevSet(devIdx)

		/*
    // Create Streams
		driver.stream = new Array[Long](30)
    for(i <- 0 until 30)
			driver.stream(i) = driver.cudaCreateStream
		*/

    // Get Kernel Functions
    val gKernelHandler = new Array[Long](gKernelName.length)
    for(i <- 0 until gKernelName.length) {
      gKernelHandler(i) = driver.cudaGetFunction(module, gKernelName(i))
    }
    
		// Initialize the static variables in the native methods
		driver.initIDs
	  (driver, gKernelHandler)
	}
}


/*
	val Async2I1D:Int = 1  // 2 Input 1 Grid dimension Kernel
	val Async2I2D:Int = 2  // 2 Input 2 Grid dimension Kernel
	val Async1I1D:Int = 3  // 1 Input 1 Grid dimension Kernel
	val Async1I2D:Int = 4  // 1 Input 2 Grid dimension Kernel
	val AsyncMAP:Int = 5	// MAP function for Naive Bayes
	val AsyncMAPToVec:Int = 6	// MAPToVec function for Naive Bayes
	val AsyncMAPLR:Int = 7	// MAP function for Naive Bayes
	val Async3D2I:Int = 8
	val Async3D3I:Int = 9
	val Async2D1D1I:Int = 10
	val Async2D2I2Dim:Int = 11

  // JNI functions for RBM
  val AsyncRBM_1I:Int = 12
  val AsyncRBM_1I1S:Int = 13
  val AsyncRBM_2I:Int = 14
  val AsyncRBM_Repmat:Int = 15
  val AysncRBM_1I2D:Int = 16
  val AysncRBM_1I3D:Int = 17

  // JNI function for K-means
  val AsyncKM1:Int = 18
  val AsyncKM2:Int = 19
  val AsyncKM3:Int = 20

  val AsyncMdotV: Int = 21

  val AsyncRand: Int = 22

  val AsyncMatDotV:Int = 23
	val Async3D3IReg:Int = 24

	val AsyncLaunch1D:Int = 25
  val AsyncLaunch2D:Int = 26
 */
