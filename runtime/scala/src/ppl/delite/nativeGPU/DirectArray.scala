package ppl.delite.nativeGPU

import sun.misc.Cleaner
import ppl.delite.agent.UnsafeAccessor

/**
 * Author: Kevin J. Brown
 * Date: Feb 2, 2010
 * Time: 7:16:25 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * NOTE: To increase performance, bounds checking on apply, update methods have been removed 
 * NOTE: To increase performance, array contains garbage upon allocation; explicitly zero with .zero
 */

abstract class DirectArray[T <: AnyVal](private val cap: Int) {

  //accessors
  def apply(i: Int) : T
  def update(i: Int, x: T)

  //unsafe operations accessor
  //protected val unsafe = sun.misc.Unsafe.getUnsafe
  protected val unsafe = UnsafeAccessor.unsafe

  //allocate the array
  protected val elms = unsafe.allocateMemory(cap)

  //add the capacity to the global counter of DirectArray memory usage
  DirectArray.add(cap)

  //unsafe.setMemory(elms, cap, 0)

  //create a cleaner object which will run Deallocator thunk when instance of DirectArray becomes phantom reachable
  private val cleaner = Cleaner.create(this, new Deallocator(elms, cap))

  //size of array in bytes
  protected def capacity = cap

  //set all bytes of native array to 0
  def zero { unsafe.setMemory(elms, cap, 0) }

  //explicitly free native memory
  def clean = cleaner.clean
 
  //address accessor
  private[delite] def getAddr = elms

  //hack so that the JVM sees this object with it's proper size (incl. native memory); this array should never be used
  //private val buoy = new Array[Byte](cap)

}

//class to free memory after object GC
private class Deallocator(addr: Long, size: Int) extends Runnable {
  def run {
    UnsafeAccessor.unsafe.freeMemory(addr)
    DirectArray.remove(size)
    //println("freed " + addr)
  }
}

object DirectArray {

  private var memUsed = 0

  private val threshold = Runtime.getRuntime.maxMemory / 2

  private def add(amt: Int) {
    synchronized {
      memUsed += amt
      checkUsage
    }
  }

  private[nativeGPU] def remove(amt: Int) {
    synchronized {
      if (memUsed > amt) memUsed -= amt
      else memUsed = 0
    }
  }

  private def checkUsage {
    if (memUsed > threshold) {
      System.gc
      //println("threshold breached: " + System.currentTimeMillis)
    }
  }

}


object DoubleArray {
  def apply(cap: Int) = new DoubleArray(cap)

  def copy(src: DoubleArray, srcPos: Int, dest: DoubleArray, destPos: Int, len: Int) {
    var si = srcPos
    var di = destPos
    var c = 0
    while (c < len){
      dest(di) = src(si)
      di += 1
      si += 1
      c += 1
    }
  }
}

class DoubleArray(private val cap: Int) extends DirectArray[Double](8 * cap) {

  //read
  def apply(i: Int): Double = {
    //if (i < 0 || i >= cap) throw new IndexOutOfBoundsException
    unsafe.getDouble(elms + i*8)
  }

  //write
  def update(i: Int, d: Double) {
    //if (i < 0 || i >= cap) throw new IndexOutOfBoundsException
    unsafe.putDouble(elms + i*8, d)
  }

  //copy contents of orig into this
  def fillWith(orig: DoubleArray) {
    if (orig.capacity > cap) throw new IllegalArgumentException("Cannot fill a smaller array from a larger array")
    var i = 0
    val end = orig.capacity
    val origaddr = orig.elms
    while (i != end) {
      unsafe.putDouble(elms + i*8, unsafe.getDouble(origaddr + i*8))
      i += 1
    }
  }

  //copy contents of this
  override def clone: DoubleArray = {
    val ret = new DoubleArray(cap)
    ret fillWith this
    ret
  }

  //size of array in doubles
  override def capacity = cap

}

object FloatArray {
  def apply(cap: Int) = new FloatArray(cap)

  def copy(src: FloatArray, srcPos: Int, dest: FloatArray, destPos: Int, len: Int) {
    var si = srcPos
    var di = destPos
    var c = 0
    while (c < len){
      dest(di) = src(si)
      di += 1
      si += 1
      c += 1
    }
  }
}

class FloatArray(private val cap: Int) extends DirectArray[Float](4 * cap) {

  //read
  def apply(i: Int): Float = {
    //if (i < 0 || i >= cap) throw new IndexOutOfBoundsException
    unsafe.getFloat(elms + i*4)
  }

  //write
  def update(i: Int, f: Float) {
    //if (i < 0 || i >= cap) throw new IndexOutOfBoundsException
    unsafe.putFloat(elms + i*4, f)
  }

  //copy contents of orig into this
  def fillWith(orig: FloatArray) {
    if (orig.capacity > cap) throw new IllegalArgumentException("Cannot fill a smaller array from a larger array")
    var i = 0
    val end = orig.capacity
    val origaddr = orig.elms
    while (i != end) {
      unsafe.putFloat(elms + i*4, unsafe.getFloat(origaddr + i*4))
      i += 1
    }
  }

  //copy contents of this
  override def clone: FloatArray = {
    val ret = new FloatArray(cap)
    ret fillWith this
    ret
  }

  //size of array in floats
  override def capacity = cap

}


object IntArray {
  def apply(cap: Int) = new IntArray(cap)

  def copy(src: IntArray, srcPos: Int, dest: IntArray, destPos: Int, len: Int) {
    var si = srcPos
    var di = destPos
    var c = 0
    while (c < len){
      dest(di) = src(si)
      di += 1
      si += 1
      c += 1
    }
  }
}

class IntArray(private val cap: Int) extends DirectArray[Int](4 * cap) {

  //read
  def apply(i: Int): Int = {
    //if (i < 0 || i >= cap) throw new IndexOutOfBoundsException
    unsafe.getInt(elms + i*4)
  }

  //write
  def update(i: Int, v: Int) {
    //if (i < 0 || i >= cap) throw new IndexOutOfBoundsException
    unsafe.putInt(elms + i*4, v)
  }

  //copy contents of orig into this
  def fillWith(orig: IntArray) {
    if (orig.capacity > cap) throw new IllegalArgumentException("Cannot fill a smaller array from a larger array")
    var i = 0
    val end = orig.capacity
    val origaddr = orig.elms
    while (i != end) {
      unsafe.putInt(elms + i*4, unsafe.getInt(origaddr + i*4))
      i += 1
    }
  }

  //copy contents of this
  override def clone: IntArray = {
    val ret = new IntArray(cap)
    ret fillWith this
    ret
  }

  //size of array in ints
  override def capacity = cap

}

object LongArray {
  def apply(cap: Int) = new LongArray(cap)

  def copy(src: LongArray, srcPos: Int, dest: LongArray, destPos: Int, len: Int) {
    var si = srcPos
    var di = destPos
    var c = 0
    while (c < len){
      dest(di) = src(si)
      di += 1
      si += 1
      c += 1
    }
  }
}

class LongArray(private val cap: Int) extends DirectArray[Long](8 * cap) {

  //read
  def apply(i: Int): Long = {
    //if (i < 0 || i >= cap) throw new IndexOutOfBoundsException
    unsafe.getLong(elms + i*8)
  }

  //write
  def update(i: Int, l: Long) {
    //if (i < 0 || i >= cap) throw new IndexOutOfBoundsException
    unsafe.putLong(elms + i*8, l)
  }

  //copy contents of orig into this
  def fillWith(orig: LongArray) {
    if (orig.capacity > cap) throw new IllegalArgumentException("Cannot fill a smaller array from a larger array")
    var i = 0
    val end = orig.capacity
    val origaddr = orig.elms
    while (i != end) {
      unsafe.putLong(elms + i*8, unsafe.getLong(origaddr + i*8))
      i += 1
    }
  }

  //copy contents of this
  override def clone: LongArray = {
    val ret = new LongArray(cap)
    ret fillWith this
    ret
  }

  //size of array in longs
  override def capacity = cap

}

object ShortArray {
  def apply(cap: Int) = new ShortArray(cap)

  def copy(src: ShortArray, srcPos: Int, dest: ShortArray, destPos: Int, len: Int) {
    var si = srcPos
    var di = destPos
    var c = 0
    while (c < len){
      dest(di) = src(si)
      di += 1
      si += 1
      c += 1
    }
  }
}

class ShortArray(private val cap: Int) extends DirectArray[Short](2 * cap) {

  //read
  def apply(i: Int): Short = {
    //if (i < 0 || i >= cap) throw new IndexOutOfBoundsException
    unsafe.getShort(elms + i*2)
  }

  //write
  def update(i: Int, s: Short) {
    //if (i < 0 || i >= cap) throw new IndexOutOfBoundsException
    unsafe.putShort(elms + i*2, s)
  }

  //copy contents of orig into this
  def fillWith(orig: ShortArray) {
    if (orig.capacity > cap) throw new IllegalArgumentException("Cannot fill a smaller array from a larger array")
    var i = 0
    val end = orig.capacity
    val origaddr = orig.elms
    while (i != end) {
      unsafe.putShort(elms + i*2, unsafe.getShort(origaddr + i*2))
      i += 1
    }
  }

  //copy contents of this
  override def clone: ShortArray = {
    val ret = new ShortArray(cap)
    ret fillWith this
    ret
  }

  //size of array in shorts
  override def capacity = cap

}

object ByteArray {
  def apply(cap: Int) = new ByteArray(cap)

  def copy(src: ByteArray, srcPos: Int, dest: ByteArray, destPos: Int, len: Int) {
    var si = srcPos
    var di = destPos
    var c = 0
    while (c < len){
      dest(di) = src(si)
      di += 1
      si += 1
      c += 1
    }
  }
}

class ByteArray(private val cap: Int) extends DirectArray[Byte](cap) {

  //read
  def apply(i: Int): Byte = {
    //if (i < 0 || i >= cap) throw new IndexOutOfBoundsException
    unsafe.getByte(elms + i)
  }

  //write
  def update(i: Int, b: Byte) = {
    //if (i < 0 || i >= cap) throw new IndexOutOfBoundsException
    unsafe.putByte(elms + i, b)
  }

  //copy contents of orig into this
  def fillWith(orig: ByteArray) {
    if (orig.capacity > cap) throw new IllegalArgumentException("Cannot fill a smaller array from a larger array")
    var i = 0
    val end = orig.capacity
    val origaddr = orig.elms
    while (i != end) {
      unsafe.putByte(elms + i, unsafe.getByte(origaddr + i))
      i += 1
    }
  }

  //copy contents of this
  override def clone: ByteArray = {
    val ret = new ByteArray(cap)
    ret fillWith this
    ret
  }

  //size of array in bytes
  override def capacity = cap

}

object CharArray {
  def apply(cap: Int) = new CharArray(cap)

  def copy(src: CharArray, srcPos: Int, dest: CharArray, destPos: Int, len: Int) {
    var si = srcPos
    var di = destPos
    var c = 0
    while (c < len){
      dest(di) = src(si)
      di += 1
      si += 1
      c += 1
    }
  }
}

class CharArray(private val cap: Int) extends DirectArray[Char](2 * cap) {

  //read
  def apply(i: Int): Char = {
    //if (i < 0 || i >= cap) throw new IndexOutOfBoundsException
    unsafe.getChar(elms + i*2)
  }

  //write
  def update(i: Int, c: Char) {
    //if (i < 0 || i >= cap) throw new IndexOutOfBoundsException
    unsafe.putChar(elms + i*2, c)
  }

  //copy contents of orig into this
  def fillWith(orig: CharArray) {
    if (orig.capacity > cap) throw new IllegalArgumentException("Cannot fill a smaller array from a larger array")
    var i = 0
    val end = orig.capacity
    val origaddr = orig.elms
    while (i != end) {
      unsafe.putChar(elms + i*2, unsafe.getChar(origaddr + i*2))
      i += 1
    }
  }

  //copy contents of this
  override def clone: CharArray = {
    val ret = new CharArray(cap)
    ret fillWith this
    ret
  }

  //size of array in chars
  override def capacity = cap

}

object BooleanArray {
  def apply(cap: Int) = new BooleanArray(cap)

  def copy(src: BooleanArray, srcPos: Int, dest: BooleanArray, destPos: Int, len: Int) {
    var si = srcPos
    var di = destPos
    var c = 0
    while (c < len){
      dest(di) = src(si)
      di += 1
      si += 1
      c += 1
    }
  }
}

class BooleanArray(private val cap: Int) extends DirectArray[Boolean](cap) {

  implicit def byteToBoolean(b: Byte): Boolean = if(b == 0) false else true
  implicit def booleanToByte(b: Boolean): Byte = if(b) 1 else 0

  //read
  def apply(i: Int): Boolean = {
    //if (i < 0 || i >= cap) throw new IndexOutOfBoundsException
    unsafe.getByte(elms + i)
  }

  //write
  def update(i: Int, b: Boolean) {
    //if (i < 0 || i >= cap) throw new IndexOutOfBoundsException
    unsafe.putByte(elms + i, b)
  }

  //copy contents of orig into this
  def fillWith(orig: BooleanArray) {
    if (orig.capacity > cap) throw new IllegalArgumentException("Cannot fill a smaller array from a larger array")
    var i = 0
    val end = orig.capacity
    val origaddr = orig.elms
    while (i != end) {
      unsafe.putByte(elms + i, unsafe.getByte(origaddr + i))
      i += 1
    }
  }

  //copy contents of this
  override def clone: BooleanArray = {
    val ret = new BooleanArray(cap)
    ret fillWith this
    ret
  }

  //size of array in booleans
  override def capacity = cap

}

object UnitArray {
  def apply(cap: Int) = throw new RuntimeException("DirectArray of type Unit not supported")
}
