package ppl.apps.bio.flow

import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.core.appinclude._
import collection.mutable.PriorityQueue
import ppl.delite.dsl.optiml._
import train.TrainingSet

/**
 * Author: Bo Wang
 * Date: Jan 6, 2011
 * Time: 11:05:01 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class PQ(fold:Int) {
/*
  case class pair(_1:ACluster, _2:Double) {}
  implicit def orderedPair(self: pair): Ordered[pair] =  new Ordered[pair]{
      def compare(other: pair) = if(self._2 < other._2) -1 else if(self._2 > other._2) 1 else 0
  }
  var Q = new PriorityQueue[pair]()(orderedPair)
*/
/*
  type Pair = Tuple2[ACluster, Double]
  implicit def PairToOrdered(thisPair: Pair): Ordered[Pair] = new Ordered[Pair]{
    def compare(thatPair: Pair) = if(thisPair._2 < thatPair._2) -1 else if(thisPair._2 > thatPair._2) 1 else 0
  }
  var Q = new PriorityQueue[Pair]()(PairToOrdered)
*/

  case class Pair(_1:ACluster, _2:Double) extends Ordered[Pair] {
    def compare(that: Pair) = if(this._2 < that._2) -1 else if(this._2 > that._2) 1 else 0
  }

  var Q = new PriorityQueue[Pair]()

  var MMD: Double = Math.MAX_DOUBLE
  var Fold: Int = fold

  def empty = Q.isEmpty
  def normalize(){
    var flag = true
    while(!Q.isEmpty && flag){
      if(Q.head._2 >= MMD)
        Q.dequeue
      else
        flag = false
    }
  }
  def push(c:ACluster, d:Double){
  /*
    if(c.get_merged)
      MMD = min(MMD, d)
    else if (d < MMD){
      Q.enqueue(new Pair(c,d))
      if (Q.size > Fold)
        Q.dequeue
    }
  */
    if (d < MMD){
      Q.enqueue(new Pair(c,d))
      if (Q.size > Fold)
        Q.dequeue
    }
  }
  def top = Q.head._1
  def pop(){
    Q.dequeue
  }
}

class ACluster() extends Ordered[ACluster] {
  var dim: Int = 0
  var center: Vector[Double] = null
  var valid: Boolean = false
  var merged: Boolean = false
  var members: Vector[Vector[Double]] = null
  var num_members: Int = 0

  def pprint(){
    print("["+num_members+"] ")
    center.pprint
    members.foreach{_.pprint}
  }
  
  def compare(that:ACluster) = this.num_members - that.num_members

  def this(d: Int){
    this()
    dim = d
  }
  
  def init_RM(data:Vector[Double], i:Int){
    center = data.clone
    valid = true
    merged = false
    members = Vector[Vector[Double]](0)
    members += data.clone
    members(0).index = i
    num_members = 1
  }

  def destroy(){}

  def get_valid = valid
  def set_valid(v:Boolean) {
    valid = v
  }
  def get_merged = merged
  def set_merged(m:Boolean) {
    merged = m
  }
  def get_num_members = num_members

  def reset_RM(){
    merged = false
    var i = 0
    while(i < dim){
      ColLT_RM.set(i)
      nth_element(members, 0, num_members/2, num_members)
      center(i) = members(num_members/2)(i)
      i += 1
    }
  }

  def push_on_pq(from: ACluster, pq: PQ){
    from.members.foreach{ member =>
      pq.push(from, absdist(center, member))
    }
  }

  def merge_in_pq(pq: PQ) {
    pq.normalize
    while(!pq.empty){
      val rhs = pq.top
      if(!rhs.get_merged)
        this.merge_in(rhs)
      pq.pop
    }
  }

  def merge_in(rhs: ACluster){
    rhs.merged = true
    rhs.valid = false
    members ++= rhs.members
    num_members += rhs.num_members
  }

  object ColLT_RM {
    var col:Int = -1
    def set(c:Int) {col = c}
    def apply(l:Vector[Double], r:Vector[Double]) = l(col)<r(col)
  }

  def insertion_sort (array:Vector[Vector[Double]], first:Int, last:Int) {
    var current = first + 1
    while (current < last) {
      val tmp = array(current)
      var i = current
      var tmp1 = array(i-1)
      while(ColLT_RM(tmp, tmp1) && first<i-1){
        array(i) = tmp1
        i -= 1
        tmp1 = array(i - 1)
      }
      if(ColLT_RM(tmp,tmp1)){
        array(i-1) = tmp
        array(i)   = tmp1
      }
      else{
        array(i) = tmp
      }
      current += 1
    }
  }

  def quickPartition(array:Vector[Vector[Double]], first:Int, last:Int):Int = {
    var ff = first
    var ll = last
    var f = array(ff)
    var l = array(ll - 1)
    var pivot = array(ff + (ll-ff)/2)

    if (ColLT_RM(pivot,f)) {
      if (ColLT_RM(f,l))  pivot = f
      else if (ColLT_RM(pivot,l))  pivot = l
    }
    else if (ColLT_RM(l,f))  pivot = f
    else if (ColLT_RM(l,pivot))  pivot = l

    ff -= 1
    while (true) {
      ff += 1
	    while (ColLT_RM(array(ff), pivot)){ ff+=1 }
      ll -= 1
      while (ColLT_RM(pivot, array(ll))){ ll-=1 }
      if (ff >= ll)  return ff
      val tmp = array(ff)
      array(ff) = array(ll)
      array(ll) = tmp
    }
    ff
  }

  def nth_element(array:Vector[Vector[Double]], f:Int, nth:Int, l:Int){
    var first = f
    var last  = l
    while (last - first > 3) {
	    val cut = quickPartition(array, first, last);
	    if (cut <= nth)  first = cut;
	    else  last = cut;
      }
    insertion_sort(array, first, last);
  }

}

object Clustering {

  def cluster(data: TrainingSet[Double], k: Int):Vector[Int] = {

    // val fout = new java.io.FileWriter("output.txt")

    printf("obs = %d, dim = %d, k = %d\n", data.numRows, data.numCols, k)

    val c_ap = (0::data.numRows){i=> new ACluster(data.numCols)}
    (0::c_ap.length){ i =>
      c_ap(i).init_RM(data(i), i)
    }

    var vec_pair: (Vector[ACluster],Vector[ACluster]) = null
    var ac_valid: Vector[ACluster] = c_ap
    var flag = true
    var round = 0
    while(flag){
      vec_pair = ac_valid.stable_partition(ac=> ac.get_valid)
      ac_valid = vec_pair._1

      println("\nround = " + round + ", num_valid = " + ac_valid.length)
      // ac_valid.foreach( _.pprint )

      if(round==5){
        ac_valid.foreach{ ac=>
          if(ac.get_num_members==1)
            ac.set_valid(false)
        }
        vec_pair = ac_valid.stable_partition(ac=> ac.get_valid)
        ac_valid = vec_pair._1
      }

      val num_valid = ac_valid.length
      if(num_valid < 1.5*k){
        flag = false
      }
      else{
        if(round!=0){
          ac_valid.sort
          ac_valid.foreach{ _.reset_RM }
        }
        val fold = max(1, num_valid/5000).toInt
        var into = 0
        /*
        while(into < num_valid){
          if(!ac_valid(into).get_merged){
            ac_valid(into).set_merged(true)
            val pq = new PQ(fold)
            (0::num_valid-1){ from =>
              if(into != from)
                ac_valid(into).push_on_pq(ac_valid(from), pq)
            }
            ac_valid(into).merge_in_pq(pq)
          }
          into += 1
        }
        */

        while(into < num_valid){
          if(!ac_valid(into).get_merged){
            ac_valid(into).set_merged(true)
            val pq = new PQ(fold)
            val into_center = ac_valid(into).center
            val _data1 = into_center.data
            val dim = ac_valid(into).dim
            (0::num_valid-1){ from =>
              if(into != from){
                // ac_valid(into).push_on_pq(ac_valid(from), pq)
                val from_cluster = ac_valid(from)
                var i = 0
                while(i < from_cluster.num_members){
                  // val d = absdist(into_center, from_cluster.members(i))
                  val _data2 = from_cluster.members(i).data
                  var d:Double = 0
                  var idx = 0
                  while(idx < dim){
                    val tmp = _data1(idx) - _data2(idx)
                    d += java.lang.Double.longBitsToDouble((java.lang.Double.doubleToRawLongBits(tmp)<<1)>>>1)
                    idx += 1
                  }
                  pq.push(from_cluster, d)
                  i += 1
                }
              }
            }
            ac_valid(into).merge_in_pq(pq)
          }
          into += 1
        }

        round += 1
      }

    }

    // Assignment and clean up
    var assgn = Vector[Int](data.numRows)
    var cur_Id = 1
    vec_pair = ac_valid.stable_partition(ac=> ac.get_valid)
    ac_valid = vec_pair._1
    for(i <- 0 until ac_valid.length){
      for(b <- ac_valid(i).members)
        assgn(b.index) = cur_Id
      cur_Id += 1
    }

    assgn
  }

  
}