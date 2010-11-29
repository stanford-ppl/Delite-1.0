package ppl.apps.genome

import scala.collection.mutable.Set

import ppl.delite.core.DeliteApplication
import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml.{Vector, Matrix}
import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.dsl.primitive.DeliteLong

object Genome extends DeliteApplication {
  val random = new java.util.Random(100)

  val bias = 37
  val gene_len = 256
  val segment_len = 16
  val segment_num = 128//1024//16384
  val nucleotides = Array("A","C","G","T")
  val hash_func: (Int, Int) => Int = (a: Int, b: Int) => (a + (b << 6) + (b << 16) - b)
  val start_hashes: Array[Map[Long, Set[Int]]] = Array.fromFunction(_ => Map.empty[Long, Set[Int]])(segment_len)

  def gene_create(len: Int) : Vector[Int] = {
    val g = Vector[Int](len)
    for (ii <- 0 until len) g(ii) = random.nextInt(4) + bias
    g
  }

  def segs_create(num: Int, len: Int, gl: Int, g: Vector[Int]) : Vector[Vector[Int]] = {
    val s = Vector[Vector[Int]](segment_num)
    for (ii <- 0 until segment_num) {
      val ix = random.nextInt(gl - len + 1)
      s(ii) = g.slice(ix, ix + len)
    }
    s
  }

  def eliminate_dups(s: Vector[Vector[Int]]) : Vector[Vector[Int]] = {
    val hashset = Set.empty[Long]
    s.filter(it => {
      val hash: Long = it.hash(0,segment_len,hash_func)
      val pred = hashset.contains(hash)
      if (!pred) hashset += hash
      pred
    })
  }

/*
  def hash_subsegs(segs: Vector[Vector[Int]]) {
    for (s <- 0 until segs.length) {
      for (ii <- 0 until segment_len - 1) {
        val starthashval: Long = segs(s).hash(0, ii + 1, 0, hash_func)
        if (!start_hashes(ii).contains(starthashval))
          start_hashes(ii) += (starthashval -> Set(s))
        else start_hashes(ii)(starthashval) += s
      }
    }
  }
*/

  def hash_subsegs(segs: Vector[Vector[Int]]) {
      for (ii <- 0 until segment_len - 1) {
        val it_hashes = segs.map(s => {
          val starthashval: Long = s.hash(0, ii + 1, hash_func)
          start_hashes(ii) += (starthashval -> Set.empty[Int])
          starthashval
        })
        for (it <- 0 until it_hashes.size)
          start_hashes(ii)(it_hashes(it)) += it
      }
  }

  def match_ends(segs: Vector[Vector[Int]]) {
    var len = segment_len - 1
    while(len > 0) {
      println("  END HASHES LENGTH " + len)// + " START KEYS " + start_hashes(len-1).keys.mkString(","))
      for (s <- 0 until segs.length) {
        val endhashval: Long = segs(s).hash(segment_len - len, segment_len, hash_func)
        if (start_hashes(len-1).contains(endhashval)) {
          start_hashes(len-1)(endhashval).foreach(it => {
            val start = segs(it).slice(0,len)
            val end = segs(s).slice(segment_len - len, segment_len)
            if (start cmp end) {
//              println("    SEGMENTS " + s + " and " + it + " connect")
            } else {
              println("      SEGS " + s + " and " + it + " matched, but do not connect ")
              print("      "); print_gene(start); println()
              print("      "); print_gene(end); println()
            }
          })
        }
      }
      len -= 1
    }
  }

  def print_gene(g: Vector[Int]) {
    print("{"); g.foreach(e => print(nucleotides(e-bias))); print("}")
  }

  def print_segs(s: Vector[Vector[Int]]) {
    for (it <- 0 until s.size) {
      print_gene(s(it))
      s(it).pprint
    }
  }

  def print_start_hashes(segs: Vector[Vector[Int]]) {
    var len = segment_len - 2
    println("START HASHES")
    while(len >= 0) {
      println((len+1) + ":")
      for (k <- start_hashes(len).keys) println("    " + k + ": " + start_hashes(len)(k).mkString(","))
      len -= 1
    }
  }

  def run(args: Array[String]) = {
    val gene = gene_create(gene_len)
    val segs = segs_create(segment_num, segment_len, gene_len, gene)
    val usegs = eliminate_dups(segs)
    hash_subsegs(usegs)
    match_ends(usegs)
    println("SEGS (" + usegs.size + "):"); print_segs(usegs)
    println("GENE:"); print_gene(gene); println()
  }
}
