package ppl.apps.ml.baseline.lbp

import java.io._

object LBPParser {
  import BPNode._
  import LBPModel._
   /**
   * Parse pertinent statistics from an edge file representing a graph (e.g. onlyedges-stanford)
   *
   * @param File handle to be read
   * @returns (num_nodes, num_edges, max_line_size, max_node_num)
   */
  def get_graph_values(inFile: File) : (Int,Int,Int,Int) = {

    /* first pass:  find number of nodes, edges, and max line size */
    var num_nodes = 0
    var num_edges = 0
    var max_line_size = 0

    var max_node_num = 0 /*xli*/
    var cur_line_size = 0
    var node_num_index = 0

    val reader = new BufferedReader(new FileReader(inFile))
    var line = reader.readLine()
    while (line != null){
      val fields = line.split("\t")
      val node_str = fields(0)
      val edge_str = fields(1)
      val node = node_str.toInt
      val edges = edge_str.split(" ").map(s => s.toInt)

      num_nodes += 1
      num_edges += edges.length

      if (node > max_node_num) max_node_num = node
      for (edge <- edges){
        if (edge > max_node_num) max_node_num = edge
      }

      if (line.length() > max_line_size) max_line_size = line.length()
      line = reader.readLine()
    }

    return (num_nodes, num_edges, max_line_size, max_node_num)
  }

  /**
   * Parse an entire edge file representing a graph (e.g. onlyedges-stanford).
   *
   * TODO: This should parse into a graph or factor graph.
   *       create data types for Node and Edge instead of array[Int]
   *
   * @param inFile: File handle to be read
   * @param max_node_num
   * @param num_edges
   * @returns (edge_index, nodes[], l_edges[], r_edges[])
   */
  def read_graph(inFile: File, max_node_num: Int, num_edges: Int)
    : (Int, Array[Int], Array[Int], Array[Int]) = {

    val nodes = Array[Int](max_node_num+1)
    val l_edges = Array[Int](num_edges)
    val r_edges = Array[Int](num_edges)

    /* second pass:  build graph */
    var edge_index = 0
    val reader = new BufferedReader(new FileReader(inFile))
    var line = reader.readLine()
    while (line != null){
      val fields = line.split("\t")
      val node_str = fields(0)
      val edge_str = fields(1)
      val node = node_str.toInt
      val edges = edge_str.split(" ").map(s => s.toInt)

      nodes(node) = node

      for (edge <- edges){
        nodes(edge) = edge
        l_edges(edge_index) = node
        r_edges(edge_index) = edge
        edge_index += 1
      }
    }

    return (edge_index, nodes, l_edges, r_edges)
  }


  /**
   * Parse an entire edge file representing a graph (e.g. onlyedges-stanford).
   *
   * TODO: This should parse into a graph or factor graph.
   *       create data types for Node and Edge instead of Array[Int]
   *
   * @param inFile: File handle to be read
   * @param max_num_node
   * @param num_edges
   * @returns (edge_index, node_ids[], edges[])
   */
  def read_graph_lr(inFile: File, max_node_num: Int, num_edges: Int)
    : (Int, Array[Int], Array[Array[Int]]) = {

    val nodes = new Array[Int](max_node_num+1)
    val edges = Array.ofDim[Int](num_edges, 2)

    /* second pass:  build graph */
    var edge_index = 0
    val reader = new BufferedReader(new FileReader(inFile))
    var line = reader.readLine()
    while (line != null){
      val fields = line.split("\t")
      val node_str = fields(0)
      val edge_str = fields(1)
      val node = node_str.toInt
      val f_edges = edge_str.split(" ").map(s => s.toInt)

      nodes(node) = node

      for (edge <- f_edges){
        nodes(edge) = edge
        edges(edge_index)(0) = node
        edges(edge_index)(1) = edge
        edge_index += 1
      }

      line = reader.readLine()
    }

    return (edge_index, nodes, edges)
  }


  /**
   * Parse a belief file (e.g. graphprint-stanford)
   *
   * @param
   * @returns edge_potential
   */
  def read_beliefs(inFile: File, nodes: Array[BPNode], last_cat_node: Int, last_node: Int)
    : Array[Double] = {

    val reader = new BufferedReader(new FileReader(inFile))

    /* nothing on the first 2 lines of graphprint file */
    reader.readLine()
    reader.readLine()

    var line = reader.readLine()
    var fields: Array[String] = null

    /* parse the potentials for the category nodes */
   /* printf("parse potentials for category nodes\n");*/
    var node_id = -1
    var num_spaces = 0
    while (line != null && node_id != last_cat_node){
      fields = line.replaceAll("[\\[\\]\\=]", "").split(" ")
      num_spaces = fields.length-1
      if (num_spaces == 17){ // a connected code
        node_id = fields(1).toInt
        nodes(node_id).real_belief = fields(2).toInt
        for (i <- 1 to CAT_BELIEF_SIZE){
          val dbl = fields(2*i+1).toDouble
          nodes(node_id).potential(i-1) = dbl
          nodes(node_id).belief(i-1) = dbl
        }
        /* initialize in-coming messages */
        for (i <- 0 until nodes(node_id).num_edges)
          for (j <- 0 until CAT_BELIEF_SIZE)
            nodes(node_id).messages(i)(j) = 1.0 / CAT_BELIEF_SIZE
      }
      else if (num_spaces == 3){ // an unconnected node
        fields = line.replaceAll("[\\[\\]\\=]", "").split(" ")
        node_id = fields(1).toInt
        nodes(node_id).real_belief = fields(2).toInt
      }
      else{
        printf("error:  encountered unknown category node format\n")
        printf("%s\n", line)
        exit(-1);
      }
      line = reader.readLine()
    }

    /* forward file pointer to the relations nodes */
    num_spaces = count_spaces(reader.readLine())
    while (num_spaces == 3){
      num_spaces = count_spaces(reader.readLine())
    }

    /* parse the potentials for the relations nodes */
    /*printf("parse potentials for relations nodes\n");*/
    line = reader.readLine()
    while (line != null && node_id != last_node){
      fields = line.replaceAll("[\\[\\]\\=]", "").split(" ")
      num_spaces = fields.length-1
      if (num_spaces == 13){
        node_id = fields(1).toInt
        nodes(node_id).real_belief = fields(2).toInt

        for (i <- 1 to REL_BELIEF_SIZE){
          val dbl = fields(2*i+1).toDouble
          nodes(node_id).potential(i-1) = dbl
          nodes(node_id).belief(i-1) = dbl
        }

        /* initialize in-coming messages */
        for (i <- 0 until nodes(node_id).num_edges){
          for (j <- 0 until REL_BELIEF_SIZE){
            nodes(node_id).messages(i)(j) = 1.0 / REL_BELIEF_SIZE
          }
        }
      }
      else{
        printf("error:  encountered unknown relations node format\n");
        printf("%s\n", line);
        exit(-1);
      }

      line = reader.readLine()
    }

    /* forward file pointer to the edge potential */
    line = reader.readLine()

    /* parse the edge potential */
    /*printf("parse edge potentials\n");*/
    fields = line.replaceAll("[\\[\\]\\=]", "").split(" ")
    num_spaces = fields.length-1
    if (num_spaces != 95) {
      printf("error:  encountered unknown edge potential format\n");
      printf("%s\n", line);
      exit(-1);
    }
    val edge_potential = new Array[Double](CAT_BELIEF_SIZE * REL_BELIEF_SIZE)
    for(i <- 1 to CAT_BELIEF_SIZE * REL_BELIEF_SIZE){
      val dbl = fields(2*i-1).toDouble
      edge_potential(i-1) = dbl
    }

    return edge_potential

  }


  def print_beliefs(outFile: File, begin: Int, num_nodes: Int, nodes: Array[BPNode]) = {
    val writer = new BufferedWriter(new FileWriter(outFile))

    for (i <- begin until num_nodes) {
      if (nodes(i).valid) {
        writer.write(String.valueOf(i) + "[")
        for (j <- 0 until nodes(i).belief_size) {
          if (j == nodes(i).belief_size - 1){
            writer.write(String.valueOf(nodes(i).belief(j)))
          }
          else {
            writer.write(String.valueOf(nodes(i).belief(j)) + "  ")
          }
        }
        writer.write("]\n")
      }
    }

  }

  private def count_spaces(s: String) = {
    var num_spaces = 0

    for (c <- s)
      if (c == ' ') num_spaces += 1

    num_spaces
  }

}