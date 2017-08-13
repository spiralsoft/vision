package primatives

import breeze.linalg.DenseVector
import primatives.IndexType.IndexType

object IndexType extends Enumeration {
  type IndexType = Value
  val RowMajor, ColumnMajor = Value
}

/**
  *
  * Storing multidimensional matrices in scala
  *
  * @param dim The dimension of the matrix as a sequence of integers
  */


// What I'll be getting:
// [[x, y, z, {c, r, g, b}], [x, y, z, c], [x, y, z, c]]
// I want to retrieve the information at point [x, y, z, 1-4]
// The other thing I may receive is a 3 dimension matrix. I may want to receive all of the values with [x, y]

// I want to be able to get all zs at a certain x,y
// I want to be able to get all {c,r,g,b} at a certain xyz
// I want to be able to know the number of z populated at x,y

// The 1D array can be indexed in such a way that I can can give it a formula, matrix.get(Seq(x,y,z,n)) and I get back c
// Or I can use matrix.get(Seq(x,y,z)) and get back {n}/None/Null
// Or I can use matrix.get(Seq(x,y))

class MultiDimensionalMatrix[Double] (dim: Seq[Int], input_data: Array[Double]) {
  val data: DenseVector[Double] = DenseVector[Double](input_data)

  def getArrayIndexRowMajor(index: Seq[Int]): Int =
  {
    index.zipWithIndex.foldLeft(0)[Int]{
      case (accum: Int, index_dim: (Int, Int)) => index_dim._1 + index_dim._2 * accum
    }
  }


  def getArrayIndexColumnMajor(index: Seq[Int]): Int =
  {
    index.zipWithIndex.foldRight(0)[Int]{
      case (index_dim: (Int, Int), accum: Int) => index_dim._1 + index_dim._2 * accum
    }
  }

}
