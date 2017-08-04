package primatives

import breeze.linalg.DenseVector

/**
  * Created by shea on 7/25/17.
  *
  *
  * Underlying implementation is the breeze matrix with a size quality
  *
  *
  * I'm sure that I'm going top need to be doing a number of matrix operations with this
  * but since Breeze doesn't have N dimensional matrices implemented I'll probably have to use
  * a DenseVector as a backing datastructure.
  *
  * The multidimensional matrix implementation would likely be something similar, so
  * this file is marked with the intention of being pull requested into Breeze once it is near
  * feature complete. Depends on how many matrix operations I need to implement in order to solve the core
  * pcl functions.
  *
  *
  * It's probably best to just drive forward until I need to perform linear algerbra operations.
  * I'll follow breeze as a template until then.
  *
  *
  * Notes on storing multidimensional matrices in scala:
  *
  */

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
