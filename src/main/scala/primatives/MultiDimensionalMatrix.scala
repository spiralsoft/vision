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

class MultiDimensionalMatrix (
  val dim: Array[Int],
  input_data: Array[Double],
  val index_type: IndexType = IndexType.RowMajor
) {

  val data: DenseVector[Double] = DenseVector[Double](input_data)

  val getIndex: (Seq[Int]) => Int = {
    index_type match {
      case IndexType.ColumnMajor => MultiDimensionalMatrix.getArrayIndexColumnMajor
      case IndexType.RowMajor    => MultiDimensionalMatrix.getArrayIndexRowMajor
    }
  }


}


object MultiDimensionalMatrix {
  /**
    * Generates a multidimensional matrix from a flattened array of
    * (coordinate, ..., coordinate -> value, ..., value) / (value, ..., value -> coordinate, ..., coordinate) pairs
    *
    * @param dim The dimension of the target matrix.
    * @param arr Flattened array of Seq[coordinate] --> Seq[value] pairs. The number of values per coordinate is the final dimension
    *            of the array.
    */
  def apply(dim: Array[Int], arr: Seq[Double], index_type: IndexType.IndexType = IndexType.RowMajor): MultiDimensionalMatrix = {

    val pair_length = {
      index_type match {
        case IndexType.RowMajor    => dim.length - 1 + dim.last
        case IndexType.ColumnMajor => dim.length - 1 + dim.head
      }
    }

    apply(dim, arr.grouped(pair_length).toIterable, index_type)

  }
  

  private def generateCoordValDataArrayRowMajor(dim: Array[Int], points: Iterable[Iterable[Double]]): Array[Double] = {
    val size = getArrayIndexRowMajor(dim)

    val num_coords  = dim.length - 1
    val num_vals    = dim.last
    val pair_length = num_coords + num_vals

    val data_array = Array.fill[Double](size)(null)

    // Indexes the data array appropriately, filling the coordinate spaces with the values of the last dimension
    points.par.foreach(
      (point: Iterable[Double]) => {

        // Gives the coordinate excluding the last value
        val coord = point.slice(0, num_coords + 1).map(double => double.toInt)
        val index = getArrayIndexColumnMajor(coord)

        val values = point.slice(num_coords + 1, pair_length)

        // Adds the final dimension position to the current index and
        // sets the value at that index equal to the retrieved value
        var value_ind = 0
        values.foreach((point_value) => {
          data_array(index + value_ind) = point_value
          value_ind = value_ind + 1
        })

      }
    )

    data_array
  }

  private def generateCoordValDataArrayColumnMajor(dim: Array[Int], points: Iterable[Iterable[Double]]): Array[Double] = {
    val size = getArrayIndexColumnMajor(dim)

    val num_coords  = dim.length - 1
    val num_vals    = dim.head
    val pair_length = num_coords + num_vals

    val data_array = Array.fill[Double](size)(null)

    // Indexes the data array appropriately, filling the coordinate spaces with the values of the last dimension
    points.par.foreach(
      (point: Iterable[Double]) => {

        // Gives the coordinate excluding the values
        val coord = Seq(0) ++ point.slice(num_vals + 1, num_coords + 1).map(double => double.toInt)
        val index = getArrayIndexColumnMajor(coord)

        val values = point.slice(0, num_vals + 1)

        // Adds the first dimension position to the current index and
        // sets the value
        var value_ind = 0
        values.foreach((point_value) => {
          data_array(index + value_ind) = point_value
          value_ind = value_ind + 1
        })

      }
    )

    data_array
  }


  /**
    * Generates a multidimensional matrix from an array of arrays that contain coordinate, ..., coordinate, value, ..., values
    * Handles pointclouds with variable number of C->RGB->Any
    * @param points
    */
  def apply(dim: Array[Int], points: Iterable[Iterable[Double]], index_type: IndexType.IndexType = IndexType.RowMajor): MultiDimensionalMatrix = {
    val data_array: Array[Double] = {
      index_type match {
        case IndexType.ColumnMajor => generateCoordValDataArrayColumnMajor(dim, points)
        case IndexType.RowMajor    => generateCoordValDataArrayRowMajor(dim, points)
      }
    }

    new MultiDimensionalMatrix(dim, data_array, index_type)
  }

  private def getArrayIndexRowMajor(index: Iterable[Int]): Int =
  {
    index.view.zipWithIndex.foldLeft(0)[Int]{
      case (accum: Int, index_dim: (Int, Int)) => index_dim._1 + index_dim._2 * accum
    }
  }


  private def getArrayIndexColumnMajor(index: Iterable[Int]): Int =
  {
    index.view.zipWithIndex.foldRight(0)[Int]{
      case (index_dim: (Int, Int), accum: Int) => index_dim._1 + index_dim._2 * accum
    }
  }
}

