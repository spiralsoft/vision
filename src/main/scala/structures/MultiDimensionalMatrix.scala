package structures

import breeze.linalg.DenseVector
import utils.ArrayUtils

object IndexType extends Enumeration {
  type IndexType = Value
  val RowMajor, ColumnMajor = Value
}

import IndexType._
/**
  * A multidimensional matrix
  * @param dim Dimensions of the matrix in row, column -> nth dimension format.
  * @param input_data The input, flattened array of values
  * @param index_type RowMajor/ColumnMajor
  */

class MultiDimensionalMatrix (
  val dim: Seq[Int],
  input_data: Array[Double],
  val index_type: IndexType
) {

  val data: DenseVector[Double] = DenseVector[Double](input_data)

  val getIndex: (Seq[Int]) => Int = {
    index_type match {
      case ColumnMajor => MultiDimensionalMatrix.getArrayIndexColumnMajor
      case RowMajor    => MultiDimensionalMatrix.getArrayIndexRowMajor
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
  def apply(
    dim: Array[Int],
    arr: Seq[Double],
    index_type: IndexType
  ): MultiDimensionalMatrix = {

    val pair_length = {
      index_type match {
        case RowMajor    => dim.length - 1 + dim.last
        case ColumnMajor => dim.length - 1 + dim.head
      }
    }

    apply(dim, arr.grouped(pair_length).toIterable, index_type)

  }

  /**
    * Generates a multidimensional matrix from an array of arrays that contain coordinate, ..., coordinate, value, ..., values
    * Handles pointclouds with variable number of C->RGB->Any
    * @param points
    */
  def apply(
    dim: Array[Int],
    points: Iterable[Iterable[Double]],
    index_type: IndexType
  ): MultiDimensionalMatrix = {
    val data_array: Array[Double] = {
      index_type match {
        case ColumnMajor => generateCoordValDataArrayColumnMajor(dim, points)
        case RowMajor    => generateCoordValDataArrayRowMajor(dim, points)
      }
    }

    new MultiDimensionalMatrix(dim, data_array, index_type)
  }

  /**
    * Determines the nesting based on the input structure of the array and generates a matrix
    * @param arr of the format Seq(Seq(...Seq(Double)))
    */
  def apply(
    arr: Seq[Any],
    index_type: IndexType = RowMajor
  ): MultiDimensionalMatrix = {
    val dim: Seq[Int] = determineDimension(arr)

    val size = getArraySizeFromDimension(dim)
    val data_array: Array[Double] = ArrayUtils.nullArray[Double](size)


    populateArrayFromNested(arr, data_array, index_type)

    new MultiDimensionalMatrix(dim, data_array, index_type)
  }

  /**
    * Recurses into provided array and populates a data array from it
    * @param arr An nested series of arrays
    * @param data_array the reference to the data array to populate
    * @param index_type indexing method
    * @return
    */
  private def populateArrayFromNested(
    arr: Seq[Any],
    data_array: Array[Double],
    index_type: IndexType
  ): Array[Double] ={


    val get_index: (Seq[Int]) => Int =
      index_type match {
        case RowMajor    => getArrayIndexRowMajor
        case ColumnMajor => getArrayIndexColumnMajor
      }

    // Internal function to recursively build data array
    def recurse(arr: Seq[Any], data_array: Array[Double], prev_ind: Seq[Int]): Array[Double] = {
      val populateEach: (Int) => Unit = (ind: Int) => {
        val arr_value = arr(ind)
        val next_ind = prev_ind ++ Seq(ind)

        arr_value match {
          case value: Double    => data_array(get_index(next_ind)) = value
          case nested: Seq[Any] => recurse(arr, data_array, next_ind)
        }
      }

      // Parallelize if greater than a certain number. 15000 is chosen based on http://docs.scala-lang.org/overviews/parallel-collections/performance.html
      if (arr.length > 15000) {
        arr.indices.par.foreach(populateEach)
      } else {
        arr.indices.foreach(populateEach)
      }

      data_array
    }

    recurse(arr, data_array, Seq())
  }



  /**
    * Determines the dimensions of a nested series of sequences
    * @param arr
    * @return
    */
  private def determineDimension(arr: Seq[Any]): Seq[Int] = {
    arr.headOption match {
      case Some(nest: Seq[Any]) => Seq(arr.length) ++ determineDimension(nest)
      case Some(term: Double)   => Seq(arr.length)
      case _                    => Seq()
    }
  }

  /**
    * Generate data array from an array of arrays that contain (coordinate, ..., coordinate -> value, ..., value) pairs
    * @param dim Dimensions of target matrix
    * @param points
    * @return
    */
  private def generateCoordValDataArrayRowMajor(dim: Array[Int], points: Iterable[Iterable[Double]]): Array[Double] = {
    val size = getArrayIndexRowMajor(dim)

    val num_coords  = dim.length - 1
    val num_vals    = dim.last
    val pair_length = num_coords + num_vals

    val data_array = ArrayUtils.nullArray[Double](size)


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

  /**
    * Generate data array from an array of arrays that contain (value, ..., value -> coord, ..., coord) pairs
    * @param dim Dimensions of target matrix
    * @param points
    * @return
    */
  private def generateCoordValDataArrayColumnMajor(dim: Array[Int], points: Iterable[Iterable[Double]]): Array[Double] = {
    val size = getArrayIndexColumnMajor(dim)

    val num_coords  = dim.length - 1
    val num_vals    = dim.head
    val pair_length = num_coords + num_vals

    val data_array = ArrayUtils.nullArray[Double](size)

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
    * Gets the data array size from its dimensions
    * @param dim
    * @return
    */
  private def getArraySizeFromDimension(dim: Seq[Int]): Int = {
    getArrayIndexRowMajor(dim)
  }

  /**
    * Gets the index of an element from the dimensions provided, row major
    * @param index Seq of the coordinates you want
    * @return
    */
  private def getArrayIndexRowMajor(index: Iterable[Int]): Int =
  {
    index.view.zipWithIndex.foldLeft(0){
      case (accum: Int, index_dim: (Int, Int)) => index_dim._1 + index_dim._2 * accum
    }
  }


  /**
    * Gets the index of an element, column major
    * @param index Seq of the coordinates you want
    * @return
    */
  private def getArrayIndexColumnMajor(index: Iterable[Int]): Int =
  {
    index.view.zipWithIndex.foldRight(0){
      case (index_dim: (Int, Int), accum: Int) => index_dim._1 + index_dim._2 * accum
    }
  }
}

