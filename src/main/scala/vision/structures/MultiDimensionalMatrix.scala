package vision.structures

import breeze.linalg.DenseVector
import vision.types.{Converter, NaN}

import scala.reflect.ClassTag
import scala.{specialized => spec}
import MatrixIndexType._
import vision.utils.ArrayUtils

/**
  * A multidimensional matrix
  * @param dim Dimensions of the matrix in row, column -> nth dimension format.
  * @param input_data The input, flattened array of values
  * @param index_type RowMajor/ColumnMajor
  */

class MultiDimensionalMatrix[@spec(Float, Double) Num: NaN](
  val dim: Seq[Int],
  input_data: Array[Num],
  val index_type: IndexType
) {
  val data: DenseVector[Num] = DenseVector[Num](input_data)

  val dim_sizes: Seq[Int] = MultiDimensionalMatrix.getDimensionSizesFromDimensions(dim, index_type)

  // @TODO file bug about match with spec val expression
  // lazy val because compiler error occurs on constructor-executed match statement
  lazy val getFlattenedIndex: (Seq[Int]) => Int = {
    index_type match {
      case ColumnMajor => MultiDimensionalMatrix.getArrayIndexColumnMajor(_: Seq[Int], dim_sizes)
      case RowMajor    => MultiDimensionalMatrix.getArrayIndexRowMajor(_: Seq[Int], dim_sizes)
    }
  }


  /**
    * Convenience function for getting a value at a certain flat index
    * @param flat_ind the index computed by reducing the seq of dim_indices
    * @return
    */
  def get(flat_ind: Int): Num = {
    data.data(flat_ind)
  }

  /**
    * Convenience function for getting a value at a certain index
    * // @TODO Handle shortened dimensions?
    * @param ind Sequence of dimension index at which to retrieve the value
    * @return
    */
  def get(ind: Seq[Int]): Num = {
    getAtIndex(ind)
  }

  /**
    * Gets the value of the element at the provided dimensional index
    * @param ind
    * @return
    */
  def getAtIndex(ind: Seq[Int]): Num = {
    getAtFlattenedIndex(getFlattenedIndex(ind))
  }

  /**
    * Gets the value of the element at the provided flattened index
    * @param flat_ind
    * @return
    */
  def getAtFlattenedIndex(flat_ind: Int): Num = {
    data.data(flat_ind)
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
  def apply[@spec(Float, Double) Num: NaN: Converter: ClassTag](
    dim: Array[Int],
    arr: Seq[Num],
    index_type: IndexType
  ): MultiDimensionalMatrix[Num] = {

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
  def apply[@spec(Float, Double) Num: NaN: Converter: ClassTag](
    dim: Array[Int],
    points: Iterable[Iterable[Num]],
    index_type: IndexType
  ): MultiDimensionalMatrix[Num] = {
    val data_array: Array[Num] = {
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
  def apply[@spec(Float, Double) Num: NaN: ClassTag](
    arr: Seq[Any],
    index_type: IndexType = RowMajor
  ): MultiDimensionalMatrix[Num] = {

    val dim: Seq[Int]          = determineDimension(arr)

    val data_array: Array[Num] = flattenNested[Num](arr, dim, index_type)

    new MultiDimensionalMatrix(dim, data_array, index_type)
  }

  /**
    * Recurses into provided array and populates a data array from it
    * @param arr An nested series of arrays
    * @param dim The dimensions of the nested array
    * @param index_type indexing method
    * @return
    */
  private def flattenNested[@spec(Float, Double) Num: NaN: ClassTag](
    arr: Seq[Any],
    dim: Seq[Int],
    index_type: IndexType
  ): Array[Num] ={
    val dim_sizes = getDimensionSizesFromDimensions(dim, index_type)
    val size      = getArraySizeFromDimension(dim, index_type)

    val data_array: Array[Num] = ArrayUtils.nanArray[Num](size)

    val get_index: (Seq[Int]) => Int =
      index_type match {
        case RowMajor    => getArrayIndexRowMajor(_: Seq[Int], dim_sizes)
        case ColumnMajor => getArrayIndexColumnMajor(_: Seq[Int], dim_sizes)
      }

    // Internal function to recursively build data array
    // @P2-TODO file bug about default values in methods + @spec
    def recurse(arr: Seq[Any], data_array: Array[Num], prev_ind: Seq[Int]): Array[Num] = {
      val populateEach: (Int) => Unit = (ind: Int) => {
        val arr_value = arr(ind)
        val next_ind = prev_ind :+ ind
        arr_value match {
          case nested: Seq[Any]     => recurse(nested, data_array, next_ind)
          case value: Num@unchecked => data_array(get_index(next_ind)) = value
        }
      }

      // Parallelize if greater than a certain number. 15000 is chosen based on http://docs.scala-lang.org/overviews/parallel-collections/performance.html
      // @P3-TODO handle cases with large dimensions + large length -- we'll spawn too many threads if dimensions are too deep
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
  private def generateCoordValDataArrayRowMajor[@spec(Float, Double) Num: Converter: NaN: ClassTag](dim: Array[Int], points: Iterable[Iterable[Num]]): Array[Num] = {

    val convert = implicitly[Converter[Num]].convert _

    val size      = getArraySizeFromDimension(dim, RowMajor)
    val dim_sizes = getDimensionSizesFromDimensions(dim, RowMajor)

    val num_coords  = dim.length - 1
    val num_vals    = dim.last
    val pair_length = num_coords + num_vals

    val data_array = ArrayUtils.nanArray[Num](size)


    // Indexes the data array appropriately, filling the coordinate spaces with the values of the last dimension
    points.par.foreach(
      (point: Iterable[Num]) => {

        // Gives the coordinate excluding the last value
        val coord = point.slice(0, num_coords + 1).map((num: Num) => {
          convert(num).toInt
        })
        val index = getArrayIndexRowMajor(coord, dim_sizes)

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
  private def generateCoordValDataArrayColumnMajor[@spec(Float, Double) Num: Converter: NaN: ClassTag](dim: Array[Int], points: Iterable[Iterable[Num]]): Array[Num] = {
    val convert = implicitly[Converter[Num]].convert _

    val size      = getArraySizeFromDimension(dim, ColumnMajor)
    val dim_sizes = getDimensionSizesFromDimensions(dim, ColumnMajor)

    val num_coords  = dim.length - 1
    val num_vals    = dim.head
    val pair_length = num_coords + num_vals

    val data_array = ArrayUtils.nanArray[Num](size)

    // Indexes the data array appropriately, filling the coordinate spaces with the values of the last dimension
    points.par.foreach(
      (point: Iterable[Num]) => {

        // Gives the coordinate excluding the valuess
        val coord = Seq(0) ++ point.slice(num_vals + 1, num_coords + 1).map(num => convert(num).toInt)
        val index = getArrayIndexColumnMajor(coord, dim_sizes)

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

  private def getDimensionSizeColumnMajor(dim: Seq[Int]): Seq[Int] = {
    dim.dropRight(1).scanRight(dim.last)(
      (accum: Int, max_dim: Int) => accum * max_dim
    )
  }

  private def getDimensionSizesFromDimensions(dim: Seq[Int], index_type: IndexType): Seq[Int] = {
    index_type match {
      case RowMajor    => throw new Exception("Not yet implemented")
      case ColumnMajor => getDimensionSizeColumnMajor(dim)
    }
  }

  /**
    * Gets the data array size from its dimensions
    * @param dim
    * @return
    */
  private def getArraySizeFromDimension(dim: Seq[Int], index_type: IndexType): Int = {

    val dim_sizes = getDimensionSizesFromDimensions(dim, index_type)

    index_type match {
      case RowMajor    => dim_sizes.last
      case ColumnMajor => dim_sizes.head
    }
  }

  /**
    * Gets the index of an element from the dimensions provided, row major
    * @param index Seq of the coordinates you want
    * @return
    */
  private def getArrayIndexRowMajor(index: Iterable[Int], dim_sizes: Seq[Int]): Int =
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
  private def getArrayIndexColumnMajor(index: Iterable[Int], dim_sizes: Seq[Int]): Int =
  {
    index.view.zipWithIndex.foldRight(0){
      case (index_dim: (Int, Int), accum: Int) => {
        // index correction, since we're scanning left we get the index of the relevant dimension size
        // num dimensions - current dimension (ind + 1)
        val (index, dim) = index_dim

        // If this is the first dimension (corrected for index at 0), we know there is no accumulation, and the default
        if ((dim + 1) == dim_sizes.length) {
          index
        } else {
          // The index of the current dimension times the size of each of the prev dim size
          // plus the number of accumulated partial dimensions
          index * dim_sizes(dim + 1) + accum
        }
      }
    }
  }
}

