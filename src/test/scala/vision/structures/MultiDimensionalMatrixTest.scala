package vision.structures

import org.scalatest
import org.scalatest.{FlatSpec, Matchers}
import vision.utils.ArrayUtils
/**
  * Created by shea on 9/6/17.
  */
class MultiDimensionalMatrixTest extends FlatSpec with Matchers {

  def testDimensions(dim: Seq[Int], mat: MultiDimensionalMatrix[Double]): Seq[scalatest.Assertion] = {
    mat.dim.length should be (dim.length)

    dim.zipWithIndex.map {
      case (dim_at, ind) => mat_4d.dim(ind) should be (dim_at)
    }
  }

  behavior of "A single dimensional MultiDimensionalMatrix (MDM)"

  val rand_doubles: Seq[Double] = Seq.fill[Double](10)(Math.random())

  val mat: MultiDimensionalMatrix[Double] = MultiDimensionalMatrix.mdmFromNested[Double](rand_doubles, MatrixIndexType.ColumnMajor)

  it should "be created Colkumn-Major with appropriate dimensions from a sequence" in {
    mat.dim.length should be (1)
    mat.dim.head should be (10)
  }

  it should "return the correct index of the target " in {
    mat.getAtIndex(Seq(1)) should be (rand_doubles(1))
  }

  behavior of "A two dimensional MDM"

  // Generates a 2D matrix of a random set of doubles
  val row_num = 9
  val col_num = 10
  val doubles_2d: Seq[Seq[Double]] = Seq.fill[Seq[Double]](row_num)(Seq.fill[Double](col_num)(Math.random()))

  val mat_2d: MultiDimensionalMatrix[Double] = MultiDimensionalMatrix.mdmFromNested[Double](doubles_2d, MatrixIndexType.ColumnMajor)


  it should "be created column-major with appropriate dimensions from a sequence of sequences." in {
    mat_2d.dim.length should be (2)
    mat_2d.dim.head should be (row_num)
    mat_2d.dim.last should be (col_num)
  }


  it should "contain the correct values in the correct positions" in {

    mat_2d.getAtIndex(Seq(1, 1)) should be (doubles_2d(1)(1))
    mat_2d.getAtIndex(Seq(7, 8)) should be (doubles_2d(7)(8))

  }

  behavior of "A three dimensional MDM"

  val row_num_3 = 13
  val col_num_3 = 12
  val depth_num = 23

  val doubles_3d: Seq[Seq[Seq[Double]]] =
    Seq.fill[Seq[Seq[Double]]](row_num_3)(
      Seq.fill[Seq[Double]](col_num_3)(
        Seq.fill[Double](depth_num)(Math.random()
        )
      )
    )

  val mat_3d: MultiDimensionalMatrix[Double] = MultiDimensionalMatrix.mdmFromNested[Double](doubles_3d, MatrixIndexType.ColumnMajor)

  it should "be created column-major with appropriate dimensions from a sequence of sequences." in {
    mat_3d.dim.length should be (3)
    mat_3d.dim.head should be (row_num_3)
    mat_3d.dim(1)   should be (col_num_3)
    mat_3d.dim.last should be (depth_num)
  }

  it should "contain the correct values in the correct positions" in {

    mat_3d.getAtIndex(Seq(1, 1, 1)) should be (doubles_3d(1)(1)(1))
    mat_3d.getAtIndex(Seq(4, 4, 3)) should be (doubles_3d(4)(4)(3))

  }


  behavior of "A four dimensional MDM"


  val dim_4 = Seq(7, 14, 21, 4)

  val doubles_4d: Seq[Seq[Seq[Seq[Double]]]] =
    Seq.fill[Seq[Seq[Seq[Double]]]](dim_4(0))(
      Seq.fill[Seq[Seq[Double]]](dim_4(1))(
        Seq.fill[Seq[Double]](dim_4(2))(
          Seq.fill[Double](dim_4(3))(Math.random())
        )
      )
    )

  val mat_4d: MultiDimensionalMatrix[Double] = MultiDimensionalMatrix.mdmFromNested[Double](doubles_4d, MatrixIndexType.ColumnMajor)



  it should "be created column-major with appropriate dimensions from a sequence of sequences." in {
    testDimensions(dim_4, mat_4d)
  }

  it should "contain the correct values in the correct positions" in {
    Seq(
      mat_4d.getAtIndex(Seq(1, 1, 1, 1)) should be (doubles_4d(1)(1)(1)(1)),
      mat_4d.getAtIndex(Seq(4, 4, 3, 0)) should be (doubles_4d(4)(4)(3)(0))
    )
  }

  behavior of "A flat generated MDM"

  val dim_flat: Seq[Int]    = Seq(4, 5, 1)

  val flat:     Seq[Double] = Seq(
    0.0, 0.0, 4.0,
    1.0, 1.0, 2.0,
    2.0, 1.0, 1.0,
    3.0, 0.0, 3.0,
    1.0, 2.0, 6.0,
    0.0, 3.0, 3.0
  )


  val mat_flat: MultiDimensionalMatrix[Double] = MultiDimensionalMatrix.mdmFromFlattened(
    dim_flat.toArray,
    flat,
    MatrixIndexType.ColumnMajor
  )


  it should "have the right dimensions" in {
    testDimensions(dim_flat, mat_flat)
  }

  it should "contain the correct values in the correct positions" in {
    Seq(
      mat_4d.getAtIndex(Seq(2, 1, 0)) should be (1.0),
      mat_4d.getAtIndex(Seq(0, 0, 0)) should be (4.0)
    )
  }



}
