package vision.utils.structures

import org.scalatest.{FlatSpec, Matchers}
import vision.structures.{MatrixIndexType, MultiDimensionalMatrix}

import scala.util.Random
/**
  * Created by shea on 9/6/17.
  */
class MultiDimensionalMatrixTest extends FlatSpec with Matchers {


  behavior of "A single dimensional MultiDimensionalMatrix (MDM)"

  val rand_doubles: Seq[Double] = Seq.fill[Double](10)(Math.random())

  val mat: MultiDimensionalMatrix[Double] = MultiDimensionalMatrix[Double](rand_doubles, MatrixIndexType.ColumnMajor)

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

  val mat_2d: MultiDimensionalMatrix[Double] = MultiDimensionalMatrix[Double](doubles_2d, MatrixIndexType.ColumnMajor)


  it should "be created column-major with appropriate dimensions from a sequence of sequences." in {
    mat_2d.dim.length should be (2)
    mat_2d.dim.head should be (row_num)
    mat_2d.dim.last should be (col_num)
  }


  it should "contain the correct values in the correct positions" in {

    mat_2d.getAtIndex(Seq(1, 1)) should be (doubles_2d(1)(1))
    mat_2d.getAtIndex(Seq(7, 8)) should be (doubles_2d(7)(8))
    
  }


}
