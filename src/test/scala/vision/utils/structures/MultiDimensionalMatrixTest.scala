package vision.utils.structures

import org.scalatest.{FlatSpec, Matchers}
import vision.structures.{MatrixIndexType, MultiDimensionalMatrix}

import scala.util.Random
/**
  * Created by shea on 9/6/17.
  */
class MultiDimensionalMatrixTest extends FlatSpec with Matchers {


  behavior of "A single dimensional MultiDimensionalMatrix"

  val rand_doubles: Seq[Double] = Seq.fill[Double](10)(Math.random())

  val mat: MultiDimensionalMatrix[Double] = MultiDimensionalMatrix[Double](rand_doubles, MatrixIndexType.RowMajor)

  mat.dim.length should be (1)
  mat.dim.head should be (10)

  it should "be created Row-Major with appropriate dimensions from a sequence" in {
    val rand_doubles = Seq.fill[Double](10)(Math.random())

    val mat = MultiDimensionalMatrix[Double](rand_doubles, MatrixIndexType.RowMajor)

    mat.dim.length should be (1)
    mat.dim.head should be (10)
  }

  it should "return the correct index of the target " in {
    mat.getAtIndex(Seq(1)) should be (rand_doubles(1))
  }

}
