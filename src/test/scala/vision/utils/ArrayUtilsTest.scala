package vision.utils

import org.scalatest.{FlatSpec, Matchers}
import vision.utils.ArrayUtils.{nanArray}
import scala.util.Random
/**
  * Created by shea on 9/6/17.
  */
class ArrayUtilsTest extends FlatSpec with Matchers {
  "A nanFloat array" should "generate an array of designated size" in {
    val size = 100
    val arr = nanArray[Float](size)
    arr.length should be (size)
  }

  it should "be prepopulated with nans" in {
    val size = 50
    val arr = nanArray[Float](size)

    val ind = Random.nextInt(size)
    val rand_comp = Random.nextInt(100)

    arr(ind).isNaN should be (true)
    arr(ind) should not be rand_comp
  }

  "A nanDouble array" should "generate a nanDouble array of designated size" in {
    val size = 100
    val arr = nanArray[Double](size)
    arr.length should be (size)
  }

  it should "be prepopulated with nans" in {
    val size = 50
    val arr = nanArray[Double](size)

    val ind = Random.nextInt(size)
    val rand_comp = Random.nextInt(100)

    arr(ind).isNaN should be (true)
    arr(ind) should not be rand_comp
  }
}
