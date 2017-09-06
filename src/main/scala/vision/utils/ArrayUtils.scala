package vision.utils

import vision.utils.primatives.NaN

import scala.reflect.ClassTag
import scala.{specialized => spec}

/**
  * Created by shea on 8/13/17.
  */
object ArrayUtils {

  /**
    * Returns an array[M] of size filled with NaN
    * No
    *
    * @param size
    * @return
    */
  def nanArray[@spec(Double, Float) Num: ClassTag : NaN](size: Int): Array[Num] = {
    val arr: Array[Num] = Array.ofDim[Num](size)
    val NaN = implicitly[NaN[Num]].NaN
    for (ind <- 0 until size) {
      arr(ind) = NaN
    }
    arr
  }

}
