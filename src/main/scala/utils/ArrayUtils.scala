package utils

import scala.reflect.ClassTag

/**
  * Created by shea on 8/13/17.
  */
object ArrayUtils {

  /**
    * Returns an array[M] of size filled with nulls
    *
    * WHY NULL:
    * There needs to be a way to express that a value in an MDM's matrix has not been set.
    * The options here are either wrapping everything in an Option, which involves boxing, or
    * setting each unset value to a value that equates to "is not defined". This way, when
    * referencing a partially populated matrix, it is known when certain indices simply do not exist.
    *
    * Setting empty values to 0 is not feasible, as 0 is a valid value.
    *
    * @param size
    * @tparam M
    * @return
    */
  def nullArray[M: ClassTag](size: Int): Array[M] = {
    val arr: Array[M] = Array.ofDim[M](size)
    val nulled_instance = null.asInstanceOf[M]
    for (ind <- 0 to size) {
      arr(ind) = nulled_instance
    }
    arr
  }
}
