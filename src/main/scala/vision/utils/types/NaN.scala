package vision.utils.types

import breeze.storage.Zero
import breeze.storage.Zero.{BooleanZero, ByteZero, CharZero, DoubleZero, FloatZero, IntZero, ShortZero, refDefault}

/**
  * Created by shea on 9/6/17.
  *
  *
  * Specialized numeric class that allows you to retrieve a generic NaN depending on input type
  */

trait NaN[@specialized(Double, Float) T] extends Serializable {
  def NaN : T
}

object NaN {
  def forClass(clazz: Class[_]):NaN[_] = {
    if(clazz == java.lang.Float.TYPE) FloatNaN
    else if (clazz == java.lang.Double.TYPE) DoubleNaN
    else FloatNaN
  }

  implicit object FloatNaN extends NaN[Float] {
    override def NaN = Float.NaN
  }

  implicit object DoubleNaN extends NaN[Double] {
    override def NaN = Double.NaN
  }



}