package vision.types

/**
  * Created by shea on 9/6/17.
  *
  *
  * Specialized numeric class that allows you to retrieve a generic NaN depending on input type
  */

trait Converter[@specialized(Double, Float) Num] extends Serializable {
  // This allows us to implicitly derive the convertable of the provided type
  // then the convertable is used in order to generate the class that will transfiorm a primatibe type
  // into one of the desired types
  def convert(value: Num): ConvertableFor

  abstract class ConvertableFor(value: Num) {
    def toInt:    Int
    def toFloat:  Float
    def toDouble: Double
  }

//  def toNaNAble[@specialized(Double, Float) NaNable: NaN: Manifest]: NaNable
//
//  override def toNaNAble[NaNable: NaN]: NaNable = {
//    val clazz = implicitly[Manifest[NaNable]].runtimeClass
//    match NaNable
//  }

}

object Converter {
  def forClass(clazz: Class[_]):Converter[_] = {
    if(clazz == java.lang.Float.TYPE) FloatConverter
    else if (clazz == java.lang.Double.TYPE) DoubleConverter
    else FloatConverter
  }

  implicit object FloatConverter extends Converter[Float] {
    class ConvertableForFloat(fl: Float) extends ConvertableFor(fl) {
      override def toInt: Int       = fl.toInt
      override def toFloat: Float   = fl
      override def toDouble: Double = fl.toDouble
    }

    override def convert(value: Float): ConvertableForFloat = {
      new ConvertableForFloat(value)
    }
  }

  implicit object DoubleConverter extends Converter[Double] {
    class ConvertableForDouble(db: Double) extends ConvertableFor(db) {
      override def toInt: Int       = db.toInt
      override def toFloat: Float   = db.toFloat
      override def toDouble: Double = db
    }

    override def convert(value: Double): ConvertableForDouble = {
      new ConvertableForDouble(value)
    }
  }



}