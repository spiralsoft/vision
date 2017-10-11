package vision.point_clouds

import spire.ClassTag
import vision.structures.{MatrixIndexType, MultiDimensionalMatrix}
import vision.types.{Converter, NaN}
import vision.utils.ArrayUtils

import scala.{specialized => spec}

/**
  * An organized point cloud, meaning the points
  * contained within the points MDM are organized according to
  * location.
  * @param points the MDM that backs this cloud
  * @param normalized_min The minimum value present in the cloud.
  *                       This number allows us to normalize down into a smaller
  *                       datastructure so we aren't assigning thousands of locations
  *                       in order to index the large number of points.
  * Fastest access time will always be [depth, depth, depth, false, depth]
  * if height and width are NaN, depth will also be NaN for those indices
  * Because of the way that merging occurs, it may not be fair to say that
  * each width and height comes with one depth. But we don't have information on
  * this, so let's test it out.
  * @tparam Num NaNable number
  */
case class OrganizedPointCloud[@spec(Float, Double) Num :NaN](
  points: MultiDimensionalMatrix[Num]
) {

}

object OrganizedPointCloud {
  def apply[@spec(Float, Double) Num :NaN :Converter :ClassTag](
    dim: Seq[Int],
    points: Seq[Num]
  ): OrganizedPointCloud[Num] = {

    // @TODO Implement fromFlatOrdered
    val mdm: MultiDimensionalMatrix[Num] = MultiDimensionalMatrix.mdmFromFlattened(dim.toArray, points, MatrixIndexType.RowMajor)

    OrganizedPointCloud(mdm)
  }
}
