package vision.point_clouds

import vision.structures.MultiDimensionalMatrix

/**
  * Created by shea on 7/25/17.
  */
class PointCloud {
  // This is what needs to be a breeze object
  // An array: A sequence of values of designated type [4, 2, 1]
  // Can be considered an array with a size of 3 or a three dimensional matrix with a single value set to true at
  // index 4, 2, 1.


  // If we look to include some sort of information into the array, we can consider the values immediateely follwing
  // the initial three values as being the values contained within the point
  // In a RGBPointCloud, we could consider the array [4, 2, 1, 222, 241, 255] as being a matrix that has the index 4, 2, 1 set to 221, 241, 255

  // In this way, we have the number of dimensions preceeding the values of the same type that we wish to contain within the point defined
  // by the preceeding n-elements as an index.


  // It is possible to store the values contained at the point separately from the points themselves.

  // for instance, using a refgerence array. If we define the matrix to have a maximum size of maxX * maxY * maxZ = N elements
  // rgb_array: Array[T] allocated to be N elemennts * data size long
  // However, we also want to store the confidences of each registered point

  // When rendering a pointcloud and doing mathematical operations on it, it may be convenient to store the registered points
  // in a structure that is separate from the metadata(s) surrounding the point.


  // In this way we postulate... If we assume that we have a series of indices definied in the point_coud index

  // Let's just start with the simplest case.. We have a point cloud. We want to store all of the indices related to the point cloud.

  // We have N samples. We need 3 array floats per sample. Size of N*3

  //val points: MultiDimensionalMatrix[Float]()
}
