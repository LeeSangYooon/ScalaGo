package AI.Heuristic
import scala.collection.mutable

object MergeMaps extends App {
  def mergeMaps[K](map1: mutable.HashMap[K, Float], map2: mutable.HashMap[K, Float]): mutable.HashMap[K, Float] = {
    // Create a merged map with a default value of 0
    val mergedMap = new mutable.HashMap[K, Float]().withDefaultValue(0f)

    // Add entries from the first map
    map1.foreach { case (key, value) =>
      mergedMap(key) += value
    }

    // Add entries from the second map
    map2.foreach { case (key, value) =>
      mergedMap(key) += value
    }

    // Convert to HashMap explicitly
    new mutable.HashMap[K, Float] ++ mergedMap
  }


}