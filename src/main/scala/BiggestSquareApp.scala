object BiggestSquareApp extends App {

  import scala.io.Source

  override def main(args: Array[String]): Unit = {
    val skyscraperList = Source.fromURL("https://raw.githubusercontent.com/wunderdogsw/wunderpahkina-vol6/master/input.txt")
    val skyscraperHeights = skyscraperList.mkString.split("\n").map(line => Integer.parseInt(line)).toList

    val squareCalculator = new SquareCalculator
    val start: Long = System.currentTimeMillis()
    println(squareCalculator.findBiggestSquare(skyscraperHeights))
    val elapsed = System.currentTimeMillis() - start
    println("The calculation of biggest square took: %1d ms".format(elapsed))
  }

}

class SquareCalculator {

  def findBiggestSquare(heights: List[Int]) = {
    val shortestTowerHeight = heights.min
    val tallestTowerHeight = Math.min(heights.size, heights.max)

    type BlockUnderSkyline = (Int, Int)
    type CityBlocksAtHeight = List[BlockUnderSkyline]

    def getBlocksOfCityUnderSkylineAt(currentHeight: Int) = {
      def getBlocks(blocksAtHeight: CityBlocksAtHeight, remainingSkyline: List[Int], currentIndex: Int): CityBlocksAtHeight = {
        remainingSkyline match {
          case Nil => blocksAtHeight
          case _ => {
            val (belowSkyline, unknownSkyline) = remainingSkyline.span(item => item >= currentHeight)
            val nextUnhandledTower = if (belowSkyline.isEmpty) currentIndex + 1 else currentIndex + belowSkyline.size
            if(belowSkyline.isEmpty) getBlocks(blocksAtHeight, unknownSkyline.tail, nextUnhandledTower)
            else getBlocks((currentIndex, nextUnhandledTower) :: blocksAtHeight, unknownSkyline, nextUnhandledTower)
          }
        }
      }
      getBlocks(List.empty, heights, 0)
    }

    def isTallerThanTallestBuilding(height: Int) = height > tallestTowerHeight

    def getSquareSizeFromHeight(height: Int) = height * height

    def getLongestBlockOfCityUnderSkyline(height: Int): Int = {
      val skylineForHeight = getBlocksOfCityUnderSkylineAt(height)
      val longestRegionUnderSkyline = skylineForHeight.maxBy(region => region._2 - region._1)
      longestRegionUnderSkyline._2 - longestRegionUnderSkyline._1
    }

    def checkSkylineAtHeight(currentHeight: Int, currentMax: Int): Int = {
      if(isTallerThanTallestBuilding(currentHeight)) {
        getSquareSizeFromHeight(currentMax)
      } else {
        val longestStretchUnderSkyline = getLongestBlockOfCityUnderSkyline(currentHeight)
        longestStretchUnderSkyline.compareTo(currentHeight) match {
          case 0 => getSquareSizeFromHeight(currentHeight)
          case 1 => checkSkylineAtHeight(currentHeight +1, longestStretchUnderSkyline)
          case -1 => getSquareSizeFromHeight(currentMax)
        }
      }
    }
    checkSkylineAtHeight(shortestTowerHeight, shortestTowerHeight)
  }

}
