object ASCIIRenderer {
  val charList =
    """ `.-:/+osyhdmNM"""

  /**
    * Converts a grayscale image to ASCII and prints it
    *
    * @param pm            Source image as a Map[(x,y) -> luminosity]
    * @param charPixelSize Size (w,h) of one of your font's character in pixels
    * @param zoomFactor    Zoom factor, 1 will match the number of char. n to have n * charPixelSize._1 = pm.w
    * @param invert        Invert image grayscale
    */
  def imgToAscii(pm: PixelMap, charPixelSize: (Int, Int), zoomFactor: Float, invert: Boolean): Unit = {
    def go(pm: PixelMap, charCoordX: Int, charCoordY: Int, zoneSize: (Int, Int)): Unit = {
      if (charCoordY * zoneSize._2 < pm.h - zoneSize._2 + 1) {
        if ((charCoordX + 2) * zoneSize._1 + 1 < pm.w) {
          print(printChar(luminanceZone(pm, charCoordX, charCoordY, zoneSize), zoneSize, invert))
          go(pm, charCoordX + 1, charCoordY, zoneSize)
        }
        else {
          println(printChar(luminanceZone(pm, charCoordX, charCoordY, zoneSize), zoneSize, invert))
          go(pm, 0, charCoordY + 1, zoneSize)
        }
      }
    }

    val zoneSize = ((charPixelSize._1 / zoomFactor).round, (charPixelSize._2 / zoomFactor).round)
    go(pm, 0, 0, zoneSize)
  }

  /**
    * Return the corresponding character by its luminosity
    */
  def printChar(totalZoneLuminosity: Int, zoneSize: (Int, Int), invert: Boolean): Char = {
    if (invert)
      charList.reverse(totalZoneLuminosity / (zoneSize._1 * zoneSize._2) * (charList.length - 1) / 255)
    else
      charList(totalZoneLuminosity / (zoneSize._1 * zoneSize._2) * (charList.length - 1) / 255)
  }

  /**
    * Sum the total pixel luminosity of a zone zoneSize(w,h) in order to map a char. of this size
    */
  def luminanceZone(pm: PixelMap, charCoordX: Int, charCoordY: Int, zoneSize: (Int, Int)): Int = {
    def go(pm: PixelMap, charCoordX: Int, charCoordY: Int, pixelX: Int, pixelY: Int): Int = {
      if (pixelY >= zoneSize._2)
        0
      else {
        if (pixelX < zoneSize._1 - 1)
          pm.m(charCoordX * zoneSize._1 + pixelX, charCoordY * zoneSize._2 + pixelY) + go(pm, charCoordX, charCoordY, pixelX + 1, pixelY)
        else
          pm.m(charCoordX * zoneSize._1 + pixelX, charCoordY * zoneSize._2 + pixelY) + go(pm, charCoordX, charCoordY, 0, pixelY + 1)
      }
    }

    go(pm, charCoordX, charCoordY, 0, 0)
  }


  /**
    * Just an unused basic, iterative simple renderer
    */
  def simpleRender(pm: PixelMap): Unit = {
    for (x <- 0 until pm.w) {
      for (y <- 0 until pm.h)
        print(charList(pm.m(x, y) * (charList.length - 1) / 255))
      println()
    }
  }
}