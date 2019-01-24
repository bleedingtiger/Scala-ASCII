import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO

object ResourceHandler {
  def getImage(path: String): PixelMap = {
    val img: BufferedImage = ImageIO.read(new File(path))
    PixelMap(img.getWidth, img.getHeight, createImageMap(img, Map(), 0, 0))
  }

  def createImageMap(img: BufferedImage, m: Map[(Int, Int), Int], x: Int, y: Int): Map[(Int, Int), Int] = {
    if (x == img.getWidth - 1 && y == img.getHeight - 1)
      m ++ Map((x, y) -> RGBtoBW(img.getRGB(x, y)))
    else {
      if (x == img.getWidth - 1)
        createImageMap(img, m ++ Map((x, y) -> RGBtoBW(img.getRGB(x, y))), 0, y + 1)
      else
        createImageMap(img, m ++ Map((x, y) -> RGBtoBW(img.getRGB(x, y))), x + 1, y)
    }
  }

  def setImageGrayscale(img: PixelMap, path: String): Unit = {
    val m: BufferedImage = new BufferedImage(img.w, img.h, BufferedImage.TYPE_INT_RGB)
    img.m.foreach { case ((x, y), p) => m.setRGB(x, y, BWtoRGB(p)) }
    ImageIO.write(m, "png", new File(path))
  }

  def RGBtoBW(p: Int): Int = {
    val red = (p & 0xff0000) / 65536
    val green = (p & 0xff00) / 256
    val blue = p & 0xff
    (red * 0.2126f + green * 0.7152f + blue * 0.0722f).round
  }

  def BWtoRGB(p: Int): Int = {
    (p * 65536) + (p * 256) + p
  }
}
