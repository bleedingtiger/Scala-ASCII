object Main {
  def main(args: Array[String]): Unit = {
    val img = ResourceHandler.getImage("public/images/saturn.jpg")
    println("Processing image...")
    ASCIIRenderer.imgToAscii(img, (7, 18), 0.75f, false)
  }
}