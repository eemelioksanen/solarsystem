package Simulator.App

import Simulator.{SpaceObject, Vector3D}
import scalafx.scene.canvas.Canvas
import scalafx.scene.image.Image

class DrawArea(witdh: Double, height: Double) extends Canvas(witdh, height) {

  // Transforms the coordinates to the system used by ScalaFX.
  private def getCoords(coords: Vector3D, scale: Double): (Double, Double) = {
    val x = coords.x * scale
    val y = coords.y * scale
    ((this.witdh / 2) + x, (this.height / 2) - y)
  }

  // Draws the planets on the canvas.
  def draw(items: Array[SpaceObject]): Unit = {
    val arrow = new Image("arrow.png")
    val objects = items.sortBy(_.state.coords.z).reverse
    val maxDist = objects.map(_.state.coords.length).max
    val scale = (math.min(witdh, height)) /  (2.1 * maxDist)
    val g = this.graphicsContext2D
    for (i <- objects.indices) {
      val n = objects(i)
      val image: Image = n.name.toLowerCase match {
        case "earth" => new Image("earth.png")
        case _ => new Image("default.png")
      }
      val coords = getCoords(n.state.coords, scale)
      val distance = scale * (n.state.coords.z + maxDist)
      val depth = (n.radius * scale * 2) + n.state.coords.z * scale
      if (depth >= 10) {
      g.drawImage(image, coords._1 - (depth / 2), coords._2 - (depth / 2), depth, depth)
      } else {
        g.drawImage(image, coords._1 - (depth / 2), coords._2 - (depth / 2), 10, 10)
      }
      /**
      g.setStroke(Color.Red)
      g.setLineWidth(3)
      g.strokeLine(coords._1, coords._2, (coords._1 - forces(i).getUnit.x * 10), coords._2 + forces(i).getUnit.y * 10)
      **/
    }
  }

}
