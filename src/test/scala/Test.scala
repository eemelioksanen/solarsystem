import Simulator._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Mathematics._

import java.io._
import scala.reflect.internal.util.FileUtils.LineWriter

class UnitTest extends AnyFlatSpec with Matchers {

  val testLocation = "D:\\Ohjelmointi\\Solar System Simulator\\src\\test\\resources\\testfilenew.txt"

  val testWriteFile = "D:\\Ohjelmointi\\Solar System Simulator\\src\\test\\resources\\testWriteFile.dat"

  val x = new Simulation(1000, 4665600, testLocation)

  val velocities = 2

  val lineWriter = new FileWriter(testWriteFile)

  val bufferedWriter = new BufferedWriter(lineWriter)

  try {
    bufferedWriter.write("#test")
    bufferedWriter.newLine()

    while (!x.simulationEnded) {
      var writeData = x.getTime.toString
      x.getItems.foreach(n => writeData += s" ${n.state.coords.x} ${n.state.coords.y} ${n.state.coords.z}")
      bufferedWriter.write(writeData)
      x.advance()
      bufferedWriter.newLine()
  }

  } finally {
    bufferedWriter.close()
    lineWriter.close()
  }

}