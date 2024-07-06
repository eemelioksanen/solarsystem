package Simulator
import Simulator.Mathematics.{distanceTo, integrate}

import java.io._
import scala.collection.mutable

class Simulation(val step: Double, val duration: Double, val sourceFile: String) {

  if (step >= duration) throw new Exception("Simulation step was greater than the duration!")

  private var endReached: Boolean = false // A variable containing the boolean value for whether the end has been reached or not.

  private val items: Array[SpaceObject] = readFile()  // Read the items from the simulation file.

  private var t = 0.0 // The current time that has passed since the beginning of the simulation.

  def getItems = this.items

  def simulationEnded = endReached

  def getTime = t

  /**
  A method used for reading a file.
  Returns an array consisting of created SpaceObjects.
  **/
  private def readFile(): Array[SpaceObject] = {

    // Initialize a buffer to add the new SpaceObjects to.
    val items = mutable.Buffer[SpaceObject]()

    // A method for adding items to the simulation.
    def addItem(name: String, mass: Double, velocity: Vector3D, position: Vector3D, radius: Double): Unit = {
      name.take(2) match {
        case "s_" => items += new Satellite(name, radius, new State(mass, velocity, position)) // Items with the prerfix s_ are considered satellites.
        case _ => items += new Planet(name, radius, new State(mass, velocity, position))
      }
    }
    // A method for extracting data from a line.
    def getData(data: String) = {

      try {

        // Filter spaces from the data and take all text until a comment is found.
        val items = data.filter(_ != ' ').takeWhile(_ != '#').split(";")

        if (items.length != 5) throw new Exception(s"There was an error in the file at [$data]") // Throw an error when a line doesn't have the expected data.
        val name = items(0).trim // Get the name.
        val mass = items(1).trim.toDouble // Get the mass.
        val velocity = { // Get the velocity
          val components = items(2).trim.split(',').map(_.toDouble) // Split the contents of the third block to get the x, y and z values of the velocity.
          if (components.length != 3) throw new Exception(s"Invalid velocity data at [$data]") // Throw an exception in case of invalid velocity data.
          Vector3D(components(0), components(1), components(2)) // Create and return a new 3D-vector with the found data.
        }
        val position = { // Get the position.
          val components = items(3).trim.split(',').map(_.toDouble) // Split the contents of the third fourth to get the x, y and z values of the position.
          if (components.length != 3) throw new Exception(s"Invalid position data at [$data]") // Throw an error in case of invalid position data.
          Vector3D(components(0), components(1), components(2)) // Create and return a new 3D-vector with the found data.
        }
        val radius = items(4).trim.toDouble // Get the radius.
        (name, mass, velocity, position, radius) // Finally teturn all the found data as a tuple.

      } catch {
        // Throw an exception in the case that the numberical data is invalid.
        case e: NumberFormatException => throw new Exception(s"Invalid numerical data at [$data]")
      }

    }

    val fileReader = new FileReader(sourceFile) // Create a new file reader.

    val lineReader = new BufferedReader(fileReader) // Create a new BufferedReader for reading lines.

    try {

      var inputLine = lineReader.readLine() // Read a line from the file.

      while (inputLine != null) { // Read lines until the end of the file is reached.

        if (inputLine != "") { // Skip empty lines
          inputLine.trim.head match { // Get the first character of the string.
          case '#' => // Do nothing for commented lines.
          case _ => // Take lines without comments.
            val data = getData(inputLine) // Read the data in the line.
            addItem(data._1, data._2, data._3, data._4, data._5) // Add a planet to the items buffer using the read data.
          }
        }
        inputLine = lineReader.readLine() // Read the next line again after processing the previos line.
      }

    } finally {
      // Close all the data streams.
      fileReader.close()
      lineReader.close()
    }
    if (items.size <= 0) throw new Exception("The file did not contain any data!")
    items.toArray // Finally return all the created SpaceObjects as an array.
  }

  // A method for checking if any of the objecs in the system are colliding.
  private def checkForCollisions: Boolean = {
    var isColliding = false   // Initially the return value is false.
    var i = 0   // Index starts at 0.
    while (!isColliding && i < items.length) {  // Continue the loop while a collision has not been found and there are still items left to check.
      items.foreach(n =>
        if (!items(i).eq(n) && items(i).radius >= distanceTo(items(i), n)) isColliding = true  // If the radius of an object is greater or equal to the distance between the two objects, they are colliding.
      )
      i += 1  // Increase index by one.
    }
    isColliding   // Finally return the final value. True if a collision has been found, otherwise will return false.
  }

  // Advances the simulation state by one step.
  def advance() = {
    if (!endReached) { // First check if the end has been reached. If true, don't do anything.
      t = t + step  // Advance the time to t + dt.
      val currentStates = items.map(_.state)  // Get the current states of items (at time t, not t + dt).
      val newStates = integrate(currentStates, step)  // Now calculate the new states at t + dt.
      items.indices.foreach(i => items(i).state = newStates(i))  // Apply the new states to the space objects.
      if (checkForCollisions || t >= duration) endReached = true  // Finally check for any collisions. If a collision has happened, the end of the simulation is reached.
    }
  }

}